// Modified by SolAEcho (2026-10): `//` comments are stripped before statement
// joining, and `signal_groups` is keyed by `(message id, group name)`.
//! Reading and writing of CAN DBC database files.
//! The central type is `DBCFile`: it holds nodes (`BU_`), messages (`BO_`)
//! with their signals (`SG_`), value tables (`VAL_TABLE_`), environment
//! variables (`EV_`), attribute definitions (`BA_DEF_`) and their defaults
//! (`BA_DEF_DEF_`), attribute values (`BA_`), signal groups (`SIG_GROUP_`),
//! and comments (`CM_`).
//! Parsing is deliberately lenient: unrecognized or malformed lines are
//! skipped, and errors are only reported where recovery is impossible
//! (invalid numbers, unterminated multi-line statements). Writing aims to be
//! byte-stable so that parse → write round-trips do not churn files.
//! Notable format details:
//! - Node and environment variable attribute assignments (`BA_ ... BU_` /
//!   `BA_ ... EV_`) attach by node / environment variable name, as the DBC
//!   standard specifies.
//! - Environment variable attributes and comments (`CM_ EV_`) are parsed
//!   but never written.
//! - Floating-point numbers are always written in an invariant,
//!   locale-independent format.
//! - `EnvAccessType::Read` uses the nonstandard value 8001 (the DBC
//!   standard assigns 1) and is written as `DUMMY_NODE_VECTOR8001`; parsing
//!   accepts only "0"/"2"/"3"/"8001" (anything else, including the standard
//!   "1", maps to `Unrestricted`).

use crate::error::{Error, Result};
use indexmap::IndexMap;
use std::borrow::Cow;
use std::collections::{BTreeMap, HashMap};
use std::fmt;
use std::path::Path;

/// Fixed `NS_` block written to every output file.
/// Note: the `NS` symbol list collected during parsing is *not* used when
/// writing; this fixed block is always emitted instead.
pub const NS_BLOCK: &str = "NS_ : \r\n\tNS_DESC_\r\n\tCM_\r\n\tBA_DEF_\r\n\tBA_\r\n\tVAL_\r\n\
\tCAT_DEF_\r\n\tCAT_\r\n\tFILTER\r\n\tBA_DEF_DEF_\r\n\tEV_DATA_\r\n\tENVVAR_DATA_\r\n\
\tSGTYPE_\r\n\tSGTYPE_VAL_\r\n\tBA_DEF_SGTYPE_\r\n\tBA_SGTYPE_\r\n\tSIG_TYPE_REF_\r\n\
\tVAL_TABLE_\r\n\tSIG_GROUP_\r\n\tSIG_VALTYPE_\r\n\tSIGTYPE_VALTYPE_\r\n\tBO_TX_BU_\r\n\
\tBA_DEF_REL_\r\n\tBA_REL_\r\n\tBA_DEF_DEF_REL_\r\n\tBU_SG_REL_\r\n\tBU_EV_REL_\r\n\
\tBU_BO_REL_\r\n\tSG_MUL_VAL_\r\n\r\nBS_:\r\n";

/// Virtual node name defined by the DBC standard; skipped when writing the
/// `BU_:` line.
pub const DUMMY_NODE: &str = "Vector__XXX";

// ---------------------------------------------------------------------------
// Helper functions
// ---------------------------------------------------------------------------

/// Locale-invariant float formatting. Rust's `Display` for `f64` never uses
/// exponential notation, so very small magnitudes print as plain decimals
/// (e.g. `0.00001`) — semantically equivalent to exponential forms and
/// re-parseable either way.
fn fmt_num(v: f64) -> String {
    format!("{v}")
}

/// Round to the nearest integer, ties to even (banker's rounding).
fn to_i64(v: f64) -> i64 {
    v.round_ties_even() as i64
}

/// Trim leading/trailing spaces, tabs, and colons.
fn trim_kws(s: &str) -> &str {
    s.trim_matches(|c| c == ' ' || c == '\t' || c == ':')
}

/// Trim leading/trailing double quotes and semicolons.
fn trim_qs(s: &str) -> &str {
    s.trim_matches(|c| c == '"' || c == ';')
}

/// Parse a number token; unparseable tokens yield 0.0.
fn pnum(tok: &str) -> f64 {
    tok.parse().unwrap_or(0.0)
}

/// Characters accepted in a numeric token.
fn is_num_char(c: char) -> bool {
    c.is_ascii_digit() || matches!(c, '.' | '-' | '+' | 'E' | 'e')
}

/// Line cursor used for tokenizing (avoids a regex dependency).
struct Cur<'a> {
    s: &'a str,
    pos: usize,
}

impl<'a> Cur<'a> {
    fn new(s: &'a str) -> Self {
        Cur { s, pos: 0 }
    }

    fn rest(&self) -> &'a str {
        &self.s[self.pos..]
    }

    fn peek(&self) -> Option<char> {
        self.rest().chars().next()
    }

    fn skip_ws(&mut self) {
        while matches!(self.peek(), Some(' ' | '\t')) {
            self.pos += 1;
        }
    }

    /// Skip at least one whitespace character; `None` when there is none.
    fn ws(&mut self) -> Option<()> {
        if matches!(self.peek(), Some(' ' | '\t')) {
            self.skip_ws();
            Some(())
        } else {
            None
        }
    }

    fn expect(&mut self, c: char) -> Option<()> {
        if self.peek() == Some(c) {
            self.pos += c.len_utf8();
            Some(())
        } else {
            None
        }
    }

    /// Run of consecutive digits.
    fn digits(&mut self) -> &'a str {
        let start = self.pos;
        while matches!(self.peek(), Some(c) if c.is_ascii_digit()) {
            self.pos += 1;
        }
        &self.s[start..self.pos]
    }

    /// Numeric token (`[\d.\-+Ee]+`).
    fn num_token(&mut self) -> &'a str {
        let start = self.pos;
        while matches!(self.peek(), Some(c) if is_num_char(c)) {
            self.pos += 1;
        }
        &self.s[start..self.pos]
    }

    /// Non-whitespace token.
    fn token(&mut self) -> &'a str {
        let start = self.pos;
        while let Some(c) = self.peek() {
            if c == ' ' || c == '\t' {
                break;
            }
            self.pos += c.len_utf8();
        }
        &self.s[start..self.pos]
    }

    /// Token up to whitespace or `:`.
    fn token_no_colon(&mut self) -> &'a str {
        let start = self.pos;
        while let Some(c) = self.peek() {
            if c == ' ' || c == '\t' || c == ':' {
                break;
            }
            self.pos += c.len_utf8();
        }
        &self.s[start..self.pos]
    }

    /// Quoted string `"..."`; returns the contents, `None` when unquoted.
    fn quoted(&mut self) -> Option<&'a str> {
        self.expect('"')?;
        let start = self.pos;
        let end = self.s[start..].find('"')? + start;
        self.pos = end + 1;
        Some(&self.s[start..end])
    }
}

// ---------------------------------------------------------------------------
// Enumerations
// ---------------------------------------------------------------------------

/// Kind of object an attribute definition attaches to.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, PartialOrd, Ord)]
pub enum AttribDefObjectType {
    /// Network level (`BA_DEF_ "name" ...`).
    #[default]
    Network,
    /// Node (`BU_`).
    Bu,
    /// Message (`BO_`).
    Bo,
    /// Signal (`SG_`).
    Sg,
    /// Environment variable (`EV_`).
    Ev,
    /// Node-to-transmitted-message relation (`BU_BO_REL_`).
    BuBoRel,
    /// Node-to-mapped-received-signal relation (`BU_SG_REL_`).
    BuSgRel,
    /// Node-to-environment-variable relation (`BU_EV_REL_`).
    BuEvRel,
}

impl AttribDefObjectType {
    /// DBC keyword (Network has none).
    pub fn keyword(self) -> &'static str {
        match self {
            Self::Network => "",
            Self::Bu => "BU_",
            Self::Bo => "BO_",
            Self::Sg => "SG_",
            Self::Ev => "EV_",
            Self::BuBoRel => "BU_BO_REL_",
            Self::BuSgRel => "BU_SG_REL_",
            Self::BuEvRel => "BU_EV_REL_",
        }
    }

    /// Parse from a DBC keyword; unknown keywords yield `None` (callers fall
    /// back to `Network`).
    pub fn from_keyword(kw: &str) -> Option<Self> {
        Some(match kw {
            "BU_" => Self::Bu,
            "BO_" => Self::Bo,
            "SG_" => Self::Sg,
            "EV_" => Self::Ev,
            "BU_BO_REL_" => Self::BuBoRel,
            "BU_SG_REL_" => Self::BuSgRel,
            "BU_EV_REL_" => Self::BuEvRel,
            _ => return None,
        })
    }
}

impl fmt::Display for AttribDefObjectType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.keyword())
    }
}

/// Data type of an attribute value.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub enum AttribDefValueType {
    /// `STRING`.
    #[default]
    Str,
    /// `INT`.
    Int,
    /// `FLOAT`.
    Float,
    /// `HEX`.
    Hex,
    /// `ENUM`.
    Enum,
}

impl AttribDefValueType {
    /// DBC keyword.
    pub fn keyword(self) -> &'static str {
        match self {
            Self::Str => "STRING",
            Self::Int => "INT",
            Self::Float => "FLOAT",
            Self::Hex => "HEX",
            Self::Enum => "ENUM",
        }
    }

    /// Parse from a DBC keyword (case-sensitive).
    pub fn from_keyword(kw: &str) -> Option<Self> {
        Some(match kw {
            "STRING" => Self::Str,
            "INT" => Self::Int,
            "FLOAT" => Self::Float,
            "HEX" => Self::Hex,
            "ENUM" => Self::Enum,
            _ => return None,
        })
    }
}

impl fmt::Display for AttribDefValueType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.keyword())
    }
}

/// Environment variable access type.
/// `Read` uses the nonstandard value 8001 (the DBC standard assigns 1) and
/// is written as `DUMMY_NODE_VECTOR8001`; the value is preserved verbatim so
/// that files round-trip unchanged.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub enum EnvAccessType {
    /// `DUMMY_NODE_VECTOR0`.
    #[default]
    Unrestricted,
    /// `DUMMY_NODE_VECTOR2`.
    Write,
    /// `DUMMY_NODE_VECTOR3`.
    ReadWrite,
    /// `DUMMY_NODE_VECTOR8001` (nonstandard; see the module documentation).
    Read,
}

impl EnvAccessType {
    /// Numeric value (used when writing).
    pub fn as_i32(self) -> i32 {
        match self {
            Self::Unrestricted => 0,
            Self::Write => 2,
            Self::ReadWrite => 3,
            Self::Read => 8001,
        }
    }

    /// Parse from the access_type token: the `DUMMY_NODE_VECTOR` prefix is
    /// stripped first. "0" → Unrestricted, "2" → Write, "3" → ReadWrite,
    /// "8001" → Read, anything else → Unrestricted (so the standard value
    /// "1" also maps to Unrestricted).
    pub fn from_dbc_token(tok: &str) -> Self {
        match tok.replace("DUMMY_NODE_VECTOR", "").as_str() {
            "2" => Self::Write,
            "3" => Self::ReadWrite,
            "8001" => Self::Read,
            _ => Self::Unrestricted,
        }
    }
}

/// Environment variable data type.
/// Numeric values without a defined meaning (e.g. "2") parse successfully
/// and are written back unchanged, so they are preserved via `Other`.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub enum EnvType {
    /// Integer (0).
    #[default]
    Integer,
    /// Float (1).
    Float,
    /// Any other (undefined) numeric value, preserved verbatim.
    Other(i32),
}

impl EnvType {
    /// Numeric value (used when writing).
    pub fn as_i32(self) -> i32 {
        match self {
            Self::Integer => 0,
            Self::Float => 1,
            Self::Other(n) => n,
        }
    }

    /// Convert from the numeric value used in the file.
    pub fn from_i32(n: i32) -> Self {
        match n {
            0 => Self::Integer,
            1 => Self::Float,
            other => Self::Other(other),
        }
    }
}

/// Signal floating-point type (`SIG_VALTYPE_`).
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub enum SignalFloatType {
    /// Integer signal.
    #[default]
    Integer,
    /// IEEE 32-bit float (`SIG_VALTYPE_ id sig: 1;`).
    IeeeFloat32,
    /// IEEE 64-bit float (`SIG_VALTYPE_ id sig: 2;`).
    IeeeFloat64,
}

/// Signal bit flags (byte order, sign, multiplexing).
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub struct SignalFlagsType(u8);

impl SignalFlagsType {
    /// Big-endian (Motorola).
    pub const BIG_ENDIAN: Self = Self(1);
    /// Signed.
    pub const SIGNED: Self = Self(2);
    /// Multiplexor (switch) signal.
    pub const MULTIPLEX_SIGNAL: Self = Self(4);
    /// Multiplexed signal.
    pub const MULTIPLEXED: Self = Self(8);

    /// Raw bit value.
    pub fn bits(self) -> u8 {
        self.0
    }

    /// Whether the given flag is set.
    pub fn contains(self, f: Self) -> bool {
        self.0 & f.0 != 0
    }

    fn set(&mut self, f: Self, on: bool) {
        if on {
            self.0 |= f.0;
        } else {
            self.0 &= !f.0;
        }
    }
}

// ---------------------------------------------------------------------------
// Attribute definitions and values
// ---------------------------------------------------------------------------

/// Default value of an attribute definition (text for STRING/ENUM, number
/// for INT/FLOAT/HEX).
#[derive(Debug, Clone, PartialEq)]
pub enum AttribDefault {
    /// Text (STRING/ENUM, without quotes).
    Text(String),
    /// Number (INT/FLOAT/HEX).
    Number(f64),
}

/// Attribute definition (`BA_DEF_`).
#[derive(Debug, Clone, Default, PartialEq)]
pub struct AttribDefType {
    /// Attribute name.
    pub name: String,
    /// Kind of object the attribute attaches to.
    pub object_type: AttribDefObjectType,
    /// Data type.
    pub data_type: AttribDefValueType,
    /// Minimum (INT/FLOAT/HEX).
    pub min: f64,
    /// Maximum (INT/FLOAT/HEX).
    pub max: f64,
    /// Enum candidates (ENUM).
    pub enums: Vec<String>,
    /// Default value (`BA_DEF_DEF_`).
    pub default_value: Option<AttribDefault>,
}

impl AttribDefType {
    /// Create an attribute definition with the given name.
    pub fn new(name: impl Into<String>) -> Self {
        AttribDefType {
            name: name.into(),
            ..Default::default()
        }
    }

    /// `BA_DEF_` line.
    /// Format: `BA_DEF_ {obj} "{name}" {TYPE} ...;`; for Network, `obj` is
    /// empty (two spaces after `BA_DEF_`); STRING has no min/max (a space is
    /// kept before `;`).
    pub(crate) fn file_line(&self) -> String {
        let mut s = format!(
            "BA_DEF_ {} \"{}\" {} ",
            self.object_type, self.name, self.data_type
        );
        match self.data_type {
            AttribDefValueType::Int | AttribDefValueType::Hex => {
                // min/max are rounded to integers, ties to even
                s.push_str(&format!("{} {}", to_i64(self.min), to_i64(self.max)));
            }
            AttribDefValueType::Float => {
                s.push_str(&format!("{} {}", fmt_num(self.min), fmt_num(self.max)));
            }
            AttribDefValueType::Enum => {
                for e in &self.enums {
                    s.push_str(&format!("\"{e}\","));
                }
                if !self.enums.is_empty() {
                    s.pop();
                }
            }
            AttribDefValueType::Str => {}
        }
        s.push(';');
        s
    }

    /// `BA_DEF_DEF_` line; `None` when there is no default value (the caller
    /// then writes an empty line).
    pub(crate) fn default_line(&self) -> Option<String> {
        let dv = self.default_value.as_ref()?;
        let val = match (&self.data_type, dv) {
            (AttribDefValueType::Str | AttribDefValueType::Enum, AttribDefault::Text(t)) => {
                format!("\"{t}\"")
            }
            (AttribDefValueType::Int | AttribDefValueType::Hex, AttribDefault::Number(n)) => {
                to_i64(*n).to_string()
            }
            (AttribDefValueType::Float, AttribDefault::Number(n)) => fmt_num(*n),
            // The arms below are tolerant fallbacks for manually constructed
            // type mismatches (the parse path never produces them)
            (AttribDefValueType::Str | AttribDefValueType::Enum, AttribDefault::Number(n)) => {
                format!("\"{}\"", fmt_num(*n))
            }
            (AttribDefValueType::Int | AttribDefValueType::Hex, AttribDefault::Text(t)) => {
                t.parse::<i64>().unwrap_or(0).to_string()
            }
            (AttribDefValueType::Float, AttribDefault::Text(t)) => {
                fmt_num(t.parse::<f64>().unwrap_or(0.0))
            }
        };
        Some(format!("BA_DEF_DEF_ \"{}\" {};", self.name, val))
    }
}

/// Target object of an attribute value assignment.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum AttrTarget<'a> {
    /// Network-level attribute.
    Network,
    /// Node attribute.
    Node(&'a str),
    /// Message attribute (message ID).
    Message(u32),
    /// Signal attribute (message ID + signal name).
    Signal(u32, &'a str),
}

/// Attribute value (`BA_`).
/// The value is associated with its definition by name; the definition's
/// data type is looked up in `DBCFile::attribute_definitions` when writing.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct AttributeType {
    /// Attribute definition name.
    pub attrib_def: String,
    /// Raw value text: STRING/ENUM without quotes, INT/FLOAT/HEX as decimal text.
    pub value: String,
}

impl AttributeType {
    /// Create an attribute value for the named definition.
    pub fn new(attrib_def: impl Into<String>, value: impl Into<String>) -> Self {
        AttributeType {
            attrib_def: attrib_def.into(),
            value: value.into(),
        }
    }

    /// Display text: an ENUM value holding a valid index maps to the enum's text.
    pub fn display(&self, def: Option<&AttribDefType>) -> String {
        if let Some(d) = def {
            if d.data_type == AttribDefValueType::Enum {
                if let Ok(i) = self.value.parse::<usize>() {
                    if i < d.enums.len() {
                        return d.enums[i].clone();
                    }
                }
            }
        }
        self.value.clone()
    }

    /// `BA_` line. `data_type` is the definition's data type (`None` when the
    /// definition is missing; the value is then written as-is, without quoting).
    pub(crate) fn file_line(
        &self,
        data_type: Option<AttribDefValueType>,
        target: AttrTarget,
    ) -> String {
        let text = if data_type == Some(AttribDefValueType::Str) {
            format!("\"{}\"", self.value)
        } else {
            self.value.clone()
        };
        match target {
            AttrTarget::Network => format!("BA_ \"{}\" {};", self.attrib_def, text),
            AttrTarget::Node(node) => format!("BA_ \"{}\" BU_ {} {};", self.attrib_def, node, text),
            AttrTarget::Message(id) => format!("BA_ \"{}\" BO_ {} {};", self.attrib_def, id, text),
            AttrTarget::Signal(id, sig) => {
                format!("BA_ \"{}\" SG_ {} {} {};", self.attrib_def, id, sig, text)
            }
        }
    }
}

// ---------------------------------------------------------------------------
// Value tables
// ---------------------------------------------------------------------------

/// Value table (`VAL_TABLE_`).
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct ValueTable {
    /// Table name.
    pub name: String,
    /// Value → description (insertion order).
    pub enums: IndexMap<i64, String>,
}

impl ValueTable {
    /// Create a value table with the given name.
    pub fn new(name: impl Into<String>) -> Self {
        ValueTable {
            name: name.into(),
            ..Default::default()
        }
    }

    /// `VAL_TABLE_` line (no space before `;`; an empty table keeps one space).
    pub(crate) fn file_line(&self) -> String {
        let mut s = format!("VAL_TABLE_ {} ", self.name);
        for (k, v) in &self.enums {
            s.push_str(&format!("{k} \"{v}\" "));
        }
        if !self.enums.is_empty() {
            s.pop();
        }
        s.push(';');
        s
    }
}

impl fmt::Display for ValueTable {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(&self.name)
    }
}

// ---------------------------------------------------------------------------
// Signals
// ---------------------------------------------------------------------------

/// Signal (`SG_`).
/// Runtime reception state (frame caches, timestamps) is not part of this
/// type; the owning message is expressed through `MsgType::signals`.
#[derive(Debug, Clone, PartialEq)]
pub struct SignalType {
    /// Signal name.
    pub name: String,
    /// Start bit within the message (0-based; the MSB index for big-endian signals).
    pub start: u16,
    /// Length in bits.
    pub len: u16,
    /// Multiplex switch value (only meaningful when `is_multiplexed`).
    pub multiplex_value: u32,
    /// Conversion factor (raw * factor + offset = physical).
    pub factor: f64,
    /// Physical offset.
    pub offset: f64,
    /// Physical minimum.
    pub min: f64,
    /// Physical maximum.
    pub max: f64,
    /// Unit.
    pub unit: String,
    /// Receiver node names.
    pub receivers: Vec<String>,
    /// Bit flags (byte order / sign / multiplexing).
    pub flags: SignalFlagsType,
    /// Float type (`SIG_VALTYPE_`).
    pub float_type: SignalFloatType,
    /// Comment (`CM_ SG_`).
    pub comment: String,
    /// Value descriptions (`VAL_`; `None` when the message has no `VAL_`
    /// entry for this signal).
    pub enums: Option<IndexMap<i64, String>>,
    /// Signal attributes (`BA_ ... SG_`).
    pub attributes: IndexMap<String, AttributeType>,
}

impl SignalType {
    /// Create a signal with the given name and comment.
    pub fn new(name: impl Into<String>, comment: impl Into<String>) -> Self {
        SignalType {
            name: name.into(),
            comment: comment.into(),
            factor: 1.0,
            ..Default::default()
        }
    }

    /// Bit mask with `len` bits set (2^len - 1); `len >= 64` yields `u64::MAX`.
    pub fn mask(&self) -> u64 {
        if self.len >= 64 {
            u64::MAX
        } else {
            (1u64 << self.len) - 1
        }
    }
}

impl Default for SignalType {
    fn default() -> Self {
        Self {
            name: String::new(),
            start: 0,
            len: 0,
            multiplex_value: 0,
            factor: 1.0,
            offset: 0.0,
            min: 0.0,
            max: 0.0,
            unit: String::new(),
            receivers: Vec::new(),
            flags: SignalFlagsType::default(),
            float_type: SignalFloatType::Integer,
            comment: String::new(),
            enums: None,
            attributes: IndexMap::new(),
        }
    }
}

impl SignalType {
    /// Returns `true` for Intel byte order.
    pub fn is_little_endian(&self) -> bool {
        !self.flags.contains(SignalFlagsType::BIG_ENDIAN)
    }

    /// Selects Intel (`true`) or Motorola (`false`) byte order.
    pub fn set_little_endian(&mut self, little: bool) {
        self.flags.set(SignalFlagsType::BIG_ENDIAN, !little);
    }

    /// Returns whether integer values use signed interpretation.
    pub fn is_signed(&self) -> bool {
        self.flags.contains(SignalFlagsType::SIGNED)
    }

    /// Changes signed integer interpretation.
    pub fn set_signed(&mut self, signed: bool) {
        self.flags.set(SignalFlagsType::SIGNED, signed);
    }

    /// Returns whether this signal selects a multiplex branch.
    pub fn is_multiplex_signal(&self) -> bool {
        self.flags.contains(SignalFlagsType::MULTIPLEX_SIGNAL)
    }

    /// Marks this signal as the multiplex selector.
    pub fn set_multiplex_signal(&mut self, on: bool) {
        self.flags.set(SignalFlagsType::MULTIPLEX_SIGNAL, on);
    }

    /// Returns whether this signal belongs to a multiplex branch.
    pub fn is_multiplexed(&self) -> bool {
        self.flags.contains(SignalFlagsType::MULTIPLEXED)
    }

    /// Changes the multiplex-branch flag.
    pub fn set_multiplexed(&mut self, on: bool) {
        self.flags.set(SignalFlagsType::MULTIPLEXED, on);
    }

    /// Sets the encoded numeric type and enforces its required width.
    pub fn set_float_type(&mut self, value: SignalFloatType) {
        self.float_type = value;
        match value {
            SignalFloatType::IeeeFloat32 => {
                self.len = 32;
                self.set_signed(true);
            }
            SignalFloatType::IeeeFloat64 => {
                self.len = 64;
                self.set_signed(true);
            }
            SignalFloatType::Integer => {}
        }
    }

    /// Returns the linear bit position used for bounds checking.
    pub fn get_lsb_start(&self) -> u16 {
        if self.is_little_endian() {
            return self.start;
        }
        let in_byte = 7 - self.start % 8;
        if u32::from(in_byte) + u32::from(self.len.saturating_sub(1)) < 8 {
            self.start.saturating_sub(self.len.saturating_sub(1))
        } else {
            (self.start & !7) + in_byte
        }
    }

    /// Extracts the raw bit pattern from a CAN payload.
    /// Zero-width, wider-than-64, and out-of-range signals return `None`.
    pub fn extract_raw(&self, data: &[u8]) -> Option<i64> {
        if self.len == 0 || self.len > 64 || data.len() > 16 {
            return None;
        }
        let bit_len = data.len().checked_mul(8)?;
        if self.is_little_endian() {
            let end = usize::from(self.start).checked_add(usize::from(self.len))?;
            if end > bit_len {
                return None;
            }
            let mut word = 0u128;
            for (index, byte) in data.iter().enumerate() {
                word |= u128::from(*byte) << (index * 8);
            }
            Some(((word >> self.start) & u128::from(self.mask())) as u64 as i64)
        } else {
            let linear_start = usize::from((self.start & !7) + (7 - self.start % 8));
            let end = linear_start.checked_add(usize::from(self.len))?;
            if end > bit_len {
                return None;
            }
            let mut word = 0u128;
            for byte in data {
                word = (word << 8) | u128::from(*byte);
            }
            let shift = bit_len - end;
            Some(((word >> shift) & u128::from(self.mask())) as u64 as i64)
        }
    }

    /// Applies the signal's numeric interpretation, factor, and offset.
    pub fn to_physical(&self, raw: i64) -> f64 {
        let value = match self.float_type {
            SignalFloatType::IeeeFloat32 => f32::from_bits(raw as u32) as f64,
            SignalFloatType::IeeeFloat64 => f64::from_bits(raw as u64),
            SignalFloatType::Integer if self.is_signed() => match self.len {
                8 => (raw as i8) as f64,
                16 => (raw as i16) as f64,
                32 => (raw as i32) as f64,
                64 => raw as f64,
                _ => raw as f64,
            },
            SignalFloatType::Integer if self.len == 64 => (raw as u64) as f64,
            SignalFloatType::Integer => raw as f64,
        };
        value * self.factor + self.offset
    }

    fn file_line(&self) -> String {
        let mux = if self.is_multiplex_signal() {
            " M".to_string()
        } else if self.is_multiplexed() {
            format!(" m{}", self.multiplex_value)
        } else {
            String::new()
        };
        let byte_order = if self.is_little_endian() { '1' } else { '0' };
        let sign = if self.is_signed() { '-' } else { '+' };
        let mut line = format!(
            " SG_ {}{} : {}|{}@{}{} ({},{}) [{}|{}] \"{}\"",
            self.name,
            mux,
            self.start,
            self.len,
            byte_order,
            sign,
            fmt_num(self.factor),
            fmt_num(self.offset),
            fmt_num(self.min),
            fmt_num(self.max),
            self.unit
        );
        if !self.receivers.is_empty() {
            line.push(' ');
            line.push_str(&self.receivers.join(","));
        }
        line
    }

    fn comment_line(&self, msg_id: u32) -> Option<String> {
        (!self.comment.is_empty())
            .then(|| format!("CM_ SG_ {msg_id} {} \"{}\";", self.name, self.comment))
    }

    fn value_line(&self, msg_id: u32) -> Option<String> {
        let values = self.enums.as_ref().filter(|values| !values.is_empty())?;
        let mut line = format!("VAL_ {msg_id} {} ", self.name);
        for (value, text) in values {
            line.push_str(&format!("{value} \"{text}\" "));
        }
        line.push(';');
        Some(line)
    }
}

// ---------------------------------------------------------------------------
// Messages, nodes, environment variables, and signal groups
// ---------------------------------------------------------------------------

/// CAN message (`BO_`) and its signals.
#[derive(Debug, Clone, Default, PartialEq)]
pub struct MsgType {
    pub id: u32,
    pub name: String,
    pub dlc: u8,
    pub source: String,
    pub comment: String,
    pub signals: Vec<SignalType>,
    pub attributes: IndexMap<String, AttributeType>,
}

impl MsgType {
    pub fn new(name: impl Into<String>, comment: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            comment: comment.into(),
            ..Default::default()
        }
    }

    /// Sets a classic or CAN FD payload length.
    pub fn set_dlc(&mut self, dlc: u8) -> Result<()> {
        if dlc <= 8 || matches!(dlc, 12 | 16 | 20 | 24 | 32 | 48 | 64) {
            self.dlc = dlc;
            Ok(())
        } else {
            Err(Error::Write(format!(
                "unsupported CAN payload length {dlc}"
            )))
        }
    }

    pub fn get_signal(&self, signal_name: &str) -> Option<&SignalType> {
        self.signals
            .iter()
            .find(|signal| signal.name == signal_name)
    }

    pub fn get_signal_mut(&mut self, signal_name: &str) -> Option<&mut SignalType> {
        self.signals
            .iter_mut()
            .find(|signal| signal.name == signal_name)
    }

    pub fn get_multiplex_signal(&self) -> Option<&SignalType> {
        self.signals
            .first()
            .filter(|signal| signal.is_multiplex_signal())
    }

    pub fn add_signal(&mut self, signal: SignalType) {
        if signal.is_multiplex_signal() {
            self.signals.insert(0, signal);
        } else {
            self.signals.push(signal);
        }
    }

    fn file_line(&self) -> String {
        format!(
            "BO_ {} {}: {} {}",
            self.id, self.name, self.dlc, self.source
        )
    }

    fn comment_line(&self) -> Option<String> {
        (!self.comment.is_empty()).then(|| format!("CM_ BO_ {} \"{}\";", self.id, self.comment))
    }
}

/// Network node (`BU_`) and the messages it transmits.
#[derive(Debug, Clone, Default, PartialEq)]
pub struct SourceType {
    pub name: String,
    pub comment: String,
    pub messages: Vec<MsgType>,
    pub attributes: IndexMap<String, AttributeType>,
}

impl SourceType {
    pub fn new(name: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            ..Default::default()
        }
    }

    fn comment_line(&self) -> Option<String> {
        (!self.comment.is_empty()).then(|| format!("CM_ BU_ {} \"{}\";", self.name, self.comment))
    }
}

/// Environment variable (`EV_`).
#[derive(Debug, Clone, Default, PartialEq)]
pub struct EnvironmentType {
    pub name: String,
    pub env_type: EnvType,
    pub min: f64,
    pub max: f64,
    pub unit: String,
    pub start_value: f64,
    pub index: i32,
    pub access: EnvAccessType,
    pub receivers: Vec<String>,
    pub comment: String,
    pub attributes: IndexMap<String, AttributeType>,
}

impl EnvironmentType {
    pub fn new(name: impl Into<String>, comment: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            comment: comment.into(),
            ..Default::default()
        }
    }

    fn file_line(&self) -> String {
        let mut line = format!(
            "EV_ {}: {} [{}|{}] \"{}\" {} {} DUMMY_NODE_VECTOR{}",
            self.name,
            self.env_type.as_i32(),
            fmt_num(self.min),
            fmt_num(self.max),
            self.unit,
            fmt_num(self.start_value),
            self.index,
            self.access.as_i32()
        );
        if !self.receivers.is_empty() {
            line.push(' ');
            line.push_str(&self.receivers.join(","));
        }
        line.push(';');
        line
    }
}

/// Signal group (`SIG_GROUP_`).
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct SignalGroupType {
    pub id: u32,
    pub name: String,
    pub number: i32,
    pub signal_refs: Vec<String>,
}

impl SignalGroupType {
    pub fn new(id: u32, name: impl Into<String>, number: i32) -> Self {
        Self {
            id,
            name: name.into(),
            number,
            signal_refs: Vec::new(),
        }
    }

    fn file_line(&self) -> String {
        let suffix = if self.signal_refs.is_empty() {
            String::new()
        } else {
            format!(" {}", self.signal_refs.join(" "))
        };
        format!(
            "SIG_GROUP_ {} {} {}:{};",
            self.id, self.name, self.number, suffix
        )
    }
}

/// Object associated with a plausibility issue.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PlausTarget {
    Signal { msg_id: u32, signal: String },
    Message { msg_id: u32, name: String },
}

/// A non-fatal consistency issue found in a parsed database.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PlausibilityIssue {
    pub target: PlausTarget,
    pub message: String,
}

// ---------------------------------------------------------------------------
// Database parsing and writing
// ---------------------------------------------------------------------------

/// Editable DBC database.
#[derive(Debug, Clone, Default)]
pub struct DBCFile {
    pub source_file: Option<String>,
    pub version: String,
    pub comment: String,
    pub ns: Vec<String>,
    pub sources: BTreeMap<String, SourceType>,
    pub value_tables: IndexMap<String, ValueTable>,
    pub environments: BTreeMap<String, EnvironmentType>,
    pub attribute_definitions: BTreeMap<String, AttribDefType>,
    /// Keyed by `(message id, group name)`; a message may own several groups.
    pub signal_groups: IndexMap<(u32, String), SignalGroupType>,
    pub attributes: IndexMap<String, AttributeType>,
}

impl DBCFile {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn with_version(version: impl Into<String>) -> Self {
        Self {
            version: version.into(),
            ..Default::default()
        }
    }

    /// Parses DBC text into an editable object model.
    pub fn parse_str(text: &str) -> Result<Self> {
        let statements = logical_lines(text)?;
        let mut file = Self::new();
        let mut current_message: Option<(String, usize)> = None;
        let mut message_locations = HashMap::new();
        let mut in_ns = false;

        for (line_no, statement) in statements {
            let line = statement.trim();
            if line.is_empty() {
                continue;
            }
            if in_ns {
                if line.starts_with("BS_") {
                    in_ns = false;
                    continue;
                }
                if line.starts_with("BU_") {
                    in_ns = false;
                } else {
                    file.ns.push(trim_kws(line).to_string());
                    continue;
                }
            }
            let (keyword, rest) = split_keyword(line);
            match keyword {
                "VERSION" => file.version = trim_qs(rest.trim()).to_string(),
                "NS_" => {
                    in_ns = true;
                    if !trim_kws(rest).is_empty() {
                        file.ns.push("NS_".to_string());
                    }
                }
                "BS_" => in_ns = false,
                "BU_" => {
                    for name in trim_kws(rest).split_whitespace() {
                        file.sources.insert(name.to_string(), SourceType::new(name));
                    }
                }
                "BO_" => {
                    if let Some(message) = parse_message(rest) {
                        let source_name = message.source.clone();
                        let source = file
                            .sources
                            .entry(source_name.clone())
                            .or_insert_with(|| SourceType::new(&source_name));
                        let message_index = source.messages.len();
                        message_locations
                            .entry(message.id)
                            .or_insert_with(|| (source_name.clone(), message_index));
                        source.messages.push(message);
                        current_message = Some((source_name, message_index));
                    }
                }
                "SG_" => {
                    if let (Some((source_name, message_index)), Some(signal)) =
                        (current_message.as_ref(), parse_signal(rest))
                    {
                        if let Some(message) = file
                            .sources
                            .get_mut(source_name)
                            .and_then(|source| source.messages.get_mut(*message_index))
                        {
                            message.add_signal(signal);
                        }
                    }
                }
                "EV_" => {
                    if let Some(environment) = parse_environment(rest) {
                        file.environments
                            .insert(environment.name.clone(), environment);
                    }
                }
                "VAL_TABLE_" => {
                    if let Some(table) = parse_value_table(rest) {
                        file.value_tables.insert(table.name.clone(), table);
                    }
                }
                "BA_DEF_" | "BA_DEF_REL_" => {
                    if let Some(definition) = parse_attribute_definition(rest) {
                        file.attribute_definitions
                            .insert(definition.name.clone(), definition);
                    }
                }
                "BA_DEF_DEF_" | "BA_DEF_DEF_REL_" => {
                    parse_attribute_default(&mut file, rest);
                }
                "BA_" => parse_attribute(&mut file, &message_locations, rest),
                "CM_" => parse_comment(&mut file, &message_locations, rest),
                "VAL_" => parse_signal_values(&mut file, &message_locations, rest),
                "SIG_GROUP_" => {
                    if let Some(group) = parse_signal_group(rest) {
                        file.signal_groups
                            .insert((group.id, group.name.clone()), group);
                    }
                }
                "SIG_VALTYPE_" => parse_signal_float_type(&mut file, &message_locations, rest),
                _ => {}
            }

            if statement_has_unterminated_quote(line) {
                return Err(Error::Parse {
                    line: line_no,
                    message: "unterminated quoted string".to_string(),
                });
            }
        }
        Ok(file)
    }

    /// Parses a DBC file using UTF-8 text decoding.
    pub fn parse_file(path: impl AsRef<Path>) -> Result<Self> {
        let path = path.as_ref();
        let text = std::fs::read_to_string(path)?;
        let mut file = Self::parse_str(&text)?;
        file.source_file = Some(path.to_string_lossy().into_owned());
        Ok(file)
    }

    /// Serializes this database using CRLF line endings.
    pub fn write_string(&self) -> String {
        let mut output = String::new();
        push_line(&mut output, &format!("VERSION \"{}\"", self.version));
        push_line(&mut output, "");
        output.push_str(NS_BLOCK);
        push_line(&mut output, "");

        let nodes = self
            .sources
            .keys()
            .filter(|name| name.as_str() != DUMMY_NODE)
            .cloned()
            .collect::<Vec<_>>();
        let node_line = if nodes.is_empty() {
            "BU_:".to_string()
        } else {
            format!("BU_: {}", nodes.join(" "))
        };
        push_line(&mut output, &node_line);
        push_line(&mut output, "");

        for table in self.value_tables.values() {
            push_line(&mut output, &table.file_line());
        }
        push_line(&mut output, "");

        for source in self.sources.values() {
            for message in &source.messages {
                push_line(&mut output, "");
                push_line(&mut output, &message.file_line());
                for signal in &message.signals {
                    push_line(&mut output, &signal.file_line());
                }
            }
        }

        push_line(&mut output, "");
        for environment in self.environments.values() {
            push_line(&mut output, &environment.file_line());
        }
        push_line(&mut output, "");

        if !self.comment.is_empty() {
            push_line(&mut output, &format!("CM_ \"{}\";", self.comment));
        }
        for source in self.sources.values() {
            if let Some(line) = source.comment_line() {
                push_line(&mut output, &line);
            }
            for message in &source.messages {
                if let Some(line) = message.comment_line() {
                    push_line(&mut output, &line);
                }
                for signal in &message.signals {
                    if let Some(line) = signal.comment_line(message.id) {
                        push_line(&mut output, &line);
                    }
                }
            }
        }
        push_line(&mut output, "");

        for definition in self.attribute_definitions.values() {
            push_line(&mut output, &definition.file_line());
        }
        push_line(&mut output, "");
        for definition in self.attribute_definitions.values() {
            if let Some(line) = definition.default_line() {
                push_line(&mut output, &line);
            }
        }
        push_line(&mut output, "");

        for attribute in self.attributes.values() {
            let data_type = self
                .attribute_definitions
                .get(&attribute.attrib_def)
                .map(|definition| definition.data_type);
            push_line(
                &mut output,
                &attribute.file_line(data_type, AttrTarget::Network),
            );
        }
        for source in self.sources.values() {
            for attribute in source.attributes.values() {
                let data_type = self
                    .attribute_definitions
                    .get(&attribute.attrib_def)
                    .map(|definition| definition.data_type);
                push_line(
                    &mut output,
                    &attribute.file_line(data_type, AttrTarget::Node(&source.name)),
                );
            }
            for message in &source.messages {
                for attribute in message.attributes.values() {
                    let data_type = self
                        .attribute_definitions
                        .get(&attribute.attrib_def)
                        .map(|definition| definition.data_type);
                    push_line(
                        &mut output,
                        &attribute.file_line(data_type, AttrTarget::Message(message.id)),
                    );
                }
                for signal in &message.signals {
                    for attribute in signal.attributes.values() {
                        let data_type = self
                            .attribute_definitions
                            .get(&attribute.attrib_def)
                            .map(|definition| definition.data_type);
                        push_line(
                            &mut output,
                            &attribute
                                .file_line(data_type, AttrTarget::Signal(message.id, &signal.name)),
                        );
                    }
                }
            }
        }

        for source in self.sources.values() {
            for message in &source.messages {
                for signal in &message.signals {
                    if let Some(line) = signal.value_line(message.id) {
                        push_line(&mut output, &line);
                    }
                }
            }
        }
        for source in self.sources.values() {
            for message in &source.messages {
                for signal in &message.signals {
                    let code = match signal.float_type {
                        SignalFloatType::IeeeFloat32 => Some(1),
                        SignalFloatType::IeeeFloat64 => Some(2),
                        SignalFloatType::Integer => None,
                    };
                    if let Some(code) = code {
                        push_line(
                            &mut output,
                            &format!("SIG_VALTYPE_ {} {}: {code};", message.id, signal.name),
                        );
                    }
                }
            }
        }
        push_line(&mut output, "");
        for group in self.signal_groups.values() {
            push_line(&mut output, &group.file_line());
        }
        push_line(&mut output, "");
        output
    }

    /// Writes UTF-8 without a byte-order mark.
    pub fn save(&mut self, path: impl AsRef<Path>) -> Result<()> {
        let path = path.as_ref();
        std::fs::write(path, self.write_string())?;
        self.source_file = Some(path.to_string_lossy().into_owned());
        Ok(())
    }

    pub fn count_sources(&self) -> usize {
        self.sources.len()
    }

    pub fn count_messages(&self) -> usize {
        self.sources
            .values()
            .map(|source| source.messages.len())
            .sum()
    }

    pub fn count_signals(&self) -> usize {
        self.messages().map(|message| message.signals.len()).sum()
    }

    pub fn messages(&self) -> impl Iterator<Item = &MsgType> {
        self.sources
            .values()
            .flat_map(|source| source.messages.iter())
    }

    pub fn get_message(&self, id: u32) -> Option<&MsgType> {
        self.messages().find(|message| message.id == id)
    }

    /// Reports inconsistent limits and multiplex configurations.
    pub fn plausibility_check(&self) -> Vec<PlausibilityIssue> {
        let mut issues = Vec::new();
        for message in self.messages() {
            let mut has_multiplexed = false;
            for signal in &message.signals {
                has_multiplexed |= signal.is_multiplexed();
                if signal.min > signal.max {
                    issues.push(PlausibilityIssue {
                        target: PlausTarget::Signal {
                            msg_id: message.id,
                            signal: signal.name.clone(),
                        },
                        message: "Minimum is greater than maximum".to_string(),
                    });
                }
            }
            match (has_multiplexed, message.get_multiplex_signal()) {
                (true, None) => issues.push(PlausibilityIssue {
                    target: PlausTarget::Message {
                        msg_id: message.id,
                        name: message.name.clone(),
                    },
                    message: "Multiplexed signals present but no multiplexor signal".to_string(),
                }),
                (false, Some(selector)) => issues.push(PlausibilityIssue {
                    target: PlausTarget::Message {
                        msg_id: message.id,
                        name: message.name.clone(),
                    },
                    message: format!(
                        "Multiplexor signal '{}' present but no multiplexed signals",
                        selector.name
                    ),
                }),
                _ => {}
            }
        }
        issues
    }

    /// Validates identifier syntax and workspace-wide uniqueness.
    pub fn check_identifier(&self, name: &str) -> Result<bool> {
        if name.is_empty() || name.len() > 255 {
            return Ok(false);
        }
        let mut chars = name.chars();
        let first = chars.next().expect("checked non-empty");
        if !(first == '_' || first.is_ascii_alphabetic())
            || !chars.all(|character| character == '_' || character.is_ascii_alphanumeric())
        {
            return Err(Error::Parse {
                line: 0,
                message: format!("invalid DBC identifier {name:?}"),
            });
        }
        let used = self.value_tables.contains_key(name)
            || self.environments.contains_key(name)
            || self.attribute_definitions.contains_key(name)
            || self.sources.contains_key(name)
            || self.messages().any(|message| message.name.as_str() == name);
        if used {
            return Err(Error::Parse {
                line: 0,
                message: format!("DBC identifier {name:?} is already in use"),
            });
        }
        Ok(true)
    }
}

fn push_line(output: &mut String, line: &str) {
    output.push_str(line);
    output.push_str("\r\n");
}

fn split_keyword(line: &str) -> (&str, &str) {
    let index = line
        .find(|character: char| character.is_ascii_whitespace() || character == ':')
        .unwrap_or(line.len());
    let keyword = &line[..index];
    let rest = if index < line.len() {
        &line[index..]
    } else {
        ""
    };
    (keyword.trim_end_matches(':'), trim_kws(rest))
}

fn statement_has_unterminated_quote(statement: &str) -> bool {
    let mut escaped = false;
    let mut quoted = false;
    for character in statement.chars() {
        if escaped {
            escaped = false;
        } else if character == '\\' && quoted {
            escaped = true;
        } else if character == '"' {
            quoted = !quoted;
        }
    }
    quoted
}

/// Removes a `//` comment outside quoted strings. `quoted` carries the quote
/// state from earlier lines of the same statement; the end state is returned.
fn strip_line_comment(line: &str, mut quoted: bool) -> (&str, bool) {
    let mut escaped = false;
    let mut characters = line.char_indices().peekable();
    while let Some((index, character)) = characters.next() {
        if escaped {
            escaped = false;
        } else if character == '\\' && quoted {
            escaped = true;
        } else if character == '"' {
            quoted = !quoted;
        } else if !quoted && character == '/' && matches!(characters.peek(), Some((_, '/'))) {
            return (line[..index].trim_end(), quoted);
        }
    }
    (line, quoted)
}

fn needs_semicolon(keyword: &str) -> bool {
    matches!(
        keyword,
        "EV_"
            | "CM_"
            | "BA_"
            | "BA_DEF_"
            | "BA_DEF_REL_"
            | "BA_DEF_DEF_"
            | "BA_DEF_DEF_REL_"
            | "VAL_"
            | "VAL_TABLE_"
            | "SIG_GROUP_"
            | "SIG_VALTYPE_"
    )
}

fn logical_lines(text: &str) -> Result<Vec<(u32, Cow<'_, str>)>> {
    let mut output = Vec::new();
    let mut lines = text.lines().enumerate().peekable();
    let mut in_ns = false;
    while let Some((index, raw)) = lines.next() {
        let (statement, mut quoted) = strip_line_comment(raw.trim(), false);
        if statement.is_empty() {
            output.push((index as u32 + 1, Cow::Borrowed(statement)));
            continue;
        }
        if statement.starts_with("NS_") {
            in_ns = true;
            output.push((index as u32 + 1, Cow::Borrowed(statement)));
            continue;
        }
        if in_ns {
            if statement.starts_with("BS_") {
                in_ns = false;
            }
            output.push((index as u32 + 1, Cow::Borrowed(statement)));
            continue;
        }
        let keyword = split_keyword(statement).0;
        if needs_semicolon(keyword) && !statement.trim_end().ends_with(';') {
            let mut statement = statement.to_string();
            while !statement.trim_end().ends_with(';') {
                let Some((_, next)) = lines.next() else {
                    return Err(Error::Parse {
                        line: index as u32 + 1,
                        message: format!("unterminated {keyword} statement"),
                    });
                };
                let (next, next_quoted) = strip_line_comment(next.trim(), quoted);
                quoted = next_quoted;
                statement.push('\n');
                statement.push_str(next);
            }
            output.push((index as u32 + 1, Cow::Owned(statement)));
        } else {
            output.push((index as u32 + 1, Cow::Borrowed(statement)));
        }
    }
    Ok(output)
}

fn parse_message(rest: &str) -> Option<MsgType> {
    let mut cursor = Cur::new(rest);
    let id = cursor.digits().parse().ok()?;
    cursor.ws()?;
    let name = cursor.token_no_colon().to_string();
    cursor.skip_ws();
    cursor.expect(':')?;
    cursor.skip_ws();
    let dlc = cursor.digits().parse().ok()?;
    cursor.ws()?;
    let source = cursor.token().trim_end_matches(';').to_string();
    Some(MsgType {
        id,
        name,
        dlc,
        source,
        ..Default::default()
    })
}

fn parse_signal(rest: &str) -> Option<SignalType> {
    let colon = rest.find(':')?;
    let descriptor = rest[..colon].trim();
    let mut descriptor_parts = descriptor.split_whitespace();
    let name = descriptor_parts.next()?.to_string();
    let multiplex = descriptor_parts.next();
    let mut cursor = Cur::new(rest[colon + 1..].trim_start());
    let start = cursor.digits().parse().ok()?;
    cursor.expect('|')?;
    let len = cursor.digits().parse().ok()?;
    cursor.expect('@')?;
    let byte_order = cursor.peek()?;
    cursor.pos += byte_order.len_utf8();
    let sign = cursor.peek()?;
    cursor.pos += sign.len_utf8();
    cursor.skip_ws();
    cursor.expect('(')?;
    let factor = pnum(cursor.num_token());
    cursor.expect(',')?;
    let offset = pnum(cursor.num_token());
    cursor.expect(')')?;
    cursor.skip_ws();
    cursor.expect('[')?;
    let min = pnum(cursor.num_token());
    cursor.expect('|')?;
    let max = pnum(cursor.num_token());
    cursor.expect(']')?;
    cursor.skip_ws();
    let unit = cursor.quoted()?.to_string();
    cursor.skip_ws();
    let receivers = cursor
        .rest()
        .trim_end_matches(';')
        .split([',', ' ', '\t'])
        .filter(|value| !value.is_empty())
        .map(str::to_string)
        .collect();
    let mut signal = SignalType {
        name,
        start,
        len,
        factor,
        offset,
        min,
        max,
        unit,
        receivers,
        ..Default::default()
    };
    signal.set_little_endian(byte_order != '0');
    signal.set_signed(sign == '-');
    if let Some(multiplex) = multiplex {
        if multiplex == "M" {
            signal.set_multiplex_signal(true);
        } else if let Some(value) = multiplex.strip_prefix('m') {
            signal.multiplex_value = value.parse().unwrap_or(0);
            signal.set_multiplexed(true);
        }
    }
    Some(signal)
}

fn parse_environment(rest: &str) -> Option<EnvironmentType> {
    let colon = rest.find(':')?;
    let name = rest[..colon].trim().to_string();
    let mut cursor = Cur::new(rest[colon + 1..].trim_start());
    let env_type = EnvType::from_i32(cursor.num_token().parse().ok()?);
    cursor.skip_ws();
    cursor.expect('[')?;
    let min = pnum(cursor.num_token());
    cursor.expect('|')?;
    let max = pnum(cursor.num_token());
    cursor.expect(']')?;
    cursor.skip_ws();
    let unit = cursor.quoted()?.to_string();
    cursor.skip_ws();
    let start_value = pnum(cursor.num_token());
    cursor.skip_ws();
    let index = cursor.num_token().parse().unwrap_or(0);
    cursor.skip_ws();
    let access = EnvAccessType::from_dbc_token(cursor.token());
    cursor.skip_ws();
    let receivers = cursor
        .rest()
        .trim_end_matches(';')
        .split([',', ' ', '\t'])
        .filter(|value| !value.is_empty())
        .map(str::to_string)
        .collect();
    Some(EnvironmentType {
        name,
        env_type,
        min,
        max,
        unit,
        start_value,
        index,
        access,
        receivers,
        ..Default::default()
    })
}

fn quoted_pairs(mut input: &str) -> IndexMap<i64, String> {
    let mut values = IndexMap::new();
    input = input.trim().trim_end_matches(';');
    while !input.is_empty() {
        let mut cursor = Cur::new(input);
        cursor.skip_ws();
        let number = cursor.num_token();
        if number.is_empty() {
            break;
        }
        cursor.skip_ws();
        let Some(text) = cursor.quoted() else {
            break;
        };
        if let Ok(number) = number.parse::<i64>() {
            values.insert(number, text.to_string());
        }
        input = cursor.rest();
    }
    values
}

fn parse_value_table(rest: &str) -> Option<ValueTable> {
    let mut cursor = Cur::new(rest);
    let name = cursor.token().to_string();
    cursor.skip_ws();
    Some(ValueTable {
        name,
        enums: quoted_pairs(cursor.rest()),
    })
}

fn parse_attribute_definition(rest: &str) -> Option<AttribDefType> {
    let mut cursor = Cur::new(rest);
    cursor.skip_ws();
    let object_type = if cursor.peek() == Some('"') {
        AttribDefObjectType::Network
    } else {
        let keyword = cursor.token();
        cursor.skip_ws();
        AttribDefObjectType::from_keyword(keyword).unwrap_or(AttribDefObjectType::Network)
    };
    let name = cursor.quoted()?.to_string();
    cursor.skip_ws();
    let data_type = AttribDefValueType::from_keyword(cursor.token().trim_end_matches(';'))?;
    cursor.skip_ws();
    let mut definition = AttribDefType {
        name,
        object_type,
        data_type,
        ..Default::default()
    };
    match data_type {
        AttribDefValueType::Int | AttribDefValueType::Float | AttribDefValueType::Hex => {
            definition.min = pnum(cursor.num_token());
            cursor.skip_ws();
            definition.max = pnum(cursor.num_token());
        }
        AttribDefValueType::Enum => {
            let mut rest = cursor.rest().trim().trim_end_matches(';');
            while !rest.is_empty() {
                let mut enum_cursor = Cur::new(rest);
                enum_cursor.skip_ws();
                if enum_cursor.peek() == Some(',') {
                    enum_cursor.pos += 1;
                    enum_cursor.skip_ws();
                }
                let Some(value) = enum_cursor.quoted() else {
                    break;
                };
                definition.enums.push(value.to_string());
                rest = enum_cursor.rest();
            }
        }
        AttribDefValueType::Str => {}
    }
    Some(definition)
}

fn parse_attribute_default(file: &mut DBCFile, rest: &str) {
    let mut cursor = Cur::new(rest);
    cursor.skip_ws();
    let Some(name) = cursor.quoted() else {
        return;
    };
    cursor.skip_ws();
    let value = trim_qs(cursor.rest().trim()).to_string();
    if let Some(definition) = file.attribute_definitions.get_mut(name) {
        definition.default_value = Some(match definition.data_type {
            AttribDefValueType::Str | AttribDefValueType::Enum => AttribDefault::Text(value),
            _ => AttribDefault::Number(value.parse().unwrap_or(0.0)),
        });
    }
}

fn parse_attribute(
    file: &mut DBCFile,
    message_locations: &HashMap<u32, (String, usize)>,
    rest: &str,
) {
    let mut cursor = Cur::new(rest);
    cursor.skip_ws();
    let Some(name) = cursor.quoted().map(str::to_string) else {
        return;
    };
    let Some(definition) = file.attribute_definitions.get(&name).cloned() else {
        return;
    };
    cursor.skip_ws();
    let target_keyword = if definition.object_type == AttribDefObjectType::Network {
        None
    } else {
        let keyword = cursor.token().trim_end_matches(';').to_string();
        cursor.skip_ws();
        Some(keyword)
    };
    let string_value = definition.data_type == AttribDefValueType::Str;
    let read_value = |cursor: &mut Cur<'_>| -> Option<String> {
        cursor.skip_ws();
        if string_value {
            cursor.quoted().map(str::to_string)
        } else {
            Some(trim_qs(cursor.token()).to_string())
        }
    };
    match (definition.object_type, target_keyword.as_deref()) {
        (AttribDefObjectType::Network, _) => {
            if let Some(value) = read_value(&mut cursor) {
                file.attributes
                    .insert(name.clone(), AttributeType::new(name, value));
            }
        }
        (AttribDefObjectType::Bu, Some("BU_")) => {
            let node = cursor.token().to_string();
            if let Some(value) = read_value(&mut cursor) {
                if let Some(source) = file.sources.get_mut(&node) {
                    source
                        .attributes
                        .insert(name.clone(), AttributeType::new(name, value));
                }
            }
        }
        (AttribDefObjectType::Bo, Some("BO_")) => {
            let id = cursor.digits().parse().ok();
            if let (Some(id), Some(value)) = (id, read_value(&mut cursor)) {
                if let Some(message) = find_message_mut(file, message_locations, id) {
                    message
                        .attributes
                        .insert(name.clone(), AttributeType::new(name, value));
                }
            }
        }
        (AttribDefObjectType::Sg, Some("SG_")) => {
            let id = cursor.digits().parse().ok();
            cursor.skip_ws();
            let signal_name = cursor.token().to_string();
            if let (Some(id), Some(value)) = (id, read_value(&mut cursor)) {
                if let Some(signal) = find_signal_mut(file, message_locations, id, &signal_name) {
                    signal
                        .attributes
                        .insert(name.clone(), AttributeType::new(name, value));
                }
            }
        }
        (AttribDefObjectType::Ev, Some("EV_")) => {
            let environment_name = cursor.token().to_string();
            if let Some(value) = read_value(&mut cursor) {
                if let Some(environment) = file.environments.get_mut(&environment_name) {
                    environment
                        .attributes
                        .insert(name.clone(), AttributeType::new(name, value));
                }
            }
        }
        _ => {}
    }
}

fn parse_comment(
    file: &mut DBCFile,
    message_locations: &HashMap<u32, (String, usize)>,
    rest: &str,
) {
    let rest = rest.trim();
    if let Some(value) = rest.strip_prefix('"') {
        file.comment = trim_qs(value).to_string();
        return;
    }
    let mut cursor = Cur::new(rest);
    let target = cursor.token();
    cursor.skip_ws();
    match target {
        "BU_" => {
            let name = cursor.token().to_string();
            cursor.skip_ws();
            if let (Some(source), Some(comment)) = (file.sources.get_mut(&name), cursor.quoted()) {
                source.comment = comment.to_string();
            }
        }
        "BO_" => {
            let id = cursor.digits().parse().ok();
            cursor.skip_ws();
            if let (Some(id), Some(comment)) = (id, cursor.quoted()) {
                if let Some(message) = find_message_mut(file, message_locations, id) {
                    message.comment = comment.to_string();
                }
            }
        }
        "SG_" => {
            let id = cursor.digits().parse().ok();
            cursor.skip_ws();
            let name = cursor.token().to_string();
            cursor.skip_ws();
            if let (Some(id), Some(comment)) = (id, cursor.quoted()) {
                if let Some(signal) = find_signal_mut(file, message_locations, id, &name) {
                    signal.comment = comment.to_string();
                }
            }
        }
        "EV_" => {
            let name = cursor.token().to_string();
            cursor.skip_ws();
            if let (Some(environment), Some(comment)) =
                (file.environments.get_mut(&name), cursor.quoted())
            {
                environment.comment = comment.to_string();
            }
        }
        _ => {}
    }
}

fn parse_signal_values(
    file: &mut DBCFile,
    message_locations: &HashMap<u32, (String, usize)>,
    rest: &str,
) {
    let mut cursor = Cur::new(rest);
    let Some(id) = cursor.digits().parse().ok() else {
        return;
    };
    cursor.skip_ws();
    let name = cursor.token().to_string();
    cursor.skip_ws();
    let values = quoted_pairs(cursor.rest());
    if let Some(signal) = find_signal_mut(file, message_locations, id, &name) {
        signal.enums = Some(values);
    }
}

fn parse_signal_group(rest: &str) -> Option<SignalGroupType> {
    let mut cursor = Cur::new(rest);
    let id = cursor.digits().parse().ok()?;
    cursor.ws()?;
    let name = cursor.token_no_colon().to_string();
    cursor.skip_ws();
    let number = cursor.num_token().parse().ok()?;
    cursor.skip_ws();
    cursor.expect(':')?;
    let signal_refs = cursor
        .rest()
        .trim()
        .trim_end_matches(';')
        .split_whitespace()
        .map(str::to_string)
        .collect();
    Some(SignalGroupType {
        id,
        name,
        number,
        signal_refs,
    })
}

fn parse_signal_float_type(
    file: &mut DBCFile,
    message_locations: &HashMap<u32, (String, usize)>,
    rest: &str,
) {
    let mut cursor = Cur::new(rest);
    let Some(id) = cursor.digits().parse().ok() else {
        return;
    };
    cursor.skip_ws();
    let name = cursor.token_no_colon().to_string();
    cursor.skip_ws();
    let _ = cursor.expect(':');
    cursor.skip_ws();
    let float_type = match cursor.digits() {
        "1" => SignalFloatType::IeeeFloat32,
        "2" => SignalFloatType::IeeeFloat64,
        _ => return,
    };
    if let Some(signal) = find_signal_mut(file, message_locations, id, &name) {
        signal.set_float_type(float_type);
    }
}

fn find_message_mut<'a>(
    file: &'a mut DBCFile,
    message_locations: &HashMap<u32, (String, usize)>,
    id: u32,
) -> Option<&'a mut MsgType> {
    let (source_name, message_index) = message_locations.get(&id)?;
    file.sources
        .get_mut(source_name)?
        .messages
        .get_mut(*message_index)
}

fn find_signal_mut<'a>(
    file: &'a mut DBCFile,
    message_locations: &HashMap<u32, (String, usize)>,
    id: u32,
    name: &str,
) -> Option<&'a mut SignalType> {
    find_message_mut(file, message_locations, id)?.get_signal_mut(name)
}

#[cfg(test)]
mod tests {
    use super::*;

    const SAMPLE: &str = r#"VERSION "1.0"
NS_ :
 NS_DESC_
BS_:
BU_: ECU1 ECU2
VAL_TABLE_ State 0 "Off" 1 "On";
BO_ 100 MsgA: 8 ECU1
 SG_ Mux M : 0|4@1+ (1,0) [0|15] "" ECU2
 SG_ Val m2 : 8|16@0- (0.5,-1) [-1|100] "V" ECU2,ECU1
EV_ Env: 1 [0|10] "u" 2.5 7 DUMMY_NODE_VECTOR2 ECU1;
CM_ "network";
CM_ BU_ ECU1 "node";
CM_ BO_ 100 "message";
CM_ SG_ 100 Val "signal";
BA_DEF_ SG_ "SigAttr" ENUM "A","B";
BA_ "SigAttr" SG_ 100 Val 1;
VAL_ 100 Val 0 "Zero" 1 "One";
SIG_GROUP_ 100 Group 1 : Val Mux;
SIG_VALTYPE_ 100 Val : 2;
"#;

    #[test]
    fn parses_all_supported_statement_families() {
        let file = DBCFile::parse_str(SAMPLE).unwrap();
        assert_eq!(file.version, "1.0");
        assert_eq!(file.count_sources(), 2);
        assert_eq!(file.count_messages(), 1);
        assert_eq!(file.count_signals(), 2);
        let message = file.get_message(100).unwrap();
        assert_eq!(message.comment, "message");
        assert!(message.signals[0].is_multiplex_signal());
        let signal = message.get_signal("Val").unwrap();
        assert_eq!(signal.float_type, SignalFloatType::IeeeFloat64);
        assert_eq!(signal.len, 64);
        assert_eq!(signal.enums.as_ref().unwrap()[&1], "One");
        assert_eq!(file.environments["Env"].access, EnvAccessType::Write);
        assert_eq!(
            file.signal_groups[&(100, "Group".to_string())].signal_refs,
            ["Val", "Mux"]
        );
    }

    #[test]
    fn parse_write_parse_preserves_model() {
        let first = DBCFile::parse_str(SAMPLE).unwrap();
        let second = DBCFile::parse_str(&first.write_string()).unwrap();
        assert_eq!(first.version, second.version);
        assert_eq!(first.sources, second.sources);
        assert_eq!(first.value_tables, second.value_tables);
        assert_eq!(first.environments, second.environments);
        assert_eq!(first.attribute_definitions, second.attribute_definitions);
        assert_eq!(first.signal_groups, second.signal_groups);
        assert_eq!(first.attributes, second.attributes);
    }

    #[test]
    fn trailing_comments_do_not_swallow_the_next_statement() {
        let file = DBCFile::parse_str(
            "// header
VERSION \"\"
BU_: E
BO_ 1 A: 8 E // message
             BA_DEF_ BO_ \"First\" INT 0 9; // [ms]
             BA_DEF_ BO_ \"Second\" INT 0 9;
             CM_ BO_ 1 \"see http://x
// still text\";
",
        )
        .unwrap();
        assert_eq!(
            file.attribute_definitions.keys().collect::<Vec<_>>(),
            ["First", "Second"]
        );
        assert_eq!(
            file.get_message(1).unwrap().comment,
            "see http://x
// still text"
        );
    }

    #[test]
    fn a_message_keeps_several_signal_groups() {
        let text = "BU_: E
BO_ 1 A: 8 E
 SG_ X : 0|8@1+ (1,0) [0|0] \"\" E
 SG_ Y : 8|8@1+ (1,0) [0|0] \"\" E
                    SIG_GROUP_ 1 G1 1 : X;
SIG_GROUP_ 1 G2 1 : X Y;
";
        let first = DBCFile::parse_str(text).unwrap();
        assert_eq!(first.signal_groups.len(), 2);
        assert_eq!(
            first.signal_groups[&(1, "G2".to_string())].signal_refs,
            ["X", "Y"]
        );
        let second = DBCFile::parse_str(&first.write_string()).unwrap();
        assert_eq!(first.signal_groups, second.signal_groups);
    }

    #[test]
    fn extracts_intel_and_motorola_values() {
        let mut signal = SignalType {
            start: 4,
            len: 8,
            ..Default::default()
        };
        assert_eq!(signal.extract_raw(&[0xA0, 0x05]), Some(0x5A));
        signal.start = 7;
        signal.set_little_endian(false);
        assert_eq!(signal.extract_raw(&[0x12, 0x34]), Some(0x12));
    }

    #[test]
    fn reports_multiplex_and_limit_issues() {
        let file =
            DBCFile::parse_str("BU_: E\nBO_ 1 A: 8 E\n SG_ A m1 : 0|8@1+ (1,0) [2|1] \"\" E\n")
                .unwrap();
        let issues = file.plausibility_check();
        assert_eq!(issues.len(), 2);
        assert_eq!(issues[0].message, "Minimum is greater than maximum");
    }
}
