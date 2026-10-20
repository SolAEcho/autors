// Modified by SolAEcho (2026-10): `Ldf::writes_node_attributes` exposes whether
// the writer emits the `Node_attributes` section.
//! Editable LIN Description File model.

use crate::error::{Error, Result};
use indexmap::IndexMap;
use std::cmp::Ordering;
use std::fmt;
use std::path::Path;
use std::str::FromStr;
use std::time::Duration;

/// LIN protocol or LDF language version.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum LinVersion {
    /// A LIN consortium version such as 2.2.
    Lin { major: u16, minor: u16 },
    /// An ISO 17987 revision.
    Iso17987 { revision: u16 },
    /// An SAE J2602 part and revision.
    SaeJ2602 { part: u16, major: u16, minor: u16 },
}

impl LinVersion {
    /// LIN 1.3.
    pub const LIN_1_3: Self = Self::Lin { major: 1, minor: 3 };
    /// LIN 2.0.
    pub const LIN_2_0: Self = Self::Lin { major: 2, minor: 0 };
    /// LIN 2.1.
    pub const LIN_2_1: Self = Self::Lin { major: 2, minor: 1 };
    /// LIN 2.2.
    pub const LIN_2_2: Self = Self::Lin { major: 2, minor: 2 };

    /// Whether this version uses the LIN 2.x object model.
    pub const fn is_lin2_or_newer(&self) -> bool {
        match self {
            Self::Lin { major, .. } => *major >= 2,
            Self::Iso17987 { .. } | Self::SaeJ2602 { .. } => true,
        }
    }

    /// Whether this is an SAE J2602 version.
    pub const fn is_j2602(&self) -> bool {
        matches!(self, Self::SaeJ2602 { .. })
    }

    fn rank(&self) -> (u8, u16, u16) {
        match self {
            Self::Lin { major, minor } => (0, *major, *minor),
            // J2602 1.x uses LIN 2.0 as its base protocol.
            Self::SaeJ2602 { major, minor, .. } => (0, 2 + (*major > 1) as u16, *minor),
            Self::Iso17987 { revision } => (1, *revision, 0),
        }
    }
}

impl PartialOrd for LinVersion {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.rank().cmp(&other.rank()))
    }
}

impl fmt::Display for LinVersion {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Lin { major, minor } => write!(f, "{major}.{minor}"),
            Self::Iso17987 { revision } => write!(f, "ISO17987:{revision}"),
            Self::SaeJ2602 { part, major, minor } => {
                write!(f, "J2602_{part}_{major}.{minor}")
            }
        }
    }
}

impl FromStr for LinVersion {
    type Err = Error;

    fn from_str(value: &str) -> Result<Self> {
        if let Some(revision) = value.strip_prefix("ISO17987:") {
            return Ok(Self::Iso17987 {
                revision: revision.parse().map_err(|_| {
                    Error::Invalid(format!("{value:?} is not a valid ISO 17987 version"))
                })?,
            });
        }
        if let Some(rest) = value.strip_prefix("J2602_") {
            let (part, version) = rest.split_once('_').ok_or_else(|| {
                Error::Invalid(format!("{value:?} is not a valid SAE J2602 version"))
            })?;
            let (major, minor) = version.split_once('.').ok_or_else(|| {
                Error::Invalid(format!("{value:?} is not a valid SAE J2602 version"))
            })?;
            let parsed = Self::SaeJ2602 {
                part: part.parse().map_err(|_| {
                    Error::Invalid(format!("{value:?} is not a valid SAE J2602 version"))
                })?,
                major: major.parse().map_err(|_| {
                    Error::Invalid(format!("{value:?} is not a valid SAE J2602 version"))
                })?,
                minor: minor.parse().map_err(|_| {
                    Error::Invalid(format!("{value:?} is not a valid SAE J2602 version"))
                })?,
            };
            if !matches!(parsed, Self::SaeJ2602 { major: 1, .. }) {
                return Err(Error::Invalid(format!(
                    "unsupported SAE J2602 version {value:?}"
                )));
            }
            return Ok(parsed);
        }
        let (major, minor) = value
            .split_once('.')
            .ok_or_else(|| Error::Invalid(format!("{value:?} is not a valid LIN version")))?;
        Ok(Self::Lin {
            major: major
                .parse()
                .map_err(|_| Error::Invalid(format!("{value:?} is not a valid LIN version")))?,
            minor: minor
                .parse()
                .map_err(|_| Error::Invalid(format!("{value:?} is not a valid LIN version")))?,
        })
    }
}

/// Bit order declaration used by ISO 17987 LDF headers.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SignalByteOrder {
    /// Least-significant bit first within a byte.
    LittleEndian,
    /// Most-significant bit first within a byte.
    BigEndian,
}

/// Initial or runtime signal value.
#[derive(Debug, Clone, PartialEq)]
pub enum SignalValue {
    /// Integer scalar value.
    Integer(i64),
    /// Floating-point physical value.
    Float(f64),
    /// Textual logical, physical-with-unit, or ASCII value.
    Text(String),
    /// Byte-array signal value in transmission order.
    Bytes(Vec<u8>),
}

impl From<i64> for SignalValue {
    fn from(value: i64) -> Self {
        Self::Integer(value)
    }
}

impl From<u8> for SignalValue {
    fn from(value: u8) -> Self {
        Self::Integer(i64::from(value))
    }
}

impl From<f64> for SignalValue {
    fn from(value: f64) -> Self {
        Self::Float(value)
    }
}

impl From<String> for SignalValue {
    fn from(value: String) -> Self {
        Self::Text(value)
    }
}

impl From<&str> for SignalValue {
    fn from(value: &str) -> Self {
        Self::Text(value.to_string())
    }
}

impl From<Vec<u8>> for SignalValue {
    fn from(value: Vec<u8>) -> Self {
        Self::Bytes(value)
    }
}

/// Network master declaration.
#[derive(Debug, Clone, PartialEq)]
pub struct MasterNode {
    /// Node name.
    pub name: String,
    /// Schedule time base.
    pub time_base: Duration,
    /// Maximum schedule jitter.
    pub jitter: Duration,
    /// Optional maximum header length in bits.
    pub max_header_length_bits: Option<u16>,
    /// Optional response tolerance as a fraction (40% is 0.4).
    pub response_tolerance: Option<f64>,
}

/// LIN product identifier.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ProductId {
    /// Supplier identifier, 0..=0x7fff.
    pub supplier_id: u16,
    /// Function identifier.
    pub function_id: u16,
    /// Variant identifier.
    pub variant: u8,
}

impl ProductId {
    /// Creates and validates a LIN product identifier.
    pub fn new(supplier_id: u16, function_id: u16, variant: u8) -> Result<Self> {
        if supplier_id > 0x7fff {
            return Err(Error::Invalid(format!(
                "supplier ID {supplier_id:#x} exceeds 0x7fff"
            )));
        }
        Ok(Self {
            supplier_id,
            function_id,
            variant,
        })
    }
}

/// A configurable-frame slot in a slave node declaration.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ConfigurableFrame {
    /// Configuration slot/index.
    pub index: u16,
    /// Referenced frame name.
    pub frame: String,
}

/// Slave node and its protocol attributes.
#[derive(Debug, Clone, PartialEq)]
pub struct SlaveNode {
    /// Node name.
    pub name: String,
    /// Protocol version implemented by the node.
    pub protocol_version: LinVersion,
    /// Configured node address.
    pub configured_nad: Option<u8>,
    /// Initial node address.
    pub initial_nad: Option<u8>,
    /// Product identifier.
    pub product_id: Option<ProductId>,
    /// Response-error signal name.
    pub response_error: Option<String>,
    /// Fault-state signal names.
    pub fault_state_signals: Vec<String>,
    /// Minimum P2 timing.
    pub p2_min: Duration,
    /// Minimum separation time.
    pub st_min: Duration,
    /// N_As timeout.
    pub n_as_timeout: Duration,
    /// N_Cr timeout.
    pub n_cr_timeout: Duration,
    /// Configurable frame slots.
    pub configurable_frames: Vec<ConfigurableFrame>,
    /// Optional response tolerance as a fraction.
    pub response_tolerance: Option<f64>,
    /// Optional wake-up time.
    pub wakeup_time: Option<Duration>,
    /// Optional power-on time.
    pub poweron_time: Option<Duration>,
}

/// Scalar or byte-array signal definition.
#[derive(Debug, Clone, PartialEq)]
pub struct Signal {
    /// Signal name.
    pub name: String,
    /// Width in bits.
    pub width: u8,
    /// Initial value.
    pub initial_value: SignalValue,
    /// Publisher node name.
    pub publisher: Option<String>,
    /// Subscriber node names.
    pub subscribers: Vec<String>,
    /// Assigned signal encoding type.
    pub encoding_type: Option<String>,
}

impl Signal {
    /// Creates a signal while enforcing LIN scalar/array width constraints.
    pub fn new(name: impl Into<String>, width: u8, initial_value: SignalValue) -> Result<Self> {
        let name = name.into();
        match &initial_value {
            SignalValue::Bytes(bytes) => {
                if !(8..=64).contains(&width) || !width.is_multiple_of(8) {
                    return Err(Error::Invalid(format!(
                        "array signal {name}:{width} must be 8..=64 bits and byte-aligned"
                    )));
                }
                if bytes.len() != usize::from(width / 8) {
                    return Err(Error::Invalid(format!(
                        "array signal {name}:{width} has {} initial bytes",
                        bytes.len()
                    )));
                }
            }
            SignalValue::Integer(value) => {
                if !(1..=16).contains(&width) {
                    return Err(Error::Invalid(format!(
                        "scalar signal {name}:{width} must be 1..=16 bits"
                    )));
                }
                if *value < 0 || u128::try_from(*value).unwrap_or(u128::MAX) >= (1_u128 << width) {
                    return Err(Error::Invalid(format!(
                        "initial value {value} does not fit signal {name}:{width}"
                    )));
                }
            }
            SignalValue::Float(_) | SignalValue::Text(_) => {
                return Err(Error::Invalid(format!(
                    "signal {name} initial values must be integers or byte arrays"
                )));
            }
        }
        Ok(Self {
            name,
            width,
            initial_value,
            publisher: None,
            subscribers: Vec::new(),
            encoding_type: None,
        })
    }

    /// Whether this signal is represented as a byte array.
    pub fn is_array(&self) -> bool {
        matches!(self.initial_value, SignalValue::Bytes(_))
    }
}

/// Placement of a signal in a frame.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SignalPlacement {
    /// Referenced signal name.
    pub signal: String,
    /// Least-significant bit offset in the frame payload.
    pub bit_offset: u16,
}

/// Unconditional frame definition.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct UnconditionalFrame {
    /// Frame name.
    pub name: String,
    /// LIN frame identifier as declared by the LDF.
    pub id: u8,
    /// Publisher node name.
    pub publisher: String,
    /// Payload length in bytes.
    pub length: u8,
    /// Ordered signal layout.
    pub signals: Vec<SignalPlacement>,
}

/// Sporadic frame definition.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SporadicFrame {
    /// Frame name.
    pub name: String,
    /// Referenced unconditional frames in priority order.
    pub frames: Vec<String>,
}

/// Event-triggered frame definition.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EventTriggeredFrame {
    /// Frame name.
    pub name: String,
    /// LIN frame identifier as declared by the LDF.
    pub id: u8,
    /// Optional collision-resolving schedule table.
    pub collision_resolving_schedule: Option<String>,
    /// Associated unconditional frames.
    pub frames: Vec<String>,
}

/// Diagnostic frame definition.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DiagnosticFrame {
    /// Frame name.
    pub name: String,
    /// Diagnostic frame identifier (normally 0x3c or 0x3d).
    pub id: u8,
    /// Diagnostic signal layout.
    pub signals: Vec<SignalPlacement>,
}

/// Node-composition variant.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct NodeComposition {
    /// Composition name.
    pub name: String,
    /// Slave node names.
    pub nodes: Vec<String>,
}

/// Named node-composition configuration.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct NodeCompositionConfiguration {
    /// Configuration name.
    pub name: String,
    /// Available compositions.
    pub compositions: Vec<NodeComposition>,
}

/// Signal group declaration.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SignalGroup {
    /// Group name.
    pub name: String,
    /// Total group size in bits.
    pub size: u16,
    /// Signal names and group-local offsets.
    pub signals: Vec<SignalPlacement>,
}

/// Signal conversion rule.
#[derive(Debug, Clone, PartialEq)]
pub enum EncodingValue {
    /// One raw value with optional display text.
    Logical { raw: i64, text: Option<String> },
    /// Linear physical conversion: `physical = raw * scale + offset`.
    Physical {
        raw_min: i64,
        raw_max: i64,
        scale: f64,
        offset: f64,
        unit: Option<String>,
    },
    /// Binary-coded decimal byte array.
    Bcd,
    /// ASCII byte array.
    Ascii,
}

/// Named ordered set of signal conversion rules.
#[derive(Debug, Clone, PartialEq)]
pub struct SignalEncodingType {
    /// Encoding name.
    pub name: String,
    /// Rules tried in declaration order.
    pub values: Vec<EncodingValue>,
}

/// Schedule table entry.
#[derive(Debug, Clone, PartialEq)]
pub struct ScheduleEntry {
    /// Command to execute.
    pub command: ScheduleCommand,
    /// Delay after the command.
    pub delay: Duration,
}

/// LIN schedule table command.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ScheduleCommand {
    /// Transmit a named frame.
    Frame(String),
    /// Master request diagnostic frame.
    MasterRequest,
    /// Slave response diagnostic frame.
    SlaveResponse,
    /// Assign NAD to the named node.
    AssignNad { node: String },
    /// Conditional change NAD service parameters.
    ConditionalChangeNad {
        nad: u8,
        identifier: u8,
        byte: u8,
        mask: u8,
        invert: u8,
        new_nad: u8,
    },
    /// Data dump service.
    DataDump { node: String, data: [u8; 5] },
    /// Save node configuration.
    SaveConfiguration { node: String },
    /// Assign a frame identifier range.
    AssignFrameIdRange {
        node: String,
        frame_index: u8,
        protected_ids: Option<[u8; 4]>,
    },
    /// Assign one frame identifier.
    AssignFrameId { node: String, frame: String },
    /// Unassign one frame identifier.
    UnassignFrameId { node: String, frame: String },
    /// Free-format diagnostic payload.
    FreeFormat([u8; 8]),
}

/// Named LIN schedule table.
#[derive(Debug, Clone, PartialEq)]
pub struct ScheduleTable {
    /// Table name.
    pub name: String,
    /// Entries in execution order.
    pub entries: Vec<ScheduleEntry>,
}

/// Complete LIN Description File.
#[derive(Debug, Clone, PartialEq)]
pub struct Ldf {
    /// Protocol version.
    pub protocol_version: LinVersion,
    /// LDF language version.
    pub language_version: LinVersion,
    /// Bus speed in bit/s.
    pub baud_rate: u32,
    /// Optional channel name.
    pub channel_name: Option<String>,
    /// Optional ISO LDF file revision.
    pub file_revision: Option<String>,
    /// Optional ISO signal byte-order declaration.
    pub signal_byte_order: Option<SignalByteOrder>,
    /// Master node.
    pub master: MasterNode,
    /// Slave nodes, keyed in declaration order.
    pub slaves: IndexMap<String, SlaveNode>,
    /// Application signals.
    pub signals: IndexMap<String, Signal>,
    /// Diagnostic signals.
    pub diagnostic_signals: IndexMap<String, Signal>,
    /// Unconditional frames.
    pub unconditional_frames: IndexMap<String, UnconditionalFrame>,
    /// Sporadic frames.
    pub sporadic_frames: IndexMap<String, SporadicFrame>,
    /// Event-triggered frames.
    pub event_triggered_frames: IndexMap<String, EventTriggeredFrame>,
    /// Diagnostic frames.
    pub diagnostic_frames: IndexMap<String, DiagnosticFrame>,
    /// LIN 1.3 diagnostic node addresses.
    pub diagnostic_addresses: IndexMap<String, u8>,
    /// Node compositions.
    pub node_compositions: Vec<NodeCompositionConfiguration>,
    /// Schedule tables.
    pub schedule_tables: IndexMap<String, ScheduleTable>,
    /// Signal groups.
    pub signal_groups: IndexMap<String, SignalGroup>,
    /// Signal encoding types.
    pub signal_encoding_types: IndexMap<String, SignalEncodingType>,
    /// Captured comments, including their delimiters.
    pub comments: Vec<String>,
    pub(crate) has_node_attributes: bool,
}

impl Ldf {
    /// Parses an LDF document from UTF-8 text.
    pub fn parse_str(input: &str) -> Result<Self> {
        crate::parser::parse(input)
    }

    /// Reads and parses an UTF-8 LDF file.
    pub fn read(path: impl AsRef<Path>) -> Result<Self> {
        Self::parse_str(&std::fs::read_to_string(path)?)
    }

    /// Serializes the document in canonical LDF syntax.
    pub fn write_string(&self) -> Result<String> {
        crate::writer::write(self)
    }

    /// Writes the document as UTF-8 LDF text.
    pub fn write(&self, path: impl AsRef<Path>) -> Result<()> {
        std::fs::write(path, self.write_string()?)?;
        Ok(())
    }

    /// Returns whether [`Ldf::write_string`] emits a `Node_attributes` section,
    /// and with it each slave's protocol, NAD, product ID, response error,
    /// fault-state signals, P2_min, ST_min, timeouts and configurable frames.
    /// Always true for LIN 2.0 and newer; for older language versions only
    /// when the parsed document declared `Node_attributes`.
    pub fn writes_node_attributes(&self) -> bool {
        self.has_node_attributes || self.language_version >= LinVersion::LIN_2_0
    }

    /// Validates cross-references, frame identifiers, and signal layouts.
    /// This is useful after editing public model fields in memory.
    pub fn validate(&self) -> Result<()> {
        crate::parser::validate(self)
    }

    /// Looks up an unconditional frame by name.
    pub fn unconditional_frame(&self, name: &str) -> Option<&UnconditionalFrame> {
        self.unconditional_frames.get(name)
    }

    /// Looks up an unconditional frame by its declared identifier.
    pub fn unconditional_frame_by_id(&self, id: u8) -> Option<&UnconditionalFrame> {
        self.unconditional_frames
            .values()
            .find(|frame| frame.id == id)
    }

    /// Looks up any frame kind by name.
    pub fn frame(&self, name: &str) -> Option<FrameRef<'_>> {
        if let Some(frame) = self.unconditional_frames.get(name) {
            return Some(FrameRef::Unconditional(frame));
        }
        if let Some(frame) = self.event_triggered_frames.get(name) {
            return Some(FrameRef::EventTriggered(frame));
        }
        if let Some(frame) = self.sporadic_frames.get(name) {
            return Some(FrameRef::Sporadic(frame));
        }
        self.diagnostic_frames.get(name).map(FrameRef::Diagnostic)
    }

    /// Looks up any numbered frame kind by its declared identifier.
    pub fn frame_by_id(&self, id: u8) -> Option<FrameRef<'_>> {
        if let Some(frame) = self
            .unconditional_frames
            .values()
            .find(|frame| frame.id == id)
        {
            return Some(FrameRef::Unconditional(frame));
        }
        if let Some(frame) = self
            .event_triggered_frames
            .values()
            .find(|frame| frame.id == id)
        {
            return Some(FrameRef::EventTriggered(frame));
        }
        self.diagnostic_frames
            .values()
            .find(|frame| frame.id == id)
            .map(FrameRef::Diagnostic)
    }

    /// Looks up an application or diagnostic signal by name.
    pub fn signal(&self, name: &str) -> Option<SignalRef<'_>> {
        self.signals
            .get(name)
            .map(SignalRef::Application)
            .or_else(|| self.diagnostic_signals.get(name).map(SignalRef::Diagnostic))
    }
}

/// Borrowed reference to any frame kind.
#[derive(Debug, Clone, Copy)]
pub enum FrameRef<'a> {
    /// Unconditional frame.
    Unconditional(&'a UnconditionalFrame),
    /// Event-triggered frame.
    EventTriggered(&'a EventTriggeredFrame),
    /// Sporadic frame.
    Sporadic(&'a SporadicFrame),
    /// Diagnostic frame.
    Diagnostic(&'a DiagnosticFrame),
}

/// Borrowed reference to an application or diagnostic signal.
#[derive(Debug, Clone, Copy)]
pub enum SignalRef<'a> {
    /// Application signal.
    Application(&'a Signal),
    /// Diagnostic signal.
    Diagnostic(&'a Signal),
}
