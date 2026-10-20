use crate::error::{Error, Result};
use crate::model::*;
use indexmap::IndexMap;
use std::time::Duration;

#[derive(Debug, Clone, PartialEq)]
enum TokenKind {
    Word(String),
    Number(String),
    String(String),
    LBrace,
    RBrace,
    Colon,
    Comma,
    Semicolon,
    Equals,
    Percent,
}

#[derive(Debug, Clone)]
struct Token {
    kind: TokenKind,
    line: usize,
    column: usize,
}

struct Lexer<'a> {
    input: &'a str,
    offset: usize,
    line: usize,
    column: usize,
    comments: Vec<String>,
}

impl<'a> Lexer<'a> {
    fn new(input: &'a str) -> Self {
        Self {
            input,
            offset: 0,
            line: 1,
            column: 1,
            comments: Vec::new(),
        }
    }

    fn tokenize(mut self) -> Result<(Vec<Token>, Vec<String>)> {
        let mut tokens = Vec::new();
        while let Some(character) = self.peek() {
            if character.is_whitespace() {
                self.bump();
                continue;
            }
            let line = self.line;
            let column = self.column;
            if self.starts_with("//") {
                let start = self.offset;
                while !matches!(self.peek(), None | Some('\n' | '\r')) {
                    self.bump();
                }
                self.comments
                    .push(self.input[start..self.offset].to_string());
                continue;
            }
            if self.starts_with("/*") {
                let start = self.offset;
                self.bump();
                self.bump();
                while !self.starts_with("*/") {
                    if self.peek().is_none() {
                        return Err(Error::Parse {
                            line,
                            column,
                            message: "unterminated block comment".to_string(),
                        });
                    }
                    self.bump();
                }
                self.bump();
                self.bump();
                self.comments
                    .push(self.input[start..self.offset].to_string());
                continue;
            }
            let kind = match character {
                '{' => {
                    self.bump();
                    TokenKind::LBrace
                }
                '}' => {
                    self.bump();
                    TokenKind::RBrace
                }
                ':' => {
                    self.bump();
                    TokenKind::Colon
                }
                ',' => {
                    self.bump();
                    TokenKind::Comma
                }
                ';' => {
                    self.bump();
                    TokenKind::Semicolon
                }
                '=' => {
                    self.bump();
                    TokenKind::Equals
                }
                '%' => {
                    self.bump();
                    TokenKind::Percent
                }
                '"' => TokenKind::String(self.string(line, column)?),
                '-' | '0'..='9' => TokenKind::Number(self.number()),
                value if value.is_ascii_alphabetic() || value == '_' => {
                    TokenKind::Word(self.word())
                }
                other => {
                    return Err(Error::Parse {
                        line,
                        column,
                        message: format!("unexpected character {other:?}"),
                    })
                }
            };
            tokens.push(Token { kind, line, column });
        }
        Ok((tokens, self.comments))
    }

    fn peek(&self) -> Option<char> {
        self.input[self.offset..].chars().next()
    }

    fn starts_with(&self, value: &str) -> bool {
        self.input[self.offset..].starts_with(value)
    }

    fn bump(&mut self) -> Option<char> {
        let character = self.peek()?;
        self.offset += character.len_utf8();
        if character == '\n' {
            self.line += 1;
            self.column = 1;
        } else {
            self.column += 1;
        }
        Some(character)
    }

    fn word(&mut self) -> String {
        let start = self.offset;
        while matches!(self.peek(), Some(value) if value.is_ascii_alphanumeric() || matches!(value, '_' | '.'))
        {
            self.bump();
        }
        self.input[start..self.offset].to_string()
    }

    fn number(&mut self) -> String {
        let start = self.offset;
        if self.peek() == Some('-') {
            self.bump();
        }
        if self.starts_with("0x") || self.starts_with("0X") {
            self.bump();
            self.bump();
            while matches!(self.peek(), Some(value) if value.is_ascii_hexdigit()) {
                self.bump();
            }
        } else {
            while matches!(self.peek(), Some(value) if value.is_ascii_digit()) {
                self.bump();
            }
            if self.peek() == Some('.') {
                self.bump();
                while matches!(self.peek(), Some(value) if value.is_ascii_digit()) {
                    self.bump();
                }
            }
            if matches!(self.peek(), Some('e' | 'E')) {
                self.bump();
                if matches!(self.peek(), Some('+' | '-')) {
                    self.bump();
                }
                while matches!(self.peek(), Some(value) if value.is_ascii_digit()) {
                    self.bump();
                }
            }
        }
        self.input[start..self.offset].to_string()
    }

    fn string(&mut self, line: usize, column: usize) -> Result<String> {
        self.bump();
        let mut output = String::new();
        loop {
            match self.bump() {
                Some('"') => return Ok(output),
                Some('\\') => match self.bump() {
                    Some('"') => output.push('"'),
                    Some('\\') => output.push('\\'),
                    Some('n') => output.push('\n'),
                    Some('r') => output.push('\r'),
                    Some('t') => output.push('\t'),
                    Some(other) => {
                        output.push('\\');
                        output.push(other);
                    }
                    None => break,
                },
                Some(value) => output.push(value),
                None => break,
            }
        }
        Err(Error::Parse {
            line,
            column,
            message: "unterminated string".to_string(),
        })
    }
}

#[derive(Default)]
struct PartialSlave {
    protocol_version: Option<LinVersion>,
    configured_nad: Option<u8>,
    initial_nad: Option<u8>,
    product_id: Option<ProductId>,
    response_error: Option<String>,
    fault_state_signals: Vec<String>,
    p2_min: Option<Duration>,
    st_min: Option<Duration>,
    n_as_timeout: Option<Duration>,
    n_cr_timeout: Option<Duration>,
    configurable_frames: Vec<ConfigurableFrame>,
    response_tolerance: Option<f64>,
    wakeup_time: Option<Duration>,
    poweron_time: Option<Duration>,
}

#[derive(Default)]
struct Builder {
    protocol_version: Option<LinVersion>,
    language_version: Option<LinVersion>,
    baud_rate: Option<u32>,
    channel_name: Option<String>,
    file_revision: Option<String>,
    signal_byte_order: Option<SignalByteOrder>,
    master: Option<MasterNode>,
    slave_names: Vec<String>,
    slave_attributes: IndexMap<String, PartialSlave>,
    signals: IndexMap<String, Signal>,
    diagnostic_signals: IndexMap<String, Signal>,
    unconditional_frames: IndexMap<String, UnconditionalFrame>,
    sporadic_frames: IndexMap<String, SporadicFrame>,
    event_triggered_frames: IndexMap<String, EventTriggeredFrame>,
    diagnostic_frames: IndexMap<String, DiagnosticFrame>,
    diagnostic_addresses: IndexMap<String, u8>,
    node_compositions: Vec<NodeCompositionConfiguration>,
    schedule_tables: IndexMap<String, ScheduleTable>,
    signal_groups: IndexMap<String, SignalGroup>,
    signal_encoding_types: IndexMap<String, SignalEncodingType>,
    representations: Vec<(String, Vec<String>)>,
    comments: Vec<String>,
    has_node_attributes: bool,
}

struct Parser {
    tokens: Vec<Token>,
    position: usize,
    builder: Builder,
}

pub(crate) fn parse(input: &str) -> Result<Ldf> {
    let (tokens, comments) = Lexer::new(input).tokenize()?;
    let mut parser = Parser {
        tokens,
        position: 0,
        builder: Builder {
            comments,
            ..Builder::default()
        },
    };
    parser.document()?;
    parser.finish()
}

impl Parser {
    fn document(&mut self) -> Result<()> {
        while !self.at_end() {
            let keyword = self.expect_identifier()?;
            match keyword.as_str() {
                "LIN_description_file" => self.expect(TokenKind::Semicolon)?,
                "LIN_protocol_version" => {
                    self.expect(TokenKind::Equals)?;
                    self.builder.protocol_version = Some(self.version()?);
                    self.expect(TokenKind::Semicolon)?;
                }
                "LIN_language_version" => {
                    self.expect(TokenKind::Equals)?;
                    self.builder.language_version = Some(self.version()?);
                    self.expect(TokenKind::Semicolon)?;
                }
                "LIN_speed" => {
                    self.expect(TokenKind::Equals)?;
                    let speed = self.expect_float()?;
                    self.expect_word("kbps")?;
                    self.expect(TokenKind::Semicolon)?;
                    if !speed.is_finite() || speed <= 0.0 || speed * 1000.0 > u32::MAX as f64 {
                        return Err(Error::Invalid(format!("invalid LIN speed {speed}")));
                    }
                    self.builder.baud_rate = Some((speed * 1000.0).round() as u32);
                }
                "Channel_name" => {
                    self.expect(TokenKind::Equals)?;
                    self.builder.channel_name = Some(self.expect_string()?);
                    self.expect(TokenKind::Semicolon)?;
                }
                "LDF_file_revision" => {
                    self.expect(TokenKind::Equals)?;
                    self.builder.file_revision = Some(self.expect_string()?);
                    self.expect(TokenKind::Semicolon)?;
                }
                "LIN_sig_byte_order_big_endian" => {
                    self.builder.signal_byte_order = Some(SignalByteOrder::BigEndian);
                    self.expect(TokenKind::Semicolon)?;
                }
                "LIN_sig_byte_order_little_endian" => {
                    self.builder.signal_byte_order = Some(SignalByteOrder::LittleEndian);
                    self.expect(TokenKind::Semicolon)?;
                }
                "Nodes" => self.nodes()?,
                "composite" => self.node_compositions()?,
                "Signals" => self.signals(false)?,
                "Diagnostic_signals" => self.signals(true)?,
                "Diagnostic_addresses" => self.diagnostic_addresses()?,
                "Frames" => self.frames()?,
                "Sporadic_frames" => self.sporadic_frames()?,
                "Event_triggered_frames" => self.event_triggered_frames()?,
                "Diagnostic_frames" => self.diagnostic_frames()?,
                "Node_attributes" => self.node_attributes()?,
                "Schedule_tables" => self.schedule_tables()?,
                "Signal_groups" => self.signal_groups()?,
                "Signal_encoding_types" => self.signal_encoding_types()?,
                "Signal_representation" => self.signal_representations()?,
                unknown => return self.error(format!("unknown top-level declaration {unknown:?}")),
            }
        }
        Ok(())
    }

    fn nodes(&mut self) -> Result<()> {
        self.expect(TokenKind::LBrace)?;
        while !self.take(TokenKind::RBrace) {
            let kind = self.expect_identifier()?;
            self.expect(TokenKind::Colon)?;
            match kind.as_str() {
                "Master" => {
                    let name = self.expect_identifier()?;
                    self.expect(TokenKind::Comma)?;
                    let time_base = self.milliseconds()?;
                    self.expect(TokenKind::Comma)?;
                    let jitter = self.milliseconds()?;
                    let mut max_header_length_bits = None;
                    let mut response_tolerance = None;
                    if self.take(TokenKind::Comma) {
                        max_header_length_bits = Some(self.expect_u16()?);
                        self.expect_word("bits")?;
                        self.expect(TokenKind::Comma)?;
                        response_tolerance = Some(self.percent()?);
                    }
                    self.expect(TokenKind::Semicolon)?;
                    self.builder.master = Some(MasterNode {
                        name,
                        time_base,
                        jitter,
                        max_header_length_bits,
                        response_tolerance,
                    });
                }
                "Slaves" => {
                    if !self.check(TokenKind::Semicolon) {
                        loop {
                            let slave = self.expect_identifier()?;
                            self.builder.slave_names.push(slave);
                            if !self.take(TokenKind::Comma) {
                                break;
                            }
                        }
                    }
                    self.expect(TokenKind::Semicolon)?;
                }
                _ => return self.error(format!("unknown Nodes entry {kind:?}")),
            }
        }
        Ok(())
    }

    fn node_compositions(&mut self) -> Result<()> {
        self.expect(TokenKind::LBrace)?;
        while !self.take(TokenKind::RBrace) {
            self.expect_word("configuration")?;
            let name = self.expect_identifier()?;
            self.expect(TokenKind::LBrace)?;
            let mut compositions = Vec::new();
            while !self.take(TokenKind::RBrace) {
                let composition_name = self.expect_identifier()?;
                self.expect(TokenKind::LBrace)?;
                let mut nodes = Vec::new();
                if !self.check(TokenKind::RBrace) {
                    loop {
                        nodes.push(self.expect_identifier()?);
                        if !self.take(TokenKind::Comma) {
                            break;
                        }
                    }
                }
                self.expect(TokenKind::RBrace)?;
                compositions.push(NodeComposition {
                    name: composition_name,
                    nodes,
                });
            }
            self.builder
                .node_compositions
                .push(NodeCompositionConfiguration { name, compositions });
        }
        Ok(())
    }

    fn signals(&mut self, diagnostic: bool) -> Result<()> {
        self.expect(TokenKind::LBrace)?;
        while !self.take(TokenKind::RBrace) {
            let name = self.expect_identifier()?;
            self.expect(TokenKind::Colon)?;
            let width = self.expect_u8()?;
            self.expect(TokenKind::Comma)?;
            let initial_value = self.signal_initial_value()?;
            let mut signal = Signal::new(name.clone(), width, initial_value)?;
            if !diagnostic {
                self.expect(TokenKind::Comma)?;
                signal.publisher = Some(self.expect_identifier()?);
                while self.take(TokenKind::Comma) {
                    signal.subscribers.push(self.expect_identifier()?);
                }
            }
            self.expect(TokenKind::Semicolon)?;
            let collection = if diagnostic {
                &mut self.builder.diagnostic_signals
            } else {
                &mut self.builder.signals
            };
            insert_unique(collection, name, signal)?;
        }
        Ok(())
    }

    fn signal_initial_value(&mut self) -> Result<SignalValue> {
        if self.take(TokenKind::LBrace) {
            let mut bytes = Vec::new();
            if !self.check(TokenKind::RBrace) {
                loop {
                    bytes.push(self.expect_u8()?);
                    if !self.take(TokenKind::Comma) {
                        break;
                    }
                }
            }
            self.expect(TokenKind::RBrace)?;
            Ok(SignalValue::Bytes(bytes))
        } else {
            Ok(SignalValue::Integer(self.expect_integer()?))
        }
    }

    fn frames(&mut self) -> Result<()> {
        self.expect(TokenKind::LBrace)?;
        while !self.take(TokenKind::RBrace) {
            let name = self.expect_identifier()?;
            self.expect(TokenKind::Colon)?;
            let id = self.expect_frame_id()?;
            self.expect(TokenKind::Comma)?;
            let publisher = self.expect_identifier()?;
            let length = if self.take(TokenKind::Comma) {
                Some(self.expect_u8()?)
            } else {
                None
            };
            self.expect(TokenKind::LBrace)?;
            let signals = self.signal_placements()?;
            self.expect(TokenKind::RBrace)?;
            let length = match length {
                Some(length) => length,
                None => derive_frame_length(id, self.builder.language_version.as_ref())?,
            };
            let frame = UnconditionalFrame {
                name: name.clone(),
                id,
                publisher,
                length,
                signals,
            };
            insert_unique(&mut self.builder.unconditional_frames, name, frame)?;
        }
        Ok(())
    }

    fn signal_placements(&mut self) -> Result<Vec<SignalPlacement>> {
        let mut signals = Vec::new();
        while !self.check(TokenKind::RBrace) {
            let signal = self.expect_identifier()?;
            self.expect(TokenKind::Comma)?;
            let bit_offset = self.expect_u16()?;
            self.expect(TokenKind::Semicolon)?;
            signals.push(SignalPlacement { signal, bit_offset });
        }
        Ok(signals)
    }

    fn sporadic_frames(&mut self) -> Result<()> {
        self.expect(TokenKind::LBrace)?;
        while !self.take(TokenKind::RBrace) {
            let name = self.expect_identifier()?;
            self.expect(TokenKind::Colon)?;
            let frames = self.identifier_list_until_semicolon()?;
            let frame = SporadicFrame {
                name: name.clone(),
                frames,
            };
            insert_unique(&mut self.builder.sporadic_frames, name, frame)?;
        }
        Ok(())
    }

    fn event_triggered_frames(&mut self) -> Result<()> {
        self.expect(TokenKind::LBrace)?;
        while !self.take(TokenKind::RBrace) {
            let name = self.expect_identifier()?;
            self.expect(TokenKind::Colon)?;
            let collision_resolving_schedule = if self.peek_number() {
                None
            } else {
                let schedule = self.expect_identifier()?;
                self.expect(TokenKind::Comma)?;
                Some(schedule)
            };
            let id = self.expect_frame_id()?;
            self.expect(TokenKind::Comma)?;
            let frames = self.identifier_list_until_semicolon()?;
            let frame = EventTriggeredFrame {
                name: name.clone(),
                id,
                collision_resolving_schedule,
                frames,
            };
            insert_unique(&mut self.builder.event_triggered_frames, name, frame)?;
        }
        Ok(())
    }

    fn diagnostic_frames(&mut self) -> Result<()> {
        self.expect(TokenKind::LBrace)?;
        while !self.take(TokenKind::RBrace) {
            let name = self.expect_identifier()?;
            self.expect(TokenKind::Colon)?;
            let id = self.expect_frame_id()?;
            self.expect(TokenKind::LBrace)?;
            let signals = self.signal_placements()?;
            self.expect(TokenKind::RBrace)?;
            let frame = DiagnosticFrame {
                name: name.clone(),
                id,
                signals,
            };
            insert_unique(&mut self.builder.diagnostic_frames, name, frame)?;
        }
        Ok(())
    }

    fn diagnostic_addresses(&mut self) -> Result<()> {
        self.expect(TokenKind::LBrace)?;
        while !self.take(TokenKind::RBrace) {
            let name = self.expect_identifier()?;
            self.expect(TokenKind::Colon)?;
            let address = self.expect_u8()?;
            self.expect(TokenKind::Semicolon)?;
            if self
                .builder
                .diagnostic_addresses
                .insert(name.clone(), address)
                .is_some()
            {
                return Err(Error::Invalid(format!(
                    "duplicate diagnostic address {name:?}"
                )));
            }
        }
        Ok(())
    }

    fn node_attributes(&mut self) -> Result<()> {
        self.builder.has_node_attributes = true;
        self.expect(TokenKind::LBrace)?;
        while !self.take(TokenKind::RBrace) {
            let name = self.expect_identifier()?;
            self.expect(TokenKind::LBrace)?;
            let mut node = PartialSlave::default();
            while !self.take(TokenKind::RBrace) {
                let attribute = self.expect_identifier()?;
                match attribute.as_str() {
                    "LIN_protocol" => {
                        self.expect(TokenKind::Equals)?;
                        node.protocol_version = Some(self.version()?);
                        self.expect(TokenKind::Semicolon)?;
                    }
                    "configured_NAD" => {
                        self.expect(TokenKind::Equals)?;
                        node.configured_nad = Some(self.expect_u8()?);
                        self.expect(TokenKind::Semicolon)?;
                    }
                    "initial_NAD" => {
                        self.expect(TokenKind::Equals)?;
                        node.initial_nad = Some(self.expect_u8()?);
                        self.expect(TokenKind::Semicolon)?;
                    }
                    "product_id" => {
                        self.expect(TokenKind::Equals)?;
                        let supplier = self.expect_u16()?;
                        self.expect(TokenKind::Comma)?;
                        let function = self.expect_u16()?;
                        let variant = if self.take(TokenKind::Comma) {
                            self.expect_u8()?
                        } else {
                            0
                        };
                        self.expect(TokenKind::Semicolon)?;
                        node.product_id = Some(ProductId::new(supplier, function, variant)?);
                    }
                    "response_error" => {
                        self.expect(TokenKind::Equals)?;
                        node.response_error = Some(self.expect_identifier()?);
                        self.expect(TokenKind::Semicolon)?;
                    }
                    "fault_state_signals" => {
                        self.expect(TokenKind::Equals)?;
                        node.fault_state_signals = self.identifier_list_until_semicolon()?;
                    }
                    "P2_min" => {
                        self.expect(TokenKind::Equals)?;
                        node.p2_min = Some(self.milliseconds()?);
                        self.expect(TokenKind::Semicolon)?;
                    }
                    "ST_min" => {
                        self.expect(TokenKind::Equals)?;
                        node.st_min = Some(self.milliseconds()?);
                        self.expect(TokenKind::Semicolon)?;
                    }
                    "N_As_timeout" => {
                        self.expect(TokenKind::Equals)?;
                        node.n_as_timeout = Some(self.milliseconds()?);
                        self.expect(TokenKind::Semicolon)?;
                    }
                    "N_Cr_timeout" => {
                        self.expect(TokenKind::Equals)?;
                        node.n_cr_timeout = Some(self.milliseconds()?);
                        self.expect(TokenKind::Semicolon)?;
                    }
                    "response_tolerance" => {
                        self.expect(TokenKind::Equals)?;
                        node.response_tolerance = Some(self.percent()?);
                        self.expect(TokenKind::Semicolon)?;
                    }
                    "wakeup_time" => {
                        self.expect(TokenKind::Equals)?;
                        node.wakeup_time = Some(self.milliseconds()?);
                        self.expect(TokenKind::Semicolon)?;
                    }
                    "poweron_time" => {
                        self.expect(TokenKind::Equals)?;
                        node.poweron_time = Some(self.milliseconds()?);
                        self.expect(TokenKind::Semicolon)?;
                    }
                    "configurable_frames" => {
                        self.expect(TokenKind::LBrace)?;
                        let mut implicit_index = 0_u16;
                        while !self.take(TokenKind::RBrace) {
                            let frame = self.expect_identifier()?;
                            let index = if self.take(TokenKind::Equals) {
                                self.expect_u16()?
                            } else {
                                implicit_index
                            };
                            self.expect(TokenKind::Semicolon)?;
                            node.configurable_frames
                                .push(ConfigurableFrame { index, frame });
                            implicit_index = implicit_index.saturating_add(1);
                        }
                    }
                    unknown => return self.error(format!("unknown node attribute {unknown:?}")),
                }
            }
            if self
                .builder
                .slave_attributes
                .insert(name.clone(), node)
                .is_some()
            {
                return Err(Error::Invalid(format!(
                    "duplicate node attributes for {name:?}"
                )));
            }
        }
        Ok(())
    }

    fn schedule_tables(&mut self) -> Result<()> {
        self.expect(TokenKind::LBrace)?;
        while !self.take(TokenKind::RBrace) {
            let name = self.expect_identifier()?;
            self.expect(TokenKind::LBrace)?;
            let mut entries = Vec::new();
            while !self.take(TokenKind::RBrace) {
                let command_name = self.expect_identifier()?;
                let command = self.schedule_command(command_name)?;
                self.expect_word("delay")?;
                let delay = self.milliseconds()?;
                self.expect(TokenKind::Semicolon)?;
                entries.push(ScheduleEntry { command, delay });
            }
            let table = ScheduleTable {
                name: name.clone(),
                entries,
            };
            insert_unique(&mut self.builder.schedule_tables, name, table)?;
        }
        Ok(())
    }

    fn schedule_command(&mut self, name: String) -> Result<ScheduleCommand> {
        Ok(match name.as_str() {
            "MasterReq" => ScheduleCommand::MasterRequest,
            "SlaveResp" => ScheduleCommand::SlaveResponse,
            "AssignNAD" => {
                self.expect(TokenKind::LBrace)?;
                let node = self.expect_identifier()?;
                self.expect(TokenKind::RBrace)?;
                ScheduleCommand::AssignNad { node }
            }
            "ConditionalChangeNAD" => {
                self.expect(TokenKind::LBrace)?;
                let values = self.comma_bytes::<6>()?;
                self.expect(TokenKind::RBrace)?;
                ScheduleCommand::ConditionalChangeNad {
                    nad: values[0],
                    identifier: values[1],
                    byte: values[2],
                    mask: values[3],
                    invert: values[4],
                    new_nad: values[5],
                }
            }
            "DataDump" => {
                self.expect(TokenKind::LBrace)?;
                let node = self.expect_identifier()?;
                self.expect(TokenKind::Comma)?;
                let data = self.comma_bytes::<5>()?;
                self.expect(TokenKind::RBrace)?;
                ScheduleCommand::DataDump { node, data }
            }
            "SaveConfiguration" => {
                self.expect(TokenKind::LBrace)?;
                let node = self.expect_identifier()?;
                self.expect(TokenKind::RBrace)?;
                ScheduleCommand::SaveConfiguration { node }
            }
            "AssignFrameIdRange" => {
                self.expect(TokenKind::LBrace)?;
                let node = self.expect_identifier()?;
                self.expect(TokenKind::Comma)?;
                let frame_index = self.expect_u8()?;
                let protected_ids = if self.take(TokenKind::Comma) {
                    Some(self.comma_bytes::<4>()?)
                } else {
                    None
                };
                self.expect(TokenKind::RBrace)?;
                ScheduleCommand::AssignFrameIdRange {
                    node,
                    frame_index,
                    protected_ids,
                }
            }
            "AssignFrameId" | "UnassignFrameId" => {
                self.expect(TokenKind::LBrace)?;
                let node = self.expect_identifier()?;
                self.expect(TokenKind::Comma)?;
                let frame = self.expect_identifier()?;
                self.expect(TokenKind::RBrace)?;
                if name == "AssignFrameId" {
                    ScheduleCommand::AssignFrameId { node, frame }
                } else {
                    ScheduleCommand::UnassignFrameId { node, frame }
                }
            }
            "FreeFormat" => {
                self.expect(TokenKind::LBrace)?;
                let data = self.comma_bytes::<8>()?;
                self.expect(TokenKind::RBrace)?;
                ScheduleCommand::FreeFormat(data)
            }
            frame => ScheduleCommand::Frame(frame.to_string()),
        })
    }

    fn comma_bytes<const N: usize>(&mut self) -> Result<[u8; N]> {
        let mut output = [0; N];
        for (index, byte) in output.iter_mut().enumerate() {
            if index > 0 {
                self.expect(TokenKind::Comma)?;
            }
            *byte = self.expect_u8()?;
        }
        Ok(output)
    }

    fn signal_groups(&mut self) -> Result<()> {
        self.expect(TokenKind::LBrace)?;
        while !self.take(TokenKind::RBrace) {
            let name = self.expect_identifier()?;
            self.expect(TokenKind::Colon)?;
            let size = self.expect_u16()?;
            self.expect(TokenKind::LBrace)?;
            let signals = self.signal_placements()?;
            self.expect(TokenKind::RBrace)?;
            let group = SignalGroup {
                name: name.clone(),
                size,
                signals,
            };
            insert_unique(&mut self.builder.signal_groups, name, group)?;
        }
        Ok(())
    }

    fn signal_encoding_types(&mut self) -> Result<()> {
        self.expect(TokenKind::LBrace)?;
        while !self.take(TokenKind::RBrace) {
            let name = self.expect_identifier()?;
            self.expect(TokenKind::LBrace)?;
            let mut values = Vec::new();
            while !self.take(TokenKind::RBrace) {
                let kind = self.expect_identifier()?;
                let value = match kind.as_str() {
                    "logical_value" => {
                        self.expect(TokenKind::Comma)?;
                        let raw = self.expect_integer()?;
                        let text = if self.take(TokenKind::Comma) {
                            Some(self.expect_string()?)
                        } else {
                            None
                        };
                        EncodingValue::Logical { raw, text }
                    }
                    "physical_value" => {
                        self.expect(TokenKind::Comma)?;
                        let raw_min = self.expect_integer()?;
                        self.expect(TokenKind::Comma)?;
                        let raw_max = self.expect_integer()?;
                        self.expect(TokenKind::Comma)?;
                        let scale = self.expect_float()?;
                        self.expect(TokenKind::Comma)?;
                        let offset = self.expect_float()?;
                        let unit = if self.take(TokenKind::Comma) {
                            Some(self.expect_string()?)
                        } else {
                            None
                        };
                        EncodingValue::Physical {
                            raw_min,
                            raw_max,
                            scale,
                            offset,
                            unit,
                        }
                    }
                    "bcd_value" => EncodingValue::Bcd,
                    "ascii_value" => EncodingValue::Ascii,
                    unknown => return self.error(format!("unknown encoding value {unknown:?}")),
                };
                self.expect(TokenKind::Semicolon)?;
                values.push(value);
            }
            let encoding = SignalEncodingType {
                name: name.clone(),
                values,
            };
            insert_unique(&mut self.builder.signal_encoding_types, name, encoding)?;
        }
        Ok(())
    }

    fn signal_representations(&mut self) -> Result<()> {
        self.expect(TokenKind::LBrace)?;
        while !self.take(TokenKind::RBrace) {
            let encoding = self.expect_identifier()?;
            self.expect(TokenKind::Colon)?;
            let signals = self.identifier_list_until_semicolon()?;
            self.builder.representations.push((encoding, signals));
        }
        Ok(())
    }

    fn finish(mut self) -> Result<Ldf> {
        let protocol_version = self
            .builder
            .protocol_version
            .take()
            .ok_or_else(|| Error::Invalid("missing LIN_protocol_version".to_string()))?;
        let language_version = self
            .builder
            .language_version
            .take()
            .ok_or_else(|| Error::Invalid("missing LIN_language_version".to_string()))?;
        let baud_rate = self
            .builder
            .baud_rate
            .ok_or_else(|| Error::Invalid("missing LIN_speed".to_string()))?;
        let mut master = self
            .builder
            .master
            .take()
            .ok_or_else(|| Error::Invalid("missing master node".to_string()))?;
        if protocol_version.is_j2602() {
            master.max_header_length_bits.get_or_insert(48);
            master.response_tolerance.get_or_insert(0.4);
        }

        ensure_unique_names(&self.builder.slave_names, "slave")?;
        if self
            .builder
            .slave_names
            .iter()
            .any(|name| name == &master.name)
        {
            return Err(Error::Invalid(format!(
                "master node {:?} is also listed as a slave",
                master.name
            )));
        }
        for name in self.builder.slave_attributes.keys() {
            if !self.builder.slave_names.contains(name) {
                return Err(Error::Invalid(format!(
                    "node {name:?} has attributes but is not listed as a slave"
                )));
            }
        }
        let mut slaves = IndexMap::new();
        for name in &self.builder.slave_names {
            let attributes = self
                .builder
                .slave_attributes
                .swap_remove(name)
                .unwrap_or_default();
            if language_version >= LinVersion::LIN_2_0 && !self.builder.has_node_attributes {
                return Err(Error::Invalid(
                    "Node_attributes is required for LIN 2.0 and newer".to_string(),
                ));
            }
            if language_version >= LinVersion::LIN_2_0 && attributes.protocol_version.is_none() {
                return Err(Error::Invalid(format!("node {name:?} has no LIN_protocol")));
            }
            let node_protocol = attributes
                .protocol_version
                .unwrap_or_else(|| protocol_version.clone());
            if language_version >= LinVersion::LIN_2_0 && attributes.configured_nad.is_none() {
                return Err(Error::Invalid(format!(
                    "node {name:?} has no configured_NAD"
                )));
            }
            if language_version >= LinVersion::LIN_2_1 && attributes.product_id.is_none() {
                return Err(Error::Invalid(format!("node {name:?} has no product_id")));
            }
            let configured_nad = attributes
                .configured_nad
                .or_else(|| self.builder.diagnostic_addresses.get(name).copied());
            let initial_nad = attributes.initial_nad.or(configured_nad);
            let j2602 = node_protocol.is_j2602();
            slaves.insert(
                name.clone(),
                SlaveNode {
                    name: name.clone(),
                    protocol_version: node_protocol,
                    configured_nad,
                    initial_nad,
                    product_id: attributes.product_id,
                    response_error: attributes.response_error,
                    fault_state_signals: attributes.fault_state_signals,
                    p2_min: attributes.p2_min.unwrap_or(Duration::from_millis(50)),
                    st_min: attributes.st_min.unwrap_or(Duration::ZERO),
                    n_as_timeout: attributes.n_as_timeout.unwrap_or(Duration::from_secs(1)),
                    n_cr_timeout: attributes.n_cr_timeout.unwrap_or(Duration::from_secs(1)),
                    configurable_frames: attributes.configurable_frames,
                    response_tolerance: attributes.response_tolerance.or(j2602.then_some(0.4)),
                    wakeup_time: attributes
                        .wakeup_time
                        .or(j2602.then_some(Duration::from_millis(100))),
                    poweron_time: attributes
                        .poweron_time
                        .or(j2602.then_some(Duration::from_millis(100))),
                },
            );
        }

        for (encoding, signal_names) in &self.builder.representations {
            if !self.builder.signal_encoding_types.contains_key(encoding) {
                return Err(Error::Invalid(format!(
                    "signal representation references missing encoding {encoding:?}"
                )));
            }
            for signal_name in signal_names {
                let signal = self.builder.signals.get_mut(signal_name).ok_or_else(|| {
                    Error::Invalid(format!(
                        "signal representation references missing signal {signal_name:?}"
                    ))
                })?;
                if signal.encoding_type.replace(encoding.clone()).is_some() {
                    return Err(Error::Invalid(format!(
                        "signal {signal_name:?} has more than one encoding type"
                    )));
                }
            }
        }

        let ldf = Ldf {
            protocol_version,
            language_version,
            baud_rate,
            channel_name: self.builder.channel_name,
            file_revision: self.builder.file_revision,
            signal_byte_order: self.builder.signal_byte_order,
            master,
            slaves,
            signals: self.builder.signals,
            diagnostic_signals: self.builder.diagnostic_signals,
            unconditional_frames: self.builder.unconditional_frames,
            sporadic_frames: self.builder.sporadic_frames,
            event_triggered_frames: self.builder.event_triggered_frames,
            diagnostic_frames: self.builder.diagnostic_frames,
            diagnostic_addresses: self.builder.diagnostic_addresses,
            node_compositions: self.builder.node_compositions,
            schedule_tables: self.builder.schedule_tables,
            signal_groups: self.builder.signal_groups,
            signal_encoding_types: self.builder.signal_encoding_types,
            comments: self.builder.comments,
            has_node_attributes: self.builder.has_node_attributes,
        };
        validate(&ldf)?;
        Ok(ldf)
    }

    fn version(&mut self) -> Result<LinVersion> {
        let token = self.next().ok_or_else(|| self.unexpected("version"))?;
        let mut value = match token.kind {
            TokenKind::String(value) | TokenKind::Word(value) | TokenKind::Number(value) => value,
            _ => return Err(self.at_token(&token, "expected LIN version")),
        };
        if value == "ISO17987" && self.take(TokenKind::Colon) {
            let revision = self.expect_integer()?;
            value = format!("ISO17987:{revision}");
        }
        value.parse()
    }

    fn milliseconds(&mut self) -> Result<Duration> {
        let value = self.expect_float()?;
        self.expect_word("ms")?;
        if !value.is_finite() || value < 0.0 {
            return Err(Error::Invalid(format!("invalid duration {value} ms")));
        }
        Ok(Duration::from_secs_f64(value / 1000.0))
    }

    fn percent(&mut self) -> Result<f64> {
        let value = self.expect_float()?;
        self.expect(TokenKind::Percent)?;
        Ok(value / 100.0)
    }

    fn identifier_list_until_semicolon(&mut self) -> Result<Vec<String>> {
        let mut output = Vec::new();
        if !self.check(TokenKind::Semicolon) {
            loop {
                output.push(self.expect_identifier()?);
                if !self.take(TokenKind::Comma) {
                    break;
                }
            }
        }
        self.expect(TokenKind::Semicolon)?;
        Ok(output)
    }

    fn expect_identifier(&mut self) -> Result<String> {
        let token = self.next().ok_or_else(|| self.unexpected("identifier"))?;
        match token.kind {
            TokenKind::Word(value) => Ok(value),
            _ => Err(self.at_token(&token, "expected identifier")),
        }
    }

    fn expect_string(&mut self) -> Result<String> {
        let token = self.next().ok_or_else(|| self.unexpected("string"))?;
        match token.kind {
            TokenKind::String(value) => Ok(value),
            _ => Err(self.at_token(&token, "expected quoted string")),
        }
    }

    fn expect_integer(&mut self) -> Result<i64> {
        let token = self.next().ok_or_else(|| self.unexpected("integer"))?;
        let TokenKind::Number(value) = &token.kind else {
            return Err(self.at_token(&token, "expected integer"));
        };
        let hexadecimal = value.starts_with("0x")
            || value.starts_with("0X")
            || value.starts_with("-0x")
            || value.starts_with("-0X");
        if !hexadecimal && value.contains(['.', 'e', 'E']) {
            return Err(self.at_token(&token, "expected integer"));
        }
        let parsed = if let Some(hex) = value
            .strip_prefix("0x")
            .or_else(|| value.strip_prefix("0X"))
        {
            i64::from_str_radix(hex, 16)
        } else if let Some(hex) = value
            .strip_prefix("-0x")
            .or_else(|| value.strip_prefix("-0X"))
        {
            i64::from_str_radix(hex, 16).map(|number| -number)
        } else {
            value.parse()
        };
        parsed.map_err(|_| self.at_token(&token, "invalid integer"))
    }

    fn expect_float(&mut self) -> Result<f64> {
        let token = self.next().ok_or_else(|| self.unexpected("number"))?;
        let TokenKind::Number(value) = &token.kind else {
            return Err(self.at_token(&token, "expected number"));
        };
        value
            .parse()
            .map_err(|_| self.at_token(&token, "invalid number"))
    }

    fn expect_u8(&mut self) -> Result<u8> {
        let value = self.expect_integer()?;
        u8::try_from(value).map_err(|_| Error::Invalid(format!("{value} is outside 0..=255")))
    }

    fn expect_u16(&mut self) -> Result<u16> {
        let value = self.expect_integer()?;
        u16::try_from(value).map_err(|_| Error::Invalid(format!("{value} is outside 0..=65535")))
    }

    fn expect_frame_id(&mut self) -> Result<u8> {
        self.expect_u8()
    }

    fn expect_word(&mut self, expected: &str) -> Result<()> {
        let token = self.next().ok_or_else(|| self.unexpected(expected))?;
        match &token.kind {
            TokenKind::Word(actual) if actual == expected => Ok(()),
            _ => Err(self.at_token(&token, format!("expected {expected:?}"))),
        }
    }

    fn expect(&mut self, expected: TokenKind) -> Result<()> {
        let token = self
            .next()
            .ok_or_else(|| self.unexpected(format!("{expected:?}")))?;
        if token.kind == expected {
            Ok(())
        } else {
            Err(self.at_token(&token, format!("expected {expected:?}")))
        }
    }

    fn take(&mut self, expected: TokenKind) -> bool {
        if self.check(expected) {
            self.position += 1;
            true
        } else {
            false
        }
    }

    fn check(&self, expected: TokenKind) -> bool {
        self.tokens
            .get(self.position)
            .is_some_and(|token| token.kind == expected)
    }

    fn peek_number(&self) -> bool {
        matches!(
            self.tokens.get(self.position).map(|token| &token.kind),
            Some(TokenKind::Number(_))
        )
    }

    fn next(&mut self) -> Option<Token> {
        let token = self.tokens.get(self.position)?.clone();
        self.position += 1;
        Some(token)
    }

    fn at_end(&self) -> bool {
        self.position >= self.tokens.len()
    }

    fn unexpected(&self, expected: impl Into<String>) -> Error {
        let (line, column) = self
            .tokens
            .last()
            .map(|token| (token.line, token.column + 1))
            .unwrap_or((1, 1));
        Error::Parse {
            line,
            column,
            message: format!("unexpected end of input; expected {}", expected.into()),
        }
    }

    fn at_token(&self, token: &Token, message: impl Into<String>) -> Error {
        Error::Parse {
            line: token.line,
            column: token.column,
            message: message.into(),
        }
    }

    fn error<T>(&self, message: impl Into<String>) -> Result<T> {
        let (line, column) = self
            .tokens
            .get(self.position.saturating_sub(1))
            .map(|token| (token.line, token.column))
            .unwrap_or((1, 1));
        Err(Error::Parse {
            line,
            column,
            message: message.into(),
        })
    }
}

fn derive_frame_length(id: u8, language_version: Option<&LinVersion>) -> Result<u8> {
    if language_version.is_some_and(|version| version > &LinVersion::LIN_2_0) {
        return Err(Error::Invalid(format!(
            "frame {id:#x} has no length; this is only allowed through LIN 2.0"
        )));
    }
    Ok(match id {
        0..=31 => 2,
        32..=47 => 4,
        _ => 8,
    })
}

fn insert_unique<T>(map: &mut IndexMap<String, T>, name: String, value: T) -> Result<()> {
    if map.insert(name.clone(), value).is_some() {
        return Err(Error::Invalid(format!("duplicate declaration {name:?}")));
    }
    Ok(())
}

fn ensure_unique_names(names: &[String], kind: &str) -> Result<()> {
    for (index, name) in names.iter().enumerate() {
        if names[..index].contains(name) {
            return Err(Error::Invalid(format!("duplicate {kind} {name:?}")));
        }
    }
    Ok(())
}

pub(crate) fn validate(ldf: &Ldf) -> Result<()> {
    // The writer emits comments verbatim, so each must lex back to comments only.
    for comment in &ldf.comments {
        if !matches!(Lexer::new(comment).tokenize(), Ok((tokens, _)) if tokens.is_empty()) {
            return Err(Error::Invalid(format!(
                "comment {comment:?} is not delimited by // or /* */"
            )));
        }
    }
    let node_exists = |name: &str| name == ldf.master.name || ldf.slaves.contains_key(name);
    for signal in ldf.signals.values() {
        let publisher = signal
            .publisher
            .as_deref()
            .ok_or_else(|| Error::Invalid(format!("signal {} has no publisher", signal.name)))?;
        if !node_exists(publisher) {
            return Err(Error::Invalid(format!(
                "signal {} references missing publisher {publisher:?}",
                signal.name
            )));
        }
        for subscriber in &signal.subscribers {
            if !node_exists(subscriber) {
                return Err(Error::Invalid(format!(
                    "signal {} references missing subscriber {subscriber:?}",
                    signal.name
                )));
            }
        }
    }
    let mut ids = IndexMap::<u8, String>::new();
    for frame in ldf.unconditional_frames.values() {
        if !node_exists(&frame.publisher) {
            return Err(Error::Invalid(format!(
                "frame {} references missing publisher {:?}",
                frame.name, frame.publisher
            )));
        }
        validate_layout(&frame.name, frame.length, &frame.signals, &ldf.signals)?;
        insert_frame_id(&mut ids, frame.id, &frame.name)?;
    }
    for frame in ldf.event_triggered_frames.values() {
        insert_frame_id(&mut ids, frame.id, &frame.name)?;
        for referenced in &frame.frames {
            if !ldf.unconditional_frames.contains_key(referenced) {
                return Err(Error::Invalid(format!(
                    "event-triggered frame {} references missing frame {referenced:?}",
                    frame.name
                )));
            }
        }
        if let Some(schedule) = &frame.collision_resolving_schedule {
            if !ldf.schedule_tables.contains_key(schedule) {
                return Err(Error::Invalid(format!(
                    "event-triggered frame {} references missing schedule {schedule:?}",
                    frame.name
                )));
            }
        }
    }
    for frame in ldf.sporadic_frames.values() {
        for referenced in &frame.frames {
            if !ldf.unconditional_frames.contains_key(referenced) {
                return Err(Error::Invalid(format!(
                    "sporadic frame {} references missing frame {referenced:?}",
                    frame.name
                )));
            }
        }
    }
    for frame in ldf.diagnostic_frames.values() {
        validate_layout(&frame.name, 8, &frame.signals, &ldf.diagnostic_signals)?;
        insert_frame_id(&mut ids, frame.id, &frame.name)?;
    }
    for slave in ldf.slaves.values() {
        if let Some(signal) = &slave.response_error {
            if !ldf.signals.contains_key(signal) {
                return Err(Error::Invalid(format!(
                    "node {} references missing response_error signal {signal:?}",
                    slave.name
                )));
            }
        }
        for signal in &slave.fault_state_signals {
            if !ldf.signals.contains_key(signal) {
                return Err(Error::Invalid(format!(
                    "node {} references missing fault-state signal {signal:?}",
                    slave.name
                )));
            }
        }
        for frame in &slave.configurable_frames {
            if ldf.frame(&frame.frame).is_none() {
                return Err(Error::Invalid(format!(
                    "node {} references missing configurable frame {:?}",
                    slave.name, frame.frame
                )));
            }
        }
    }
    for configuration in &ldf.node_compositions {
        for composition in &configuration.compositions {
            for node in &composition.nodes {
                if !ldf.slaves.contains_key(node) {
                    return Err(Error::Invalid(format!(
                        "composition {} references missing slave {node:?}",
                        composition.name
                    )));
                }
            }
        }
    }
    for schedule in ldf.schedule_tables.values() {
        for entry in &schedule.entries {
            validate_schedule_command(ldf, &schedule.name, &entry.command)?;
        }
    }
    for group in ldf.signal_groups.values() {
        for placement in &group.signals {
            if !ldf.signals.contains_key(&placement.signal) {
                return Err(Error::Invalid(format!(
                    "signal group {} references missing signal {:?}",
                    group.name, placement.signal
                )));
            }
        }
    }
    Ok(())
}

fn insert_frame_id(ids: &mut IndexMap<u8, String>, id: u8, name: &str) -> Result<()> {
    if let Some(previous) = ids.insert(id, name.to_string()) {
        return Err(Error::Invalid(format!(
            "frames {previous:?} and {name:?} share ID {id:#x}"
        )));
    }
    Ok(())
}

fn validate_layout(
    frame_name: &str,
    length: u8,
    placements: &[SignalPlacement],
    signals: &IndexMap<String, Signal>,
) -> Result<()> {
    if !(1..=8).contains(&length) {
        return Err(Error::Invalid(format!(
            "frame {frame_name} length {length} is outside 1..=8"
        )));
    }
    let mut ordered = placements.to_vec();
    ordered.sort_by_key(|placement| placement.bit_offset);
    let mut end = 0_u16;
    let mut previous = None::<&str>;
    for placement in &ordered {
        let signal = signals.get(&placement.signal).ok_or_else(|| {
            Error::Invalid(format!(
                "frame {frame_name} references missing signal {:?}",
                placement.signal
            ))
        })?;
        if placement.bit_offset < end {
            return Err(Error::Invalid(format!(
                "frame {frame_name} signal {} overlaps {}",
                signal.name,
                previous.unwrap_or("a previous signal")
            )));
        }
        end = placement.bit_offset + u16::from(signal.width);
        if end > u16::from(length) * 8 {
            return Err(Error::Invalid(format!(
                "frame {frame_name} signal {} extends beyond the payload",
                signal.name
            )));
        }
        previous = Some(&signal.name);
    }
    Ok(())
}

fn validate_schedule_command(ldf: &Ldf, table: &str, command: &ScheduleCommand) -> Result<()> {
    let require_node = |name: &str| {
        ldf.slaves.contains_key(name).then_some(()).ok_or_else(|| {
            Error::Invalid(format!(
                "schedule {table} references missing slave {name:?}"
            ))
        })
    };
    let require_frame = |name: &str| {
        ldf.frame(name).is_some().then_some(()).ok_or_else(|| {
            Error::Invalid(format!(
                "schedule {table} references missing frame {name:?}"
            ))
        })
    };
    match command {
        ScheduleCommand::Frame(frame) => require_frame(frame),
        ScheduleCommand::AssignNad { node }
        | ScheduleCommand::DataDump { node, .. }
        | ScheduleCommand::SaveConfiguration { node }
        | ScheduleCommand::AssignFrameIdRange { node, .. } => require_node(node),
        ScheduleCommand::AssignFrameId { node, frame }
        | ScheduleCommand::UnassignFrameId { node, frame } => {
            require_node(node)?;
            require_frame(frame)
        }
        ScheduleCommand::MasterRequest
        | ScheduleCommand::SlaveResponse
        | ScheduleCommand::ConditionalChangeNad { .. }
        | ScheduleCommand::FreeFormat(_) => Ok(()),
    }
}
