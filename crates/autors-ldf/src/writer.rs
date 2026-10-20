use crate::error::Result;
use crate::model::*;
use indexmap::IndexMap;
use std::fmt::Write;
use std::time::Duration;

trait InfallibleStringWrite {
    fn infallible(self);
}

impl InfallibleStringWrite for std::fmt::Result {
    fn infallible(self) {
        debug_assert!(self.is_ok(), "formatting into a String failed");
    }
}

pub(crate) fn write(ldf: &Ldf) -> Result<String> {
    ldf.validate()?;
    let mut output = String::new();
    for comment in &ldf.comments {
        writeln!(output, "{comment}").infallible();
    }
    if !ldf.comments.is_empty() {
        output.push('\n');
    }
    writeln!(output, "LIN_description_file;").infallible();
    writeln!(
        output,
        "LIN_protocol_version = \"{}\";",
        ldf.protocol_version
    )
    .infallible();
    writeln!(
        output,
        "LIN_language_version = \"{}\";",
        ldf.language_version
    )
    .infallible();
    writeln!(
        output,
        "LIN_speed = {} kbps;",
        format_float(ldf.baud_rate as f64 / 1000.0)
    )
    .infallible();
    if let Some(channel) = &ldf.channel_name {
        writeln!(output, "Channel_name = \"{}\";", escape(channel)).infallible();
    }
    if let Some(revision) = &ldf.file_revision {
        writeln!(output, "LDF_file_revision = \"{}\";", escape(revision)).infallible();
    }
    if let Some(byte_order) = ldf.signal_byte_order {
        match byte_order {
            SignalByteOrder::BigEndian => output.push_str("LIN_sig_byte_order_big_endian;\n"),
            SignalByteOrder::LittleEndian => output.push_str("LIN_sig_byte_order_little_endian;\n"),
        }
    }
    output.push('\n');

    write_nodes(&mut output, ldf);
    write_compositions(&mut output, ldf);
    write_signals(&mut output, "Signals", ldf.signals.values(), false);
    if !ldf.diagnostic_signals.is_empty() {
        write_signals(
            &mut output,
            "Diagnostic_signals",
            ldf.diagnostic_signals.values(),
            true,
        );
    }
    if !ldf.diagnostic_addresses.is_empty() {
        output.push_str("Diagnostic_addresses {\n");
        for (name, address) in &ldf.diagnostic_addresses {
            writeln!(output, "    {name}: {address};").infallible();
        }
        output.push_str("}\n\n");
    }
    write_frames(&mut output, ldf);
    write_sporadic_frames(&mut output, ldf);
    write_event_frames(&mut output, ldf);
    write_diagnostic_frames(&mut output, ldf);
    write_node_attributes(&mut output, ldf);
    write_schedules(&mut output, ldf);
    write_signal_groups(&mut output, ldf);
    write_encodings(&mut output, ldf);
    Ok(output)
}

fn write_nodes(output: &mut String, ldf: &Ldf) {
    output.push_str("Nodes {\n");
    write!(
        output,
        "    Master: {}, {} ms, {} ms",
        ldf.master.name,
        duration_ms(ldf.master.time_base),
        duration_ms(ldf.master.jitter)
    )
    .infallible();
    if let (Some(bits), Some(tolerance)) = (
        ldf.master.max_header_length_bits,
        ldf.master.response_tolerance,
    ) {
        write!(
            output,
            ", {bits} bits, {} %",
            format_float(tolerance * 100.0)
        )
        .infallible();
    }
    output.push_str(";\n");
    output.push_str("    Slaves: ");
    for (index, name) in ldf.slaves.keys().enumerate() {
        if index > 0 {
            output.push_str(", ");
        }
        output.push_str(name);
    }
    output.push_str(";\n}\n\n");
}

fn write_compositions(output: &mut String, ldf: &Ldf) {
    if ldf.node_compositions.is_empty() {
        return;
    }
    output.push_str("composite {\n");
    for configuration in &ldf.node_compositions {
        writeln!(output, "    configuration {} {{", configuration.name).infallible();
        for composition in &configuration.compositions {
            write!(output, "        {} {{ ", composition.name).infallible();
            write_joined(output, composition.nodes.iter(), ", ");
            output.push_str(" }\n");
        }
        output.push_str("    }\n");
    }
    output.push_str("}\n\n");
}

fn write_signals<'a>(
    output: &mut String,
    section: &str,
    signals: impl Iterator<Item = &'a Signal>,
    diagnostic: bool,
) {
    writeln!(output, "{section} {{").infallible();
    for signal in signals {
        write!(
            output,
            "    {}: {}, {}",
            signal.name,
            signal.width,
            signal_initial_value(&signal.initial_value)
        )
        .infallible();
        if !diagnostic {
            if let Some(publisher) = &signal.publisher {
                write!(output, ", {publisher}").infallible();
            }
            for subscriber in &signal.subscribers {
                write!(output, ", {subscriber}").infallible();
            }
        }
        output.push_str(";\n");
    }
    output.push_str("}\n\n");
}

fn signal_initial_value(value: &SignalValue) -> String {
    match value {
        SignalValue::Integer(value) => value.to_string(),
        SignalValue::Bytes(bytes) => {
            let values = bytes
                .iter()
                .map(u8::to_string)
                .collect::<Vec<_>>()
                .join(", ");
            format!("{{ {values} }}")
        }
        SignalValue::Float(_) | SignalValue::Text(_) => unreachable!("validated initial value"),
    }
}

fn write_frames(output: &mut String, ldf: &Ldf) {
    output.push_str("Frames {\n");
    for frame in ldf.unconditional_frames.values() {
        writeln!(
            output,
            "    {}: {}, {}, {} {{",
            frame.name, frame.id, frame.publisher, frame.length
        )
        .infallible();
        write_placements(output, &frame.signals, 8);
        output.push_str("    }\n");
    }
    output.push_str("}\n\n");
}

fn write_sporadic_frames(output: &mut String, ldf: &Ldf) {
    if ldf.sporadic_frames.is_empty() {
        return;
    }
    output.push_str("Sporadic_frames {\n");
    for frame in ldf.sporadic_frames.values() {
        write!(output, "    {}: ", frame.name).infallible();
        write_joined(output, frame.frames.iter(), ", ");
        output.push_str(";\n");
    }
    output.push_str("}\n\n");
}

fn write_event_frames(output: &mut String, ldf: &Ldf) {
    if ldf.event_triggered_frames.is_empty() {
        return;
    }
    output.push_str("Event_triggered_frames {\n");
    for frame in ldf.event_triggered_frames.values() {
        write!(output, "    {}: ", frame.name).infallible();
        if let Some(schedule) = &frame.collision_resolving_schedule {
            write!(output, "{schedule}, ").infallible();
        }
        write!(output, "{}", frame.id).infallible();
        for referenced in &frame.frames {
            write!(output, ", {referenced}").infallible();
        }
        output.push_str(";\n");
    }
    output.push_str("}\n\n");
}

fn write_diagnostic_frames(output: &mut String, ldf: &Ldf) {
    if ldf.diagnostic_frames.is_empty() {
        return;
    }
    output.push_str("Diagnostic_frames {\n");
    for frame in ldf.diagnostic_frames.values() {
        writeln!(output, "    {}: {} {{", frame.name, frame.id).infallible();
        write_placements(output, &frame.signals, 8);
        output.push_str("    }\n");
    }
    output.push_str("}\n\n");
}

fn write_placements(output: &mut String, placements: &[SignalPlacement], indent: usize) {
    let padding = " ".repeat(indent);
    for placement in placements {
        writeln!(
            output,
            "{padding}{}, {};",
            placement.signal, placement.bit_offset
        )
        .infallible();
    }
}

fn write_node_attributes(output: &mut String, ldf: &Ldf) {
    if !ldf.writes_node_attributes() {
        return;
    }
    output.push_str("Node_attributes {\n");
    for node in ldf.slaves.values() {
        writeln!(output, "    {} {{", node.name).infallible();
        writeln!(
            output,
            "        LIN_protocol = \"{}\";",
            node.protocol_version
        )
        .infallible();
        if let Some(nad) = node.configured_nad {
            writeln!(output, "        configured_NAD = {nad};").infallible();
        }
        if node.initial_nad != node.configured_nad {
            if let Some(nad) = node.initial_nad {
                writeln!(output, "        initial_NAD = {nad};").infallible();
            }
        }
        if let Some(product) = node.product_id {
            writeln!(
                output,
                "        product_id = {}, {}, {};",
                product.supplier_id, product.function_id, product.variant
            )
            .infallible();
        }
        if let Some(signal) = &node.response_error {
            writeln!(output, "        response_error = {signal};").infallible();
        }
        if !node.fault_state_signals.is_empty() {
            output.push_str("        fault_state_signals = ");
            write_joined(output, node.fault_state_signals.iter(), ", ");
            output.push_str(";\n");
        }
        writeln!(output, "        P2_min = {} ms;", duration_ms(node.p2_min)).infallible();
        writeln!(output, "        ST_min = {} ms;", duration_ms(node.st_min)).infallible();
        writeln!(
            output,
            "        N_As_timeout = {} ms;",
            duration_ms(node.n_as_timeout)
        )
        .infallible();
        writeln!(
            output,
            "        N_Cr_timeout = {} ms;",
            duration_ms(node.n_cr_timeout)
        )
        .infallible();
        if let Some(tolerance) = node.response_tolerance {
            writeln!(
                output,
                "        response_tolerance = {} %;",
                format_float(tolerance * 100.0)
            )
            .infallible();
        }
        if let Some(duration) = node.wakeup_time {
            writeln!(
                output,
                "        wakeup_time = {} ms;",
                duration_ms(duration)
            )
            .infallible();
        }
        if let Some(duration) = node.poweron_time {
            writeln!(
                output,
                "        poweron_time = {} ms;",
                duration_ms(duration)
            )
            .infallible();
        }
        if !node.configurable_frames.is_empty() {
            output.push_str("        configurable_frames {\n");
            for frame in &node.configurable_frames {
                if node.protocol_version <= LinVersion::LIN_2_0 {
                    writeln!(output, "            {} = {};", frame.frame, frame.index).infallible();
                } else {
                    writeln!(output, "            {};", frame.frame).infallible();
                }
            }
            output.push_str("        }\n");
        }
        output.push_str("    }\n");
    }
    output.push_str("}\n\n");
}

fn write_schedules(output: &mut String, ldf: &Ldf) {
    if ldf.schedule_tables.is_empty() {
        return;
    }
    output.push_str("Schedule_tables {\n");
    for table in ldf.schedule_tables.values() {
        writeln!(output, "    {} {{", table.name).infallible();
        for entry in &table.entries {
            output.push_str("        ");
            write_schedule_command(output, &entry.command);
            writeln!(output, " delay {} ms;", duration_ms(entry.delay)).infallible();
        }
        output.push_str("    }\n");
    }
    output.push_str("}\n\n");
}

fn write_schedule_command(output: &mut String, command: &ScheduleCommand) {
    match command {
        ScheduleCommand::Frame(frame) => output.push_str(frame),
        ScheduleCommand::MasterRequest => output.push_str("MasterReq"),
        ScheduleCommand::SlaveResponse => output.push_str("SlaveResp"),
        ScheduleCommand::AssignNad { node } => {
            write!(output, "AssignNAD {{ {node} }}").infallible()
        }
        ScheduleCommand::ConditionalChangeNad {
            nad,
            identifier,
            byte,
            mask,
            invert,
            new_nad,
        } => write!(
            output,
            "ConditionalChangeNAD {{ {nad}, {identifier}, {byte}, {mask}, {invert}, {new_nad} }}"
        )
        .infallible(),
        ScheduleCommand::DataDump { node, data } => write!(
            output,
            "DataDump {{ {node}, {}, {}, {}, {}, {} }}",
            data[0], data[1], data[2], data[3], data[4]
        )
        .infallible(),
        ScheduleCommand::SaveConfiguration { node } => {
            write!(output, "SaveConfiguration {{ {node} }}").infallible()
        }
        ScheduleCommand::AssignFrameIdRange {
            node,
            frame_index,
            protected_ids,
        } => {
            write!(output, "AssignFrameIdRange {{ {node}, {frame_index}").infallible();
            if let Some(ids) = protected_ids {
                for id in ids {
                    write!(output, ", {id}").infallible();
                }
            }
            output.push_str(" }");
        }
        ScheduleCommand::AssignFrameId { node, frame } => {
            write!(output, "AssignFrameId {{ {node}, {frame} }}").infallible()
        }
        ScheduleCommand::UnassignFrameId { node, frame } => {
            write!(output, "UnassignFrameId {{ {node}, {frame} }}").infallible()
        }
        ScheduleCommand::FreeFormat(data) => {
            output.push_str("FreeFormat { ");
            for (index, byte) in data.iter().enumerate() {
                if index > 0 {
                    output.push_str(", ");
                }
                write!(output, "{byte}").infallible();
            }
            output.push_str(" }");
        }
    }
}

fn write_signal_groups(output: &mut String, ldf: &Ldf) {
    if ldf.signal_groups.is_empty() {
        return;
    }
    output.push_str("Signal_groups {\n");
    for group in ldf.signal_groups.values() {
        writeln!(output, "    {}: {} {{", group.name, group.size).infallible();
        write_placements(output, &group.signals, 8);
        output.push_str("    }\n");
    }
    output.push_str("}\n\n");
}

fn write_encodings(output: &mut String, ldf: &Ldf) {
    if ldf.signal_encoding_types.is_empty() {
        return;
    }
    output.push_str("Signal_encoding_types {\n");
    for encoding in ldf.signal_encoding_types.values() {
        writeln!(output, "    {} {{", encoding.name).infallible();
        for value in &encoding.values {
            output.push_str("        ");
            match value {
                EncodingValue::Logical { raw, text } => {
                    write!(output, "logical_value, {raw}").infallible();
                    if let Some(text) = text {
                        write!(output, ", \"{}\"", escape(text)).infallible();
                    }
                }
                EncodingValue::Physical {
                    raw_min,
                    raw_max,
                    scale,
                    offset,
                    unit,
                } => {
                    write!(
                        output,
                        "physical_value, {raw_min}, {raw_max}, {}, {}",
                        format_float(*scale),
                        format_float(*offset)
                    )
                    .infallible();
                    if let Some(unit) = unit {
                        write!(output, ", \"{}\"", escape(unit)).infallible();
                    }
                }
                EncodingValue::Bcd => output.push_str("bcd_value"),
                EncodingValue::Ascii => output.push_str("ascii_value"),
            }
            output.push_str(";\n");
        }
        output.push_str("    }\n");
    }
    output.push_str("}\n\n");

    let mut representations = IndexMap::<&str, Vec<&str>>::new();
    for signal in ldf.signals.values() {
        if let Some(encoding) = &signal.encoding_type {
            representations
                .entry(encoding.as_str())
                .or_default()
                .push(signal.name.as_str());
        }
    }
    if !representations.is_empty() {
        output.push_str("Signal_representation {\n");
        for (encoding, signals) in representations {
            write!(output, "    {encoding}: ").infallible();
            write_joined(output, signals.into_iter(), ", ");
            output.push_str(";\n");
        }
        output.push_str("}\n\n");
    }
}

fn duration_ms(duration: Duration) -> String {
    format_float(duration.as_secs_f64() * 1000.0)
}

fn format_float(value: f64) -> String {
    let output = value.to_string();
    if output == "-0" {
        "0".to_string()
    } else {
        output
    }
}

fn escape(value: &str) -> String {
    value
        .replace('\\', "\\\\")
        .replace('"', "\\\"")
        .replace('\n', "\\n")
        .replace('\r', "\\r")
        .replace('\t', "\\t")
}

fn write_joined<I, S>(output: &mut String, values: I, separator: &str)
where
    I: IntoIterator<Item = S>,
    S: AsRef<str>,
{
    for (index, value) in values.into_iter().enumerate() {
        if index > 0 {
            output.push_str(separator);
        }
        output.push_str(value.as_ref());
    }
}
