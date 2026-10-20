use autors_ldf::codec::SignalValues;
use autors_ldf::model::{EncodingValue, Ldf, ScheduleCommand, SignalValue};

const SAMPLE: &str = r#"
// compact integration fixture
LIN_description_file;
LIN_protocol_version = "2.2";
LIN_language_version = "2.2";
LIN_speed = 19.2 kbps;
Channel_name = "LIN1";

Nodes {
    Master: Master, 5 ms, 0.1 ms;
    Slaves: Slave;
}

Signals {
    Speed: 8, 0, Slave, Master;
    Error: 1, 0, Slave, Master;
    CommandValue: 8, 7, Master, Slave;
}

Diagnostic_signals {
    Req: 8, 0;
    Resp: 8, 0;
}

Frames {
    Status: 1, Slave, 2 {
        Speed, 0;
        Error, 8;
    }
    Command: 3, Master, 1 {
        CommandValue, 0;
    }
}

Sporadic_frames {
    Burst: Command;
}

Event_triggered_frames {
    StatusEvent: Collision, 2, Status;
}

Diagnostic_frames {
    MasterReq: 60 { Req, 0; }
    SlaveResp: 61 { Resp, 0; }
}

Node_attributes {
    Slave {
        LIN_protocol = "2.2";
        configured_NAD = 1;
        product_id = 1, 2, 3;
        response_error = Error;
        fault_state_signals = Error;
        configurable_frames {
            Status;
            Command;
            StatusEvent;
        }
    }
}

Schedule_tables {
    Main {
        Command delay 10 ms;
        StatusEvent delay 20 ms;
        MasterReq delay 10 ms;
        SlaveResp delay 10 ms;
    }
    Collision {
        Status delay 10 ms;
    }
    Configuration {
        AssignNAD { Slave } delay 10 ms;
        ConditionalChangeNAD { 127, 1, 3, 1, 255, 1 } delay 10 ms;
        DataDump { Slave, 1, 2, 3, 4, 5 } delay 10 ms;
        SaveConfiguration { Slave } delay 10 ms;
        AssignFrameIdRange { Slave, 0, 64, 66, 255, 255 } delay 10 ms;
        AssignFrameId { Slave, Status } delay 10 ms;
        UnassignFrameId { Slave, Status } delay 10 ms;
        FreeFormat { 60, 178, 0, 0, 255, 127, 255, 255 } delay 10 ms;
    }
}

Signal_groups {
    StatusGroup: 9 {
        Speed, 0;
        Error, 8;
    }
}

Signal_encoding_types {
    SpeedType {
        physical_value, 0, 254, 2, 0, "rpm";
        logical_value, 255, "invalid";
    }
    ErrorType {
        logical_value, 0, "ok";
        logical_value, 1, "fault";
    }
}

Signal_representation {
    SpeedType: Speed;
    ErrorType: Error;
}
"#;

#[test]
fn parses_model_and_round_trips_without_loss() {
    let first = Ldf::parse_str(SAMPLE).unwrap();
    assert_eq!(first.baud_rate, 19_200);
    assert_eq!(first.channel_name.as_deref(), Some("LIN1"));
    assert_eq!(first.master.name, "Master");
    assert_eq!(first.slaves["Slave"].configured_nad, Some(1));
    assert_eq!(first.unconditional_frames["Status"].length, 2);
    assert_eq!(first.signal_groups["StatusGroup"].size, 9);
    assert!(matches!(
        first.schedule_tables["Configuration"].entries[4].command,
        ScheduleCommand::AssignFrameIdRange {
            protected_ids: Some([64, 66, 255, 255]),
            ..
        }
    ));
    assert!(matches!(
        first.signal_encoding_types["SpeedType"].values[0],
        EncodingValue::Physical { .. }
    ));
    assert_eq!(first.comments, ["// compact integration fixture"]);

    let serialized = first.write_string().unwrap();
    let second = Ldf::parse_str(&serialized).unwrap();
    assert_eq!(first, second);
}

#[test]
fn parsed_encodings_drive_frame_codec() {
    let ldf = Ldf::parse_str(SAMPLE).unwrap();
    let mut values = SignalValues::new();
    values.insert("Speed".to_string(), SignalValue::Text("100rpm".to_string()));
    values.insert("Error".to_string(), SignalValue::Text("fault".to_string()));
    let payload = ldf.encode_frame("Status", &values).unwrap();
    assert_eq!(payload, [50, 1]);

    let decoded = ldf.decode_frame("Status", &payload, true).unwrap();
    assert_eq!(
        decoded["Speed"],
        SignalValue::Text("100.000 rpm".to_string())
    );
    assert_eq!(decoded["Error"], SignalValue::Text("fault".to_string()));
}

#[test]
fn rejects_cross_reference_and_layout_errors() {
    let missing_node = SAMPLE.replace(
        "Speed: 8, 0, Slave, Master;",
        "Speed: 8, 0, Missing, Master;",
    );
    assert!(Ldf::parse_str(&missing_node).is_err());

    let overlap = SAMPLE.replace("Error, 8;", "Error, 7;");
    assert!(Ldf::parse_str(&overlap).is_err());
}

#[test]
fn node_attributes_are_written_from_lin_2_0_or_when_declared() {
    assert!(Ldf::parse_str(SAMPLE).unwrap().writes_node_attributes());

    let lin13 = r#"
LIN_description_file;
LIN_protocol_version = "1.3";
LIN_language_version = "1.3";
LIN_speed = 19.2 kbps;
Nodes { Master: Master, 5 ms, 0.1 ms; Slaves: Slave; }
Signals { Speed: 8, 0, Slave, Master; }
Frames { Status: 1, Slave, 1 { Speed, 0; } }
"#;
    let plain = Ldf::parse_str(lin13).unwrap();
    assert!(!plain.writes_node_attributes());
    assert!(!plain.write_string().unwrap().contains("Node_attributes"));

    let declared = Ldf::parse_str(&format!(
        "{lin13}Node_attributes {{ Slave {{ LIN_protocol = \"1.3\"; P2_min = 75 ms; }} }}\n"
    ))
    .unwrap();
    assert!(declared.writes_node_attributes());
    let reparsed = Ldf::parse_str(&declared.write_string().unwrap()).unwrap();
    assert!(reparsed.writes_node_attributes());
    assert_eq!(reparsed.slaves["Slave"].p2_min.as_millis(), 75);
}

#[test]
fn comments_must_keep_their_delimiters_to_be_written() {
    let mut ldf = Ldf::parse_str(SAMPLE).unwrap();
    ldf.comments = vec!["// line
/* block
spans */"
        .to_string()];
    let reparsed = Ldf::parse_str(&ldf.write_string().unwrap()).unwrap();
    assert_eq!(
        reparsed.comments,
        [
            "// line",
            "/* block
spans */"
        ]
    );

    for invalid in [
        "plain text",
        "// ok
plain",
        "/* open",
        "/* a */ b */",
    ] {
        ldf.comments = vec![invalid.to_string()];
        assert!(ldf.validate().is_err(), "{invalid:?}");
        assert!(ldf.write_string().is_err(), "{invalid:?}");
    }
}
