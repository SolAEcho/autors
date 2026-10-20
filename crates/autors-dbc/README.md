# autors-dbc

An editable CAN DBC database model with text parsing, writing, and signal value
extraction.

## Highlights

- `DBCFile` reads DBC text or files and writes the resulting database back.
- Models messages (including `BO_TX_BU_` transmitters), signals, nodes,
  environment variables (with `ENVVAR_DATA_` and value descriptions), value tables,
  comments, attributes, attribute definitions, and signal groups.
- Stable insertion ordering supports predictable round trips.
- `SignalType` handles Intel/Motorola bit placement, signed values,
  multiplexing, masks, raw extraction, and physical conversion.
- Lookup helpers make it easy to find messages and signals by name or CAN ID.

Use this crate for network-description work. Live CAN frames and hardware
access are provided separately by `autors-can`.

## Development

```text
cargo test -p autors-dbc
cargo doc -p autors-dbc --no-deps
```

See the [workspace README](../../README.md) for the bus and logging layers.
