# Typed JSON

Typed JSON is a Scala 3 command-line tool and educational library for validating
JSON Lines (JSONL) files. It is intended for a practical task: check that
records in a data export match an expected structure before importing them into
another system or passing them to a downstream application.

The CLI can use either the built-in transaction format or a schema supplied in
a separate JSON file. It reads records as a stream, reports errors with their
record and field paths, and can write valid records back as JSONL.

## What the program does

For each non-empty line in an input file, the program:

1. Reads the line with a configurable size limit.
2. Parses it as one JSON value.
3. Checks that value against the selected schema.
4. Reports errors and warnings, while continuing with later records.
5. In `parse` mode, writes each valid record to standard output.

For example, the included inventory schema can check product identifiers,
quantities, tags, and nested supplier data. This can be useful for catching
malformed records in exports before they are loaded into a database, sent to an
API, or used by another batch job.

The project is also a demonstration of functional programming in Scala 3:
schemas and codecs are values, validation results are explicit, independent
errors can accumulate, and file processing uses FS2 streams.

## Requirements

- Java 17 or newer
- sbt

## Run the examples

Run the tests:

```bash
sbt test
```

Validate the built-in transaction example:

```bash
sbt "run validate examples/transactions.jsonl"
```

Validate the included inventory data using an external schema:

```bash
sbt "run validate examples/inventory.jsonl --schema examples/schemas/inventory.schema.json"
```

Print normalized valid inventory records:

```bash
sbt "run parse examples/inventory.jsonl --schema examples/schemas/inventory.schema.json --format json"
```

Other useful commands:

```bash
sbt "run sample"                  # Print built-in transaction JSONL examples
sbt "run explain amount"          # Explain a built-in transaction field
sbt "run schema --format json"    # Print the built-in transaction schema
```

## Use the CLI with your own schema

Create a JSON Schema file describing one JSON object per input line, then pass
it to `validate` or `parse` with `--schema FILE`. For example:

```bash
sbt "run validate data.jsonl --schema schemas/data.schema.json"
sbt "run parse data.jsonl --schema schemas/data.schema.json --format json"
```

Each non-empty line of `data.jsonl` must contain one complete JSON value.
Blank lines are ignored. The external schema validates the input JSON and does
not add defaults, rename fields, or otherwise transform records. In `parse`
mode, valid values are emitted as JSON Lines.

### Supported JSON Schema features

The external schema loader supports a documented subset of JSON Schema
Draft 2020-12:

- Objects with `properties`, `required`, and boolean `additionalProperties`.
- Nested objects and arrays with an `items` schema.
- `minItems` and `maxItems` for arrays.
- Strings with `minLength`, `maxLength`, and `pattern`.
- Numbers and integers with inclusive `minimum` and `maximum`.
- Boolean and null values.

String patterns use JVM regular-expression syntax, which may differ from
patterns in other JSON Schema implementations. Unsupported keywords, invalid
types, and contradictory bounds are rejected when loading the schema instead
of being silently ignored. Features such as `$ref`, `oneOf`, date formats, and
conditional schemas are not implemented. This is not a complete JSON Schema
validator, so arbitrary schemas from external tools may not be compatible.

`additionalProperties` defaults to `true`: unknown object properties are
accepted. Set it to `false` to reject them. The example files
[inventory.schema.json](./examples/schemas/inventory.schema.json) and
[inventory.jsonl](./examples/inventory.jsonl) demonstrate nested objects,
arrays, required fields, and value constraints.

## Built-in transaction format

When `--schema` is omitted, the CLI validates transaction records. Each record
must have:

| Field | Rule |
|---|---|
| `date` | Required string in `YYYY-MM-DD` format |
| `category` | Required non-empty string |
| `amount` | Required number from 0 to 1,000,000 |
| `note` | Optional string; required when `amount` is at least 1,000 |

The deprecated field name `memo` is accepted in place of `note` and produces
a warning. Unknown fields also produce warnings. The `schema` subcommand
prints the built-in schema; its `x-validationRules` extension documents the
cross-field rule, which is enforced by the Scala decoder.

## CLI behavior and options

`validate` reports a summary; `parse` emits valid records. Both continue
processing after invalid records. The program uses a non-zero exit status if
validation fails. `--strict` also makes warnings fail the run.

`parse` is deliberately streaming and non-atomic: output is written as each
valid record is decoded. If an error appears later, earlier output cannot be
retracted, and valid records after the error may also be emitted. In strict
mode, warnings can likewise cause a failure after output has been written.
When an all-or-nothing result is required, redirect output to a temporary file
and use it only if the command exits successfully.

Common options for `validate` and `parse`:

- `--schema FILE` — validate using an external JSON Schema subset.
- `--config FILE` — load HOCON runtime settings.
- `--format text|json` — choose summary or parsed-record output format.
- `--strict` — treat warnings as failures.
- `--quiet` — suppress valid parsed output, summaries, and warnings; errors
  remain visible.
- `--max-errors N` — cap the number of printed error details. Metrics still
  count all errors.
- `--max-record-bytes N` — maximum input record size, enforced while reading.

Defaults for stream chunking, parallelism, logging, and limits are in
[application.conf](./src/main/resources/application.conf). A supplied HOCON
file overrides those defaults. Structured run logs are written to standard
error, separately from parsed JSONL output on standard output.

## Library API

The JSON Lines streaming API is generic over `JsonCodec[A]`, so Scala
applications can use their own model and schema:

```scala
val records: fs2.Stream[cats.effect.IO, StreamRecord[MyRecord]] =
  JsonLines.records[MyRecord](path, config)(using myRecordSchema.codec)
```

The built-in library schema is assembled from typed `Column[A]` definitions.
`Column.required[A]` decodes to `A`; `Column.optional[A]` decodes to
`Option[A]`. Codecs are provided for primitive values, `Option[A]`, and
`List[A]`. Schemas can be composed for nested objects and extended with
cross-field validation.

The convenience API `JsonData.parseTransactions` accepts a JSON array and
accumulates errors across its elements. This differs from the CLI, which reads
JSON Lines one record at a time.

## Diagnostics and validation model

Errors contain structured paths such as `$[2].supplier.name`. The library
distinguishes missing fields, wrong types, validation failures, out-of-range
values, malformed JSON, and invalid top-level shapes. Warnings are carried
alongside errors and do not fail a run unless strict mode is enabled.

Independent field checks use `zipAccumulating`, allowing multiple errors to be
reported together. Sequential checks use `andThen` and stop after a failure.
`orElse` and `recover` demonstrate fallback and error-recovery behavior.

This project is an educational and practical prototype, not a fully general
serialization framework or production-certified JSON Schema implementation.
