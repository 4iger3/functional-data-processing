# Typed JSON

A small Scala 3 library and CLI demonstrating type-driven JSON parsing. A schema
connects named JSON fields to typed Scala values; required and optional fields
have different types and validation rules. Invalid data is returned explicitly
as `Either`, and independent field errors are accumulated.

The sample domain is a transaction with a date, category, amount, and optional
note. `amountWithTax` demonstrates a derived value computed from validated data.
It is a regular pure method, not a value read from the JSON input.

## Run

Requirements: Java 17+ and sbt.

```bash
sbt test
sbt "run examples/transactions.json"
```

The input must be a JSON array of transaction objects. Each object must include
`date`, `category`, and `amount`; `note` is optional and may be absent or null.
Dates use `YYYY-MM-DD`; amounts must be JSON numbers. Additional JSON fields are
ignored. Nested values are supported by JSON itself, but the sample transaction
schema intentionally uses scalar fields.

Malformed JSON and invalid fields are reported with record and field information; the application
exits with a non-zero status instead of producing a partial success report.

## Library usage

```scala
val parsed: Either[Vector[JsonError], Vector[Transaction]] =
  JsonData.parseTransactions(jsonText)
```

The schema is assembled from typed `Column[A]` definitions. `required[A]`
produces an `A`, while `optional[A]` produces an `Option[A]`; each uses a
`JsonCodec[A]` to validate and convert JSON values. Adding a schema field
therefore requires handling its corresponding Scala type in the model constructor.

This is a focused educational example, not a general-purpose serialization
framework. The schema describes expected data and validation at runtime; the
compiler cannot validate the contents of an external JSON file.
