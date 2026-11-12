# Typed CSV

A small Scala 3 library and CLI demonstrating type-driven CSV parsing. A schema
connects named CSV columns to typed Scala values; required and optional columns
have different types and validation rules. Invalid data is returned explicitly
as `Either`, and independent field errors are accumulated.

The sample domain is a transaction with a date, category, amount, and optional
note. `amountWithTax` demonstrates a derived value computed from validated data.
It is a regular pure method, not a value read from the CSV.

## Run

Requirements: Java 17+ and sbt.

```bash
sbt test
sbt "run examples/transactions.csv"
```

The CSV header must include `date`, `category`, and `amount`. The `note` column
is optional. Dates use `YYYY-MM-DD`; amounts use decimal notation. Quoted cells,
commas inside quoted cells, escaped quotes (`""`), LF, and CRLF are supported.
Each CSV record must occupy one physical line.

Malformed input is reported with line and field information; the application
exits with a non-zero status instead of producing a partial success report.

## Library usage

```scala
val parsed: Either[Vector[CsvError], Vector[Transaction]] =
  Csv.parse(csvText, Transaction.schema)
```

The schema is assembled from typed `Column[A]` definitions. `required[A]`
produces an `A`, while `optional[A]` produces an `Option[A]`; each uses a
`CsvCodec[A]` to convert and validate text. Adding a schema field therefore
requires handling its corresponding Scala type in the model constructor.

This is a focused educational example, not a general-purpose CSV/JSON
serialization framework. The schema describes expected data and validation at
runtime; the compiler cannot validate the contents of an external CSV file.
