# Typed JSON

A small Scala 3 library and CLI demonstrating type-driven JSON parsing. A schema
connects named JSON fields to typed Scala values; required and optional fields
have different types and validation rules. Decode results carry either typed
data or structured errors, alongside any non-fatal warnings.

The sample domain is a transaction with a date, non-empty category, bounded
amount (0–1,000,000), and optional note. A note is required for transactions of
1000 or more.
`amountWithTax` demonstrates a derived value computed from validated data.

## Run

Requirements: Java 17+ and sbt.

```bash
sbt test
sbt "run examples/transactions.json"
```

The input must be a JSON array of transaction objects. Each object must include
`date`, `category`, and `amount`; `note` is optional and may be absent or null.
Dates use `YYYY-MM-DD`; amounts must be JSON numbers. Additional JSON fields are
ignored with warnings. Schemas can be composed to validate nested objects.

Malformed JSON and invalid fields are reported with precise paths; the
application exits with a non-zero status instead of producing a partial success
report.

## Library usage

```scala
val parsed: DecodeResult[Vector[Transaction]] =
  JsonData.parseTransactions(jsonText)
```

The schema is assembled from typed `Column[A]` definitions. `required[A]`
produces an `A`, while `optional[A]` produces an `Option[A]`; each uses a
`JsonCodec[A]` to validate and convert JSON values. Adding a schema field
therefore requires handling its corresponding Scala type in the model constructor.

The codec typeclass includes reusable `Option[A]` and `List[A]` instances.
Missing or `null` optional fields decode as `None`; list item errors are
accumulated with paths that include each zero-based index. `NonEmptyString` is
an opaque refinement type that can only be constructed through validation.
Schemas can add cross-field rules with `validateWith`, which runs after the
independent fields have decoded successfully.

This is a focused educational example, not a general-purpose serialization
framework. The schema describes expected data and validation at runtime; the
compiler cannot validate the contents of an external JSON file.

## Diagnostics

Decode paths use JSON-style zero-based array indices, for example
`$[0].details.amount`. Errors are structured as `MissingField`, `WrongType`,
`ValidationFailed`, `OutOfRange`, and syntax/shape errors. Unknown fields and
deprecated keys are reported as warnings. Warnings are retained even if another
field fails, and never make an otherwise valid document fail.

The result separates a possibly failed value from accumulated warnings:

```scala
val result: DecodeResult[Vector[Transaction]] =
  JsonData.parseTransactions(jsonText)

result.warnings.foreach(println)
result.value match
  case Left(errors) => errors.foreach(println)
  case Right(rows)  => println(rows.size)
```

Independent field checks use `zipAccumulating`, so several errors can be
reported together. Sequential checks use `andThen`, which short-circuits after
the first failure. `orElse` attempts a fallback and combines errors if both
alternatives fail; `recover` explicitly turns an error into a value. Both
fallback operations preserve warnings.

For nested decoding, a schema can be used as a field codec:

```scala
val detailsSchema =
  Schema.field(Column.required[BigDecimal]("amount"))(Details.apply)
val orderSchema =
  Schema.field(Column.required[Details]("details")(using detailsSchema.codec))(Order.apply)
```

For example, the `note` field accepts the deprecated `memo` name and emits a
warning recommending `note`. Other unrecognized keys are ignored with warnings.
