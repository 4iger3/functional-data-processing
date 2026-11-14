package typedjson

import io.circe.Json
import java.time.LocalDate
import munit.FunSuite
import typedjson.JsonError.*

final class JsonSpec extends FunSuite:
  private def category(value: String): NonEmptyString =
    NonEmptyString.from(value).toOption.get

  test("parses typed values and optional fields from a JSON array") {
    val input =
      """[
        |  {"date":"2026-01-03","category":"food","amount":12.50,"note":"lunch"},
        |  {"date":"2026-01-04","category":"travel","amount":30.00,"note":null},
        |  {"date":"2026-01-05","category":"food","amount":8.25}
        |]""".stripMargin

    val result = JsonData.parseTransactions(input)
    assertEquals(
      result.value,
      Right(
        Vector(
          Transaction(LocalDate.parse("2026-01-03"), category("food"), BigDecimal("12.50"), Some("lunch")),
          Transaction(LocalDate.parse("2026-01-04"), category("travel"), BigDecimal("30.00"), None),
          Transaction(LocalDate.parse("2026-01-05"), category("food"), BigDecimal("8.25"), None)
        )
      )
    )
    assertEquals(result.warnings, Vector.empty)
  }

  test("reports typed errors with exact array and field paths") {
    val result = JsonData.parseTransactions(
      """[
        |  {"date":"bad-date","category":false,"amount":"not-a-number"},
        |  {"date":"2026-01-05","amount":10}
        |]""".stripMargin
    )

    assertEquals(
      result.value,
      Left(
        Vector(
          ValidationFailed(JsonPath.root.index(0).field("date"), "expected a date in YYYY-MM-DD format"),
          WrongType(JsonPath.root.index(0).field("category"), "a string", "a boolean"),
          WrongType(JsonPath.root.index(0).field("amount"), "a number", "a string"),
          MissingField(JsonPath.root.index(1).field("category"))
        )
      )
    )
  }

  test("composes schemas and reports paths through nested objects") {
    final case class Details(amount: BigDecimal)
    final case class Order(details: Details)

    val detailsSchema = Schema.field(Column.required[BigDecimal]("amount"))(Details.apply)
    val orderSchema = Schema.field(
      Column.required[Details]("details")(using detailsSchema.codec)
    )(Order.apply)
    val input = Json.obj(
      "details" -> Json.obj("amount" -> Json.fromString("wrong"))
    )
    val result = orderSchema.decodeJson(input, JsonPath.root.index(3))

    assertEquals(
      result.value,
      Left(
        Vector(
          WrongType(
            JsonPath.root.index(3).field("details").field("amount"),
            "a number",
            "a string"
          )
        )
      )
    )
  }

  test("preserves warnings alongside decoding errors") {
    val result = JsonData.parseTransactions(
      """[{"date":"bad","amount":"nope","extra":true}]"""
    )

    assert(result.value.isLeft)
    assertEquals(
      result.warnings.map(_.path),
      Vector(JsonPath.root.index(0).field("extra"))
    )
  }

  test("warns about unknown fields and accepts deprecated aliases") {
    val result = JsonData.parseTransactions(
      """[{"date":"2026-01-03","category":"food","amount":12.5,"memo":"old name","extra":1}]"""
    )

    assertEquals(
      result.value,
      Right(
        Vector(
          Transaction(
            LocalDate.parse("2026-01-03"),
            category("food"),
            BigDecimal("12.5"),
            Some("old name")
          )
        )
      )
    )
    assertEquals(
      result.warnings.map(_.message),
      Vector(
        "Unknown field at $[0].extra; it was ignored.",
        "Deprecated field at $[0].memo; use 'note' instead."
      )
    )
  }

  test("uses the current field when both it and its deprecated alias are present") {
    val result = JsonData.parseTransactions(
      """[{"date":"2026-01-03","category":"food","amount":12.5,"note":"current","memo":"old"}]"""
    )

    assertEquals(result.value.toOption.get.head.note, Some("current"))
    assertEquals(result.warnings.map(_.path), Vector(JsonPath.root.index(0).field("memo")))
  }

  test("accumulates range errors independently") {
    val bounded = Column.required[BigDecimal]("amount")
      .validateRange(BigDecimal(0), BigDecimal(100))
    val schema = Schema.field(bounded)(identity)
    val result = schema.decodeJson(Json.obj("amount" -> Json.fromInt(101)))

    assertEquals(
      result.value,
      Left(
        Vector(
          OutOfRange(JsonPath.root.field("amount"), "0", "100", "101")
        )
      )
    )
  }

  test("decodes Option values from non-null and null JSON values") {
    val codec = summon[JsonCodec[Option[String]]]

    assertEquals(codec.decode(Json.fromString("memo"), JsonPath.root).value, Right(Some("memo")))
    assertEquals(codec.decode(Json.Null, JsonPath.root).value, Right(None))
  }

  test("decodes List values and accumulates item errors with indices") {
    val codec = summon[JsonCodec[List[String]]]
    val path = JsonPath.root.field("tags")
    assertEquals(
      codec.decode(Json.arr(Json.fromString("food"), Json.fromString("work")), path).value,
      Right(List("food", "work"))
    )

    val result = codec.decode(
      Json.arr(Json.fromString("food"), Json.fromInt(2), Json.fromBoolean(true)),
      path
    )

    assertEquals(
      result.value,
      Left(
        Vector(
          WrongType(path.index(1), "a string", "a number"),
          WrongType(path.index(2), "a string", "a boolean")
        )
      )
    )
  }

  test("validates a non-empty refinement type") {
    val result = JsonData.parseTransactions(
      """[{"date":"2026-01-03","category":"  ","amount":12.5}]"""
    )

    assertEquals(
      result.value,
      Left(
        Vector(
          ValidationFailed(
            JsonPath.root.index(0).field("category"),
            "value must not be empty"
          )
        )
      )
    )
  }

  test("runs cross-field validation after independent fields decode") {
    val result = JsonData.parseTransactions(
      """[{"date":"2026-01-03","category":"equipment","amount":1500}]"""
    )

    assertEquals(
      result.value,
      Left(
        Vector(
          ValidationFailed(
            JsonPath.root.index(0).field("note"),
            "note is required when the transaction amount is at least 1000"
          )
        )
      )
    )
  }

  test("reports a value outside the transaction amount bounds") {
    val result = JsonData.parseTransactions(
      """[{"date":"2026-01-03","category":"food","amount":-1}]"""
    )

    assertEquals(
      result.value,
      Left(
        Vector(
          OutOfRange(
            JsonPath.root.index(0).field("amount"),
            "0",
            "1000000",
            "-1"
          )
        )
      )
    )
  }

  test("andThen short-circuits while zip accumulates independent failures") {
    var called = false
    val failed = DecodeResult.failure[Int](
      MissingField(JsonPath.root.field("first"))
    )
    val sequential = failed.andThen { value =>
      called = true
      DecodeResult.success(value + 1)
    }

    val independent = failed.zipAccumulating(
      DecodeResult.failure[String](MissingField(JsonPath.root.field("second")))
    )

    assert(!called)
    assertEquals(
      sequential.value,
      Left(Vector(MissingField(JsonPath.root.field("first"))))
    )
    assertEquals(
      independent.value,
      Left(
        Vector(
          MissingField(JsonPath.root.field("first")),
          MissingField(JsonPath.root.field("second"))
        )
      )
    )
  }

  test("orElse tries the fallback and retains warnings") {
    val first = DecodeResult.failure[Int](
      ValidationFailed(JsonPath.root, "primary decoding failed")
    ).copy(warnings = Vector(JsonWarning.unknownField(JsonPath.root.field("extra"))))

    val recovered = first.orElse(DecodeResult.success(42))
    assertEquals(recovered.value, Right(42))
    assertEquals(recovered.warnings, first.warnings)
  }

  test("orElse combines errors if both alternatives fail") {
    val first = DecodeResult.failure[Int](MissingField(JsonPath.root.field("first")))
    val second = DecodeResult.failure[Int](MissingField(JsonPath.root.field("second")))

    assertEquals(
      first.orElse(second).value,
      Left(
        Vector(
          MissingField(JsonPath.root.field("first")),
          MissingField(JsonPath.root.field("second"))
        )
      )
    )
  }

  test("recover turns a failure into a value without discarding warnings") {
    val warning = JsonWarning.unknownField(JsonPath.root.field("extra"))
    val failed = DecodeResult.failure[Int](MissingField(JsonPath.root.field("amount")))
      .copy(warnings = Vector(warning))

    assertEquals(failed.recover(_ => 0).value, Right(0))
    assertEquals(failed.recover(_ => 0).warnings, Vector(warning))
  }

  test("rejects a non-array top-level value") {
    assertEquals(
      JsonData.parseTransactions("""{"date":"2026-01-03"}""").value,
      Left(Vector(ExpectedArray(JsonPath.root)))
    )
  }

  test("rejects non-object entries with their JSON index") {
    assertEquals(
      JsonData.parseTransactions("""[{"date":"2026-01-03","category":"food","amount":1}, 42]""")
        .value,
      Left(Vector(ExpectedObject(JsonPath.root.index(1))))
    )
  }

  test("reports malformed JSON at the root path") {
    val result = JsonData.parseTransactions("""[{"date":""")
    val error = result.value.swap.toOption.get.head
    assert(error.isInstanceOf[MalformedJson])
    assertEquals(error.path, JsonPath.root)
    assert(error.message.nonEmpty)
  }

  test("computes a derived amount without changing the source value") {
    val transaction =
      Transaction(LocalDate.parse("2026-01-03"), category("food"), BigDecimal("10.00"), None)

    assertEquals(transaction.amountWithTax(BigDecimal("0.10")), BigDecimal("11.00"))
    assertEquals(transaction.amount, BigDecimal("10.00"))
  }
