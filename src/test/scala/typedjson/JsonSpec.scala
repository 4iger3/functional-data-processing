package typedjson

import java.time.LocalDate
import munit.FunSuite
import typedjson.JsonError.*

final class JsonSpec extends FunSuite:
  test("parses typed values and optional fields from a JSON array") {
    val input =
      """[
        |  {"date":"2026-01-03","category":"food","amount":12.50,"note":"lunch"},
        |  {"date":"2026-01-04","category":"travel","amount":30.00,"note":null},
        |  {"date":"2026-01-05","category":"food","amount":8.25}
        |]""".stripMargin

    assertEquals(
      JsonData.parseTransactions(input),
      Right(
        Vector(
          Transaction(LocalDate.parse("2026-01-03"), "food", BigDecimal("12.50"), Some("lunch")),
          Transaction(LocalDate.parse("2026-01-04"), "travel", BigDecimal("30.00"), None),
          Transaction(LocalDate.parse("2026-01-05"), "food", BigDecimal("8.25"), None)
        )
      )
    )
  }

  test("accumulates invalid fields across records") {
    val input =
      """[
        |  {"date":"bad-date","category":false,"amount":"not-a-number"},
        |  {"date":"2026-01-05","amount":10}
        |]""".stripMargin

    val result = JsonData.parseTransactions(input)
    assert(result.isLeft)
    val errors = result.swap.toOption.get
    assertEquals(errors.size, 4)
    assert(errors.exists(_.message.contains("Record 1, field 'date'")))
    assert(errors.exists(_.message.contains("Record 1, field 'category'")))
    assert(errors.exists(_.message.contains("Record 1, field 'amount'")))
    assert(errors.exists(_.message.contains("Record 2, field 'category'")))
  }

  test("reports a missing required field") {
    assertEquals(
      JsonData.parseTransactions("""[{"date":"2026-01-03","amount":12.5}]"""),
      Left(
        Vector(
          InvalidField(1, "category", None, "required field is missing")
        )
      )
    )
  }

  test("rejects a non-array top-level value") {
    assertEquals(
      JsonData.parseTransactions("""{"date":"2026-01-03"}"""),
      Left(Vector(ExpectedArray()))
    )
  }

  test("rejects non-object entries with their record number") {
    assertEquals(
      JsonData.parseTransactions("""[{"date":"2026-01-03","category":"food","amount":1}, 42]"""),
      Left(Vector(ExpectedObject(2)))
    )
  }

  test("reports malformed JSON explicitly") {
    assert(JsonData.parseTransactions("""[{"date":""").swap.toOption.get.head.isInstanceOf[MalformedJson])
  }

  test("computes a derived amount without changing the source value") {
    val transaction =
      Transaction(LocalDate.parse("2026-01-03"), "food", BigDecimal("10.00"), None)

    assertEquals(transaction.amountWithTax(BigDecimal("0.10")), BigDecimal("11.00"))
    assertEquals(transaction.amount, BigDecimal("10.00"))
  }
