package typedcsv

import java.time.LocalDate
import munit.FunSuite
import typedcsv.CsvError.*

final class CsvSpec extends FunSuite:
  test("parses typed values, optional columns, and quoted commas") {
    val input =
      """date,category,amount,note
        |2026-01-03,food,12.50,"lunch, with a friend"
        |2026-01-04,travel,30.00,
        |""".stripMargin

    val result = Csv.parse(input, Transaction.schema)
    assertEquals(
      result,
      Right(
        Vector(
          Transaction(
            LocalDate.parse("2026-01-03"),
            "food",
            BigDecimal("12.50"),
            Some("lunch, with a friend")
          ),
          Transaction(LocalDate.parse("2026-01-04"), "travel", BigDecimal("30.00"), None)
        )
      )
    )
  }

  test("reports multiple invalid field values in a row") {
    val result = Csv.parse(
      "date,category,amount\nnot-a-date,,oops\n",
      Transaction.schema
    )

    assertEquals(
      result,
      Left(
        Vector(
          InvalidField(2, "date", Some("not-a-date"), "expected a date in YYYY-MM-DD format"),
          InvalidField(2, "category", Some(""), "required value is empty"),
          InvalidField(2, "amount", Some("oops"), "expected a decimal number")
        )
      )
    )
  }

  test("allows an optional column to be absent") {
    val result = Csv.parse(
      "date,category,amount\n2026-01-03,food,12.50\n",
      Transaction.schema
    )

    assertEquals(
      result,
      Right(Vector(Transaction(LocalDate.parse("2026-01-03"), "food", BigDecimal("12.50"), None)))
    )
  }

  test("reports missing required headers") {
    val result = Csv.parse("date,amount\n2026-01-03,12.50\n", Transaction.schema)
    assertEquals(result, Left(Vector(MissingHeader("category"))))
  }

  test("rejects malformed quoting") {
    val result = Csv.parse(
      "date,category,amount\n2026-01-03,food,12.50\"extra\n",
      Transaction.schema
    )
    assertEquals(
      result,
      Left(Vector(MalformedCsv(2, "quote inside an unquoted value")))
    )
  }

  test("computes a derived amount without changing the source value") {
    val transaction =
      Transaction(LocalDate.parse("2026-01-03"), "food", BigDecimal("10.00"), None)

    assertEquals(transaction.amountWithTax(BigDecimal("0.10")), BigDecimal("11.00"))
    assertEquals(transaction.amount, BigDecimal("10.00"))
  }
