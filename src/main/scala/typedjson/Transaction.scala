package typedjson

import java.time.LocalDate

final case class Transaction(
    date: LocalDate,
    category: NonEmptyString,
    amount: BigDecimal,
    note: Option[String]
):
  def amountWithTax(rate: BigDecimal): BigDecimal =
    (amount * (BigDecimal(1) + rate)).setScale(2, BigDecimal.RoundingMode.HALF_UP)

object Transaction:
  private val dateColumn =
    Column.required[LocalDate]("date").describe("Transaction date in YYYY-MM-DD format.")
  private val categoryColumn =
    Column.required[NonEmptyString]("category").describe("Non-empty transaction category.")
  private val amountColumn =
    Column
      .required[BigDecimal]("amount")
      .validateRange(BigDecimal(0), BigDecimal(1000000))
      .describe("Transaction amount in the inclusive range 0–1,000,000.")
  private val noteColumn =
    Column
      .optional[String]("note")
      .deprecatedAlias("memo")
      .describe("Optional note; required when amount is at least 1000.")

  val schema: Schema[Transaction] =
    Schema
      .map3(dateColumn, categoryColumn, amountColumn) { (date, category, amount) =>
        Transaction(date, category, amount, None)
      }
      .zip(noteColumn)((transaction, note) => transaction.copy(note = note))
      .validateWith { (transaction, path) =>
        if transaction.amount >= BigDecimal(1000) && transaction.note.isEmpty then
          Left(
            JsonError.ValidationFailed(
              path.field("note"),
              "note is required when the transaction amount is at least 1000"
            )
          )
        else Right(())
      }
      .withValidationRule("note is required when amount is at least 1000")
