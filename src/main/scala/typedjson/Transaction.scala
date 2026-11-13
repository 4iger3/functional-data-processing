package typedjson

import java.time.LocalDate

final case class Transaction(
    date: LocalDate,
    category: String,
    amount: BigDecimal,
    note: Option[String]
):
  def amountWithTax(rate: BigDecimal): BigDecimal =
    (amount * (BigDecimal(1) + rate)).setScale(2, BigDecimal.RoundingMode.HALF_UP)

object Transaction:
  private val dateColumn = Column.required[LocalDate]("date")
  private val categoryColumn = Column.required[String]("category")
  private val amountColumn = Column.required[BigDecimal]("amount")
  private val noteColumn = Column.optional[String]("note")

  val schema: Schema[Transaction] =
    Schema
      .map3(dateColumn, categoryColumn, amountColumn) { (date, category, amount) =>
        Transaction(date, category, amount, None)
      }
      .zip(noteColumn)((transaction, note) => transaction.copy(note = note))
