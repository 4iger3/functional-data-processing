package typedcsv

import java.time.LocalDate
import java.time.format.DateTimeParseException

trait CsvCodec[A]:
  def decode(value: String): Either[String, A]

object CsvCodec:
  given CsvCodec[String] with
    def decode(value: String): Either[String, String] = Right(value)

  given CsvCodec[BigDecimal] with
    def decode(value: String): Either[String, BigDecimal] =
      try Right(BigDecimal(value))
      catch
        case _: NumberFormatException => Left("expected a decimal number")

  given CsvCodec[LocalDate] with
    def decode(value: String): Either[String, LocalDate] =
      try Right(LocalDate.parse(value))
      catch
        case _: DateTimeParseException => Left("expected a date in YYYY-MM-DD format")
