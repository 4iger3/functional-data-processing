package typedcsv

sealed trait CsvError:
  def message: String

object CsvError:
  final case class MalformedCsv(line: Int, detail: String) extends CsvError:
    def message: String = s"Line $line: malformed CSV: $detail"

  final case class EmptyInput() extends CsvError:
    def message: String = "The CSV input is empty."

  final case class InvalidHeader(line: Int, detail: String) extends CsvError:
    def message: String = s"Header error on line $line: $detail"

  final case class MissingHeader(name: String) extends CsvError:
    def message: String = s"Required header '$name' is missing."

  final case class RowWidth(line: Int, expected: Int, actual: Int) extends CsvError:
    def message: String =
      s"Line $line: expected $expected columns, but found $actual."

  final case class InvalidField(line: Int, name: String, raw: Option[String], detail: String)
      extends CsvError:
    def message: String =
      val value = raw.fold("missing value")(v => s"'$v'")
      s"Line $line, field '$name': $detail ($value)."
