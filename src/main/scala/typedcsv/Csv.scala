package typedcsv

import scala.collection.mutable
import typedcsv.CsvError.*

object Csv:
  private final case class Record(line: Int, cells: Vector[String])

  def parse[A](input: String, schema: Schema[A]): Either[Vector[CsvError], Vector[A]] =
    parseRecords(input).flatMap { records =>
      records.headOption match
        case None => Left(Vector(EmptyInput()))
        case Some(headerRecord) =>
          val headers = headerRecord.cells.zipWithIndex.map { case (header, index) =>
            val cleaned =
              if index == 0 then header.stripPrefix("\uFEFF").trim else header.trim
            cleaned
          }
          val headerErrors =
            if headers.exists(_.isEmpty) then
              Vector(InvalidHeader(headerRecord.line, "header names cannot be empty"))
            else
              headers
                .groupBy(identity)
                .collect { case (name, occurrences) if occurrences.size > 1 =>
                  InvalidHeader(headerRecord.line, s"duplicate header '$name'")
                }
                .toVector

          val missing = schema.columns
            .filter(_.headerRequired)
            .map(_.name)
            .distinct
            .filterNot(headers.contains)
            .map(MissingHeader.apply)

          val schemaErrors = headerErrors ++ missing
          if schemaErrors.nonEmpty then Left(schemaErrors)
          else
            val dataRows = records.tail
            val decoded = dataRows.map { record =>
              if record.cells.length != headers.length then
                Left(Vector(RowWidth(record.line, headers.length, record.cells.length)))
              else
                val values = headers.zip(record.cells).toMap
                schema.decodeRow(values, record.line)
            }
            val errors = decoded.collect { case Left(rowErrors) => rowErrors }.flatten
            if errors.nonEmpty then Left(errors)
            else Right(decoded.collect { case Right(value) => value })
    }

  private def parseRecords(input: String): Either[Vector[CsvError], Vector[Record]] =
    val records = Vector.newBuilder[Record]
    val cells = mutable.ArrayBuffer.empty[String]
    val cell = new StringBuilder
    var inQuotes = false
    var afterQuote = false
    var line = 1
    var recordLine = 1
    var recordStarted = false
    var error: Option[CsvError] = None
    var index = 0

    def finishRecord(): Unit =
      cells += cell.toString
      val completed = cells.toVector
      if completed.exists(_.nonEmpty) then records += Record(recordLine, completed)

    def finishCell(): Unit =
      cells += cell.toString
      cell.clear()
      afterQuote = false

    while index < input.length && error.isEmpty do
      val current = input.charAt(index)
      if inQuotes then
        if current == '"' then
          if index + 1 < input.length && input.charAt(index + 1) == '"' then
            cell.append('"')
            index += 1
          else
            inQuotes = false
            afterQuote = true
        else
          cell.append(current)
          if current == '\n' then line += 1
      else if afterQuote && current != ',' && current != '\n' && current != '\r' then
        error = Some(MalformedCsv(line, "only a comma or line break may follow a closing quote"))
      else
        current match
          case '"' if cell.isEmpty =>
            inQuotes = true
            recordStarted = true
          case '"' =>
            error = Some(MalformedCsv(line, "quote inside an unquoted value"))
          case ',' =>
            finishCell()
            recordStarted = true
          case '\n' =>
            finishRecord()
            cells.clear()
            cell.clear()
            line += 1
            recordLine = line
            recordStarted = false
            afterQuote = false
          case '\r' if index + 1 < input.length && input.charAt(index + 1) == '\n' =>
            finishRecord()
            cells.clear()
            cell.clear()
            index += 1
            line += 1
            recordLine = line
            recordStarted = false
            afterQuote = false
          case '\r' =>
            error = Some(MalformedCsv(line, "bare carriage return is not supported"))
          case _ =>
            cell.append(current)
            recordStarted = true
      index += 1

    if error.nonEmpty then Left(error.toVector)
    else if inQuotes then Left(Vector(MalformedCsv(line, "quoted value was not closed")))
    else
      val hasPendingRecord = recordStarted || afterQuote || cell.nonEmpty || cells.nonEmpty
      if hasPendingRecord then finishRecord()
      Right(records.result())
