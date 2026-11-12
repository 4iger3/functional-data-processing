package typedcsv

import typedcsv.CsvError.InvalidField

final class Column[A] private (
    val name: String,
    val headerRequired: Boolean,
    private[typedcsv] val decodeValue: (Option[String], Int) => Either[Vector[CsvError], A]
)

object Column:
  def required[A](name: String)(using codec: CsvCodec[A]): Column[A] =
    new Column(
      name,
      headerRequired = true,
      (raw, line) =>
        raw match
          case None | Some("") =>
            Left(Vector(InvalidField(line, name, raw, "required value is empty")))
          case Some(value) =>
            codec.decode(value).left.map(reason =>
              Vector(InvalidField(line, name, raw, reason))
            )
    )

  def optional[A](name: String)(using codec: CsvCodec[A]): Column[Option[A]] =
    new Column(
      name,
      headerRequired = false,
      (raw, line) =>
        raw match
          case None | Some("") => Right(None)
          case Some(value) =>
            codec.decode(value).map(Some(_)).left.map(reason =>
              Vector(InvalidField(line, name, raw, reason))
            )
    )
