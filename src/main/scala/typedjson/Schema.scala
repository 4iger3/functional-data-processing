package typedjson

import io.circe.{Json, JsonObject}
import typedjson.JsonError.*

final class Schema[A] private (
    private[typedjson] val columns: Vector[Column[?]],
    private val decodeFields: (JsonObject, JsonPath) => DecodeResult[A]
):
  private[typedjson] def decodeRecord(record: JsonObject, path: JsonPath): DecodeResult[A] =
    val decoded = decodeFields(record, path)
    val knownFields = columns.flatMap(column =>
      column.name +: column.deprecatedAliases
    ).toSet
    val unknownWarnings = record.keys
      .filterNot(knownFields)
      .map(name => JsonWarning.unknownField(path.field(name)))
      .toVector
    DecodeResult(decoded.value, unknownWarnings ++ decoded.warnings)

  def decodeJson(value: Json, path: JsonPath = JsonPath.root): DecodeResult[A] =
    value.asObject match
      case Some(record) => decodeRecord(record, path)
      case None => DecodeResult.failure(ExpectedObject(path))

  def codec: JsonCodec[A] = new JsonCodec[A]:
    def decode(value: Json, path: JsonPath): DecodeResult[A] =
      decodeJson(value, path)

  def map[B](f: A => B): Schema[B] =
    Schema(columns) { (record: JsonObject, path: JsonPath) =>
      decodeFields(record, path).map(f)
    }

  def zip[B, C](column: Column[B])(f: (A, B) => C): Schema[C] =
    Schema(columns :+ column) { (record: JsonObject, path: JsonPath) =>
      val left = decodeFields(record, path)
      val right = column.decodeValue(record, path)
      left.zipAccumulating(right).map { case (a, b) => f(a, b) }
    }

  def validateWith(
      validate: (A, JsonPath) => Either[JsonError, Unit]
  ): Schema[A] =
    Schema(columns) { (record: JsonObject, path: JsonPath) =>
      decodeFields(record, path).andThen { value =>
        validate(value, path) match
          case Right(_)    => DecodeResult.success(value)
          case Left(error) => DecodeResult.failure(error)
      }
    }

object Schema:
  def apply[A](columns: Vector[Column[?]])(
      decode: (JsonObject, JsonPath) => DecodeResult[A]
  ): Schema[A] =
    new Schema(columns, decode)

  def field[A, Z](column: Column[A])(f: A => Z): Schema[Z] =
    Schema(Vector(column)) { (record: JsonObject, path: JsonPath) =>
      column.decodeValue(record, path).map(f)
    }

  def map3[A, B, C, Z](a: Column[A], b: Column[B], c: Column[C])(
      f: (A, B, C) => Z
  ): Schema[Z] =
    Schema(Vector(a, b, c)) { (record: JsonObject, path: JsonPath) =>
      val decodedA = a.decodeValue(record, path)
      val decodedB = b.decodeValue(record, path)
      val decodedC = c.decodeValue(record, path)
      decodedA
        .zipAccumulating(decodedB)
        .zipAccumulating(decodedC)
        .map { case ((valueA, valueB), valueC) =>
          f(valueA, valueB, valueC)
        }
    }
