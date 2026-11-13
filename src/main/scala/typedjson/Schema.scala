package typedjson

final class Schema[A] private (
    private[typedjson] val columns: Vector[Column[?]],
    private[typedjson] val decodeRecord: (io.circe.JsonObject, Int) =>
      Either[Vector[JsonError], A]
):
  def map[B](f: A => B): Schema[B] =
    Schema(columns) { (record: io.circe.JsonObject, index: Int) =>
      decodeRecord(record, index).map(f)
    }

  def zip[B, C](column: Column[B])(f: (A, B) => C): Schema[C] =
    Schema(columns :+ column) { (record: io.circe.JsonObject, index: Int) =>
        val left = decodeRecord(record, index)
        val right = column.decodeValue(record, index)
        (left, right) match
          case (Right(a), Right(b)) => Right(f(a, b))
          case (Left(a), Left(b))   => Left(a ++ b)
          case (Left(errors), _)    => Left(errors)
          case (_, Left(errors))    => Left(errors)
    }

object Schema:
  def apply[A](columns: Vector[Column[?]])(
      decode: (io.circe.JsonObject, Int) => Either[Vector[JsonError], A]
  ): Schema[A] =
    new Schema(columns, decode)

  def map3[A, B, C, Z](a: Column[A], b: Column[B], c: Column[C])(
      f: (A, B, C) => Z
  ): Schema[Z] =
    Schema(Vector(a, b, c)) { (record: io.circe.JsonObject, index: Int) =>
      val decodedA = a.decodeValue(record, index)
      val decodedB = b.decodeValue(record, index)
      val decodedC = c.decodeValue(record, index)
      val errors = decodedA.left.toOption.getOrElse(Vector.empty) ++
        decodedB.left.toOption.getOrElse(Vector.empty) ++
        decodedC.left.toOption.getOrElse(Vector.empty)
      if errors.nonEmpty then Left(errors)
      else
        for
          valueA <- decodedA
          valueB <- decodedB
          valueC <- decodedC
        yield f(valueA, valueB, valueC)
    }
