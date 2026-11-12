package typedcsv

final class Schema[A] private (
    private[typedcsv] val columns: Vector[Column[?]],
    private[typedcsv] val decodeRow: (Map[String, String], Int) => Either[Vector[CsvError], A]
):
  def map[B](f: A => B): Schema[B] =
    Schema(columns) { (row: Map[String, String], line: Int) =>
      decodeRow(row, line).map(f)
    }

  def zip[B, C](column: Column[B])(f: (A, B) => C): Schema[C] =
    Schema(columns :+ column) { (row: Map[String, String], line: Int) =>
        val left = decodeRow(row, line)
        val right = column.decodeValue(row.get(column.name), line)
        (left, right) match
          case (Right(a), Right(b)) => Right(f(a, b))
          case (Left(a), Left(b))   => Left(a ++ b)
          case (Left(errors), _)    => Left(errors)
          case (_, Left(errors))    => Left(errors)
    }

object Schema:
  def apply[A](columns: Vector[Column[?]])(
      decode: (Map[String, String], Int) => Either[Vector[CsvError], A]
  ): Schema[A] =
    new Schema(columns, decode)

  def map2[A, B, Z](a: Column[A], b: Column[B])(f: (A, B) => Z): Schema[Z] =
    Schema(Vector(a, b)) { (row: Map[String, String], line: Int) =>
      val decodedA = a.decodeValue(row.get(a.name), line)
      val decodedB = b.decodeValue(row.get(b.name), line)
      (decodedA, decodedB) match
        case (Right(valueA), Right(valueB)) => Right(f(valueA, valueB))
        case (Left(errorsA), Left(errorsB)) => Left(errorsA ++ errorsB)
        case (Left(errors), _)              => Left(errors)
        case (_, Left(errors))              => Left(errors)
    }

  def map3[A, B, C, Z](a: Column[A], b: Column[B], c: Column[C])(
      f: (A, B, C) => Z
  ): Schema[Z] =
    Schema(Vector(a, b, c)) { (row: Map[String, String], line: Int) =>
      val decodedA = a.decodeValue(row.get(a.name), line)
      val decodedB = b.decodeValue(row.get(b.name), line)
      val decodedC = c.decodeValue(row.get(c.name), line)
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
