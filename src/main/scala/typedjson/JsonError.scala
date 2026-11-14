package typedjson

sealed trait JsonError:
  def path: JsonPath
  def message: String

object JsonError:
  final case class MalformedJson(path: JsonPath, detail: String) extends JsonError:
    def message: String = s"Malformed JSON at $path: $detail"

  final case class ExpectedArray(path: JsonPath) extends JsonError:
    def message: String = s"Expected a JSON array at $path."

  final case class ExpectedObject(path: JsonPath) extends JsonError:
    def message: String = s"Expected a JSON object at $path."

  final case class MissingField(path: JsonPath) extends JsonError:
    def message: String = s"Required field is missing at $path."

  final case class WrongType(path: JsonPath, expected: String, actual: String) extends JsonError:
    def message: String = s"Expected $expected at $path, found $actual."

  final case class ValidationFailed(path: JsonPath, detail: String) extends JsonError:
    def message: String = s"Validation failed at $path: $detail."

  final case class OutOfRange(
      path: JsonPath,
      minimum: String,
      maximum: String,
      actual: String
  ) extends JsonError:
    def message: String = s"Value $actual at $path is outside [$minimum, $maximum]."

final case class JsonWarning(path: JsonPath, message: String)

object JsonWarning:
  def unknownField(path: JsonPath): JsonWarning =
    JsonWarning(path, s"Unknown field at $path; it was ignored.")

  def deprecatedField(path: JsonPath, replacement: String): JsonWarning =
    JsonWarning(path, s"Deprecated field at $path; use '$replacement' instead.")

final case class DecodeResult[A](
    value: Either[Vector[JsonError], A],
    warnings: Vector[JsonWarning] = Vector.empty
):
  def map[B](f: A => B): DecodeResult[B] =
    DecodeResult(value.map(f), warnings)

  def andThen[B](f: A => DecodeResult[B]): DecodeResult[B] =
    value match
      case Left(errors) => DecodeResult(Left(errors), warnings)
      case Right(decoded) =>
        val next = f(decoded)
        DecodeResult(next.value, warnings ++ next.warnings)

  def orElse[B >: A](fallback: => DecodeResult[B]): DecodeResult[B] =
    value match
      case Right(decoded) => DecodeResult(Right(decoded), warnings)
      case Left(errors) =>
        val alternative = fallback
        val combinedErrors = alternative.value match
          case Left(fallbackErrors) => Left(errors ++ fallbackErrors)
          case Right(decoded)       => Right(decoded)
        DecodeResult(combinedErrors, warnings ++ alternative.warnings)

  def recover[B >: A](f: Vector[JsonError] => B): DecodeResult[B] =
    value match
      case Left(errors) => DecodeResult(Right(f(errors)), warnings)
      case Right(decoded) => DecodeResult(Right(decoded), warnings)

  def zipAccumulating[B](other: DecodeResult[B]): DecodeResult[(A, B)] =
    val combinedValue = (value, other.value) match
      case (Right(left), Right(right))       => Right((left, right))
      case (Left(leftErrors), Left(rightErrors)) => Left(leftErrors ++ rightErrors)
      case (Left(errors), _)                 => Left(errors)
      case (_, Left(errors))                 => Left(errors)
    DecodeResult(combinedValue, warnings ++ other.warnings)

object DecodeResult:
  def success[A](value: A): DecodeResult[A] =
    DecodeResult(Right(value))

  def failure[A](error: JsonError): DecodeResult[A] =
    DecodeResult(Left(Vector(error)))

  def failures[A](errors: Vector[JsonError]): DecodeResult[A] =
    DecodeResult(Left(errors))
