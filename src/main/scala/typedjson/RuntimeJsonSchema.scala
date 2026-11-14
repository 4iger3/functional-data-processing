package typedjson

import io.circe.{Json, JsonObject, parser}
import scala.util.control.NonFatal
import typedjson.JsonError.*

final class RuntimeJsonSchema private (
    private val root: RuntimeJsonSchema.Node,
    val definition: Json
) extends JsonCodec[Json]:
  val schema: Json = definition

  def decode(value: Json, path: JsonPath): DecodeResult[Json] =
    root.validate(value, path).map(_ => value)

object RuntimeJsonSchema:
  private val draft2020 =
    "https://json-schema.org/draft/2020-12/schema"

  private sealed trait Node:
    def validate(value: Json, path: JsonPath): DecodeResult[Unit]

  private final case class StringNode(
      minLength: Option[Int],
      maxLength: Option[Int],
      pattern: Option[scala.util.matching.Regex]
  ) extends Node:
    def validate(value: Json, path: JsonPath): DecodeResult[Unit] =
      value.asString match
        case None => wrongType(value, path, "a string")
        case Some(text) =>
          val length = text.codePointCount(0, text.length)
          val errors =
            minLength.filter(length < _).map(min =>
              ValidationFailed(path, s"string must contain at least $min characters")
            ).toVector ++
              maxLength.filter(length > _).map(max =>
                ValidationFailed(path, s"string must contain at most $max characters")
              ).toVector ++
              pattern.filterNot(_.findFirstIn(text).nonEmpty).map(regex =>
                ValidationFailed(path, s"string does not match pattern '${regex.regex}'")
              ).toVector
          fromErrors(errors)

  private final case class NumberNode(
      integerOnly: Boolean,
      minimum: Option[BigDecimal],
      maximum: Option[BigDecimal]
  ) extends Node:
    def validate(value: Json, path: JsonPath): DecodeResult[Unit] =
      value.asNumber.flatMap(_.toBigDecimal) match
        case None => wrongType(value, path, if integerOnly then "an integer" else "a number")
        case Some(number) =>
          val errors =
            (if integerOnly && !number.isWhole then
               Vector(ValidationFailed(path, "number must be an integer"))
             else Vector.empty) ++
              (if minimum.exists(number < _) || maximum.exists(number > _) then
                 Vector(
                   OutOfRange(
                     path,
                     minimum.fold("-infinity")(_.toString),
                     maximum.fold("infinity")(_.toString),
                     number.toString
                   )
                 )
               else Vector.empty)
          fromErrors(errors)

  private case object BooleanNode extends Node:
    def validate(value: Json, path: JsonPath): DecodeResult[Unit] =
      if value.isBoolean then DecodeResult.success(())
      else wrongType(value, path, "a boolean")

  private case object NullNode extends Node:
    def validate(value: Json, path: JsonPath): DecodeResult[Unit] =
      if value.isNull then DecodeResult.success(())
      else wrongType(value, path, "null")

  private final case class ArrayNode(
      items: Node,
      minItems: Option[Int],
      maxItems: Option[Int]
  ) extends Node:
    def validate(value: Json, path: JsonPath): DecodeResult[Unit] =
      value.asArray match
        case None => wrongType(value, path, "an array")
        case Some(values) =>
          val sizeErrors =
            minItems.filter(values.size < _).map(min =>
              ValidationFailed(path, s"array must contain at least $min items")
            ).toVector ++
              maxItems.filter(values.size > _).map(max =>
                ValidationFailed(path, s"array must contain at most $max items")
              ).toVector
          val itemResults = values.zipWithIndex.map { case (item, index) =>
            items.validate(item, path.index(index))
          }
          collect(sizeErrors, itemResults)

  private final case class ObjectNode(
      properties: Vector[(String, Node)],
      required: Vector[String],
      additionalProperties: Boolean
  ) extends Node:
    def validate(value: Json, path: JsonPath): DecodeResult[Unit] =
      value.asObject match
        case None => wrongType(value, path, "an object")
        case Some(fields) =>
          val known = properties.map(_._1).toSet
          val requiredErrors = required.collect {
            case name if !fields.contains(name) => MissingField(path.field(name))
          }
          val propertyResults = properties.flatMap { case (name, node) =>
            fields(name).map(node.validate(_, path.field(name)))
          }
          val additionalErrors =
            if additionalProperties then Vector.empty
            else
              fields.keys.filterNot(known).map { name =>
                ValidationFailed(path.field(name), "additional properties are not allowed")
              }.toVector
          collect(requiredErrors ++ additionalErrors, propertyResults)

  def load(path: java.nio.file.Path): Either[String, RuntimeJsonSchema] =
    try
      val content = java.nio.file.Files.readString(path, java.nio.charset.StandardCharsets.UTF_8)
      parser.parse(content).left.map(error => s"Invalid JSON Schema: ${error.message}")
        .flatMap(fromJson)
    catch
      case NonFatal(error) =>
        Left(s"Could not load JSON Schema '${path.toString}': ${error.getMessage}")

  def fromJson(json: Json): Either[String, RuntimeJsonSchema] =
    json.asObject.toRight("JSON Schema root must be an object.")
      .flatMap(parseObject(_, "$"))
      .map(node => new RuntimeJsonSchema(node, json))

  private def parseObject(value: JsonObject, path: String): Either[String, ObjectNode] =
    for
      _ <- validateKeys(
        value,
        Set("$schema", "type", "properties", "required", "additionalProperties", "description"),
        path
      )
      _ <- readType(value, path, "object")
      _ <- validateAnnotations(value, path)
      propertiesJson <- value("properties") match
        case None => Right(JsonObject.empty)
        case Some(json) =>
          json.asObject.toRight(s"$path.properties must be an object.")
      properties <- propertiesJson.toVector.foldLeft[Either[String, Vector[(String, Node)]]](
        Right(Vector.empty)
      ) { case (acc, (name, definition)) =>
        for
          fields <- definition.asObject.toRight(s"$path.properties.$name must be an object.")
          node <- parseNode(fields, s"$path.properties.$name")
          previous <- acc
        yield previous :+ (name -> node)
      }
      required <- parseRequired(value, path)
      additional <- parseAdditionalProperties(value, path)
    yield ObjectNode(properties, required, additional)

  private def parseNode(value: JsonObject, path: String): Either[String, Node] =
    value("type").flatMap(_.asString) match
      case Some("string") =>
        for
          _ <- validateKeys(value, Set("type", "minLength", "maxLength", "pattern", "description"), path)
          _ <- validateAnnotations(value, path)
          minimum <- readNonNegativeInt(value, "minLength", path)
          maximum <- readNonNegativeInt(value, "maxLength", path)
          _ <- validateBounds(minimum, maximum, path)
          pattern <- value("pattern") match
            case None => Right(None)
            case Some(json) =>
              json.asString.toRight(s"$path.pattern must be a string.").flatMap { source =>
                try Right(Some(source.r))
                catch case NonFatal(error) => Left(s"$path.pattern is invalid: ${error.getMessage}")
              }
        yield StringNode(minimum, maximum, pattern)
      case Some("number") | Some("integer") =>
        val integerOnly = value("type").flatMap(_.asString).contains("integer")
        for
          _ <- validateKeys(value, Set("type", "minimum", "maximum", "description"), path)
          _ <- validateAnnotations(value, path)
          minimum <- readDecimal(value, "minimum", path)
          maximum <- readDecimal(value, "maximum", path)
          _ <- validateBounds(minimum, maximum, path)
        yield NumberNode(integerOnly, minimum, maximum)
      case Some("boolean") =>
        for
          _ <- validateKeys(value, Set("type", "description"), path)
          _ <- validateAnnotations(value, path)
        yield BooleanNode
      case Some("null") =>
        for
          _ <- validateKeys(value, Set("type", "description"), path)
          _ <- validateAnnotations(value, path)
        yield NullNode
      case Some("array") =>
        for
          _ <- validateKeys(value, Set("type", "items", "minItems", "maxItems", "description"), path)
          _ <- validateAnnotations(value, path)
          itemJson <- value("items").flatMap(_.asObject)
            .toRight(s"$path.items must be an object.")
          itemNode <- parseNode(itemJson, s"$path.items")
          minimum <- readNonNegativeInt(value, "minItems", path)
          maximum <- readNonNegativeInt(value, "maxItems", path)
          _ <- validateBounds(minimum, maximum, path)
        yield ArrayNode(itemNode, minimum, maximum)
      case Some("object") => parseObject(value, path)
      case Some(other) => Left(s"$path.type '$other' is not supported.")
      case None => Left(s"$path.type must be a supported string.")

  private def readType(value: JsonObject, path: String, expected: String): Either[String, Unit] =
    value("type").flatMap(_.asString) match
      case Some(`expected`) => Right(())
      case Some(other) => Left(s"$path.type must be '$expected', found '$other'.")
      case None => Left(s"$path.type must be '$expected'.")

  private def parseRequired(
      value: JsonObject,
      path: String
  ): Either[String, Vector[String]] =
    value("required") match
      case None => Right(Vector.empty)
      case Some(json) =>
        json.asArray.toRight(s"$path.required must be an array of strings.").flatMap { entries =>
          val names = entries.toVector.map(_.asString)
          if names.exists(_.isEmpty) then Left(s"$path.required must contain only strings.")
          else
            val requiredNames = names.flatten
            val duplicates = requiredNames.distinct.size != requiredNames.size
            if duplicates then Left(s"$path.required must not contain duplicate names.")
            else Right(requiredNames)
        }

  private def parseAdditionalProperties(value: JsonObject, path: String): Either[String, Boolean] =
    value("additionalProperties") match
      case None => Right(true)
      case Some(json) =>
        json.asBoolean.toRight(s"$path.additionalProperties must be a boolean.")

  private def readNonNegativeInt(
      value: JsonObject,
      key: String,
      path: String
  ): Either[String, Option[Int]] =
    value(key) match
      case None => Right(None)
      case Some(json) =>
        json.asNumber.flatMap(_.toInt).filter(_ >= 0)
          .map(number => Right(Some(number)))
          .getOrElse(Left(s"$path.$key must be a non-negative integer."))

  private def readDecimal(
      value: JsonObject,
      key: String,
      path: String
  ): Either[String, Option[BigDecimal]] =
    value(key) match
      case None => Right(None)
      case Some(json) =>
        json.asNumber.flatMap(_.toBigDecimal)
          .map(number => Right(Some(number)))
          .getOrElse(Left(s"$path.$key must be a number."))

  private def validateBounds[A: Ordering](
      minimum: Option[A],
      maximum: Option[A],
      path: String
  ): Either[String, Unit] =
    (minimum, maximum) match
      case (Some(min), Some(max)) if summon[Ordering[A]].gt(min, max) =>
        Left(s"$path has a minimum greater than its maximum.")
      case _ => Right(())

  private def validateKeys(
      value: JsonObject,
      supported: Set[String],
      path: String
  ): Either[String, Unit] =
    value.keys.find(key => !supported.contains(key)) match
      case Some(unsupported) => Left(s"$path.$unsupported is not supported by this schema subset.")
      case None              => Right(())

  private def validateAnnotations(value: JsonObject, path: String): Either[String, Unit] =
    val description = value("description") match
      case None => Right(())
      case Some(json) =>
        Either.cond(json.isString, (), s"$path.description must be a string.")
    val dialect = value("$schema") match
      case None => Right(())
      case Some(json) =>
        json.asString match
          case Some(`draft2020`) => Right(())
          case Some(other) =>
            Left(s"$path.$$schema '$other' is not supported; use JSON Schema Draft 2020-12.")
          case None => Left(s"$path.$$schema must be a string.")
    description.flatMap(_ => dialect)

  private def wrongType(value: Json, path: JsonPath, expected: String): DecodeResult[Unit] =
    DecodeResult.failure(WrongType(path, expected, JsonCodec.jsonType(value)))

  private def fromErrors(errors: Vector[JsonError]): DecodeResult[Unit] =
    if errors.isEmpty then DecodeResult.success(())
    else DecodeResult.failures(errors)

  private def collect(
      directErrors: Vector[JsonError],
      results: Vector[DecodeResult[Unit]]
  ): DecodeResult[Unit] =
    val errors = directErrors ++ results.flatMap(_.value.left.toOption.getOrElse(Vector.empty))
    val warnings = results.flatMap(_.warnings)
    DecodeResult(if errors.isEmpty then Right(()) else Left(errors), warnings)
