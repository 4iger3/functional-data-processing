package typedjson

import io.circe.{Json, parser}
import java.nio.charset.StandardCharsets
import java.nio.file.Files
import munit.FunSuite
import typedjson.JsonError.*

final class RuntimeJsonSchemaSpec extends FunSuite:
  private val schemaJson =
    """{
      |  "$schema": "https://json-schema.org/draft/2020-12/schema",
      |  "type": "object",
      |  "properties": {
      |    "sku": {"type": "string", "minLength": 1, "pattern": "\\S"},
      |    "quantity": {"type": "integer", "minimum": 0, "maximum": 100},
      |    "tags": {"type": "array", "items": {"type": "string", "minLength": 1}},
      |    "supplier": {
      |      "type": "object",
      |      "properties": {"name": {"type": "string"}, "active": {"type": "boolean"}},
      |      "required": ["name"],
      |      "additionalProperties": false
      |    }
      |  },
      |  "required": ["sku", "quantity"],
      |  "additionalProperties": false
      |}""".stripMargin

  private def schema: RuntimeJsonSchema =
    RuntimeJsonSchema.fromJson(parser.parse(schemaJson).toOption.get).toOption.get

  test("validates nested object and array records and preserves input JSON") {
    val input = parser.parse(
      """{"sku":"A-104","quantity":12,"tags":["sale"],"supplier":{"name":"Northwind","active":true}}"""
    ).toOption.get

    val result = schema.decode(input, JsonPath.root.index(0))

    assertEquals(result.value, Right(input))
    assertEquals(result.warnings, Vector.empty)
  }

  test("accumulates missing, type, range, nested, and additional-property errors") {
    val input = parser.parse(
      """{"quantity":101.5,"tags":["ok",2],"supplier":{"active":"yes","extra":true},"extra":1}"""
    ).toOption.get

    val result = schema.decode(input, JsonPath.root.index(2))
    val errors = result.value.swap.toOption.get

    assertEquals(
      errors.map(_.path.toString),
      Vector(
        "$[2].sku",
        "$[2].extra",
        "$[2].quantity",
        "$[2].quantity",
        "$[2].tags[1]",
        "$[2].supplier.name",
        "$[2].supplier.extra",
        "$[2].supplier.active"
      )
    )
    assert(errors.exists(_.isInstanceOf[OutOfRange]))
    assert(errors.exists(_.isInstanceOf[ValidationFailed]))
    assert(errors.exists(_.isInstanceOf[WrongType]))
  }

  test("rejects unsupported JSON Schema keywords instead of silently ignoring them") {
    val result = parser
      .parse("""{"type":"object","properties":{},"patternProperties":{}}""")
      .toOption.get

    assert(
      RuntimeJsonSchema.fromJson(result).left.toOption.get
        .contains("patternProperties is not supported")
    )
  }

  test("rejects malformed or contradictory schema definitions") {
    val invalidSchemas = Vector(
      """{"type":"object","required":"sku"}""",
      """{"type":"object","properties":{"count":{"type":"integer","minimum":5,"maximum":1}}}""",
      """{"type":"object","properties":{"name":{"type":"string","pattern":"["}}}""",
      """{"$schema":"https://json-schema.org/draft/2020-12/schema","type":"object","properties":{},"required":["sku","sku"]}"""
    )

    invalidSchemas.foreach { source =>
      val parsed = parser.parse(source).toOption.get
      assert(RuntimeJsonSchema.fromJson(parsed).isLeft, clues(source))
    }
  }

  test("loads a schema from a UTF-8 file and reports invalid files explicitly") {
    val schemaPath = Files.createTempFile("runtime-schema-", ".json")
    val invalidPath = Files.createTempFile("runtime-schema-invalid-", ".json")
    try
      Files.writeString(schemaPath, schemaJson, StandardCharsets.UTF_8)
      Files.writeString(invalidPath, """{"type":""", StandardCharsets.UTF_8)
      assert(RuntimeJsonSchema.load(schemaPath).isRight)
      assert(RuntimeJsonSchema.load(invalidPath).left.toOption.get.contains("Invalid JSON Schema"))
    finally
      Files.deleteIfExists(schemaPath)
      Files.deleteIfExists(invalidPath)
  }

  test("string length uses Unicode code points") {
    val definition = Json.obj(
      "type" -> Json.fromString("object"),
      "properties" -> Json.obj(
        "name" -> Json.obj(
          "type" -> Json.fromString("string"),
          "minLength" -> Json.fromInt(1)
        )
      ),
      "required" -> Json.arr(Json.fromString("name"))
    )
    val runtimeSchema = RuntimeJsonSchema.fromJson(definition).toOption.get
    val input = Json.obj("name" -> Json.fromString("😀"))

    assert(runtimeSchema.decode(input, JsonPath.root).value.isRight)
  }
