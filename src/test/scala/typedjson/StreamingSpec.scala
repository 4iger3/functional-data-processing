package typedjson

import cats.effect.unsafe.implicits.global
import cats.effect.Ref
import cats.effect.IO
import io.circe.Json
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import munit.FunSuite
import typedjson.JsonError.*

final class StreamingSpec extends FunSuite:
  private def withInput[A](content: String)(run: Path => A): A =
    val path = Files.createTempFile("typed-json-", ".jsonl")
    try
      Files.writeString(path, content, StandardCharsets.UTF_8)
      run(path)
    finally Files.deleteIfExists(path)

  private def config: AppConfig =
    AppConfig.load(None).toOption.get.copy(logLevel = "off")

  test("streams JSON Lines records and reports physical line paths") {
    val input =
      """{"date":"2026-01-03","category":"food","amount":12.5}
        |
        |{"date":"bad","category":"food","amount":12.5}
        |""".stripMargin

    withInput(input) { path =>
      val records = JsonLines.records[Transaction](path, config)(using Transaction.schema.codec)
        .compile.toVector.unsafeRunSync()

      assertEquals(records.map(_.line), Vector(1L, 3L))
      assert(records.head.result.value.isRight)
      assertEquals(
        records(1).result.value,
        Left(
          Vector(
            ValidationFailed(
              JsonPath.root.index(2).field("date"),
              "expected a date in YYYY-MM-DD format"
            )
          )
        )
      )
    }
  }

  test("rejects oversized records while continuing with the next line") {
    val input = s"""${" " * 200}
                   |{"payload":"${"x" * 200}"}
                   |{"date":"2026-01-03","category":"food","amount":12.5}
                   |""".stripMargin
    val boundedConfig = config.copy(maxRecordBytes = 64, inputChunkSize = 8)

    withInput(input) { path =>
      val records = JsonLines.records[Transaction](path, boundedConfig)(using Transaction.schema.codec)
        .compile.toVector.unsafeRunSync()

      assertEquals(records.size, 2)
      assertEquals(records.head.line, 2L)
      assert(records.head.result.value.swap.toOption.get.head.message.contains("max-record-bytes"))
      assertEquals(records(1).line, 3L)
      assert(records(1).result.value.isRight)
    }
  }

  test("accepts a record at the byte limit with a CRLF terminator") {
    val record = """{"date":"2026-01-03","category":"food","amount":12.5}"""
    val boundedConfig = config.copy(
      maxRecordBytes = record.getBytes(StandardCharsets.UTF_8).length,
      inputChunkSize = 7
    )

    withInput(record + "\r\n") { path =>
      val records = JsonLines.records[Transaction](path, boundedConfig)(using Transaction.schema.codec)
        .compile.toVector.unsafeRunSync()
      assertEquals(records.size, 1)
      assert(records.head.result.value.isRight)
    }
  }

  test("max-errors caps printed error details across records") {
    val first = StreamRecord(
      1L,
      DecodeResult.failures(
        Vector(
          MissingField(JsonPath.root.index(0).field("date")),
          MissingField(JsonPath.root.index(0).field("amount"))
        )
      )
    )
    val second = StreamRecord(
      2L,
      DecodeResult.failure(MissingField(JsonPath.root.index(1).field("category")))
    )

    val initial = StreamingRunner.DiagnosticState()
    val firstPlan = StreamingRunner.diagnosticPlan(first, 1, quiet = false, state = initial)
    val secondPlan =
      StreamingRunner.diagnosticPlan(second, 1, quiet = false, state = firstPlan.state)

    assertEquals(firstPlan.messages.map(_._1), Vector("Error", "Warning"))
    assertEquals(firstPlan.state.errorsPrinted, 1)
    assertEquals(secondPlan.messages.map(_._1), Vector.empty)
    assert(secondPlan.state.limitNoticePrinted)
    assertEquals(secondPlan.state.errorsPrinted, 1)
  }

  test("runner returns failure for invalid data and strict warnings") {
    val invalid = """{"date":"bad","category":"food","amount":12.5}""" + "\n"
    val warning = """{"date":"2026-01-03","category":"food","amount":12.5,"extra":true}""" + "\n"

    withInput(invalid) { path =>
      val result = StreamingRunner
        .run(path, config, ProcessingMode.Validate, "text", quiet = true, strict = false,
          renderJson = StreamingJson.renderTransaction,
          renderText = _.category.value
        )(using Transaction.schema.codec)
        .unsafeRunSync()
      assert(result.isLeft)
    }
    withInput(warning) { path =>
      val result = StreamingRunner
        .run(path, config, ProcessingMode.Validate, "text", quiet = true, strict = true,
          renderJson = StreamingJson.renderTransaction,
          renderText = _.category.value
        )(using Transaction.schema.codec)
        .unsafeRunSync()
      assert(result.isLeft)
    }
    withInput(warning) { path =>
      val result = StreamingRunner
        .run(path, config, ProcessingMode.Validate, "text", quiet = true, strict = false,
          renderJson = StreamingJson.renderTransaction,
          renderText = _.category.value
        )(using Transaction.schema.codec)
        .unsafeRunSync()
      assertEquals(result.toOption.map(_.warnings), Some(1L))
    }
  }

  test("generated JSON Schema follows the decoding schema") {
    val schema = Transaction.schema.jsonSchema
    val properties = schema.hcursor.downField("properties").focus.get
    val amount = properties.hcursor.downField("amount").focus.get

    assertEquals(
      schema.hcursor.get[Vector[String]]("required").toOption.get,
      Vector("date", "category", "amount")
    )
    assertEquals(
      properties.asObject.toVector.flatMap(_.keys),
      Transaction.schema.columns.map(_.name)
    )
    assertEquals(amount.hcursor.get[BigDecimal]("minimum").toOption, Some(BigDecimal(0)))
    assertEquals(amount.hcursor.get[BigDecimal]("maximum").toOption, Some(BigDecimal(1000000)))
    assertEquals(
      properties.hcursor.downField("category").get[String]("pattern").toOption,
      Some("\\S")
    )
    assertEquals(
      schema.hcursor.get[Vector[String]]("x-validationRules").toOption.get,
      Vector("note is required when amount is at least 1000")
    )
  }

  test("generic JSON Lines decoding supports a non-transaction schema") {
    val labelSchema = Schema.field(Column.required[String]("label"))(identity)
    val input = """{"label":"first"}""" + "\n"

    withInput(input) { path =>
      val records = JsonLines.records[String](path, config)(using labelSchema.codec)
        .compile.toVector.unsafeRunSync()
      assertEquals(records.map(_.result.value), Vector(Right("first")))
    }
  }

  test("parse emits valid records before and after an invalid record, then fails") {
    val labelSchema = Schema.field(Column.required[String]("label"))(identity)
    val input =
      """{"label":"before"}
        |{"label":1}
        |{"label":"after"}
        |""".stripMargin

    withInput(input) { path =>
      val result = for
        output <- Ref.of[IO, Vector[String]](Vector.empty)
        outcome <- StreamingRunner
          .run[String](
            path,
            config,
            ProcessingMode.Parse,
            "json",
            quiet = false,
            strict = false,
            renderJson = Json.fromString,
            renderText = identity,
            writeLine = line => output.update(_ :+ line)
          )(using labelSchema.codec)
        emitted <- output.get
      yield (outcome, emitted)
      val (outcome, emitted) = result.unsafeRunSync()

      assert(outcome.isLeft)
      assertEquals(emitted, Vector(""""before"""", """"after""""))
    }
  }
