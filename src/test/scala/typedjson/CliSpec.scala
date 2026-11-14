package typedjson

import cats.effect.ExitCode
import cats.effect.unsafe.implicits.global
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import munit.FunSuite

final class CliSpec extends FunSuite:
  private def withInput[A](content: String)(run: Path => A): A =
    val path = Files.createTempFile("typed-json-cli-", ".jsonl")
    try
      Files.writeString(path, content, StandardCharsets.UTF_8)
      run(path)
    finally Files.deleteIfExists(path)

  test("validate command returns success for valid JSON Lines") {
    val input = """{"date":"2026-01-03","category":"food","amount":12.5}""" + "\n"

    withInput(input) { path =>
      val result =
        Main.run(List("validate", path.toString, "--quiet", "--format", "json")).unsafeRunSync()
      assertEquals(result, ExitCode.Success)
    }
  }

  test("validate command returns a failure exit code for invalid records") {
    val input = """{"date":"not-a-date","category":"food","amount":12.5}""" + "\n"

    withInput(input) { path =>
      val result = Main.run(List("validate", path.toString, "--quiet")).unsafeRunSync()
      assertEquals(result, ExitCode.Error)
    }
  }

  test("strict mode treats warnings as failures") {
    val input =
      """{"date":"2026-01-03","category":"food","amount":12.5,"extra":true}""" + "\n"

    withInput(input) { path =>
      val result =
        Main.run(List("validate", path.toString, "--quiet", "--strict")).unsafeRunSync()
      assertEquals(result, ExitCode.Error)
    }
  }

  test("invalid output format returns a usage error exit code") {
    val input = """{"date":"2026-01-03","category":"food","amount":12.5}""" + "\n"

    withInput(input) { path =>
      val result =
        Main.run(List("validate", path.toString, "--format", "yaml", "--quiet")).unsafeRunSync()
      assertEquals(result, ExitCode(2))
    }
  }

  test("non-positive diagnostic limits return a usage error exit code") {
    val input = """{"date":"2026-01-03","category":"food","amount":12.5}""" + "\n"

    withInput(input) { path =>
      val maxErrors =
        Main.run(List("validate", path.toString, "--quiet", "--max-errors", "0")).unsafeRunSync()
      val maxRecordBytes =
        Main.run(List("validate", path.toString, "--quiet", "--max-record-bytes", "-1"))
          .unsafeRunSync()
      assertEquals(maxErrors, ExitCode(2))
      assertEquals(maxRecordBytes, ExitCode(2))
    }
  }

  test("positive record and diagnostic limits are accepted as command overrides") {
    val input = """{"date":"2026-01-03","category":"food","amount":12.5}""" + "\n"
    val recordBytes = input.trim.getBytes(StandardCharsets.UTF_8).length.toString

    withInput(input) { path =>
      val result = Main
        .run(
          List(
            "validate",
            path.toString,
            "--quiet",
            "--max-errors",
            "1",
            "--max-record-bytes",
            recordBytes
          )
        )
        .unsafeRunSync()
      assertEquals(result, ExitCode.Success)
    }
  }

  test("explain command accepts schema fields and rejects unknown fields even in quiet mode") {
    val known = Main.run(List("explain", "category", "--quiet")).unsafeRunSync()
    val unknown = Main.run(List("explain", "unknown", "--quiet")).unsafeRunSync()

    assertEquals(known, ExitCode.Success)
    assertEquals(unknown, ExitCode(2))
  }

  test("invalid configuration returns a failure exit code") {
    val input = """{"date":"2026-01-03","category":"food","amount":12.5}""" + "\n"
    val config = Files.createTempFile("typed-json-config-", ".conf")
    try
      Files.writeString(config, "typed-json { parallelism = 0 }", StandardCharsets.UTF_8)
      withInput(input) { path =>
        val result = Main
          .run(List("validate", path.toString, "--quiet", "--config", config.toString))
          .unsafeRunSync()
        assertEquals(result, ExitCode.Error)
      }
    finally Files.deleteIfExists(config)
  }
