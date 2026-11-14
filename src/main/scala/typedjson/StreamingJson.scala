package typedjson

import cats.effect.IO
import fs2.{Chunk, Stream, text}
import fs2.io.readInputStream
import io.circe.{Json, parser}
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import typedjson.JsonError.MalformedJson

final case class StreamRecord(line: Long, result: DecodeResult[Transaction])

object StreamingJson:
  def records(path: Path, config: AppConfig): Stream[IO, StreamRecord] =
    readInputStream(
      IO.blocking(Files.newInputStream(path)),
      chunkSize = config.inputChunkSize,
      closeAfterUse = true
    )
      .through(text.utf8.decode)
      .through(text.lines)
      .zipWithIndex
      .filter { case (line, _) => line.trim.nonEmpty }
      .chunkN(config.streamChunkSize)
      .parEvalMap(config.parallelism) { lines =>
        IO.blocking(lines.toVector.map { case (line, lineIndex) =>
          decodeLine(line, lineIndex, config.maxRecordBytes)
        })
      }
      .flatMap(Stream.emits)

  private def decodeLine(line: String, lineIndex: Long, maxRecordBytes: Int): StreamRecord =
    val path = JsonPath.root.index(lineIndex.toInt)
    if line.getBytes(StandardCharsets.UTF_8).length > maxRecordBytes then
      StreamRecord(
        lineIndex + 1,
        DecodeResult.failure(
          MalformedJson(path, s"record exceeds the $maxRecordBytes-byte limit")
        )
      )
    else
      parser.parse(line) match
        case Left(error) =>
          StreamRecord(lineIndex + 1, DecodeResult.failure(MalformedJson(path, error.message)))
        case Right(json) =>
          StreamRecord(lineIndex + 1, JsonData.decodeTransaction(json, path))

  def renderTransaction(transaction: Transaction): Json =
    Json.obj(
      "date" -> Json.fromString(transaction.date.toString),
      "category" -> Json.fromString(transaction.category.value),
      "amount" -> Json.fromBigDecimal(transaction.amount),
      "note" -> transaction.note.fold(Json.Null)(Json.fromString)
    )
