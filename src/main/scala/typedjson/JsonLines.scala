package typedjson

import cats.effect.IO
import fs2.{Chunk, Pipe, Pull, Stream}
import fs2.io.readInputStream
import io.circe.{Json, parser}
import java.io.ByteArrayOutputStream
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import typedjson.JsonError.MalformedJson

final case class StreamRecord[A](line: Long, result: DecodeResult[A])

object JsonLines:
  def records[A](
      path: Path,
      config: AppConfig
  )(using codec: JsonCodec[A]): Stream[IO, StreamRecord[A]] =
    readInputStream(
      IO.blocking(Files.newInputStream(path)),
      chunkSize = config.inputChunkSize,
      closeAfterUse = true
    )
      .through(boundedLines(config.maxRecordBytes))
      .filter(line => line.content.forall(_.trim.nonEmpty))
      .chunkN(config.streamChunkSize)
      .parEvalMap(config.parallelism) { lines =>
        IO.blocking(lines.toVector.map(decodeLine[A]))
      }
      .flatMap(Stream.emits)

  private final case class InputLine(index: Long, content: Option[String])

  private def decodeLine[A](line: InputLine)(using codec: JsonCodec[A]): StreamRecord[A] =
    val path = JsonPath.root.index(line.index)
    val result = line.content match
      case None =>
        DecodeResult.failure[A](
          MalformedJson(path, "record exceeds the configured max-record-bytes limit")
        )
      case Some(content) =>
        parser.parse(content) match
          case Left(error) => DecodeResult.failure(MalformedJson(path, error.message))
          case Right(json) => codec.decode(json, path)
    StreamRecord(line.index + 1, result)

  private def boundedLines(maxRecordBytes: Int): Pipe[IO, Byte, InputLine] = input =>
    def loop(stream: Stream[IO, Byte], accumulator: LineAccumulator): Pull[IO, InputLine, Unit] =
      stream.pull.uncons.flatMap {
        case None =>
          accumulator.finish match
            case Some(lastLine) => Pull.output1(lastLine)
            case None           => Pull.done
        case Some((chunk, tail)) =>
          val completed = Vector.newBuilder[InputLine]
          chunk.foreach(byte => accumulator.add(byte).foreach(completed += _))
          Pull.output(Chunk.from(completed.result())) >> loop(tail, accumulator)
      }

    loop(input, new LineAccumulator(maxRecordBytes)).stream

  private final class LineAccumulator(maxRecordBytes: Int):
    private val bytes = new ByteArrayOutputStream(math.min(maxRecordBytes, 8192))
    private var lineIndex = 0L
    private var oversized = false
    private var hasContent = false
    private var hasNonWhitespace = false
    private var pendingCarriageReturn = false

    def add(byte: Byte): Option[InputLine] =
      if byte == '\n'.toByte then
        val line = completedLine()
        lineIndex += 1
        bytes.reset()
        oversized = false
        hasContent = false
        hasNonWhitespace = false
        pendingCarriageReturn = false
        Some(line)
      else
        hasContent = true
        hasNonWhitespace = hasNonWhitespace || (byte & 0xff) > ' '.toInt
        if pendingCarriageReturn then append('\r'.toByte)
        pendingCarriageReturn = byte == '\r'.toByte
        if !pendingCarriageReturn then append(byte)
        None

    def finish: Option[InputLine] =
      if pendingCarriageReturn then append('\r'.toByte)
      if hasContent then Some(completedLine())
      else None

    private def append(byte: Byte): Unit =
      if !oversized then
        if bytes.size() < maxRecordBytes then bytes.write(byte.toInt)
        else
          oversized = true
          bytes.reset()

    private def completedLine(): InputLine =
      if oversized && hasNonWhitespace then InputLine(lineIndex, None)
      else if oversized then InputLine(lineIndex, Some(""))
      else
        val raw = bytes.toByteArray
        InputLine(lineIndex, Some(new String(raw, StandardCharsets.UTF_8)))
