package typedjson

import cats.effect.IO
import fs2.{Chunk, Pipe, Pull, Stream}
import fs2.io.readInputStream
import io.circe.{Json, parser}
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
    def loop(stream: Stream[IO, Byte], state: LineState): Pull[IO, InputLine, Unit] =
      stream.pull.uncons.flatMap {
        case None =>
          state.finish(maxRecordBytes) match
            case Some(lastLine) => Pull.output1(lastLine)
            case None           => Pull.done
        case Some((chunk, tail)) =>
          val (nextState, completed) = chunk.foldLeft((state, Vector.empty[InputLine])) {
            case ((currentState, lines), byte) =>
              val (updatedState, line) = currentState.add(byte, maxRecordBytes)
              (updatedState, lines ++ line)
          }
          Pull.output(Chunk.from(completed)) >> loop(tail, nextState)
      }

    loop(input, LineState()).stream

  private final case class LineState(
      index: Long = 0L,
      bytes: Vector[Byte] = Vector.empty,
      oversized: Boolean = false,
      hasContent: Boolean = false,
      hasNonWhitespace: Boolean = false,
      pendingCarriageReturn: Boolean = false
  ):
    def add(byte: Byte, maxRecordBytes: Int): (LineState, Vector[InputLine]) =
      if byte == '\n'.toByte then
        (LineState(index = index + 1), Vector(completedLine))
      else
        val withPreviousCarriageReturn =
          if pendingCarriageReturn then append('\r'.toByte, maxRecordBytes)
          else this
        val isCarriageReturn = byte == '\r'.toByte
        val updated =
          if isCarriageReturn then withPreviousCarriageReturn
          else withPreviousCarriageReturn.append(byte, maxRecordBytes)
        (
          updated.copy(
            hasContent = true,
            hasNonWhitespace = hasNonWhitespace || (byte & 0xff) > ' '.toInt,
            pendingCarriageReturn = isCarriageReturn
          ),
          Vector.empty
        )

    def finish(maxRecordBytes: Int): Option[InputLine] =
      val finalized =
        if pendingCarriageReturn then
          append('\r'.toByte, maxRecordBytes).copy(pendingCarriageReturn = false)
        else this
      Option.when(finalized.hasContent)(finalized.completedLine)

    private def append(byte: Byte, maxRecordBytes: Int): LineState =
      if !oversized then
        if bytes.size < maxRecordBytes then copy(bytes = bytes :+ byte)
        else
          copy(bytes = Vector.empty, oversized = true)
      else this

    private def completedLine: InputLine =
      if oversized && hasNonWhitespace then InputLine(index, None)
      else if oversized then InputLine(index, Some(""))
      else
        InputLine(index, Some(new String(bytes.toArray, StandardCharsets.UTF_8)))
