package typedjson

import cats.effect.{IO, Ref, Resource}
import cats.effect.unsafe.implicits.global
import cats.syntax.all.*
import com.sun.net.httpserver.{HttpExchange, HttpServer}
import io.circe.{Json, parser}
import java.net.{InetAddress, InetSocketAddress, URI}
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import scala.util.control.NonFatal

object GuiServer:
  private val maxRequestBytes = 12 * 1024 * 1024

  private final case class Request(
      input: String,
      schema: Option[String],
      mode: ProcessingMode,
      strict: Boolean
  )

  private final case class Response(status: Int, contentType: String, body: String)

  def run(port: Int): IO[cats.effect.ExitCode] =
    if port < 1 || port > 65535 then
      IO.println("Error: --port must be between 1 and 65535").as(cats.effect.ExitCode(2))
    else
      for
        page <- IO.blocking(readPage())
        _ <- Resource.make(IO.blocking(startServer(port, page))) { server =>
          IO.blocking(server.stop(0))
        }.use { _ =>
          IO.println(s"Local interface: http://127.0.0.1:$port") *> IO.never
        }
      yield cats.effect.ExitCode.Success

  private def readPage(): String =
    val stream = Option(getClass.getResourceAsStream("/gui/index.html"))
      .getOrElse(throw new IllegalStateException("GUI page resource /gui/index.html was not found."))
    try new String(stream.readAllBytes(), StandardCharsets.UTF_8)
    finally stream.close()

  private def startServer(port: Int, page: String): HttpServer =
    val server = HttpServer.create(
      new InetSocketAddress(InetAddress.getByName("127.0.0.1"), port),
      0
    )
    server.createContext("/", exchange => handleExchange(exchange, page))
    server.start()
    server

  private def handleExchange(exchange: HttpExchange, page: String): Unit =
    try
      val response = route(exchange, page).unsafeRunSync()
      send(exchange, response)
    catch
      case NonFatal(error) =>
        send(
          exchange,
          jsonResponse(500, Json.obj("error" -> Json.fromString(messageOf(error))))
        )
    finally exchange.close()

  private def route(exchange: HttpExchange, page: String): IO[Response] =
    (exchange.getRequestMethod, exchange.getRequestURI.getPath) match
      case ("GET", "/") =>
        IO.pure(Response(200, "text/html; charset=utf-8", page))
      case ("POST", "/api/run") =>
        IO.blocking(readRequestBody(exchange)).flatMap {
          case Left(response) => IO.pure(response)
          case Right(body) =>
            parser.parse(body).leftMap(error =>
              jsonResponse(400, Json.obj("error" -> Json.fromString(error.message)))
            ).flatMap(parseRequest) match
              case Left(response) => IO.pure(response)
              case Right(request) =>
                process(request)
                  .map(json => jsonResponse(200, json))
                  .handleErrorWith {
                    case error: IllegalArgumentException =>
                      IO.pure(
                        jsonResponse(400, Json.obj("error" -> Json.fromString(messageOf(error))))
                      )
                    case error => IO.raiseError(error)
                  }
        }
      case _ =>
        IO.pure(jsonResponse(404, Json.obj("error" -> Json.fromString("Not found."))))

  private def readRequestBody(exchange: HttpExchange): Either[Response, String] =
    val bytes = exchange.getRequestBody.readNBytes(maxRequestBytes + 1)
    if bytes.length > maxRequestBytes then
      Left(jsonResponse(413, Json.obj("error" -> Json.fromString("Request exceeds 12 MiB."))))
    else Right(new String(bytes, StandardCharsets.UTF_8))

  private def parseRequest(json: Json): Either[Response, Request] =
    val cursor = json.hcursor
    val parsed = for
      input <- cursor.get[String]("input").leftMap(_.message)
      modeText <- cursor.get[String]("mode").leftMap(_.message)
      mode <- modeText match
        case "validate" => Right(ProcessingMode.Validate)
        case "parse"    => Right(ProcessingMode.Parse)
        case _          => Left("mode must be either 'validate' or 'parse'.")
      strict <- cursor.get[Option[Boolean]]("strict").leftMap(_.message).map(_.getOrElse(false))
      schema <- cursor.get[Option[String]]("schema").leftMap(_.message)
    yield Request(input, schema, mode, strict)
    parsed.leftMap(error => jsonResponse(400, Json.obj("error" -> Json.fromString(error))))

  private def process(request: Request): IO[Json] =
    val config = AppConfig.load(None).leftMap(new IllegalArgumentException(_))
    IO.fromEither(config).flatMap { baseConfig =>
      val config = baseConfig.copy(logLevel = "off")
      request.schema match
        case None =>
          runRecords(
            request,
            config,
            Transaction.schema.codec,
            StreamingJson.renderTransaction
          )
        case Some(schemaText) =>
          val schema = for
            json <- parser.parse(schemaText).leftMap(error => s"Invalid JSON Schema: ${error.message}")
            loaded <- RuntimeJsonSchema.fromJson(json)
          yield loaded
          IO.fromEither(schema.leftMap(new IllegalArgumentException(_))).flatMap { loaded =>
            runRecords(request, config, loaded, identity)
          }
    }

  private def runRecords[A](
      request: Request,
      config: AppConfig,
      codec: JsonCodec[A],
      renderJson: A => Json
  ): IO[Json] =
    val inputResource = Resource.make(
      IO.blocking(Files.createTempFile("typed-json-gui-", ".jsonl"))
    )(path => IO.blocking(Files.deleteIfExists(path)).void)
    inputResource.use { path =>
      for
        _ <- IO.blocking(Files.writeString(path, request.input, StandardCharsets.UTF_8))
        diagnostics <- Ref.of[IO, Vector[String]](Vector.empty)
        validationOutput <- Ref.of[IO, Vector[String]](Vector.empty)
        outcome <- StreamingRunner.run[A](
          path,
          config,
          ProcessingMode.Validate,
          "json",
          quiet = false,
          strict = request.strict,
          renderJson = renderJson,
          renderText = value => renderJson(value).noSpaces,
          writeLine = line => validationOutput.update(_ :+ line),
          writeDiagnostic = line => diagnostics.update(_ :+ line)
        )(using codec)
        summaryLines <- validationOutput.get
        summary <- IO.fromEither(
          summaryLines.lastOption
            .toRight(new IllegalStateException("The validator did not produce a summary."))
            .flatMap(line =>
              parser.parse(line).leftMap(error =>
                new IllegalStateException(s"Could not read the validation summary: ${error.message}")
              )
            )
        )
        diagnosticLines <- diagnostics.get
        normalized <-
          if request.mode == ProcessingMode.Parse then
            val output = Ref.of[IO, Vector[String]](Vector.empty)
            output.flatMap { records =>
              StreamingRunner.run[A](
                path,
                config,
                ProcessingMode.Parse,
                "json",
                quiet = false,
                strict = request.strict,
                renderJson = renderJson,
                renderText = value => renderJson(value).noSpaces,
                writeLine = line => records.update(_ :+ line),
                writeDiagnostic = _ => IO.unit
              )(using codec) *> records.get.flatMap { lines =>
                IO.fromEither(lines.toList.traverse(line =>
                  parser.parse(line).leftMap(error =>
                    new IllegalStateException(s"Could not read normalized JSON: ${error.message}")
                  )
                ))
              }
            }
          else IO.pure(List.empty[Json])
      yield Json.obj(
        "success" -> Json.fromBoolean(outcome.isRight),
        "summary" -> summary,
        "diagnostics" -> Json.arr(diagnosticLines.map(Json.fromString)*),
        "records" -> Json.arr(normalized*)
      )
    }

  private def send(exchange: HttpExchange, response: Response): Unit =
    val bytes = response.body.getBytes(StandardCharsets.UTF_8)
    exchange.getResponseHeaders.set("Content-Type", response.contentType)
    exchange.getResponseHeaders.set("Cache-Control", "no-store")
    exchange.sendResponseHeaders(response.status, bytes.length.toLong)
    val output = exchange.getResponseBody
    try output.write(bytes)
    finally output.close()

  private def jsonResponse(status: Int, json: Json): Response =
    Response(status, "application/json; charset=utf-8", json.noSpaces)

  private def messageOf(error: Throwable): String =
    Option(error.getMessage).filter(_.nonEmpty).getOrElse(error.getClass.getSimpleName)
