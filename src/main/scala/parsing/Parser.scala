package parsing

import models._

import scala.io.{Source}

object Parser extends Encoding {

  def parse(parserInput: ParserInput): ParsedCSV = {
    parsedCsv(extractHeaders(parserInput))
  }

  private val parsedCsv: ParserInput => ParsedCSV = parserInput => {
    parserInput.in match {
      case IteratorType(input) => ???
      case IterableType(input) => ???
      case ReaderType(input)   =>
        val source: Source =  Source.fromString(input.toString)
        val sourceLines: Iterator[String] = source.getLines()
        val parsed: Seq[Either[EncodeType, EncodeType]] = sourceLines.toList.map(parsingLogic)
          source.close()
        val (
          droppedLines: List[Either[String, String]],
          parsedLines: List[Either[String, String]]
          ) = parsed.partition(_.isLeft)

        ParsedCSV(
          headers = parserInput.headers,
          parsedLines = droppedLines.map(_.merge),
          droppedLines = parsedLines.map(_.merge)
        )
    case SeqType(input) =>
        val parsed: Seq[Either[String, String]] = input.map(parsingLogic)
        val (
          droppedLines: List[Either[String, String]],
          parsedLines: List[Either[String, String]]
        ) = parsed.partition(_.isLeft)

        ParsedCSV(
          headers = parserInput.headers,
          parsedLines = droppedLines.map(_.merge),
          droppedLines = parsedLines.map(_.merge)
        )

    case SourceType(input) =>
        val parsed: Iterator[Either[EncodeType, EncodeType]] = input.getLines().map(parsingLogic)

        val(
          droppedLines: Iterator[Either[String, String]],
          parsedLines: Iterator[Either[String, String]]
          ) = parsed.partition(_.isLeft)

        ParsedCSV(
          headers = parserInput.headers,
          parsedLines = droppedLines.map(_.merge).toList,
          droppedLines = parsedLines.map(_.merge).toList
        )
    case StreamType(input) => ???
    }
  }

  private val extractHeaders: ParserInput => ParserInput = { parserInput =>
    {
      if (parserInput.headers.isEmpty) {
        parserInput.in match {
          case FileType(input) =>
            val source: Source = Source.fromFile(input.toString)
            val sourceLines: Iterator[String] = source.getLines()
            val headers: List[EncodeType] = sourceLines.take(1).toList
            val lines: Iterator[String] = sourceLines
              source.close()
              ParserInput(
                in = IteratorType(lines),
                csvDefinition = parserInput.csvDefinition,
                headers = headers)
          case IteratorType(input) => ???
          case IterableType(input) => ???
          case ReaderType(input) =>
            val source: Source = Source.fromString(input.toString)
            val sourceLines: Iterator[String] = source.getLines()
            val headers: Seq[EncodeType] = sourceLines.take(1).toList
            val lines = sourceLines
            source.close()
              ParserInput(
                in = IteratorType(lines),
                csvDefinition = parserInput.csvDefinition,
                headers = headers.toList
              )
          case SeqType(input) =>
            val headers: List[String] = input.take(1).toList
            val lines: Seq[String] = input.drop(1)
            ParserInput(
              in = SeqType(lines),
              csvDefinition = parserInput.csvDefinition,
              headers = headers
            )
          case SourceType(input) =>
            val headers: Iterator[String] = input.getLines().take(1)
            val lines: Iterator[String] = input.getLines()
            ParserInput(
              in = IteratorType(lines),
              csvDefinition = parserInput.csvDefinition,
              headers = headers.toList
            )
          case StreamType(input) => ???
        }
      } else {
        parserInput
      }
    }
  }

  private val parsingLogic: String => Either[String, String] = { ??? }

}
