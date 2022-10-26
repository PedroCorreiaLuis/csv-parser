package parsing

import models._

object Parser extends Encoding {

  def parse(parserInput: ParserInput): ParsedCSV = {
    parsedCsv(extractHeaders(parserInput))
  }

  private val parsedCsv: ParserInput => ParsedCSV = parserInput => {
    parserInput.in match {
      case FileType(input)     => ???
      case IteratorType(input) => ???
      case IterableType(input) => ???
      case ReaderType(input)   => ???
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

      case SourceType(input) => ???
      case StreamType(input) =>
        val parsed: Stream[Either[String, String]] = input.map(parsingLogic)

        val (
          droppedLines: List[Either[String, String]],
          parsedLines: List[Either[String, String]]
        ) = parsed.toList.partition(_.isLeft)

        ParsedCSV(
          headers = parserInput.headers,
          parsedLines = droppedLines.map(_.merge),
          droppedLines = parsedLines.map(_.merge)
        )
    }
  }

  private val extractHeaders: ParserInput => ParserInput = { parserInput =>
    {
      if (parserInput.headers.isEmpty) {
        parserInput.in match {
          case FileType(input)     => ???
          case IteratorType(input) => ???
          case IterableType(input) => ???
          case ReaderType(input)   => ???
          case SeqType(input) =>
            val headers: List[String] = input.take(1).toList
            val lines: Seq[String] = input.drop(1)
            ParserInput(
              in = SeqType(lines),
              csvDefinition = parserInput.csvDefinition,
              headers = headers
            )
          case SourceType(input) => ???
          case StreamType(input) =>
            val headers: List[String] = input.take(1).toList
            val lines: Stream[String] = input.drop(1)
            ParserInput(
              in = StreamType(lines),
              csvDefinition = parserInput.csvDefinition,
              headers = headers
            )
        }
      } else {
        parserInput
      }
    }
  }

  private val parsingLogic: String => Either[String, String] = { ??? }

}
