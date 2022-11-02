package parsing

import models._

import scala.io.Source

object Parser extends Encoding {

  def parse(parserInput: ParserInput): ParsedCSV = {
    parsedCsv(extractHeaders(toTraversable(parserInput)))
  }

  private val toTraversable: ParserInput => ParserInput = { parserInput =>
    val convert: NonTraversableType[_] => TraversableType[_] = {
      case SourceType(input) =>
        val allLines: Iterator[String] = input.getLines()
        input.close()
        IteratorType(allLines)

      case ReaderType(input) =>
        val chars: Array[Char] = Iterator.continually(input.read().toChar).toArray
        val source: Source = Source.fromChars(chars)
        val allLines: Iterator[String] = source.getLines()
        source.close()
        IteratorType(allLines)

      case FileType(input) =>
        val source: Source = Source.fromFile(input.toString)
        val allLines: Iterator[String] = source.getLines()
        source.close()
        IteratorType(allLines)
    }

    parserInput.in match {
      case _: TraversableType[_] => parserInput
      case input: NonTraversableType[_] =>
        ParserInput(
          in = convert(input),
          csvDefinition = parserInput.csvDefinition,
          headers = parserInput.headers
        )
    }
  }

  private val extractHeaders: ParserInput => ParserInput = { parserInput =>
    def buildParserInput(input: TraversableType[_], headers: List[String]): ParserInput = ParserInput(
      in = input,
      csvDefinition = parserInput.csvDefinition,
      headers = headers
    )

    if (parserInput.headers.nonEmpty) parserInput
    else {
      parserInput.in match {
        case IteratorType(input) => buildParserInput(IteratorType(input.drop(1)), input.take(1).toList)
        case IterableType(input) => buildParserInput(IterableType(input.drop(1)), input.take(1).toList)
        case SeqType(input)      => buildParserInput(SeqType(input.drop(1)), input.take(1).toList)
        case StreamType(input)   => buildParserInput(StreamType(input.drop(1)), input.take(1).toList)
      }
    }
  }

  private val parsedCsv: ParserInput => ParsedCSV = parserInput => {
    def applyParsingLogic(lines: List[String]): ParsedCSV = {
      val parsed: List[Either[EncodeType, EncodeType]] = lines.map(parsingLogic)

      val (
        droppedLines: List[Either[String, String]],
        parsedLines: List[Either[String, String]]
      ) = parsed.partition(_.isLeft)

      ParsedCSV(
        headers = parserInput.headers,
        parsedLines = parsedLines.map(_.merge),
        droppedLines = droppedLines.map(_.merge)
      )
    }

    parserInput.in match {
      case traversableType: TraversableType[_] => applyParsingLogic(traversableType.input.toList)
    }
  }

  private val parsingLogic: String => Either[String, String] = { ??? }

}
