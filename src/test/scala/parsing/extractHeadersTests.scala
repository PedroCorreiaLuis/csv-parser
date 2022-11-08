package parsing


import models.{IteratorType, ParserInput, SeqType}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import parsing.Parser.extractHeaders

class extractHeadersTests extends AnyFlatSpec {



  "iterator" should "return header" in{

    val source: Iterator[String] = Iterator("Ballon dor","Karim Benzema")

    val parseInputSource: ParserInput = ParserInput(
      in = IteratorType(source),
      csvDefinition = formats.CSV,
      headers = Nil
    )(Parser.encode)

    val getHeader: ParserInput = extractHeaders(parseInputSource)
    getHeader.headers shouldBe source.toList.take(1)
  }

  "Iterator with header with many commas" should "return header" in {

    val source: Iterator[String] = Iterator("Ballon dor,,,,,Ballon dor feminin,Kopa Trophy,,,,Yashing Trophy,","Karim Benzema, Alexia Putellas, Gavi, Courtois")

    val parseInputSource: ParserInput = ParserInput(
      in = IteratorType(source),
      csvDefinition = formats.CSV,
      headers = Nil
    )(Parser.encode)

    val getHeader: ParserInput = extractHeaders(parseInputSource)
    getHeader.headers shouldBe source.toList.take(1)
  }

  "Seq" should "return header" in {

    val source: Seq[String] = Seq("Ballon dor","Karim Benzema")
      val parseInputSource: ParserInput = ParserInput(
      in = SeqType(source),
      csvDefinition = formats.CSV,
      headers = Nil
    )(Parser.encode)

    val getHeader: ParserInput = extractHeaders(parseInputSource)
    getHeader.headers shouldBe source.toList.take(1)
  }

  "Seq with header with many commas" should "return header" in {

    val source: Seq[String] = Seq("Ballon dor,,,,,Ballon dor feminin,Kopa Trophy,,,,Yashing Trophy,","Karim Benzema, Alexia Putellas, Gavi, Courtois")

    val parseInputSource: ParserInput = ParserInput(
      in = SeqType(source),
      csvDefinition = formats.CSV,
      headers = Nil
    )(Parser.encode)

    val getHeader: ParserInput = extractHeaders(parseInputSource)
    getHeader.headers shouldBe source.toList.take(1)
  }
}
