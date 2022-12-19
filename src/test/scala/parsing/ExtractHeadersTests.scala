package parsing


import models.{IterableType, IteratorType, ParserInput, SeqType, StreamType}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import parsing.Parser.extractHeaders

class ExtractHeadersTests extends AnyFlatSpec {

  "Iterator" should "return headers" in {

    val source: Iterator[String] = Iterator("Ballon dor, ola", "Karim Benzema, ola")
    val source1: Iterator[String] = Iterator(
      "Ballon dor,,,,,Ballon dor feminin,Kopa Trophy,,,,Yashing Trophy,",
      "Karim Benzema, Alexia Putellas, Gavi, Courtois"
    )

    val parseInputSource: ParserInput = ParserInput(
      in = IteratorType(source),
      csvDefinition = formats.CSV,
      headers = Nil
    )(Parser.encode)

    val parseInputSource1: ParserInput = ParserInput(
      in = IteratorType(source1),
      csvDefinition = formats.CSV,
      headers = Nil
    )(Parser.encode)

    val getHeader: ParserInput = extractHeaders(parseInputSource)
    getHeader.headers shouldBe List("Ballon dor, ola")

    val getHeader1: ParserInput = extractHeaders(parseInputSource1)
    getHeader1.headers shouldBe List("Ballon dor,,,,,Ballon dor feminin,Kopa Trophy,,,,Yashing Trophy,")
  }

  "Iterator with CSV format with CSVdefinition" should "return the headers" in {

    val source: Iterator[String] = Iterator(
      "Ballon dor\n\n\n\n\n,,,\\\\\\\\,,Ballon dor feminin,Kopa Trophy,\r\n,,\r\n,Yashing Trophy,",
      "Karim Benzema, Alexia Putellas, Gavi, Courtois"
    )
    val parseInputSource: ParserInput = ParserInput(
      in = IteratorType(source),
      csvDefinition = formats.CSV,
      headers = Nil
    )(Parser.encode)

    val source1: Iterator[String] = Iterator(
      "Ballon dor\n\n\n\n\n,,,\\\\\\\\,,Ballon dor feminin,Kopa Trophy,\r\n,,\r\n,Yashing Trophy,,,,\n\n\n\n\r\n\r\n",
      "Karim Benzema, Alexia Putellas, Gavi, Courtois"
    )
    val parseInputSource1: ParserInput = ParserInput(
      in = IteratorType(source1),
      csvDefinition = formats.CSV,
      headers = Nil
    )(Parser.encode)

    val source2: Iterator[String] = Iterator(
      "\"Ballon Dor List\",\"club\"\n\"Karim Benzema\", \"Real Madrid\"\n\"Sadio Mane\", \"Liverpool\"\n\"Kevin" +
        " De Bruyne\", \"Manchester  City\"\n\"Robert Lewandowski\", \"Barcelona\"\n\"Mohamed Salah\", \"Liverpool" +
        "\"\n\"Ricardo Esgaio\", \"Sporting\"\n\"Mbappe\", \"PSG\""
    )
    val parseInputSource2: ParserInput = ParserInput(
      in = IteratorType(source2),
      csvDefinition = formats.CSV,
      headers = Nil
    )(Parser.encode)

    val getHeader: ParserInput = extractHeaders(parseInputSource)
    getHeader.headers shouldBe List(
      "Ballon dor\n\n\n\n\n,,,\\\\\\\\,,Ballon dor feminin,Kopa Trophy,\r\n,,\r\n,Yashing Trophy,"
    )

    val getHeaderSource1: ParserInput = extractHeaders(parseInputSource1)
    getHeaderSource1.headers shouldBe List(
      "Ballon dor\n\n\n\n\n,,,\\\\\\\\,,Ballon dor feminin,Kopa Trophy,\r\n,,\r\n,Yashing Trophy,,,,\n\n\n\n\r\n\r\n"
    )

    val getHeaderSource2: ParserInput = extractHeaders(parseInputSource2)
    getHeaderSource2.headers shouldBe List(
      "\"Ballon Dor List\",\"club\"\n\"Karim Benzema\", \"Real Madrid\"\n\"Sadio Mane\", \"Liverpool\"\n\"Kevin" +
        " De Bruyne\", \"Manchester  City\"\n\"Robert Lewandowski\", \"Barcelona\"\n\"Mohamed Salah\", \"" +
        "Liverpool\"\n\"Ricardo Esgaio\", \"Sporting\"\n\"Mbappe\", \"PSG\""
    )
  }

  "Iterator with PSV format from CSVDefinition" should "return headers" in {
    val source: Iterator[String] = Iterator("Ballon Dor List", "| club\nKarim Benzema")
    val parseInputSource: ParserInput = ParserInput(
      in = IteratorType(source),
      csvDefinition = formats.PSV,
      headers = Nil
    )(Parser.encode)

    val source1: Iterator[String] = Iterator(
      "Ballon Dor List",
      "| club\nKarim Benzema",
      " | Real Madrid\nSadio Mane | Liverpool\nKevin De Bruyne | Manchester  City\nRobert Lewandowski |" +
        " Barcelona\nMohamed Salah | Liverpool\nRicardo Esgaio | Sporting\nMbappe | PSG"
    )
    val parseInputSource1: ParserInput = ParserInput(
      in = IteratorType(source1),
      csvDefinition = formats.PSV,
      headers = Nil
    )(Parser.encode)

    val source2: Iterator[String] = Iterator(
      "\"Ballon Dor List\"|\"club\"\n\"Karim Benzema\"|\"Real Madrid\"\n\"Sadio Mane\" | \"Liverpool\"\n\"Kevin" +
        " De Bruyne\" | \"Manchester  City\"\n\"Robert Lewandowski\" | \"Barcelona\"\n\"Mohamed Salah\" | \"" +
        "Liverpool\"\n\"Ricardo Esgaio\" | \"Sporting\"\n\"Mbappe\" | \"PSG\""
    )
    val parseInputSource2: ParserInput = ParserInput(
      in = IteratorType(source2),
      csvDefinition = formats.PSV,
      headers = Nil
    )(Parser.encode)

    val getHeader: ParserInput = extractHeaders(parseInputSource)
    getHeader.headers shouldBe List("Ballon Dor List")

    val getHeaderSource1: ParserInput = extractHeaders(parseInputSource1)
    getHeaderSource1.headers shouldBe List("Ballon Dor List")

    val getHeaderSource2: ParserInput = extractHeaders(parseInputSource2)
    getHeaderSource2.headers shouldBe List(
      "\"Ballon Dor List\"|\"club\"\n\"Karim Benzema\"|\"Real Madrid\"\n\"Sadio Mane\" | \"Liverpool\"\n\"Kevin" +
        " De Bruyne\" | \"Manchester  City\"\n\"Robert Lewandowski\" | \"Barcelona\"\n\"Mohamed Salah\" |" +
        " \"Liverpool\"\n\"Ricardo Esgaio\" | \"Sporting\"\n\"Mbappe\" | \"PSG\""
    )
  }

  "Iterator with TSV format from CSVDefinition" should "return headers" in {

    val source: Iterator[String] = Iterator(
      "Ballon dor\tBallon dor feminin\tKopa Trophy\t\\r\\n\t\t\\r\\n\tYashing Trophy\t",
      "Karim Benzema\t,Alexia Putellas\t,Gavi\t,Courtois"
    )
    val parseInputSource: ParserInput = ParserInput(
      in = IteratorType(source),
      csvDefinition = formats.TSV,
      headers = Nil
    )(Parser.encode)

    val source1: Iterator[String] = Iterator(
      "Ballon Dor List\t club",
      "Karim Benzema\tReal Madrid\nSadio Mane\tLiverpool\nKevin De Bruyne\tManchester  City\nRobert" +
        " Lewandowski\t Barcelona\nMohamed Salah\t Liverpool\nRicardo Esgaio\t Sporting\nMbappe\tPSG"
    )
    val parseInputSource1: ParserInput = ParserInput(
      in = IteratorType(source1),
      csvDefinition = formats.TSV,
      headers = Nil
    )(Parser.encode)

    val source2: Iterator[String] = Iterator(
      "Ballon Dor List\t\t\t\tclub\r\n",
      "Karim Benzema\t\t\t\tReal Madrid\nSadio Mane\tLiverpool\nKevin De Bruyne\tManchester  City\\\nRobert" +
        " Lewandowski\t Barcelona\nMohamed Salah\t Liverpool\nRicardo Esgaio\t Sporting\nMbappe\tPSG"
    )
    val parseInputSource2: ParserInput = ParserInput(
      in = IteratorType(source2),
      csvDefinition = formats.TSV,
      headers = Nil
    )(Parser.encode)

    val getHeader: ParserInput = extractHeaders(parseInputSource)
    getHeader.headers shouldBe List("Ballon dor\tBallon dor feminin\tKopa Trophy\t\\r\\n\t\t\\r\\n\tYashing Trophy\t")

    val getHeaderSource1: ParserInput = extractHeaders(parseInputSource1)
    getHeaderSource1.headers shouldBe List("Ballon Dor List\t club")

    val getHeaderSource2: ParserInput = extractHeaders(parseInputSource2)
    getHeaderSource2.headers shouldBe List("Ballon Dor List\t\t\t\tclub\r\n")
  }

  "Seq" should "return header" in {

    val source: Seq[String] = Seq("Ballon dor", "Karim Benzema")
    val source1: Seq[String] = Seq(
      "Ballon dor,,,,,Ballon dor feminin,Kopa Trophy,,,,Yashing Trophy,",
      "Karim Benzema, Alexia Putellas, Gavi, Courtois"
    )

    val parseInputSource: ParserInput = ParserInput(
      in = SeqType(source),
      csvDefinition = formats.CSV,
      headers = Nil
    )(Parser.encode)

    val parseInputSource1: ParserInput = ParserInput(
      in = SeqType(source1),
      csvDefinition = formats.CSV,
      headers = Nil
    )(Parser.encode)

    val getHeader: ParserInput = extractHeaders(parseInputSource)
    getHeader.headers shouldBe List("Ballon dor")

    val getHeader1: ParserInput = extractHeaders(parseInputSource1)
    getHeader1.headers shouldBe List("Ballon dor,,,,,Ballon dor feminin,Kopa Trophy,,,,Yashing Trophy,")
  }

  "Seq with CSV format from CSVdefinition " should "return the headers" in {

    val source: Seq[String] = Seq(
      "Ballon dor\n\n\n\n\n,,,\\\\\\\\,,Ballon dor feminin,Kopa Trophy,\r\n,,\r\n,Yashing Trophy,",
      "Karim Benzema, Alexia Putellas, Gavi, Courtois"
    )
    val parseInputSource: ParserInput = ParserInput(
      in = SeqType(source),
      csvDefinition = formats.CSV,
      headers = Nil
    )(Parser.encode)

    val source1: Seq[String] = Seq(
      "Ballon dor\n\n\n\n\n,,,\\\\\\\\,,Ballon dor feminin,Kopa Trophy,\r\n,,\r\n,Yashing Trophy,,,,\n\n\n\n\r\n\r\n",
      "Karim Benzema, Alexia Putellas, Gavi, Courtois"
    )
    val parseInputSource1: ParserInput = ParserInput(
      in = SeqType(source1),
      csvDefinition = formats.CSV,
      headers = Nil
    )(Parser.encode)

    val getHeader: ParserInput = extractHeaders(parseInputSource)
    getHeader.headers shouldBe List(
      "Ballon dor\n\n\n\n\n,,,\\\\\\\\,,Ballon dor feminin,Kopa Trophy,\r\n,,\r\n,Yashing Trophy,"
    )

    val getHeaderSource1: ParserInput = extractHeaders(parseInputSource1)
    getHeaderSource1.headers shouldBe List(
      "Ballon dor\n\n\n\n\n,,,\\\\\\\\,,Ballon dor feminin,Kopa Trophy,\r\n,,\r\n,Yashing Trophy,,,,\n\n\n\n\r\n\r\n"
    )
  }

  "Seq with PSV format from CSVdefinition " should "return the headers" in {

    val source: Seq[String] = Seq("Ballon Dor List", "| club\nKarim Benzema")
    val parseInputSource: ParserInput = ParserInput(
      in = SeqType(source),
      csvDefinition = formats.PSV,
      headers = Nil
    )(Parser.encode)

    val source1: Seq[String] = Seq(
      "Ballon Dor List",
      "| club\nKarim Benzema",
      " | Real Madrid\nSadio Mane | Liverpool\nKevin De Bruyne | Manchester  City\nRobert Lewandowski | Barcelona\n" +
        "Mohamed Salah | Liverpool\nRicardo Esgaio | Sporting\nMbappe | PSG"
    )
    val parseInputSource1: ParserInput = ParserInput(
      in = SeqType(source1),
      csvDefinition = formats.PSV,
      headers = Nil
    )(Parser.encode)

    val source2: Seq[String] = Seq(
      "\"Ballon Dor List\"|\"club\"\n\"Karim Benzema\"|\"Real Madrid\"\n\"Sadio Mane\" | \"Liverpool\"\n\"Kevin" +
        " De Bruyne\" | \"Manchester  City\"\n\"Robert Lewandowski\" | \"Barcelona\"\n\"Mohamed Salah\" | " +
        "\"Liverpool\"\n\"Ricardo Esgaio\" | \"Sporting\"\n\"Mbappe\" | \"PSG\""
    )
    val parseInputSource2: ParserInput = ParserInput(
      in = SeqType(source2),
      csvDefinition = formats.PSV,
      headers = Nil
    )(Parser.encode)

    val getHeader: ParserInput = extractHeaders(parseInputSource)
    getHeader.headers shouldBe List("Ballon Dor List")

    val getHeaderSource1: ParserInput = extractHeaders(parseInputSource1)
    getHeaderSource1.headers shouldBe List("Ballon Dor List")

    val getHeaderSource2: ParserInput = extractHeaders(parseInputSource2)
    getHeaderSource2.headers shouldBe List(
      "\"Ballon Dor List\"|\"club\"\n\"Karim Benzema\"|\"Real Madrid\"\n\"Sadio Mane\" | \"Liverpool\"\n\"Kevin" +
        " De Bruyne\" | \"Manchester  City\"\n\"Robert Lewandowski\" | \"Barcelona\"\n\"Mohamed Salah\" |" +
        " \"Liverpool\"\n\"Ricardo Esgaio\" | \"Sporting\"\n\"Mbappe\" | \"PSG\""
    )

  }

  "Seq with TSV format from CSVdefinition " should "return the headers" in {

    val source: Seq[String] = Seq(
      "Ballon dor\tBallon dor feminin\tKopa Trophy\t\\r\\n\t\t\\r\\n\tYashing Trophy\t",
      "Karim Benzema\t,Alexia Putellas\t,Gavi\t,Courtois"
    )
    val parseInputSource: ParserInput = ParserInput(
      in = SeqType(source),
      csvDefinition = formats.TSV,
      headers = Nil
    )(Parser.encode)

    val source1: Seq[String] = Seq(
      "Ballon Dor List\t club",
      "Karim Benzema\tReal Madrid\nSadio Mane\tLiverpool\nKevin De Bruyne\tManchester  City\nRobert Lewandowski" +
        "\t Barcelona\nMohamed Salah\t Liverpool\nRicardo Esgaio\t Sporting\nMbappe\tPSG"
    )
    val parseInputSource1: ParserInput = ParserInput(
      in = SeqType(source1),
      csvDefinition = formats.TSV,
      headers = Nil
    )(Parser.encode)

    val source2: Seq[String] = Seq(
      "Ballon Dor List\t\t\t\tclub\r\n",
      "Karim Benzema\t\t\t\tReal Madrid\nSadio Mane\tLiverpool\nKevin De Bruyne\tManchester  City\\\nRobert " +
        "Lewandowski\t Barcelona\nMohamed Salah\t Liverpool\nRicardo Esgaio\t Sporting\nMbappe\tPSG"
    )
    val parseInputSource2: ParserInput = ParserInput(
      in = SeqType(source2),
      csvDefinition = formats.TSV,
      headers = Nil
    )(Parser.encode)

    val getHeader: ParserInput = extractHeaders(parseInputSource)
    getHeader.headers shouldBe List("Ballon dor\tBallon dor feminin\tKopa Trophy\t\\r\\n\t\t\\r\\n\tYashing Trophy\t")

    val getHeaderSource1: ParserInput = extractHeaders(parseInputSource1)
    getHeaderSource1.headers shouldBe List("Ballon Dor List\t club")

    val getHeaderSource2: ParserInput = extractHeaders(parseInputSource2)
    getHeaderSource2.headers shouldBe List("Ballon Dor List\t\t\t\tclub\r\n")
  }
  
  "Iterable" should "return header" in {

    val source: Iterable[String] = Iterable("Ballon dor", "Karim Benzema")
    val source1: Iterable[String] = Iterable(
      "Ballon dor,,,,,Ballon dor feminin,Kopa Trophy,,,,Yashing Trophy,",
      "Karim Benzema, Alexia Putellas, Gavi, Courtois"
    )

    val parseInputSource: ParserInput = ParserInput(
      in = IterableType(source),
      csvDefinition = formats.CSV,
      headers = Nil
    )(Parser.encode)

    val parseInputSource1: ParserInput = ParserInput(
      in = IterableType(source1),
      csvDefinition = formats.CSV,
      headers = Nil
    )(Parser.encode)

    val getHeader: ParserInput = extractHeaders(parseInputSource)
    getHeader.headers shouldBe List("Ballon dor")

    val getHeader1: ParserInput = extractHeaders(parseInputSource1)
    getHeader1.headers shouldBe List("Ballon dor,,,,,Ballon dor feminin,Kopa Trophy,,,,Yashing Trophy,")
  }

  "Iterable with CSV format from CSVdefinition " should "return the headers" in {

    val source: Iterable[String] = Iterable(
      "Ballon dor\n\n\n\n\n,,,\\\\\\\\,,Ballon dor feminin,Kopa Trophy,\r\n,,\r\n,Yashing Trophy,",
      "Karim Benzema, Alexia Putellas, Gavi, Courtois"
    )
    val parseInputSource: ParserInput = ParserInput(
      in = IterableType(source),
      csvDefinition = formats.CSV,
      headers = Nil
    )(Parser.encode)

    val source1: Iterable[String] = Iterable(
      "Ballon dor\n\n\n\n\n,,,\\\\\\\\,,Ballon dor feminin,Kopa Trophy,\r\n,,\r\n,Yashing Trophy,,,,\n\n\n\n\r\n\r\n",
      "Karim Benzema, Alexia Putellas, Gavi, Courtois"
    )
    val parseInputSource1: ParserInput = ParserInput(
      in = IterableType(source1),
      csvDefinition = formats.CSV,
      headers = Nil
    )(Parser.encode)

    val getHeader: ParserInput = extractHeaders(parseInputSource)
    getHeader.headers shouldBe List(
      "Ballon dor\n\n\n\n\n,,,\\\\\\\\,,Ballon dor feminin,Kopa Trophy,\r\n,,\r\n,Yashing Trophy,"
    )

    val getHeaderSource1: ParserInput = extractHeaders(parseInputSource1)
    getHeaderSource1.headers shouldBe List(
      "Ballon dor\n\n\n\n\n,,,\\\\\\\\,,Ballon dor feminin,Kopa Trophy,\r\n,,\r\n,Yashing Trophy,,,,\n\n\n\n\r\n\r\n"
    )
  }

  "Iterable with PSV format from CSVdefinition " should "return the headers" in {

    val source: Iterable[String] = Iterable("Ballon Dor List", "| club\nKarim Benzema")
    val parseInputSource: ParserInput = ParserInput(
      in = IterableType(source),
      csvDefinition = formats.PSV,
      headers = Nil
    )(Parser.encode)

    val source1: Iterable[String] = Iterable(
      "Ballon Dor List",
      "| club\nKarim Benzema",
      " | Real Madrid\nSadio Mane | Liverpool\nKevin De Bruyne | Manchester  City\nRobert Lewandowski | Barcelona\n" +
        "Mohamed Salah | Liverpool\nRicardo Esgaio | Sporting\nMbappe | PSG"
    )
    val parseInputSource1: ParserInput = ParserInput(
      in = IterableType(source1),
      csvDefinition = formats.PSV,
      headers = Nil
    )(Parser.encode)

    val source2: Iterable[String] = Iterable(
      "\"Ballon Dor List\"|\"club\"\n\"Karim Benzema\"|\"Real Madrid\"\n\"Sadio Mane\" | \"Liverpool\"\n\"Kevin" +
        " De Bruyne\" | \"Manchester  City\"\n\"Robert Lewandowski\" | \"Barcelona\"\n\"Mohamed Salah\" | " +
        "\"Liverpool\"\n\"Ricardo Esgaio\" | \"Sporting\"\n\"Mbappe\" | \"PSG\""
    )
    val parseInputSource2: ParserInput = ParserInput(
      in = IterableType(source2),
      csvDefinition = formats.PSV,
      headers = Nil
    )(Parser.encode)

    val getHeader: ParserInput = extractHeaders(parseInputSource)
    getHeader.headers shouldBe List("Ballon Dor List")

    val getHeaderSource1: ParserInput = extractHeaders(parseInputSource1)
    getHeaderSource1.headers shouldBe List("Ballon Dor List")

    val getHeaderSource2: ParserInput = extractHeaders(parseInputSource2)
    getHeaderSource2.headers shouldBe List(
      "\"Ballon Dor List\"|\"club\"\n\"Karim Benzema\"|\"Real Madrid\"\n\"Sadio Mane\" | \"Liverpool\"\n\"Kevin" +
        " De Bruyne\" | \"Manchester  City\"\n\"Robert Lewandowski\" | \"Barcelona\"\n\"Mohamed Salah\" |" +
        " \"Liverpool\"\n\"Ricardo Esgaio\" | \"Sporting\"\n\"Mbappe\" | \"PSG\""
    )

  }

  "Iterable with TSV format from CSVdefinition " should "return the headers" in {

    val source: Iterable[String] = Iterable(
      "Ballon dor\tBallon dor feminin\tKopa Trophy\t\\r\\n\t\t\\r\\n\tYashing Trophy\t",
      "Karim Benzema\t,Alexia Putellas\t,Gavi\t,Courtois"
    )
    val parseInputSource: ParserInput = ParserInput(
      in = IterableType(source),
      csvDefinition = formats.TSV,
      headers = Nil
    )(Parser.encode)

    val source1: Iterable[String] = Iterable(
      "Ballon Dor List\t club",
      "Karim Benzema\tReal Madrid\nSadio Mane\tLiverpool\nKevin De Bruyne\tManchester  City\nRobert Lewandowski" +
        "\t Barcelona\nMohamed Salah\t Liverpool\nRicardo Esgaio\t Sporting\nMbappe\tPSG"
    )
    val parseInputSource1: ParserInput = ParserInput(
      in = IterableType(source1),
      csvDefinition = formats.TSV,
      headers = Nil
    )(Parser.encode)

    val source2: Iterable[String] = Iterable(
      "Ballon Dor List\t\t\t\tclub\r\n",
      "Karim Benzema\t\t\t\tReal Madrid\nSadio Mane\tLiverpool\nKevin De Bruyne\tManchester  City\\\nRobert " +
        "Lewandowski\t Barcelona\nMohamed Salah\t Liverpool\nRicardo Esgaio\t Sporting\nMbappe\tPSG"
    )
    val parseInputSource2: ParserInput = ParserInput(
      in = IterableType(source2),
      csvDefinition = formats.TSV,
      headers = Nil
    )(Parser.encode)

    val getHeader: ParserInput = extractHeaders(parseInputSource)
    getHeader.headers shouldBe List("Ballon dor\tBallon dor feminin\tKopa Trophy\t\\r\\n\t\t\\r\\n\tYashing Trophy\t")

    val getHeaderSource1: ParserInput = extractHeaders(parseInputSource1)
    getHeaderSource1.headers shouldBe List("Ballon Dor List\t club")

    val getHeaderSource2: ParserInput = extractHeaders(parseInputSource2)
    getHeaderSource2.headers shouldBe List("Ballon Dor List\t\t\t\tclub\r\n")
  }
  "Stream" should "return header" in {

    val source: Stream[String] = Stream("Ballon dor", "Karim Benzema")
    val source1: Stream[String] = Stream(
      "Ballon dor,,,,,Ballon dor feminin,Kopa Trophy,,,,Yashing Trophy,",
      "Karim Benzema, Alexia Putellas, Gavi, Courtois"
    )

    val parseInputSource: ParserInput = ParserInput(
      in = StreamType(source),
      csvDefinition = formats.CSV,
      headers = Nil
    )(Parser.encode)

    val parseInputSource1: ParserInput = ParserInput(
      in = StreamType(source1),
      csvDefinition = formats.CSV,
      headers = Nil
    )(Parser.encode)

    val getHeader: ParserInput = extractHeaders(parseInputSource)
    getHeader.headers shouldBe List("Ballon dor")

    val getHeader1: ParserInput = extractHeaders(parseInputSource1)
    getHeader1.headers shouldBe List("Ballon dor,,,,,Ballon dor feminin,Kopa Trophy,,,,Yashing Trophy,")
  }
  "Stream with CSV format from CSVdefinition " should "return the headers" in {

    val source: Stream[String] = Stream(
      "Ballon dor\n\n\n\n\n,,,\\\\\\\\,,Ballon dor feminin,Kopa Trophy,\r\n,,\r\n,Yashing Trophy,",
      "Karim Benzema, Alexia Putellas, Gavi, Courtois"
    )
    val parseInputSource: ParserInput = ParserInput(
      in = StreamType(source),
      csvDefinition = formats.CSV,
      headers = Nil
    )(Parser.encode)

    val source1: Stream[String] = Stream(
      "Ballon dor\n\n\n\n\n,,,\\\\\\\\,,Ballon dor feminin,Kopa Trophy,\r\n,,\r\n,Yashing Trophy,,,,\n\n\n\n\r\n\r\n",
      "Karim Benzema, Alexia Putellas, Gavi, Courtois"
    )
    val parseInputSource1: ParserInput = ParserInput(
      in = StreamType(source1),
      csvDefinition = formats.CSV,
      headers = Nil
    )(Parser.encode)

    val getHeader: ParserInput = extractHeaders(parseInputSource)
    getHeader.headers shouldBe List(
      "Ballon dor\n\n\n\n\n,,,\\\\\\\\,,Ballon dor feminin,Kopa Trophy,\r\n,,\r\n,Yashing Trophy,"
    )

    val getHeaderSource1: ParserInput = extractHeaders(parseInputSource1)
    getHeaderSource1.headers shouldBe List(
      "Ballon dor\n\n\n\n\n,,,\\\\\\\\,,Ballon dor feminin,Kopa Trophy,\r\n,,\r\n,Yashing Trophy,,,,\n\n\n\n\r\n\r\n"
    )
  }

  "Stream with PSV format from CSVdefinition " should "return the headers" in {

    val source: Stream[String] = Stream("Ballon Dor List", "| club\nKarim Benzema")
    val parseInputSource: ParserInput = ParserInput(
      in = StreamType(source),
      csvDefinition = formats.PSV,
      headers = Nil
    )(Parser.encode)

    val source1: Stream[String] = Stream(
      "Ballon Dor List",
      "| club\nKarim Benzema",
      " | Real Madrid\nSadio Mane | Liverpool\nKevin De Bruyne | Manchester  City\nRobert Lewandowski | Barcelona\n" +
        "Mohamed Salah | Liverpool\nRicardo Esgaio | Sporting\nMbappe | PSG"
    )
    val parseInputSource1: ParserInput = ParserInput(
      in = StreamType(source1),
      csvDefinition = formats.PSV,
      headers = Nil
    )(Parser.encode)

    val source2: Stream[String] = Stream(
      "\"Ballon Dor List\"|\"club\"\n\"Karim Benzema\"|\"Real Madrid\"\n\"Sadio Mane\" | \"Liverpool\"\n\"Kevin" +
        " De Bruyne\" | \"Manchester  City\"\n\"Robert Lewandowski\" | \"Barcelona\"\n\"Mohamed Salah\" | " +
        "\"Liverpool\"\n\"Ricardo Esgaio\" | \"Sporting\"\n\"Mbappe\" | \"PSG\""
    )
    val parseInputSource2: ParserInput = ParserInput(
      in = StreamType(source2),
      csvDefinition = formats.PSV,
      headers = Nil
    )(Parser.encode)

    val getHeader: ParserInput = extractHeaders(parseInputSource)
    getHeader.headers shouldBe List("Ballon Dor List")

    val getHeaderSource1: ParserInput = extractHeaders(parseInputSource1)
    getHeaderSource1.headers shouldBe List("Ballon Dor List")

    val getHeaderSource2: ParserInput = extractHeaders(parseInputSource2)
    getHeaderSource2.headers shouldBe List(
      "\"Ballon Dor List\"|\"club\"\n\"Karim Benzema\"|\"Real Madrid\"\n\"Sadio Mane\" | \"Liverpool\"\n\"Kevin" +
        " De Bruyne\" | \"Manchester  City\"\n\"Robert Lewandowski\" | \"Barcelona\"\n\"Mohamed Salah\" |" +
        " \"Liverpool\"\n\"Ricardo Esgaio\" | \"Sporting\"\n\"Mbappe\" | \"PSG\""
    )

  }
  "Stream with TSV format from CSVdefinition " should "return the headers" in {

    val source: Stream[String] = Stream(
      "Ballon dor\tBallon dor feminin\tKopa Trophy\t\\r\\n\t\t\\r\\n\tYashing Trophy\t",
      "Karim Benzema\t,Alexia Putellas\t,Gavi\t,Courtois"
    )
    val parseInputSource: ParserInput = ParserInput(
      in = StreamType(source),
      csvDefinition = formats.TSV,
      headers = Nil
    )(Parser.encode)

    val source1: Stream[String] = Stream(
      "Ballon Dor List\t club",
      "Karim Benzema\tReal Madrid\nSadio Mane\tLiverpool\nKevin De Bruyne\tManchester  City\nRobert Lewandowski" +
        "\t Barcelona\nMohamed Salah\t Liverpool\nRicardo Esgaio\t Sporting\nMbappe\tPSG"
    )
    val parseInputSource1: ParserInput = ParserInput(
      in = StreamType(source1),
      csvDefinition = formats.TSV,
      headers = Nil
    )(Parser.encode)

    val source2: Stream[String] = Stream(
      "Ballon Dor List\t\t\t\tclub\r\n",
      "Karim Benzema\t\t\t\tReal Madrid\nSadio Mane\tLiverpool\nKevin De Bruyne\tManchester  City\\\nRobert " +
        "Lewandowski\t Barcelona\nMohamed Salah\t Liverpool\nRicardo Esgaio\t Sporting\nMbappe\tPSG"
    )
    val parseInputSource2: ParserInput = ParserInput(
      in = StreamType(source2),
      csvDefinition = formats.TSV,
      headers = Nil
    )(Parser.encode)

    val getHeader: ParserInput = extractHeaders(parseInputSource)
    getHeader.headers shouldBe List("Ballon dor\tBallon dor feminin\tKopa Trophy\t\\r\\n\t\t\\r\\n\tYashing Trophy\t")

    val getHeaderSource1: ParserInput = extractHeaders(parseInputSource1)
    getHeaderSource1.headers shouldBe List("Ballon Dor List\t club")

    val getHeaderSource2: ParserInput = extractHeaders(parseInputSource2)
    getHeaderSource2.headers shouldBe List("Ballon Dor List\t\t\t\tclub\r\n")
  }
}
