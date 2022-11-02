
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._


class Tests extends AnyFlatSpec {



  "iterator" should "return header and line" in{

    val source: Iterator[String] = Iterator("Best player World","Karim Benzema")
    val headers: Iterator[String] = source.take(1)
    val lines = source.drop(1)
    val expectedHeader: List[String] = List("Best player World")
    val expectedLine: List[String] = List("Karim Benzema")

    headers.toList shouldBe expectedHeader
    lines.toList shouldBe expectedLine

 }

  "iterators" should "return headers and lines" in{

    val source: Iterator[String] = Iterator("first name, second name, third name","Karim, Goat,Benzema", "crista, ronaldo")
    val headers: Iterator[String] = source.take(1)
    val lines = source.drop(1)
    val expectedHeader: List[String] = List("first name, second name, third name")
    val expectedLine: List[String] = List("Karim, Goat,Benzema", "crista, ronaldo")

    headers.toList shouldBe expectedHeader
    lines.toList shouldBe expectedLine
  }

  "Seq" should "return headers and Line" in {

    val source = Seq("Best player World", "Karim Benzema")
    val headers: Seq[String] = source.take(1)
    val lines = source.drop(1)
    val expectedHeader: List[String] = List("Best player World")
    val expectedLine: List[String] = List("Karim Benzema")

    headers.toList shouldBe expectedHeader
    lines.toList shouldBe expectedLine

  }

  "Seq" should "return headers and lines" in{

    val source: Seq[String] = Seq("first name, second name, third name","Karim, Goat,Benzema", "crista, ronaldo")
    val headers: Seq[String] = source.take(1)
    val lines = source.drop(1)
    val expectedHeader: List[String] = List("first name, second name, third name")
    val expectedLine: List[String] = List("Karim, Goat,Benzema", "crista, ronaldo")

    headers.toList shouldBe expectedHeader
    lines.toList shouldBe expectedLine
  }


}
