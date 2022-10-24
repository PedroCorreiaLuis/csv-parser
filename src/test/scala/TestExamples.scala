import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._


class TestExamples extends AnyFlatSpec {

  "one plus one" should "be 2" in {

    val a = 1+1

    a shouldBe 2

  }

  "two plus two" should "be 4" in {

    val b = 2+2

  b shouldBe 4
  }

}
