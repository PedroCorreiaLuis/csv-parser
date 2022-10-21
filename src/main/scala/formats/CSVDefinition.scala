package formats

// https://en.wikipedia.org/wiki/Comma-separated_values
trait CSVDefinition {

  val delimiter: Char
  val escape: Char
  val quote: Char
  val terminator: String

}
