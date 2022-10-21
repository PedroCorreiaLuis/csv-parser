package object formats {
  object CSV extends CSVDefinition {
    override val delimiter: Char = ','
    override val escape: Char = '\\'
    override val quote: Char = '"'
    override val terminator: String = "\r\n"
  }

  object PSV extends CSVDefinition {
    override val delimiter: Char = '|'
    override val quote: Char = '"'
    override val escape: Char = '\\'
    override val terminator: String = "\r\n"
  }

  object TSV extends CSVDefinition {
    override val delimiter: Char = '\t'
    override val quote: Char = '"'
    override val escape: Char = '"'
    override val terminator: String = "\r\n"
  }
}
