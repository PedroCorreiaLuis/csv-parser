package models

case class ParsedCSV(headers: List[String],
                     parsedLines: Iterator[String],
                     droppedLines: List[String] = Nil)
