package models

case class ParsedCSV(
    headers: List[String],
    parsedLines: List[String],
    droppedLines: List[String] = Nil
)
