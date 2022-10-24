package models

import formats.CSVDefinition
import parsing.Parser.EncodeType

case class ParserInput(
    in: SupportedType,
    csvDefinition: CSVDefinition,
    headers: List[String] = Nil
)(implicit encode: EncodeType)
