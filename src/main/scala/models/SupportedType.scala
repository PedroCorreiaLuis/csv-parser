package models

import java.io.{File, Reader}
import scala.io.Source

sealed trait SupportedType {
  type InputType
  val input: InputType
}

case class FileType(input: File) extends SupportedType {
  override type InputType = File
}

case class IteratorType(input: Iterator[String]) extends SupportedType {
  override type InputType = Iterator[String]
}

case class IterableType(input: Iterable[String]) extends SupportedType {
  override type InputType = Iterable[String]
}

case class ReaderType(input: Reader) extends SupportedType {
  override type InputType = Reader
}

case class SeqType(input: Seq[String]) extends SupportedType {
  override type InputType = Seq[String]
}

case class SourceType(input: Source) extends SupportedType {
  override type InputType = Source
}

case class StreamType(input: Stream[String]) extends SupportedType {
  override type InputType = Stream[String]
}
