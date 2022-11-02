package models

import java.io.{File, Reader}
import scala.io.Source

sealed trait SupportedType {
  type InputType
  val input: InputType
}

sealed trait TraversableType[A <: TraversableOnce[String]] extends SupportedType { override type InputType = A }
sealed trait NonTraversableType[A] extends SupportedType { override type InputType = A }

final case class IteratorType(input: Iterator[String]) extends TraversableType[Iterator[String]]
final case class IterableType(input: Iterable[String]) extends TraversableType[Iterable[String]]
final case class SeqType(input: Seq[String]) extends TraversableType[Seq[String]]
final case class StreamType(input: Stream[String]) extends TraversableType[Stream[String]]

final case class FileType(input: File) extends NonTraversableType[File]
final case class ReaderType(input: Reader) extends NonTraversableType[Reader]
final case class SourceType(input: Source) extends NonTraversableType[Source]
