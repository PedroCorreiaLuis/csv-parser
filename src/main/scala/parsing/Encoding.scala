package parsing

trait Encoding {
  type EncodeType = String
  implicit val encode: EncodeType = "UTF-8"
}
