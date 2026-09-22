package org.podval.xml.codec

import org.podval.xml.{Xml, XmlCodec, XmlParser}
import Xml.given
import XmlCodec.given
import org.scalatest.funsuite.AnyFunSuite
import zio.blocks.schema.Schema

final class XmlCodecGivenSpec extends AnyFunSuite:
  test("import XmlCodec.given is enough to derive an identity field") {
    val codec: XmlCodec[Note] = XmlCodec.derived
    val decoded: Note = codec.decode(
      XmlParser.parseXml("<Note><body><p>a</p></body></Note>").toOption.get
    ).toOption.get
    assert(decoded.body.getChildren.flatMap(_.asElement).map(_.getName.qName) == Seq("p"))
  }

final case class Note(body: Xml.Element)
object Note:
  given schema: Schema[Note] = Schema.derived
