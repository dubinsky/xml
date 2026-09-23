package org.podval.store

import org.podval.metadata.{HasNames, Name, Names}
import org.podval.xml.{Xml, XmlCodec}
import zio.blocks.schema.{Modifier, Schema}

final case class Selector(
  override val names: Names,
  plural: Option[Names] = None
) extends HasNames derives CanEqual:
  /** Catalogued plural of the axis; when absent, the singular `names`. */
  def pluralOrNames: Names = plural.getOrElse(names)

object Selector:
  @Modifier.config(XmlCodec.Element, "plural")
  private final case class PluralData(
    names: Seq[Name] = Seq.empty
  ) derives CanEqual

  private object PluralData:
    given schema: Schema[PluralData] = Schema.derived
    val codec: XmlCodec[PluralData] = XmlCodec.derived

  private final case class Data(
    n: Option[String] = None,
    names: Seq[Name] = Seq.empty,
    plural: Option[PluralData] = None
  ) derives CanEqual

  private object Data:
    given schema: Schema[Data] = Schema.derived
    val codec: XmlCodec[Data] = XmlCodec.derived

  val codec: XmlCodec[Selector] = new XmlCodec[Selector]:
    override def elementName: String = "selector"
    override def isRecordLike: Boolean = true

    override def unsafeDecode(element: Xml.Element): Selector =
      val data: Data = Data.codec.unsafeDecode(element)
      val pluralNames: Option[Names] = data.plural.map(_.names).filter(_.nonEmpty).map(Names(_))
      Selector(Names.fromDefaultName(data.n, data.names), pluralNames)

    override def encodeNamed(elName: String, value: Selector): Xml.Element =
      val default: Option[String] = value.names.getDefaultName
      Data.codec.encodeNamed(elName, Data(
        n = default,
        names = if default.isDefined then Seq.empty else value.names.names,
        plural = value.plural.map(n => PluralData(n.names))
      ))
