package org.podval.store

import org.podval.metadata.{HasNames, Name, Names}
import org.podval.xml.{XmlAst, XmlCodec}
import zio.blocks.schema.{Modifier, Schema}

final case class Selector(
  override val names: Names,
  plural: Option[Names] = None
) extends HasNames derives CanEqual:
  /** Catalogued plural of the axis; when absent, the singular `names`. */
  def pluralOrNames: Names = plural.getOrElse(names)

object Selector:
  private final case class PluralData(
    @Modifier.config(XmlCodec.Element, "name") names: Seq[Name.Data] = Seq.empty
  ) derives CanEqual

  private object PluralData:
    given schema: Schema[PluralData] = Schema.derived
    val codec: XmlCodec[PluralData] = XmlCodec.derived

  private final case class Data(
    @Modifier.config(XmlCodec.Attribute, "") n: Option[String] = None,
    @Modifier.config(XmlCodec.Element, "name") names: Seq[Name.Data] = Seq.empty,
    @Modifier.config(XmlCodec.Element, "plural") plural: Option[PluralData] = None
  ) derives CanEqual

  private object Data:
    given schema: Schema[Data] = Schema.derived
    val codec: XmlCodec[Data] = XmlCodec.derived

  val codec: XmlCodec[Selector] = new XmlCodec[Selector]:
    override def elementName: String = "selector"
    override def isRecordLike: Boolean = true

    override def unsafeDecode[E: XmlAst](element: E): Selector =
      val data: Data = Data.codec.unsafeDecode(element)
      val pluralNames: Option[Names] =
        data.plural.map(_.names.map(Name.fromData)).filter(_.nonEmpty).map(Names(_))
      Selector(Names.fromDefaultName(data.n, data.names.map(Name.fromData)), pluralNames)

    override def encodeNamed[E: XmlAst](elName: String, value: Selector): E =
      val default: Option[String] = value.names.getDefaultName
      Data.codec.encodeNamed(elName, Data(
        n = default,
        names = if default.isDefined then Seq.empty else value.names.names.map(Name.toData),
        plural = value.plural.map(n => PluralData(n.names.map(Name.toData)))
      ))
