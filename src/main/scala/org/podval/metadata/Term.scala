package org.podval.metadata

import org.podval.xml.{Xml, XmlCodec, XmlError}
import zio.blocks.schema.Schema

final case class Term(
  id: String,
  override val names: Names,
  /** Axis flag. Not `org.podval.store.Selector`. Lookup is `import org.podval.store.selector` (the extension in `By.scala`). */
  selector: Boolean = false
) extends HasNames derives CanEqual:
  def plural(spec: Language.Spec): Option[String] = names.doFind(spec).plural

  def cardinal(n: Int, spec: Language.Spec): String =
    val category: PluralCategory = spec.language.getOrElse(
      throw IllegalArgumentException(s"No language for term $id ($spec)")
    ).pluralCategory(n)
    names.doFind(spec).form(category).getOrElse:
      throw IllegalArgumentException(s"Missing $category for term $id ($spec)")

  def requireSelectorEnglish(): Unit =
    if selector then
      val english: Name = names.find(Language.English.toSpec).getOrElse:
        throw IllegalArgumentException(s"Selector $id has no English name")
      require(english.name == id, s"Selector $id: first English name is '${english.name}'")

object Term:
  private final case class Data(
    id: String,
    selector: Option[Boolean] = None,
    names: Seq[Name] = Seq.empty
  ) derives CanEqual

  private object Data:
    given schema: Schema[Data] = Schema.derived
    val codec: XmlCodec[Data] = XmlCodec.derived

  val codec: XmlCodec[Term] = new XmlCodec[Term]:
    override def elementName: String = "term"
    override def isRecordLike: Boolean = true

    override def unsafeDecode(element: Xml.Element): Term =
      val data: Data = Data.codec.unsafeDecode(element)
      val id: String = data.id.trim
      if id.isEmpty then throw XmlError("Blank 'id'")
      if data.names.isEmpty then throw XmlError("No names")
      checkGrammatical(id, data.names)
      val term: Term = Term(id, Names(data.names), data.selector.getOrElse(false))
      term.requireSelectorEnglish()
      term

    override def encodeNamed(elName: String, value: Term): Xml.Element =
      Data.codec.encodeNamed(elName, Data(
        id = value.id,
        selector = if value.selector then Some(true) else None,
        names = value.names.names
      ))

  private def checkGrammatical(id: String, names: Seq[Name]): Unit =
    val _: Set[Language.Spec] = names.foldLeft(Set.empty[Language.Spec]): (seen, name) =>
      if !seen.contains(name.languageSpec) then seen + name.languageSpec
      else
        val present: Seq[String] = Seq(
          Option.when(name.plural.isDefined)("plural"),
          Option.when(name.few.isDefined)("few"),
          Option.when(name.many.isDefined)("many"),
          Option.when(name.two.isDefined)("two")
        ).flatten
        if present.nonEmpty then throw XmlError(
          s"Term $id: ${present.mkString(", ")} is not on the first name of ${name.languageSpec}"
        )
        seen
