package org.podval.metadata

final case class TermCatalog(terms: Seq[Term]):
  locally:
    val duplicates: Set[String] =
      terms.groupBy(_.id).filter((_, grouped) => grouped.length > 1).keySet
    require(duplicates.isEmpty, s"Duplicate term ids: ${duplicates.toSeq.sorted.mkString(", ")}")

  terms.foreach(_.requireSelectorEnglish())

  locally:
    val pairs: Seq[(String, String)] = terms.filter(_.selector).flatMap: term =>
      term.names.names.map(name => name.name -> term.id)
    val clashes: Seq[String] = pairs.groupBy(_._1).toSeq.flatMap: (n, hits) =>
      val ids: Seq[String] = hits.map(_._2).distinct
      if ids.length > 1 then Some(s"Duplicate selector name '$n': ${ids.mkString(", ")}")
      else None
    require(clashes.isEmpty, clashes.mkString("; "))

  private val byId: Map[String, Term] = terms.map(term => term.id -> term).toMap

  def merge(extra: Seq[Term]): TermCatalog = TermCatalog(terms ++ extra)

  def getById(id: String): Term =
    val found: Option[Term] = byId.get(id)
    require(found.isDefined, s"Unknown term: $id")
    found.get

  /** Axes only. The id matches only when it is also an `n`. */
  def forName(name: String): Option[Term] =
    terms.find(term => term.selector && term.names.hasName(name))

  def word(id: String)(using Language.Spec): String = getById(id).toLanguageString

  def heading(id: String)(using spec: Language.Spec): String =
    spec.language.get.sentenceCase(word(id))

  def pluralWord(id: String)(using spec: Language.Spec): String =
    val term: Term = getById(id)
    term.plural(spec).getOrElse:
      throw IllegalArgumentException(s"No plural for term ${term.id} ($spec)")

  def pluralHeading(id: String)(using spec: Language.Spec): String =
    spec.language.get.sentenceCase(pluralWord(id))

  def cardinal(id: String, n: Int)(using spec: Language.Spec): String =
    getById(id).cardinal(n, spec)
