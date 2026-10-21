package org.podval.store

import org.podval.metadata.{HasValues, Term}
import org.podval.xml.XmlParser

/** Per-consumer axis catalog. This library has no `Terms.xml`.
  * `object Selectors extends Selectors` loads `Terms.xml` / `<Terms>` next
  * to that class (not `Selectors.xml`). Override `valuesSeq` to skip the file
  * (`Selectors(Selector("book"), …)` in tests).
  * Put `given Selectors = this` on that object; nested packages
  * `export org.example.Selectors.given` (`export given` from a package is illegal). */
trait Selectors extends HasValues.FindByName[Selector]:
  override lazy val valuesSeq: Seq[Selector] =
    XmlParser.loadCatalog(this, "Terms", Term.codec).map: term =>
      require(term.selector, s"Not a selector: ${term.id}")
      term.requireSelectorEnglish()
      Selector(term.id, term.names)

  final def getById(id: String): Selector =
    val found: Option[Selector] = valuesSeq.find(_.id == id)
    require(found.isDefined, s"Unknown term: $id")
    found.get

object Selectors:
  def apply(selectors: Selector*): Selectors = new Selectors:
    override lazy val valuesSeq: Seq[Selector] = selectors.toIndexedSeq
