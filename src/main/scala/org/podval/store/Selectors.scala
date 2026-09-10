package org.podval.store

import org.podval.metadata.HasValues
import org.podval.xml.XmlParser

/** Per-consumer axis catalog. This library has no `Selector.xml`.
  * `object Selectors extends Selectors` loads `Selector.xml` / `<Selector>` next
  * to that class (not `Selectors.xml`). Override `valuesSeq` to skip the file
  * (`Selectors(Selector(Names("book")), …)` in tests).
  * Put `given Selectors = this` on that object; nested packages
  * `export org.example.Selectors.given` (`export given` from a package is illegal). */
trait Selectors extends HasValues.FindByName[Selector]:
  override lazy val valuesSeq: Seq[Selector] =
    XmlParser.loadCatalog(this, "Selector", Selector.codec)

object Selectors:
  def apply(selectors: Selector*): Selectors = new Selectors:
    override lazy val valuesSeq: Seq[Selector] = selectors.toIndexedSeq
