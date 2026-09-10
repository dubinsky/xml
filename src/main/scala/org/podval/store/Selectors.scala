package org.podval.store

import org.podval.metadata.HasValues

/** Per-consumer axis catalog. This library has no `Selector.xml`.
  * Load with `XmlParser.loadCatalog(this, "Selector", Selector.codec)` from
  * `object Selectors` (file `Selector.xml`, not `Selectors.xml`).
  * Put `given Selectors = this` on that object; nested packages
  * `export org.example.Selectors.given` (`export given` from a package is illegal). */
trait Selectors extends HasValues.FindByName[Selector]

object Selectors:
  def apply(selectors: Selector*): Selectors = new Selectors:
    override val valuesSeq: Seq[Selector] = selectors.toIndexedSeq
