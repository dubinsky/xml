package org.podval.store

import org.podval.metadata.HasValues

trait Selectors extends HasValues.FindByName[Selector]

object Selectors:
  def apply(selectors: Selector*): Selectors = new Selectors:
    override val valuesSeq: Seq[Selector] = selectors.toIndexedSeq
