package org.podval.store

import org.podval.metadata.{HasNames, Names}

final case class Selector(
  id: String,
  override val names: Names
) extends HasNames derives CanEqual

object Selector:
  def apply(id: String): Selector = new Selector(id, Names(id))
