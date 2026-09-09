package org.podval.store

import org.podval.metadata.Language

final case class Path(stores: Seq[Store]) derives CanEqual:
  def isEmpty: Boolean = stores.isEmpty
  def last: Store = stores.last
  def lastAs[T]: T = stores.last.asInstanceOf[T]
  def :+(store: Store): Path = Path(stores :+ store)
  def tail: Path = Path(stores.tail)
  def init: Path = Path(stores.init)
  def parent: Path = init
  def structureNames: Seq[String] = stores.map(_.names.doFind(Language.English.toSpec).name)
  def toUrl: String = if stores.isEmpty then "/" else "/" + structureNames.mkString("/")
  override def toString: String = toUrl

object Path:
  val empty: Path = Path(Seq.empty)
