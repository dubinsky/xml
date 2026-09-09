package org.podval.store

import org.podval.metadata.Language

import java.net.URLEncoder
import java.nio.charset.StandardCharsets
import scala.reflect.TypeTest

final case class Path(stores: Seq[Store]) derives CanEqual:
  def isEmpty: Boolean = stores.isEmpty
  def last: Store = stores.last
  def lastAs[T <: Store](using TypeTest[Store, T]): T =
    lastOption[T].getOrElse:
      throw ClassCastException(s"${last.getClass.getName} is not the requested store type")
  def lastOption[T <: Store](using TypeTest[Store, T]): Option[T] =
    stores.lastOption.flatMap(summon[TypeTest[Store, T]].unapply)
  def :+(store: Store): Path = Path(stores :+ store)
  def tail: Path = Path(stores.tail)
  def init: Path = Path(stores.init)
  def parent: Store = init.last
  def structureNames: Seq[String] = stores.map(_.names.doFind(Language.English.toSpec).name)
  /** English structure names, selector hops included. `Stores.resolve` already skips a unique
    * `By` hop; omitting those hops here would break callers that treat `toUrl` as the public
    * path (alter-rebbe file URLs). Flag or per-site if ever done. */
  def toUrl: String =
    if stores.isEmpty then "/" else "/" + structureNames.map(Path.encodeSegment).mkString("/")
  override def toString: String = toUrl

object Path:
  val empty: Path = Path(Seq.empty)

  def encodeSegment(segment: String): String =
    URLEncoder.encode(segment, StandardCharsets.UTF_8).replace("+", "%20")
