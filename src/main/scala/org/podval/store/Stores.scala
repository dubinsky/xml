package org.podval.store

import org.podval.metadata.HasValues

import java.net.URLDecoder
import java.nio.charset.StandardCharsets

trait Stores[+T <: Store] extends Store:
  // TODO maybe pre-calculate a lazy map from all names to stores?
  def stores: Seq[T]

  def findByName(name: String): Option[T] = HasValues.find(stores, name)

  def indexOf(store: Store): Int = stores.indexWhere(_ eq store)

  final def next(store: Store): Option[T] =
    val i: Int = indexOf(store)
    Option.when(i >= 0 && i + 1 < stores.length)(stores(i + 1))

  final def prev(store: Store): Option[T] =
    val i: Int = indexOf(store)
    Option.when(i > 0)(stores(i - 1))

  final def distance(from: Store, to: Store): Int =
    val fromI: Int = indexOf(from)
    val toI: Int = indexOf(to)
    require(fromI >= 0, s"Did not find $from in $this")
    require(toI >= 0, s"Did not find $to in $this")
    toI - fromI

  /*
  Successful `resolve()` returns a non-empty `Path`.
  `Path.toUrl` is the English-name URL (selector hops included); resolving it
  again yields a path with the same `structureNames`. A hop may be omitted
  when the name uniquely matches a child of one `By` at this node. Aliases
  resolve from this node (the resolve root).
 */
  final def resolve(path: String): Path = resolve(Stores.splitAndDecodeUrl(path))

  final def resolve(path: Seq[String]): Path =
    Path(if path.isEmpty then Seq(this) else resolve(path, Vector.empty, this, Set.empty))

  private def resolve(
    path: Seq[String],
    acc: Vector[Store],
    root: Stores[?],
    expanding: Set[Alias]
  ): Vector[Store] = path match
    case Seq() => acc
    case head +: tail =>
      findByName(head) match
        case Some(next) => continue(next, tail, acc, root, expanding)
        case None =>
          val hits: Seq[(By[?], Store)] = throughBy(head)
          require(
            hits.length <= 1,
            s"Ambiguous '$head' in $this: ${hits.map((by, _) => by.names.name).mkString(", ")}"
          )
          require(hits.nonEmpty, s"Did not find '$head' in $this")
          val (by, child) = hits.head
          continue(child, tail, acc :+ by, root, expanding)

  private def throughBy(name: String): Seq[(By[?], Store)] =
    stores.collect { case by: By[?] => by }.flatMap(by => by.findByName(name).map(child => (by, child)))

  private def continue(
    next: Store,
    tail: Seq[String],
    acc: Vector[Store],
    root: Stores[?],
    expanding: Set[Alias]
  ): Vector[Store] = next match
    case alias: Alias =>
      require(!expanding.contains(alias), s"Alias cycle: ${alias.names}")
      val toPath: Vector[Store] = root.resolve(alias.to, Vector.empty, root, expanding + alias)
      require(toPath.nonEmpty, s"Alias ${alias.names} resolved empty")
      toPath.last match
        case branch: Stores[?] if tail.nonEmpty =>
          branch.resolve(tail, acc ++ toPath, root, expanding + alias)
        case leaf =>
          require(tail.isEmpty, s"Can not apply '$tail' to $leaf")
          acc ++ toPath
    case branch: Stores[?] => branch.resolve(tail, acc :+ branch, root, expanding)
    case leaf =>
      require(tail.isEmpty, s"Can not apply '$tail' to $leaf")
      acc :+ leaf

object Stores:
  trait With[+T <: Store](override val stores: Seq[T]) extends Stores[T]

  private def splitUrl(urlRaw: String): Seq[String] =
    val url: String = if urlRaw.isEmpty then "/" else urlRaw
    url.stripPrefix("/").split("/").toIndexedSeq.filterNot(_.isBlank)

  private[store] def splitAndDecodeUrl(url: String): Seq[String] =
    splitUrl(url).map(segment => URLDecoder.decode(segment, StandardCharsets.UTF_8))
