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
  Successful `resolve` returns a non-empty `Path`. `attempt` is the same
  without throwing; `resolveOption` is `attempt.toOption`.
  `Path.toUrl` is the English-name URL (selector hops included); resolving it
  again yields a path with the same `structureNames`. A hop may be omitted
  when the name uniquely matches a child of one `By` at this node. Aliases
  resolve from this node (the resolve root).
 */
  final def resolve(path: String): Path = attempt(path).fold(err => throw err, identity)

  final def resolve(path: Seq[String]): Path = attempt(path).fold(err => throw err, identity)

  final def resolveOption(path: String): Option[Path] = attempt(path).toOption

  final def resolveOption(path: Seq[String]): Option[Path] = attempt(path).toOption

  final def attempt(path: String): Either[ResolveError, Path] = attempt(Stores.splitAndDecodeUrl(path))

  final def attempt(path: Seq[String]): Either[ResolveError, Path] =
    if path.isEmpty then Right(Path(Seq(this)))
    else walk(path, Vector.empty, this, Set.empty).map(Path(_))

  private def walk(
    path: Seq[String],
    acc: Vector[Store],
    root: Stores[?],
    expanding: Set[Alias]
  ): Either[ResolveError, Vector[Store]] = path match
    case Seq() => Right(acc)
    case head +: tail =>
      findByName(head) match
        case Some(next) => continue(next, tail, acc, root, expanding)
        case None =>
          val hits: Seq[(By[?], Store)] = throughBy(head)
          hits match
            case Seq() => Left(ResolveError.NotFound(head, this))
            case Seq((by, child)) => continue(child, tail, acc :+ by, root, expanding)
            case many => Left(ResolveError.Ambiguous(head, this, many.map(_._1)))

  private def throughBy(name: String): Seq[(By[?], Store)] =
    stores.collect { case by: By[?] => by }.flatMap(by => by.findByName(name).map(child => (by, child)))

  private def continue(
    next: Store,
    tail: Seq[String],
    acc: Vector[Store],
    root: Stores[?],
    expanding: Set[Alias]
  ): Either[ResolveError, Vector[Store]] = next match
    case alias: Alias =>
      if expanding.contains(alias) then Left(ResolveError.Cycle(alias))
      else
        root.walk(alias.to, Vector.empty, root, expanding + alias).flatMap: toPath =>
          toPath.last match
            case branch: Stores[?] if tail.nonEmpty =>
              branch.walk(tail, acc ++ toPath, root, expanding + alias)
            case leaf =>
              if tail.isEmpty then Right(acc ++ toPath) else Left(ResolveError.Leftover(tail, leaf))
    case branch: Stores[?] => branch.walk(tail, acc :+ branch, root, expanding)
    case leaf =>
      if tail.isEmpty then Right(acc :+ leaf) else Left(ResolveError.Leftover(tail, leaf))

object Stores:
  trait With[+T <: Store](override val stores: Seq[T]) extends Stores[T]

  private def splitUrl(urlRaw: String): Seq[String] =
    val url: String = if urlRaw.isEmpty then "/" else urlRaw
    url.stripPrefix("/").split("/").toIndexedSeq.filterNot(_.isBlank)

  private[store] def splitAndDecodeUrl(url: String): Seq[String] =
    splitUrl(url).map(segment => URLDecoder.decode(segment, StandardCharsets.UTF_8))
