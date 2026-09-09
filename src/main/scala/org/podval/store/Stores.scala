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
  `Path.toUrl` is the English-name URL; resolving it again yields a path
  with the same `structureNames`.
 */
  final def resolve(path: String): Path = resolve(Stores.splitAndDecodeUrl(path))

  // TODO does this work with an alias "/" - and should such alias be legal?
  final def resolve(path: Seq[String]): Path =
    Path(if path.nonEmpty then this.resolve(path, Seq.empty) else Seq(this))

  private def resolve(
    path: Seq[String],
    acc: Seq[Store]
  ): Seq[Store] = path match
    case Seq() => acc.reverse
    case head +: tail =>
      val nextOpt: Option[Store] = findByName(head)
      require(nextOpt.nonEmpty, s"Did not find '$head' in $this")
      nextOpt.get match
        case alias: Alias =>
          val toPath: Path = resolve(alias.to)
          toPath.last match
            case stores: Stores[?] if tail.nonEmpty =>
              stores.resolve(tail, toPath.stores.reverse ++ acc)
            case next =>
              require(tail.isEmpty, s"Can not apply '$tail' to $next")
              (toPath.stores.reverse ++ acc).reverse
        case stores: Stores[?] => stores.resolve(tail, stores +: acc)
        case next =>
          require(tail.isEmpty, s"Can not apply '$tail' to $next")
          (next +: acc).reverse

object Stores:
  trait With[+T <: Store](override val stores: Seq[T]) extends Stores[T]

  private def splitUrl(urlRaw: String): Seq[String] =
    val url: String = if urlRaw.isEmpty then "/" else urlRaw
    // TODO? require(url.startsWith("/"))
    url.stripPrefix("/").split("/").toIndexedSeq.filterNot(_.isBlank)

  private def splitAndDecodeUrl(url: String): Seq[String] =
    splitUrl(url).map(segment => URLDecoder.decode(segment, StandardCharsets.UTF_8))
