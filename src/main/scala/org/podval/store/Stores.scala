package org.podval.store

import org.podval.metadata.HasValues

import java.net.URLDecoder
import java.nio.charset.StandardCharsets

trait Stores[+T <: Store] extends Store:
  // TODO maybe pre-calculate a lazy map from all names to stores?
  def stores: Seq[T]

  def findByName(name: String): Option[T] = HasValues.find(stores, name)

  // TODO add indexOf() and friends

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
