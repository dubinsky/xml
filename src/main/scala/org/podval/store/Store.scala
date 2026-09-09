package org.podval.store

import org.podval.metadata.HasNames

trait Store extends HasNames:
  /** Paths of matching descendants, not including this node.
    * `Path.toUrl` of a result resolves from this node. */
  final def getPaths(
    include: Store => Boolean,
    stop: Store => Boolean
  ): Seq[Path] =
    descendants(Path.empty, include, stop)

  private def descendants(
    prefix: Path,
    include: Store => Boolean,
    stop: Store => Boolean
  ): Seq[Path] = this match
    case stores: Stores[?] if !stop(this) =>
      stores.stores.flatMap(_.collectPaths(prefix, include, stop))
    case _ => Seq.empty

  private def collectPaths(
    prefix: Path,
    include: Store => Boolean,
    stop: Store => Boolean
  ): Seq[Path] = this match
    case _: Alias => Seq.empty
    case _ =>
      val selfPath: Path = prefix :+ this
      val self: Seq[Path] = if include(this) then Seq(selfPath) else Seq.empty
      self ++ descendants(selfPath, include, stop)
