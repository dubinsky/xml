package org.podval.store

import org.podval.metadata.HasNames

trait Store extends HasNames:
  final def getPaths(
    path: Path = Path.empty,
    include: Store => Boolean,
    stop: Store => Boolean
  ): Seq[Path] =
    val selfPath: Path = path :+ this
    val self: Seq[Path] = if include(this) then Seq(selfPath) else Seq.empty
    this match
      case stores: Stores[?] if !stop(this) =>
        self ++ stores.stores.flatMap(_.getPaths(selfPath, include, stop))
      case _ => self
