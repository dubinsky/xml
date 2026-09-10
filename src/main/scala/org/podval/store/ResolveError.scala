package org.podval.store

sealed abstract class ResolveError(message: String) extends IllegalArgumentException(message)

object ResolveError:
  final case class NotFound(name: String, in: Stores[?]) extends ResolveError(
    s"Did not find '$name' in $in"
  )

  final case class Ambiguous(name: String, in: Stores[?], axes: Seq[By[?]]) extends ResolveError(
    s"Ambiguous '$name' in $in: ${axes.map(_.names.name).mkString(", ")}"
  )

  final case class Leftover(tail: Seq[String], leaf: Store) extends ResolveError(
    s"Can not apply '$tail' to $leaf"
  )

  final case class Cycle(alias: Alias) extends ResolveError(
    s"Alias cycle: ${alias.names}"
  )
