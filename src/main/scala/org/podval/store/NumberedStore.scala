package org.podval.store

import org.podval.metadata.{Names, Numbered}

trait NumberedStore extends Store, Numbered[NumberedStore]:
  def oneOf: NumberedStores[NumberedStore]

  final override def names: Names = oneOf.number2names(number)

  override def equals(other: Any): Boolean =
    other.isInstanceOf[NumberedStore] && super.equals(other) &&
      (oneOf eq other.asInstanceOf[NumberedStore].oneOf)

  override def hashCode: Int = 31 * super.hashCode + System.identityHashCode(oneOf)

  override def compare(that: Numbered[NumberedStore]): Int = that match
    case other: NumberedStore =>
      if oneOf eq other.oneOf
      then this.number - other.number
      else Integer.compare(oneOf.parentId, other.oneOf.parentId)
    case _ => super.compare(that)
      