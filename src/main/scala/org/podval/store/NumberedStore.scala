package org.podval.store

import org.podval.metadata.{Names, Numbered}

trait NumberedStore extends Store, Numbered[NumberedStore]:
  def oneOf: NumberedStores[NumberedStore]

  final override def names: Names = oneOf.number2names(number)

  override def equals(other: Any): Boolean =
    other.isInstanceOf[NumberedStore] && super.equals(other) &&
      (oneOf eq other.asInstanceOf[NumberedStore].oneOf)

  override def hashCode: Int = 31 * super.hashCode + System.identityHashCode(oneOf)

  override def compare(that: Numbered[NumberedStore]): Int =
    that match
      case other: NumberedStore =>
        val byParent: Int = Integer.compare(
          System.identityHashCode(oneOf),
          System.identityHashCode(other.oneOf)
        )
        if byParent != 0 then byParent else this.number - other.number
      case _ => super.compare(that)