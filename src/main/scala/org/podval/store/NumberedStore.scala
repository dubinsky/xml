package org.podval.store

import org.podval.metadata.Names

trait NumberedStore extends Store, org.podval.metadata.Numbered[NumberedStore]:
  def oneOf: NumberedStores[NumberedStore]

  final override def names: Names = oneOf.number2names(number)

  override def equals(other: Any): Boolean =
    other.isInstanceOf[NumberedStore] && super.equals(other) &&
      (oneOf eq other.asInstanceOf[NumberedStore].oneOf)

  override def hashCode: Int = 31 * super.hashCode + System.identityHashCode(oneOf)