package org.podval.store

import org.podval.metadata.Names

trait By[+T <: Store] extends Stores[T]:
  def selector: Selector

  final override def names: Names = selector.names

object By:
  def apply[T <: Store](selectorName: String, stores: Seq[T]): By[T] =
    new WithSelector[T](selectorName) with Stores.With[T](stores)

  trait WithSelector[+T <: Store](selectorName: String) extends By[T]:
    override def selector: Selector = Selector.getForName(selectorName)

  abstract class Numbered[+T <: NumberedStore](selectorName: String) extends WithSelector[T](selectorName), NumberedStores[T]
