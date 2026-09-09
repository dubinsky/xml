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

  object Numbered:
    def apply[T <: NumberedStore](
      selectorName: String,
      min: Int,
      max: Int
    )(create: (Int, NumberedStores[T]) => T): Numbered[T] =
      new Numbered[T](selectorName):
        override def minNumber: Int = min
        override def length: Int = max - min + 1
        override protected def createNumberedStore(number: Int): T = create(number, this)
