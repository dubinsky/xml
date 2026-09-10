package org.podval.store

import org.podval.metadata.Names

trait By[+T <: Store] extends Stores[T]:
  def selector: Selector

  final override def names: Names = selector.names

object By:
  def apply[T <: Store](selectorName: String, stores: Seq[T]): By[T] =
    new WithSelector[T](selectorName) with Stores.With[T](stores)

  trait WithSelector[+T <: Store](selectorName: String) extends By[T]:
    override def selector: Selector =
      Selector.forName(selectorName).getOrElse(Selector(Names(selectorName), None))

  abstract class Numbered[+T <: NumberedStore](
    selectorName: String,
    fromName: String => Option[Int] = NumberedStores.parseNumber,
    toNames: Int => Names = NumberedStores.namesForNumber
  ) extends WithSelector[T](selectorName), NumberedStores[T]:
    override def name2number(name: String): Option[Int] = fromName(name)
    override def number2names(number: Int): Names = toNames(number)

  object Numbered:
    def apply[T <: NumberedStore](
      selectorName: String,
      min: Int,
      max: Int,
      name2number: String => Option[Int] = NumberedStores.parseNumber,
      number2names: Int => Names = NumberedStores.namesForNumber
    )(create: (Int, NumberedStores[T]) => T): Numbered[T] =
      new Numbered[T](selectorName, name2number, number2names):
        override def minNumber: Int = min
        override def length: Int = max - min + 1
        override protected def createNumberedStore(number: Int): T = create(number, this)
