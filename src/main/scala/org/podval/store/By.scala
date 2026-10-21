package org.podval.store

import org.podval.metadata.{Names, Term, TermCatalog}

trait By[+T <: Store] extends Stores[T]:
  def selector: Selector

  final override def names: Names = selector.names

object By:
  def apply[T <: Store](selector: Selector, stores: Seq[T]): By[T] =
    val sel: Selector = selector
    new Stores.With[T](stores) with By[T]:
      override def selector: Selector = sel

  /** Looks up `id` in the catalog in scope. Unknown id throws. */
  def apply[T <: Store](id: String, stores: Seq[T])(using Selectors): By[T] =
    apply(summon[Selectors].getById(id), stores)

  abstract class Numbered[+T <: NumberedStore](
    override val selector: Selector,
    name2number: String => Option[Int] = NumberedStores.parseNumber,
    number2names: Int => Names = NumberedStores.namesForNumber
  ) extends By[T], NumberedStores[T]:
    private val parseName: String => Option[Int] = name2number
    private val namesOf: Int => Names = number2names
    override def name2number(name: String): Option[Int] = parseName(name)
    override def number2names(number: Int): Names = namesOf(number)

  object Numbered:
    def apply[T <: NumberedStore](
      selector: Selector,
      min: Int,
      max: Int,
      name2number: String => Option[Int] = NumberedStores.parseNumber,
      number2names: Int => Names = NumberedStores.namesForNumber
    )(create: (Int, NumberedStores[T]) => T): Numbered[T] =
      new Numbered[T](selector, name2number, number2names):
        override def minNumber: Int = min
        override def length: Int = max - min + 1
        override protected def createNumberedStore(number: Int): T = create(number, this)

    /** Looks up `id` in the catalog in scope. Unknown id throws. */
    def apply[T <: NumberedStore](
      id: String,
      min: Int,
      max: Int
    )(create: (Int, NumberedStores[T]) => T)(using Selectors): Numbered[T] =
      apply(summon[Selectors].getById(id), min, max)(create)

    /** Looks up `id` in the catalog in scope. Unknown id throws. */
    def apply[T <: NumberedStore](
      id: String,
      min: Int,
      max: Int,
      name2number: String => Option[Int],
      number2names: Int => Names
    )(create: (Int, NumberedStores[T]) => T)(using Selectors): Numbered[T] =
      apply(summon[Selectors].getById(id), min, max, name2number, number2names)(create)

extension (catalog: TermCatalog)
  def selector(id: String): Selector =
    val term: Term = catalog.getById(id)
    require(term.selector, s"Not a selector: $id")
    Selector(term.id, term.names)
