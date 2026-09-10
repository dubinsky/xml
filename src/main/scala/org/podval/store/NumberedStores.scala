package org.podval.store

import org.podval.metadata.{HasValues, Language, Name, Names}

import java.util.concurrent.atomic.AtomicInteger

trait NumberedStores[+T <: NumberedStore] extends Stores[T]:
  private[store] val parentId: Int = NumberedStores.nextParentId.getAndIncrement()

  def minNumber: Int = 1
  def length: Int
  def maxNumber: Int = minNumber + length - 1

  final def contains(number: Int): Boolean = minNumber <= number && number <= maxNumber

  /** Digit parse for `findByName`; extra names live in `number2names`. */
  def name2number(name: String): Option[Int] = NumberedStores.parseNumber(name)

  /** Names of child `number` (`toUrl` uses English). Digit names keep URLs numeric. */
  def number2names(number: Int): Names = NumberedStores.namesForNumber(number)

  final override lazy val stores: Seq[T] = minNumber.to(maxNumber).map(createNumberedStore)

  protected def createNumberedStore(number: Int): T

  final def get(number: Int): T =
    require(contains(number), s"Unknown number $number in $this")
    stores(number - minNumber)

  final override def findByName(name: String): Option[T] =
    name2number(name).filter(contains).map(get).orElse(HasValues.find(stores, name))

  final override def indexOf(store: Store): Int = store match
    case numbered: NumberedStore if (numbered.oneOf eq this) && contains(numbered.number) =>
      numbered.number - minNumber
    case _ => -1

object NumberedStores:
  private val nextParentId: AtomicInteger = AtomicInteger()

  def parseNumber(name: String): Option[Int] =
    name.toIntOption.orElse(Language.Hebrew.numberFromString(name))

  def namesForNumber(number: Int): Names = new Names(Seq(
    Name(number.toString, Language.Spec.empty),
    Name(Language.Hebrew.numberToString(number), Language.Hebrew.toSpec)
  ))
