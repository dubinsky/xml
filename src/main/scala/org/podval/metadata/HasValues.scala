package org.podval.metadata

trait HasValues[+T]:
  def valuesSeq: Seq[T]

  final def numberOfValues: Int = valuesSeq.length

object HasValues:

  trait Distance[T <: AnyRef] extends HasValues[T]:
    final def indexOf(value: T): Int = valuesSeq.indexWhere(value eq _, 0)

    final def distance(from: T, to: T): Int = indexOf(to) - indexOf(from)

  trait FindByDefaultName[+T <: HasName] extends HasValues[T]:
    final def getForDefaultName(name: String): T = get(name, forDefaultName(name), this)
    final def forDefaultName(name: String): Option[T] = valuesSeq.find(_.name == name)

  trait FindByName[+T <: HasNames] extends HasValues[T]:
    final def getForName(name: String): T = get(name, forName(name), this)
    final def forName(name: String): Option[T] = find(valuesSeq, name)

  private def get[T](name: String, result: Option[T], where: AnyRef): T =
    require(result.isDefined, s"Unknown $where: $name")
    result.get

  def find[T <: HasNames](valuesSeq: Seq[T], name: String): Option[T] = valuesSeq.find(_.names.hasName(name))
