package org.podval.metadata

object Numbered:
  trait Companion[T]:
    def apply(number: Int): T

trait Numbered[T] extends Ordered[Numbered[T]]:
  def number: Int

  def companion: Numbered.Companion[T]

  @scala.annotation.targetName("add")
  final def +(change: Int): T = companion(number + change)

  @scala.annotation.targetName("subtract")
  final def -(change: Int): T = companion(number - change)

  final def next: T = this + 1

  final def prev: T = this - 1

  final def -(that: Numbered[T]): Int = this.number - that.number

  override def compare(that: Numbered[T]): Int = this.number - that.number

  override def equals(other: Any): Boolean =
    other.isInstanceOf[Numbered[?]] && {
      val that: Numbered[?] = other.asInstanceOf[Numbered[?]]
      (numberedClass eq that.numberedClass) && this.number == that.number
    }

  override def hashCode: Int = 31 * numberedClass.hashCode + number

  override def toString: String = number.toString

  protected def numberedClass: Class[?] =
    Iterator.iterate[Class[?]](getClass)(_.getSuperclass)
      .takeWhile(_ != null)
      .find(c => !c.isAnonymousClass && !c.isSynthetic)
      .getOrElse(getClass)
