package org.podval.metadata

trait HasNames:
  def names: Names

  def merge(that: HasNames): HasNames =
    require(this eq that)
    this

  final def toLanguageString(using spec: Language.Spec): String = names.toLanguageString(using spec)

  final def andNumber(number: Int): HasNames =
    new HasNames.Numbers(this, number, number):
      override protected def suffix(languageSpec: Language.Spec): String =
        languageSpec.toString(number)

  final def andNumbers(from: Int, to: Int): HasNames = if to == from then andNumber(from) else
    new HasNames.Numbers(this, from, to):
      override protected def suffix(languageSpec: Language.Spec): String =
        languageSpec.toString(this.from) + "-" + languageSpec.toString(this.to)

object HasNames:

  // Note: Calendar.Month is used as a Names.Loader, but this gets called during its initialization,
  // so loader parameter end up being null... Introduced thunk:
  abstract class ByLoader[Key <: ByLoader[Key]](loader: => Names.Loader[Key], nameOverride: Option[String])
    extends HasNames, HasName(nameOverride):
    final override def names: Names = loader.toNames(this.asInstanceOf[Key])

  private sealed abstract class Numbers(
    val named: HasNames,
    val from: Int,
    val to: Int
  ) extends HasNames:
    require(from > 0)
    require(to >= from)

    final override def names: Names = new Names(for name <- named.names.names yield Name(
      name = name.name + " " + suffix(name.languageSpec),
      languageSpec = name.languageSpec
    ))

    protected def suffix(languageSpec: Language.Spec): String

    override def merge(other: HasNames): HasNames =
      require(other.isInstanceOf[Numbers])
      val that: Numbers = other.asInstanceOf[Numbers]
      require(this.named eq that.named)
      require(this.to+1 == that.from)
      this.named.andNumbers(this.from, that.to)
