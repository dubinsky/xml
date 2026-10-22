package org.podval.metadata

import zio.blocks.schema.Schema
import zio.blocks.typeid.TypeId

import java.util.Locale

// Enum cases reject a template body, so the identity override is a mixin.
private trait IdentitySentenceCase:
  self: Language =>
  override def sentenceCase(text: String): String = text

enum Language(code: String) extends
  HasNames.ByLoader[Language](loader = Language, nameOverride = Some(code)),
  HasName.Enum derives CanEqual:

  final def toSpec: Language.Spec = Language.Spec(language = Some(this), isTransliterated = None, flavour = None)

  def numberToString(number: Int): String = number.toString

  def sentenceCase(text: String): String =
    if text.isEmpty then text
    else
      val first: Int = text.offsetByCodePoints(0, 1)
      text.substring(0, first).toUpperCase(Locale.ROOT) + text.substring(first)

  def pluralCategory(n: Int): PluralCategory =
    require(n >= 0, s"n must be >= 0: $n")
    this match
      case French =>
        if n == 0 || n == 1 then PluralCategory.One else PluralCategory.Other
      case Russian =>
        val mod10: Int = n % 10
        val mod100: Int = n % 100
        if mod10 == 1 && mod100 != 11 then PluralCategory.One
        else if mod10 >= 2 && mod10 <= 4 && (mod100 < 12 || mod100 > 14) then PluralCategory.Few
        else PluralCategory.Many
      case Polish =>
        val mod10: Int = n % 10
        val mod100: Int = n % 100
        if n == 1 then PluralCategory.One
        else if mod10 >= 2 && mod10 <= 4 && (mod100 < 12 || mod100 > 14) then PluralCategory.Few
        else PluralCategory.Many
      case Lithuanian =>
        val mod10: Int = n % 10
        val mod100: Int = n % 100
        val teen: Boolean = mod100 >= 11 && mod100 <= 19
        if mod10 == 1 && !teen then PluralCategory.One
        else if mod10 >= 2 && mod10 <= 9 && !teen then PluralCategory.Few
        else PluralCategory.Many
      case Hebrew =>
        if n == 1 then PluralCategory.One
        else if n == 2 then PluralCategory.Two
        else PluralCategory.Other
      case English | German | Swedish | Latin | Yiddish =>
        if n == 1 then PluralCategory.One else PluralCategory.Other

  case English    extends Language("en")
  case Russian    extends Language("ru")
  case Polish     extends Language("pl")
  case French     extends Language("fr")
  case German     extends Language("de")
  case Lithuanian extends Language("lt")
  case Hebrew     extends Language("he") with Language.Hebrew with IdentitySentenceCase
  case Latin      extends Language("la")
  case Swedish    extends Language("sv")
  case Yiddish    extends Language("yi") with IdentitySentenceCase

object Language extends Names.Loader[Language], HasValues.FindByDefaultName[Language], HasValues.FindByName[Language]:
  override val valuesSeq: Seq[Language] = values.toIndexedSeq

  given schema: Schema[Language] =
    Schema[String].transform(getForDefaultName, _.name)(using TypeId.of[Language])

  trait ToString:
    final override def toString: String = toLanguageString(using Language.Spec.empty)
    def toLanguageString(using spec: Language.Spec): String

  final case class Spec(
    language: Option[Language],
    isTransliterated: Option[Boolean],
    flavour: Option[String]
  ) derives CanEqual:
    def isEmpty: Boolean = language.isEmpty && isTransliterated.isEmpty && flavour.isEmpty

    def toString(number: Int): String = language.fold(number.toString)(_.numberToString(number))

    def dropFlavour: Spec = copy(flavour = None)

    def dropIsTransliterated: Spec = copy(isTransliterated = None)

    def dropLanguage: Spec = copy(language = None)

  object Spec:
    val empty: Spec = Spec(None, None, None)
    given schema: Schema[Spec] = Schema.derived

  trait Hebrew:
    self: Language =>

    private val units: List[Char] = "אבגדהוזחט".toList
    private val decades: List[Char] = "יכלמנסעפצ".toList
    private val hundreds: List[Char] = "קרש".toList

    override def numberToString(number: Int): String =
      // to display 0 as empty string :)      require(number > 0)
      require(0 <= number && number < 10000)

      val result: scala.collection.mutable.StringBuilder = new scala.collection.mutable.StringBuilder
      var remainder: Int = number

      def step(condition: Int => Boolean, append: Int => String, modify: Int => Int): Unit =
        if condition(remainder) then
          result.append(append(remainder))
          remainder = modify(remainder)

      def digit(num: Int, digits: List[Char]): Unit =
        step(_ >= num, n => digits((n / num) - 1).toString, _ % num)

      val addThousandsSeparator: Boolean = remainder >= 1000
      digit(1000, units)
      if addThousandsSeparator then result.append("׳")
      while remainder >= 400 do
        result.append("ת")
        remainder = remainder - 400
      digit(100, hundreds)

      if remainder == 15 then result.append("טו") else
      if remainder == 16 then result.append("טז") else
        digit(10, decades)
        digit(1, units)

      result.toString

    def numberFromString(string: String): Option[Int] =
      var failed: Boolean = false
      var result: Int = 0
      var remainder: String = string

      def addAndDrop(add: Int, drop: Int): Unit =
        result = result + add
        remainder = remainder.drop(drop)

      def digit(digits: List[Char], multiplier: Int): Unit = if remainder.nonEmpty then
        val n: Int = digits.indexOf(remainder.head)
        if n >= 0 then addAndDrop(multiplier*(n+1), 1)

      if remainder.drop(1).headOption.contains('׳') then
        val n: Int = units.indexOf(remainder.head)
        if n < 0 then failed = true
        addAndDrop(1000*(n+1), 2)

      if remainder.startsWith("תת") then addAndDrop(800, 2)
      else if remainder.startsWith("ת") then addAndDrop(400, 1)

      digit(hundreds, 100)

      if remainder == "טו" then addAndDrop(15, 2)
      else if remainder == "טז" then addAndDrop(16, 2)
      else
        digit(decades, 10)
        digit(units, 1)

      if remainder.nonEmpty then failed = true

      if failed then None else Some(result)


