package org.podval.metadata

import org.podval.xml.{XmlAttribute, XmlCodec, XmlParser, Xml}
import org.scalatest.funsuite.AnyFunSuite
import zio.blocks.schema.Schema

final class MetadataSpec extends AnyFunSuite:
  test("Language names") {
    assert(Language.English.names.names.length == 4)
    assert(Language.Hebrew.names.hasName("he"))
    assert(Language.Hebrew.names.hasName("иврит"))
    assert(Language.Latin.names.names.length == 4)
    assert(Language.Latin.names.hasName("la"))
    assert(Language.Latin.names.hasName("Latin"))
    assert(Language.getForDefaultName("la") == Language.Latin)
    assert(Language.getForDefaultName("sv") == Language.Swedish)
    assert(Language.getForDefaultName("yi") == Language.Yiddish)
    assert(Language.Swedish.names.hasName("Swedish"))
    assert(Language.Swedish.names.hasName("sv"))
    assert(Language.Swedish.names.hasName("шведский"))
    assert(Language.Swedish.names.hasName("שוודית"))
    assert(Language.Swedish.names.hasName("svenska"))
    assert(Language.Yiddish.names.hasName("Yiddish"))
    assert(Language.Yiddish.names.hasName("yi"))
    assert(Language.Yiddish.names.hasName("идиш"))
    assert(Language.Yiddish.names.hasName("יידיש"))
    assert(Language.Yiddish.names.hasName("ייִדיש"))
    assert(Language.Swedish.names.find(Language.Swedish.toSpec).get.name == "svenska")
    assert(Language.Yiddish.names.find(Language.Yiddish.toSpec).get.name == "ייִדיש")
    assert(Language.French.names.find(Language.Hebrew.toSpec).get.name == "צרפתית")
    assert(Language.German.names.find(Language.Hebrew.toSpec).get.name == "גרמנית")
  }

  test("Name codec accepts n or text but not both") {
    def decode(xml: String) =
      Name.codec.decode(XmlParser.parseXml(xml).toOption.get)

    val fromN = decode("""<name lang="en" n="English"/>""").toOption.get
    assert(fromN.name == "English")
    assert(fromN.languageSpec.language.contains(Language.English))

    val fromText = decode("""<name lang="ru">русский</name>""").toOption.get
    assert(fromText.name == "русский")
    assert(fromText.languageSpec.language.contains(Language.Russian))

    assert(decode("""<name n="x">y</name>""").isLeft)
    assert(decode("""<name lang="en"/>""").isLeft)
    assert(decode("""<name lang="en" n="" transliterated="yes"/>""").toOption.get.name == "")

    val entryCodec: XmlCodec[NamedEntry] = XmlCodec.derived(using NamedEntry.schema)
    val entry = entryCodec.decode(XmlParser.parseXml(
      """<NamedEntry><name lang="en" n="English"/></NamedEntry>"""
    ).toOption.get).toOption.get
    assert(entry.names.head.name == "English")
    assert(entry.names.head.languageSpec.language.contains(Language.English))

    val encoded = Name.codec.encode(fromN)
    assert(encoded.get("n").contains("English"))
    assert(encoded.get(XmlAttribute.Lang).contains("en"))

    val encodedEntry = entryCodec.encode(entry)
    assert(encodedEntry.childrenNamed("name").flatMap(_.get("n")) == Seq("English"))
  }

  test("Hebrew.numberToString") {
    def check(number: Int, string: String): Unit =
      assert(Language.Hebrew.numberToString(number) == string)
      assert(Language.Hebrew.numberFromString(string).contains(number))

    check(  0, "")
    check(  5, "ה")
    check( 10, "י")
    check( 15, "טו")
    check( 16, "טז")
    check( 20, "כ")
    check(100, "ק")
    check(116, "קטז")
    check(119, "קיט")
    check(400, "ת")
    check(499, "תצט")
    check(555, "תקנה")
    check(800, "תת")
    check(899, "תתצט")
    check(999, "תתקצט")
    check(1400, "א׳ת")
    check(1800, "א׳תת")
    check(5400, "ה׳ת")
    check(5800, "ה׳תת")
    check(6000, "ו׳")

    intercept[IllegalArgumentException] { Language.Hebrew.numberToString(10000) }
    intercept[IllegalArgumentException] { Language.Hebrew.numberToString(-1) }

    assert(Language.Hebrew.numberFromString("הה").isEmpty)
    assert(Language.Hebrew.numberFromString("ק׳").isEmpty)
  }

  test("HasName.bind rejects duplicate and extra keys") {
    intercept[IllegalArgumentException] {
      HasName.bind(keys = Seq(1, 2), metadatas = Seq((1, "a"), (1, "b")), getKey = _._1)
    }
    intercept[IllegalArgumentException] {
      HasName.bind(keys = Seq(1), metadatas = Seq((1, "a"), (3, "c")), getKey = _._1)
    }
    intercept[IllegalArgumentException] {
      HasName.bind(keys = Seq(1, 2), metadatas = Seq((1, "a")), getKey = _._1)
    }
    assert(HasName.bind(keys = Seq(1, 2), metadatas = Seq((1, "a"), (2, "b")), getKey = _._1) ==
      Map(1 -> (1, "a"), 2 -> (2, "b")))
  }

  test("HasName.mapByName rejects two metadatas for one key") {
    intercept[IllegalArgumentException] {
      HasName.mapByName(
        keys = Language.valuesSeq,
        metadatas = Seq(Language.English.names, Language.English.names),
        hasName = (names: Names, name: String) => names.hasName(name)
      )
    }
  }

  test("Numbered equality is per class") {
    final class A(override val number: Int) extends Numbered[A] derives CanEqual:
      override def companion: Numbered.Companion[A] = A(_)
    final class B(override val number: Int) extends Numbered[B] derives CanEqual:
      override def companion: Numbered.Companion[B] = B(_)
    assert(A(1) == A(1))
    assert(A(1) != A(2))
    assert(!A(1).equals(B(1)))

    abstract class V(override val number: Int) extends Numbered[V] derives CanEqual:
      override def companion: Numbered.Companion[V] = n => new V(n) {}
    val fiveA: V = new V(5) {}
    val fiveB: V = new V(5) {}
    val six: V = new V(6) {}
    assert(fiveA == fiveB)
    assert(fiveA != six)
  }

  test("Numbered + - next prev use companion") {
    final class N(override val number: Int) extends Numbered[N] derives CanEqual:
      override def companion: Numbered.Companion[N] = N(_)
    assert((N(2) + 3).number == 5)
    assert((N(2) - 1).number == 1)
    assert(N(2).next.number == 3)
    assert(N(2).prev.number == 1)
    assert(N(5) - N(2) == 3)
  }

  test("HasNames.andNumber suffixes each language") {
    val numbered: HasNames = Language.English.andNumber(3)
    assert(numbered.names.hasName("English 3"))
    assert(numbered.names.hasName("en 3"))
  }

  test("andNumber does not copy grammatical attributes") {
    val named: HasNames = new HasNames:
      override def names: Names = Names(Seq(
        Name("archive", en, plural = Some("archives"), few = Some("archive"), many = Some("archives"), two = Some("archive"))
      ))
    val name: Name = named.andNumber(1).names.names.head
    assert(name.name == "archive 1")
    assert(name.plural.isEmpty)
    assert(name.few.isEmpty)
    assert(name.many.isEmpty)
    assert(name.two.isEmpty)
  }

  test("Name codec reads grammatical attributes and hasName ignores them") {
    val decoded: Name = decodeName(
      """<name lang="en" n="archive" plural="  archives  " few="archive" many="archives" two="two words"/>"""
    ).toOption.get
    assert(decoded.plural.contains("archives"))
    assert(decoded.few.contains("archive"))
    assert(decoded.many.contains("archives"))
    assert(decoded.two.contains("two words"))
    assert(decoded.form(PluralCategory.One).contains("archive"))
    assert(decoded.form(PluralCategory.Other).contains("archives"))
    assert(decoded.form(PluralCategory.Few).contains("archive"))
    assert(decoded.form(PluralCategory.Many).contains("archives"))
    assert(decoded.form(PluralCategory.Two).contains("two words"))
    val names: Names = Names(Seq(decoded))
    assert(names.hasName("archive"))
    assert(!names.hasName("archives"))
    assert(names.find("archives").isEmpty)
    assert(decodeName("""<name lang="en" n="archive" plural=""/>""").isLeft)
    assert(decodeName("""<name lang="en" n="archive" few="   "/>""").isLeft)
  }

  test("term codec: selector yes, ids, and where a grammatical attribute may sit") {
    val facsimile: Term = decodeTerm(
      """<term id="facsimile" selector="yes">
           <name lang="en" n="facsimile" plural="facsimiles"/>
           <name lang="en" n="facs"/>
           <name lang="ru" n="факсимиле"/>
         </term>"""
    ).toOption.get
    assert(facsimile.selector)
    assert(facsimile.names.hasName("facs"))
    assert(!facsimile.names.hasName("facsimiles"))
    assert(facsimile.names.find(en).get.plural.contains("facsimiles"))
    assert(facsimile.names.names.find(_.name == "facs").get.plural.isEmpty)

    val menu: Term = decodeTerm(
      """<term id="nav.menu"><name lang="en" n="Menu"/></term>"""
    ).toOption.get
    assert(!menu.selector)
    assert(menu.id == "nav.menu")

    assert(decodeTerm("""<term id="note" selector="no"><name lang="en" n="note"/></term>""").toOption.get.selector == false)

    val day: Term = decodeTerm(
      """<term id="  day of the week  " selector="yes"><name lang="en" n="day of the week"/></term>"""
    ).toOption.get
    assert(day.id == "day of the week")

    assertMessage(
      decodeTerm("""<term id="   " selector="yes"><name lang="en" n="x"/></term>"""),
      "Blank"
    )
    assertMessage(
      decodeTerm("""<term id="archive" selector="yes"><name lang="en" n="archive"/><plural><name lang="en" n="Archives"/></plural></term>"""),
      "plural"
    )
    assertMessage(
      decodeTerm(
        """<term id="archive" selector="yes">
             <name lang="en" n="archive" plural="archives"/>
             <name lang="en" n="records" plural="records"/>
           </term>"""
      ),
      "archive"
    )
    assertMessage(
      decodeTerm(
        """<term id="document" selector="yes">
             <name lang="ru" n="документ"/>
             <name lang="ru" n="док" few="документа"/>
           </term>"""
      ),
      "few"
    )
    val document: Term = decodeTerm(
      """<term id="document" selector="yes">
           <name lang="en" n="document" plural="documents" few="documents" many="documents"/>
           <name lang="en" n="doc"/>
           <name lang="ru" n="документ" plural="документы" few="документа" many="документов"/>
           <name lang="ru" n="документъ"/>
         </term>"""
    ).toOption.get
    assert(document.names.hasName("doc"))
    assert(!document.names.hasName("documents"))
    assert(document.names.doFind(ru).few.contains("документа"))

    assertMessage(
      decodeTerm(
        """<term id="facsimile" selector="yes">
             <name lang="en" n="facs"/>
             <name lang="en" n="facsimile"/>
           </term>"""
      ),
      "facsimile"
    )
    assert(decodeTerm("""<term selector="yes"><name lang="en" n="book"/></term>""").isLeft)
    assert(decodeTerm("""<term id="book" selector="yes"></term>""").isLeft)
  }

  test("TermCatalog merges messages, rejects duplicate axes, and does not forName a message") {
    val book: Term = axis("book", Name("book", en), Name("книга", ru))
    val volume: Term = axis("volume", Name("volume", en), Name("книга", ru))
    val clash: IllegalArgumentException = intercept[IllegalArgumentException] { TermCatalog(Seq(book, volume)) }
    assert(clash.getMessage.contains("book"))
    assert(clash.getMessage.contains("volume"))

    val dates: TermCatalog = TermCatalog(Seq(
      Term("doc.date", Names(Seq(Name("Date", en), Name("Дата", ru))))
    )).merge(Seq(
      Term("date.date", Names(Seq(Name("Date", en), Name("Дата", ru))))
    ))
    assert(dates.forName("Date").isEmpty)
    assert(dates.forName("Дата").isEmpty)
    assert(dates.getById("doc.date").names.hasName("Дата"))

    val shared: TermCatalog = TermCatalog(Seq(
      axis("case", Name("case", en), Name("дело", ru)),
      Term("search.kind.collection", Names(Seq(Name("collection", en), Name("дело", ru))))
    ))
    assert(shared.forName("дело").exists(_.id == "case"))
    assert(shared.forName("collection").isEmpty)

    val duplicate: IllegalArgumentException = intercept[IllegalArgumentException] {
      TermCatalog(Seq(book)).merge(Seq(book))
    }
    assert(duplicate.getMessage.contains("book"))

    val unknown: IllegalArgumentException = intercept[IllegalArgumentException] { TermCatalog(Seq(book)).getById("item") }
    assert(unknown.getMessage.contains("Unknown term: item"))

    intercept[IllegalArgumentException] {
      TermCatalog(Seq(Term("book", Names(Seq(Name("volume", en))), selector = true)))
    }
    assert(TermCatalog(Seq(Term("nav.menu", Names(Seq(Name("Menu", en)))))).getById("nav.menu").selector == false)
    assert(TermCatalog(Seq(Term("nav.only", Names(Seq(Name("Меню", ru)))))).getById("nav.only").id == "nav.only")
    intercept[IllegalArgumentException] {
      TermCatalog(Seq(Term("book", Names(Seq(Name("книга", ru))), selector = true)))
    }
  }

  test("heading, plural, and cardinal") {
    val archive: Term = axis(
      "archive",
      Name("archive", en, plural = Some("archives")),
      Name("архив", ru, plural = Some("архивы"))
    )
    val document: Term = axis(
      "document",
      Name("document", en, plural = Some("documents")),
      Name("документ", ru, plural = Some("документы"), few = Some("документа"), many = Some("документов")),
      Name("מסמך", he, plural = Some("מסמכים"), two = Some("שני"))
    )
    val fund: Term = axis("fund", Name("fund", en, plural = Some("funds")), Name("фонд", ru))
    val catalog: TermCatalog = TermCatalog(Seq(archive, document, fund))
    assert(catalog.word("archive")(using en) == "archive")
    assert(catalog.heading("archive")(using en) == "Archive")
    assert(catalog.pluralWord("archive")(using en) == "archives")
    assert(catalog.pluralHeading("archive")(using en) == "Archives")
    assert(catalog.word("archive")(using ru) == "архив")
    assert(catalog.pluralHeading("archive")(using ru) == "Архивы")
    assert(catalog.cardinal("document", 1)(using en) == "document")
    assert(catalog.cardinal("document", 0)(using en) == "documents")
    assert(catalog.cardinal("document", 21)(using ru) == "документ")
    assert(catalog.cardinal("document", 2)(using ru) == "документа")
    assert(catalog.cardinal("document", 5)(using ru) == "документов")
    assert(catalog.cardinal("document", 1)(using he) == "מסמך")
    assert(catalog.cardinal("document", 2)(using he) == "שני")
    assert(catalog.cardinal("document", 5)(using he) == "מסמכים")
    val missing: IllegalArgumentException = intercept[IllegalArgumentException] {
      catalog.cardinal("archive", 5)(using ru)
    }
    assert(missing.getMessage.contains("archive"))
    assert(missing.getMessage.contains("Many"))
    assert(missing.getMessage.contains(ru.toString))
    val noRussianPlural: IllegalArgumentException = intercept[IllegalArgumentException] {
      catalog.pluralWord("fund")(using ru)
    }
    assert(noRussianPlural.getMessage.contains("fund"))
  }

  test("sentenceCase") {
    assert(Language.Russian.sentenceCase("архивы") == "Архивы")
    assert(Language.English.sentenceCase("archives") == "Archives")
    assert(Language.English.sentenceCase("iPhone") == "IPhone")
    assert(Language.English.sentenceCase("") == "")
    assert(Language.Hebrew.sentenceCase("עברית") == "עברית")
    assert(Language.Hebrew.sentenceCase("ivrit") == "ivrit")
    assert(Language.Yiddish.sentenceCase("ייִדיש") == "ייִדיש")
    assert(Language.Yiddish.sentenceCase("yiddish") == "yiddish")
  }

  test("pluralCategory") {
    assertCategories(Language.Russian, Seq(
      0 -> PluralCategory.Many,
      1 -> PluralCategory.One,
      2 -> PluralCategory.Few,
      5 -> PluralCategory.Many,
      11 -> PluralCategory.Many,
      12 -> PluralCategory.Many,
      21 -> PluralCategory.One,
      22 -> PluralCategory.Few
    ))
    assertCategories(Language.Polish, Seq(
      0 -> PluralCategory.Many,
      1 -> PluralCategory.One,
      2 -> PluralCategory.Few,
      5 -> PluralCategory.Many,
      21 -> PluralCategory.Many,
      22 -> PluralCategory.Few
    ))
    assertCategories(Language.French, Seq(
      0 -> PluralCategory.One,
      1 -> PluralCategory.One,
      2 -> PluralCategory.Other
    ))
    assertCategories(Language.Hebrew, Seq(
      1 -> PluralCategory.One,
      2 -> PluralCategory.Two,
      5 -> PluralCategory.Other
    ))
    assertCategories(Language.Yiddish, Seq(
      1 -> PluralCategory.One,
      2 -> PluralCategory.Other
    ))
    assertCategories(Language.Lithuanian, Seq(
      5 -> PluralCategory.Few,
      10 -> PluralCategory.Many
    ))
    Seq(Language.English, Language.German, Language.Swedish, Language.Latin).foreach: language =>
      assert(language.pluralCategory(0) == PluralCategory.Other)
      assert(language.pluralCategory(1) == PluralCategory.One)
      assert(language.pluralCategory(2) == PluralCategory.Other)
    intercept[IllegalArgumentException] { Language.Russian.pluralCategory(-1) }
  }

  test("doFind names an empty name list and still falls back when any name exists") {
    val missing: IllegalArgumentException = intercept[IllegalArgumentException] {
      Names(Seq.empty).doFind(ru)
    }
    assert(missing.getMessage.contains(ru.toString))
    assert(missing.getMessage.contains("present: (none)"))
    val onlyEnglish: Names = Names(Seq(Name("book", en)))
    assert(onlyEnglish.doFind(ru).name == "book")
  }

  private val en: Language.Spec = Language.English.toSpec
  private val ru: Language.Spec = Language.Russian.toSpec
  private val he: Language.Spec = Language.Hebrew.toSpec

  private def decodeName(xml: String) =
    Name.codec.decode(XmlParser.parseXml(xml).toOption.get)

  private def decodeTerm(xml: String) =
    Term.codec.decode(XmlParser.parseXml(xml).toOption.get)

  private def assertMessage(result: Either[org.podval.xml.XmlError, Term], needle: String): Unit =
    result match
      case Left(error) => assert(error.getMessage.contains(needle))
      case Right(term) => fail(term.toString)

  private def axis(id: String, names: Name*): Term = Term(id, Names(names), selector = true)

  private def assertCategories(language: Language, expected: Seq[(Int, PluralCategory)]): Unit =
    expected.foreach: (n, category) =>
      assert(language.pluralCategory(n) == category, s"${language.name} $n")

private final case class NamedEntry(
  names: Seq[Name] = Seq.empty
) derives CanEqual
private object NamedEntry:
  given schema: Schema[NamedEntry] = Schema.derived
