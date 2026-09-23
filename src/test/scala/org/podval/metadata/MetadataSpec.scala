package org.podval.metadata

import org.podval.xml.{XmlAttribute, XmlCodec, XmlParser, Xml}
import Xml.given
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
    check( 20, "כ")
    check(100, "ק")
    check(116, "קטז")
    check(119, "קיט")
    check(555, "תקנה")
    check(999, "תתקצט")
    check(6000, "ו׳")

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

private final case class NamedEntry(
  names: Seq[Name] = Seq.empty
) derives CanEqual
private object NamedEntry:
  given schema: Schema[NamedEntry] = Schema.derived
