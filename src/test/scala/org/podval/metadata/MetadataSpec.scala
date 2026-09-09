package org.podval.metadata

import org.podval.xml.{XmlAttribute, XmlParser, Xml as ZioXml}
import org.scalatest.funsuite.AnyFunSuite

final class MetadataSpec extends AnyFunSuite:
  test("Language names") {
    assert(Language.English.names.names.length == 4)
    assert(Language.Hebrew.names.hasName("he"))
    assert(Language.Hebrew.names.hasName("иврит"))
  }

  test("Name codec accepts n or text but not both") {
    def decode(xml: String) =
      Name.codec.decode(XmlParser.parseXml(xml).toOption.get)(using ZioXml)

    val fromN = decode("""<name lang="en" n="English"/>""").toOption.get
    assert(fromN.name == "English")
    assert(fromN.languageSpec.language.contains(Language.English))

    val fromText = decode("""<name lang="ru">русский</name>""").toOption.get
    assert(fromText.name == "русский")
    assert(fromText.languageSpec.language.contains(Language.Russian))

    assert(decode("""<name n="x">y</name>""").isLeft)
    assert(decode("""<name lang="en"/>""").isLeft)
    assert(decode("""<name lang="en" n="" transliterated="yes"/>""").toOption.get.name == "")

    val encoded = Name.codec.encode(fromN)(using ZioXml)
    assert(encoded.get("n").contains("English"))
    assert(encoded.get(XmlAttribute.Lang).contains("en"))
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
    final class A(override val number: Int) extends Numbered[A] derives CanEqual
    final class B(override val number: Int) extends Numbered[B] derives CanEqual
    assert(A(1) == A(1))
    assert(A(1) != A(2))
    assert(!A(1).equals(B(1)))

    abstract class V(override val number: Int) extends Numbered[V] derives CanEqual
    val fiveA: V = new V(5) {}
    val fiveB: V = new V(5) {}
    val six: V = new V(6) {}
    assert(fiveA == fiveB)
    assert(fiveA != six)
  }

  test("HasNames.andNumber suffixes each language") {
    val numbered: HasNames = Language.English.andNumber(3)
    assert(numbered.names.hasName("English 3"))
    assert(numbered.names.hasName("en 3"))
  }
