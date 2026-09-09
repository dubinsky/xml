package org.podval.store

import org.scalatest.funsuite.AnyFunSuite

final class SelectorSpec extends AnyFunSuite:
  test("Selector.getForName") {
    val inventory: Selector = Selector.getForName("inventory")
    assert(inventory.names.hasName("inventory"))
    assert(Selector.getForName("names").plural.exists(_.hasName("Имена")))
    assert(Selector.getForName("names").pluralOrNames.hasName("Имена"))
    assert(Selector.getForName("inventory").plural.isEmpty)
    assert(Selector.getForName("inventory").pluralOrNames.hasName("inventory"))
    assert(Selector.getForName("parsha").names.hasName("parsha"))
    assert(Selector.getForName("lesson").names.hasName("урок"))
  }

  test("Selector.forName matches any language name") {
    assert(Selector.forName("разряд").isDefined)
    assert(Selector.forName("category").isDefined)
    assert(Selector.forName("книга").isDefined)
    assert(Selector.forName("item").isEmpty)
  }

  test("catalog has a single day selector") {
    val days: Seq[Seq[String]] =
      Selector.values.filter(_.names.hasName("day")).map(_.names.names.map(_.name))
    assert(days.length == 1, days)
  }
