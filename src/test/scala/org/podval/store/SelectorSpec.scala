package org.podval.store

import org.podval.metadata.{Language, Name, Names, Term, TermCatalog}
import org.podval.store.selector
import org.scalatest.funsuite.AnyFunSuite

final class SelectorSpec extends AnyFunSuite:
  private val catalog: TermCatalog = TermCatalog(Seq(
    Term("book", Names(Seq(
      Name("book", Language.English.toSpec),
      Name("книга", Language.Russian.toSpec)
    )), selector = true),
    Term("inventory", Names(Seq(Name("inventory", Language.English.toSpec))), selector = true),
    Term("day of the week", Names(Seq(
      Name("day of the week", Language.English.toSpec)
    )), selector = true)
  ))

  test("getById is the id; forName still matches names") {
    assert(catalog.getById("book").id == "book")
    assert(catalog.getById("book").names.hasName("книга"))
    assert(catalog.getById("day of the week").id == "day of the week")
    assert(catalog.forName("книга").exists(_.id == "book"))
    assert(catalog.forName("inventory").exists(_.id == "inventory"))
    assert(catalog.forName("item").isEmpty)
    val missing: IllegalArgumentException = intercept[IllegalArgumentException] { catalog.getById("item") }
    assert(missing.getMessage.contains("Unknown term: item"))
  }

  test("By(selectorId) uses catalog.selector") {
    assert(By("book", Seq.empty)(using catalog).selector.id == "book")
    intercept[IllegalArgumentException] { By("item", Seq.empty)(using catalog) }
    intercept[IllegalArgumentException] { By("книга", Seq.empty)(using catalog) }
  }

  test("TermCatalog.selector") {
    val terms: TermCatalog = TermCatalog(Seq(
      Term(
        "book",
        Names(Seq(
          Name("book", Language.English.toSpec),
          Name("книга", Language.Russian.toSpec)
        )),
        selector = true
      ),
      Term("nav.menu", Names(Seq(Name("Menu", Language.English.toSpec))))
    ))
    val found: Selector = terms.selector("book")
    assert(found.id == "book")
    assert(found.names.hasName("книга"))
    val rejected: IllegalArgumentException = intercept[IllegalArgumentException] { terms.selector("nav.menu") }
    assert(rejected.getMessage.contains("nav.menu"))
    val unknown: IllegalArgumentException = intercept[IllegalArgumentException] { terms.selector("missing") }
    assert(unknown.getMessage.contains("Unknown term: missing"))
  }
