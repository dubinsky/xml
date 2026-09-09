package org.podval.store

import org.podval.metadata.Names
import org.scalatest.funsuite.AnyFunSuite

final class StoreSpec extends AnyFunSuite:
  private final class Leaf(n: String) extends Store:
    override def names: Names = Names(n)

  private final class Num(
    override val number: Int,
    override val oneOf: NumberedStores[NumberedStore]
  ) extends NumberedStore

  private object Root extends Stores[?]:
    override def names: Names = Names("Root")

    val genesis: Store = Leaf("Genesis")
    val psalms: Store = Leaf("Psalms")
    val chumash: Stores[?] = new Stores.With(Seq(By("book", Seq(genesis)))):
      override def names: Names = Names("Chumash")

    val verses: By.Numbered[Num] = new By.Numbered[Num]("verse"):
      override def maxNumber: Int = 3
      override protected def createNumberedStore(number: Int): Num = Num(number, this)

    override def stores: Seq[Store] = Seq(
      By("book", Seq(genesis, psalms)),
      By("part", Seq(chumash)),
      Alias(chumash.names, "/part/Chumash"),
      Alias(psalms.names, "/book/Psalms"),
      verses
    )

  private def roundTrip(url: String): Unit =
    val path: Path = Root.resolve(url)
    assert(Root.resolve(path.toUrl).structureNames == path.structureNames, path.toUrl)

  test("resolve empty is the root") {
    assert(Root.resolve("/").last.names.hasName("Root"))
    assert(Root.resolve("").last.names.hasName("Root"))
  }

  test("resolve selector hops") {
    assert(Root.resolve("/book/Genesis").last eq Root.genesis)
    assert(Root.resolve("/book/Psalms").last eq Root.psalms)
    assert(Root.resolve("/part/Chumash").last eq Root.chumash)
    assert(Root.resolve("/part/Chumash/book/Genesis").last eq Root.genesis)
  }

  test("alias expands to the canonical path") {
    val viaAlias: Path = Root.resolve("/Chumash")
    val canonical: Path = Root.resolve("/part/Chumash")
    assert(viaAlias.structureNames == canonical.structureNames)
    assert(viaAlias.toUrl == "/part/Chumash")
    assert(Root.resolve("/Psalms").toUrl == "/book/Psalms")
    assert(Root.resolve("/Chumash/book/Genesis").last eq Root.genesis)
  }

  test("numbered stores resolve decimal and Hebrew") {
    assert(Root.resolve("/verse/2").lastAs[Num].number == 2)
    assert(Root.resolve("/verse/ב").lastAs[Num].number == 2)
    assert(Root.resolve("/verse/ב").toUrl == "/verse/2")
  }

  test("reconstructed English URL resolves to the same structureNames") {
    roundTrip("/book/Genesis")
    roundTrip("/part/Chumash/book/Genesis")
    roundTrip("/Chumash")
    roundTrip("/Psalms")
    roundTrip("/verse/3")
    roundTrip("/verse/ג")
  }

  test("getPaths walks included nodes") {
    val paths: Seq[Path] = Root.getPaths(
      include = _.isInstanceOf[Leaf],
      stop = _ => false
    )
    val names: Seq[Seq[String]] = paths.map(_.structureNames)
    assert(names.contains(Seq("Root", "book", "Genesis")))
    assert(names.contains(Seq("Root", "book", "Psalms")))
    assert(names.contains(Seq("Root", "part", "Chumash", "book", "Genesis")))
  }

  test("missing name and leftover segments fail") {
    intercept[IllegalArgumentException] { Root.resolve("/book/Missing") }
    intercept[IllegalArgumentException] { Root.resolve("/book/Genesis/extra") }
  }
