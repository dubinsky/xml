package org.podval.store

import org.podval.metadata.Names
import org.scalatest.funsuite.AnyFunSuite

given Selectors = Selectors(
  Selector(Names("book")),
  Selector(Names("part")),
  Selector(Names("verse")),
  Selector(Names("chapter"))
)

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
    val samuel: Store = Leaf("I Samuel")
    val books: By[Store] = By("book", Seq(genesis, psalms, samuel))
    val chumash: Stores[?] = new Stores.With(Seq(By("book", Seq(genesis)))):
      override def names: Names = Names("Chumash")
    val parts: By[Store] = By("part", Seq(chumash))

    val verses: By.Numbered[Num] = By.Numbered("verse", 1, 3)(Num(_, _))
    val chapters: By.Numbered[Num] = By.Numbered("chapter", 1, 3)(Num(_, _))
    val otherVerses: By.Numbered[Num] = By.Numbered("verse", 1, 3)(Num(_, _))

    override def stores: Seq[Store] = Seq(
      books,
      parts,
      Alias(chumash.names, Path(Seq(parts, chumash))),
      Alias(psalms.names, Path(Seq(books, psalms))),
      verses,
      chapters
    )

  private def roundTrip(url: String): Unit =
    val path: Path = Root.resolve(url)
    assert(Root.resolve(path.toUrl).structureNames == path.structureNames, path.toUrl)

  test("resolve empty is the root") {
    assert(Root.resolve("/").last.names.hasName("Root"))
    assert(Root.resolve("").last.names.hasName("Root"))
  }

  test("By unknown selector fails; Selector instance does not need a catalog") {
    intercept[IllegalArgumentException] { By("item", Seq(Root.genesis)) }
    val by: By[Store] = By(Selector(Names("item")), Seq(Root.genesis))
    assert(by.names.hasName("item"))
    assert(by.findByName("Genesis").contains(Root.genesis))
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

  test("asStores axes storeAliases") {
    assert(Root.asStores.length == 6)
    assert(Root.axes.length == 4)
    assert(Root.storeAliases.length == 2)
    assert(Root.books.asStores.corresponds(Root.books.stores)(_ eq _))
    intercept[IllegalArgumentException] { Alias(Names("x"), Path.empty) }
    assert(Alias(Names("x"), Path(Seq(Root.books, Root.genesis))).to == Seq("book", "Genesis"))
  }

  test("numbered stores resolve decimal and Hebrew") {
    assert(Root.resolve("/verse/2").lastAs[Num].number == 2)
    assert(Root.resolve("/verse/ב").lastAs[Num].number == 2)
    assert(Root.resolve("/verse/ב").toUrl == "/verse/2")
  }

  test("By.Numbered name overload takes name2number / number2names") {
    val days: By.Numbered[Num] = By.Numbered(
      "verse",
      1,
      2,
      name2number = name => if name == "one" then Some(1) else NumberedStores.parseNumber(name),
      number2names = n => Names(if n == 1 then "one" else n.toString)
    )(Num(_, _))
    assert(days.findByName("one").exists(_.number == 1))
    assert(days.get(1).names.hasName("one"))
  }

  test("numbered stores are cached") {
    val two: Num = Root.verses.get(2)
    assert(Root.verses.stores(1) eq two)
    assert(Root.resolve("/verse/2").last eq two)
    assert(Root.resolve("/verse/ב").last eq two)
    assert(Root.verses.findByName("2").get eq two)
  }

  test("NumberedStore equality includes parent") {
    val two: Num = Root.verses.get(2)
    assert(two.equals(Num(2, Root.verses)))
    assert(!two.equals(Root.otherVerses.get(2)))
    assert(two.hashCode == Num(2, Root.verses).hashCode)
    assert(two.compare(Num(2, Root.verses)) == 0)
    assert(two.compare(Root.verses.get(1)) > 0)
    assert(two.compare(Root.otherVerses.get(2)) != 0)
  }

  test("indexOf next prev distance") {
    assert(Root.books.indexOf(Root.genesis) == 0)
    assert(Root.books.next(Root.genesis).get eq Root.psalms)
    assert(Root.books.prev(Root.psalms).get eq Root.genesis)
    assert(Root.books.prev(Root.genesis).isEmpty)
    assert(Root.books.next(Root.psalms).get eq Root.samuel)
    assert(Root.books.next(Root.samuel).isEmpty)
    assert(Root.books.distance(Root.genesis, Root.psalms) == 1)
    assert(Root.books.indexOf(Root.verses.get(1)) == -1)
    intercept[IllegalArgumentException] { Root.books.distance(Root.genesis, Root.verses.get(1)) }

    val two: Num = Root.verses.get(2)
    assert(Root.verses.indexOf(two) == 1)
    assert(Root.verses.prev(two).get eq Root.verses.get(1))
    assert(Root.verses.next(two).get eq Root.verses.get(3))
    assert(Root.verses.prev(Root.verses.get(1)).isEmpty)
    assert(Root.verses.next(Root.verses.get(3)).isEmpty)
    assert(Root.verses.distance(Root.verses.get(1), Root.verses.get(3)) == 2)
    val dup: Num = Num(2, Root.verses)
    assert(Root.verses.indexOf(dup) == 1)
    assert(Root.verses.next(dup).get eq Root.verses.get(3))
    intercept[IllegalArgumentException] { Root.verses.get(0) }
    intercept[IllegalArgumentException] { Root.verses.get(4) }

    val fromOther: Num = Root.otherVerses.get(2)
    assert(two.compare(fromOther) != 0)
    assert(two.compare(fromOther) == -fromOther.compare(two))
  }

  test("reconstructed English URL resolves to the same structureNames") {
    roundTrip("/book/Genesis")
    roundTrip("/part/Chumash/book/Genesis")
    roundTrip("/Chumash")
    roundTrip("/Psalms")
    roundTrip("/verse/3")
    roundTrip("/verse/ג")
  }

  test("getPaths omits the starting node and toUrl resolves") {
    val paths: Seq[Path] = Root.getPaths(include = _.isInstanceOf[Leaf])
    val names: Seq[Seq[String]] = paths.map(_.structureNames)
    assert(names.contains(Seq("book", "Genesis")))
    assert(names.contains(Seq("book", "Psalms")))
    assert(names.contains(Seq("book", "I Samuel")))
    assert(names.contains(Seq("part", "Chumash", "book", "Genesis")))
    paths.foreach(path =>
      assert(Root.resolve(path.toUrl).structureNames == path.structureNames, path.toUrl)
    )
  }

  test("getPaths skips aliases") {
    val urls: Seq[String] = Root.getPaths(include = _ => true).map(_.toUrl)
    assert(!urls.contains("/Chumash"))
    assert(!urls.contains("/Psalms"))
    assert(urls.contains("/part/Chumash"))
    assert(urls.contains("/book/Psalms"))
  }

  test("toUrl encodes segments") {
    assert(Root.resolve("/book/I Samuel").toUrl == "/book/I%20Samuel")
    assert(Root.resolve("/book/I%20Samuel").last eq Root.samuel)
    assert(Path.encodeSegment("day of the week") == "day%20of%20the%20week")
  }

  test("alias target / is illegal and cycles fail") {
    intercept[IllegalArgumentException] { Alias(Names("x"), "/") }
    intercept[IllegalArgumentException] { Alias(Names("x"), "") }
    object Cycle extends Stores[?]:
      override def names: Names = Names("Cycle")
      override def stores: Seq[Store] = Seq(Alias(Names("loop"), "/loop"))
    intercept[IllegalArgumentException] { Cycle.resolve("/loop") }
  }

  test("Path parent init tail lastAs") {
    val path: Path = Root.resolve("/part/Chumash/book/Genesis")
    assert(path.init.toUrl == "/part/Chumash/book")
    assert(path.parent eq path.init.last)
    assert(path.tail.toUrl == "/Chumash/book/Genesis")
    assert(path.lastAs[Leaf] eq Root.genesis)
    assert(path.lastOption[Leaf].get eq Root.genesis)
    assert(path.lastOption[Num].isEmpty)
    intercept[NoSuchElementException] { Root.resolve("/book").parent }
    intercept[ClassCastException] { path.lastAs[Num] }
    intercept[UnsupportedOperationException] { Path.empty.tail }
    intercept[UnsupportedOperationException] { Path.empty.init }
  }

  test("missing name and leftover segments fail") {
    intercept[IllegalArgumentException] { Root.resolve("/book/Missing") }
    intercept[IllegalArgumentException] { Root.resolve("/book/Genesis/extra") }
  }

  test("optional By hop when the name is unique") {
    assert(Root.resolve("/Genesis").last eq Root.genesis)
    assert(Root.resolve("/Genesis").toUrl == "/book/Genesis")
    assert(Root.resolve("/I%20Samuel").last eq Root.samuel)
    assert(Root.resolve("/Chumash/Genesis").last eq Root.genesis)
    assert(Root.resolve("/Chumash/Genesis").toUrl == "/part/Chumash/book/Genesis")
  }

  test("optional By hop is ambiguous across axes") {
    intercept[IllegalArgumentException] { Root.resolve("/1") }
    intercept[IllegalArgumentException] { Root.resolve("/2") }
    assert(Root.resolve("/verse/2").last eq Root.verses.get(2))
    assert(Root.resolve("/chapter/2").last eq Root.chapters.get(2))
  }

  test("attempt and resolveOption") {
    assert(Root.resolveOption("/book/Genesis").map(_.last).contains(Root.genesis))
    assert(Root.resolveOption("/book/Missing").isEmpty)
    assert(Root.attempt("/book/Genesis").exists(_.last eq Root.genesis))
    assert(Root.attempt("/book/Missing") match
      case Left(ResolveError.NotFound("Missing", _)) => true
      case other => fail(other.toString)
    )
    Root.attempt("/2") match
      case Left(err: ResolveError.Ambiguous) =>
        assert(err.name == "2")
        assert(err.axes.length == 2)
      case other => fail(other.toString)
    Root.attempt("/book/Genesis/extra") match
      case Left(ResolveError.Leftover(Seq("extra"), leaf)) => assert(leaf eq Root.genesis)
      case other => fail(other.toString)
    object Cycle extends Stores[?]:
      override def names: Names = Names("Cycle")
      override def stores: Seq[Store] = Seq(Alias(Names("loop"), "/loop"))
    assert(Cycle.attempt("/loop") match
      case Left(_: ResolveError.Cycle) => true
      case other => fail(other.toString)
    )
  }

  test("number2names extra names round-trip toUrl") {
    object NamedNumbers extends Stores[?]:
      override def names: Names = Names("NamedNumbers")
      val nums: By.Numbered[Num] = By.Numbered(
        summon[Selectors].getForName("verse"),
        1,
        3,
        number2names = n => Names(s"n$n")
      )(Num(_, _))
      override def stores: Seq[Store] = Seq(nums)

    val viaDigit: Path = NamedNumbers.resolve("/verse/2")
    assert(viaDigit.lastAs[Num].number == 2)
    assert(viaDigit.toUrl == "/verse/n2")
    assert(NamedNumbers.resolve(viaDigit.toUrl).structureNames == viaDigit.structureNames)
    assert(NamedNumbers.resolve("/verse/n2").last eq NamedNumbers.nums.get(2))
    assert(NamedNumbers.resolve("/verse/ב").last eq NamedNumbers.nums.get(2))
    assert(NamedNumbers.resolve("/verse/ב").toUrl == "/verse/n2")
  }
