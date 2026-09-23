# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## Unreleased

- Breaking: `XmlCodec` decode and encode take `Xml.Element`.
  Convert a foreign tree with `to[Xml.Element]` before decode, and the encode result with `to[TO]`.
- Breaking: walk, attribute, and CSS operations exist only on `Xml.Element`.
  Parser, writer, rebuild (`withName` / `withAttribute`), and `to[TO]` stay on `XmlAst`.
- `childElements` lists the element children.
  `flatMapNodes` and `convertElements` extend `Seq[XmlNode]` and need no `XmlAst` given.
- Walk, attribute, and CSS operations on `Xml.Element` are members.
  `import Xml.given` remains for parsing, writing, and `to[TO]`.
- Breaking: `elementById` is `getById` and returns `Option`.
  A missing id is `None`.
- `childNamed` is the first `childrenNamed` match.
- Breaking: removed `intAttr`, `intOpt`, `positiveIntOpt`, and `booleanOpt`.
  `requireAttr`, `requireName`, `requireNoOther`, and `positiveInt` stay.
- `Seq[Name]` binds repeated `<name>` elements without `@Modifier.config(XmlCodec.Element, "name")`.
  Derivation registers `Name.codec` before it snapshots type overrides.
- Breaking: `Xml2Html.is`, `get`, and `convert` take `Xml.Element`.
- The writer suppresses a namespace declaration when that prefix is already bound to the same URI.
  It does not invent `xmlns=""` for a name that has no URI.
  An author-written `xmlns=""` is kept and clears the default for descendants.
- `XmlWriterConfig.selfCloseEmpty` (default false) writes every empty element as `<e/>`.
  `selfClose` still names the empty elements that self-close when the flag is off.
- Breaking: `XmlWriterConfig.plus` is removed.
- Breaking: an unannotated primitive codec field, and an `Option` of a primitive, is an attribute named after the field.
  Sequences stay repeated child elements.
  `@Modifier.config(XmlCodec.Element, …)` still forces a child.
  A missing required primitive reports `Missing required attribute`.
- The construction DSL rebinds names with `XmlName.parseDeclared` after xmlns mods.
  `xmlns := uri` is the default namespace.
  `xmlns(prefix) := uri` declares a prefix, and a prefixed attribute picks it up regardless of mod order.
- Structural reads and child replacement are members on `XmlNode` and `XmlNode.Element`, so they need no `given`.
  `Xml`'s extension overrides for those operations delegate to the members.
- `rewrite` keeps one element or replaces it with a node list; `stopAtCode` still defaults to `true`.
- HTML local names in the construction DSL and `HtmlXmlWriterConfig` come from `XmlElement` and `XmlAttribute`.
  The sets passed to the writer are still those same local-name strings.
- `XmlMisc` is the prolog and epilogue slice of `XmlNode`.
  `Comment` and `ProcessingInstruction` live on `XmlNode` and carry `markup`.
- Breaking: the owned tree is `object Xml` (`XmlNode`: element, text, CDATA, comment, processing instruction).
  It is not a package given.
  Import `Xml.given`.
  Identity fields, catalogs, and `import org.podval.xml.dsl.given` use `Xml.Element`.
- The construction DSL is mixed into `Xml` only.
  `ScalaXml`, `ZioBlocksXml`, and `ZioBlocksHtml` are parse/convert/write adapters.
  Reach them with `element.to[TO]`.
- `zio-blocks-schema-xml` is `compileOnly`, same as `zio-blocks-html` and `scala-xml`.
- Breaking: `Html` is `ZioBlocksHtml`.
  Import `ZioBlocksHtml.given`.
  Convert with `element.to[ZioBlocksHtml.Element]`.

## [0.3.0] - 2026-09-20
- Construction DSL on `XmlAst`: tag functions, `:=` / `+=`, `ToXmlMod` flattening, `.when`, `inlineJs` / `externalJs`.
  Inspired by ZIO Blocks HTML.
- `import org.podval.xml.dsl.{*, given}` re-exports Xml-backed tags, keys, and `ToXmlMod` / `Conversion` givens.
- Drop `Html.toHtml`; convert with `element.to[Html.Element]` (`import Html.given`).
- Drop `HtmlXmlWriterConfig.render(Html.Element)` overloads.
  `render` is the inherited generic (`XmlWriterConfig.render[E: XmlAst]`); HTML trees need `import Html.given`.

## [0.2.0] - 2026-09-19
- `XmlWriterConfig.rawText`: HTML raw-text elements keep newlines, skip `encodeXmlSpecials`, and break `</` via
  `XmlEncode.protectHtmlRawText`.
- `HtmlXmlWriterConfig` opts in `script` and `style` (`XmlElement.Script` / `Style`).
- `fromElement` dispatches `rawText` then `preformat` (including the root); `fromNode` always calls `fromElement`.
- Adjacent text in those walks concatenates; CDATA in a `rawText` element is ordinary text.
- XML / `Plain` still encodes `<script>` bodies.

## [0.1.1] - 2026-09-10
- README: TagSoup extra dependency for `parseHtml`; `Selector.plural` / `pluralOrNames`; `Names` has `XmlCodec` only
  (no `Schema` given); `transform` / `gather` default `stopAtCode = true`.
- README: drop leftover Markdown `#` on AsciiDoc headings.

## [0.1.0] - 2026-09-10
- `XmlCodec` record encode/decode shares seq and text helpers.
- `Language.Latin` (`la`).
- `zio-blocks-html`, `scala-xml`, and `tagsoup` are `compileOnly` (not on the published POM); consumers that use `Html`
  / `ScalaXml` add those libraries; `parseHtml` needs TagSoup.
- `XmlCodec.xmlElementSchema` is a given on `XmlCodec`; `import XmlCodec.given` is enough when deriving identity fields.
  Package given `xmlElementSchema` remains.
- `Numbered` has `companion`, `+` / `-` / `next` / `prev`, and `-(that)` distance.
- Parse/decode I/O is `Either[XmlError, _]`; `XmlError` wraps causes.
  `attemptCatalog` / `attemptResources`; `loadCatalog` / `loadResources` still throw.
- README: one artifact, not split into modules.
  `checkReadmeVersion` keeps Maven coordinates equal to `project.version`.
- `XmlCodec.derived[A, K](tagField, tag)` is used for nested `A` without `.instance(TypeId.of[A], …)`.
- `XmlParser.loadCatalog(from, name, codec, wrapperName)`; `loadResources(from, codec, names*)` for one document per
  file.
- `Name`, `Language`, and `Language.Spec` have `Schema` givens.
  Catalog DTOs use `Seq[Name]` (`Name.Data` is private).
- `XmlCodec.IgnoreUnknown` skips leftover attributes/elements/text.
  `XmlCodec.Include` on `Seq[String]` gathers `xi:include/@href`.
  Identity fields (`Xml.Element`) use the Scala field name as the child tag.
- `element.toHtml` (`import Html.toHtml`).
  `HtmlXmlWriterConfig.render` takes `Html.Element` without `Html.given`.
- `By.Numbered(name, min, max, name2number, number2names)` with catalog summon.
- `object Selectors extends Selectors` loads `Selector.xml` / `<Selector>` by default.
- Breaking: `By.Numbered` constructor params are `name2number` / `number2names` (were `fromName` / `toNames`).
- Breaking: `Selector` is data; catalogs are `Selectors` (`forName` / `loadCatalog`).
  Drop library `Selector.xml`.
  `By(name)` / `By.Numbered(name, …)` need `given Selectors`; unknown name fails.
  `By(selector, stores)` does not.
- `Stores.asStores` / `axes` / `storeAliases`; `Alias(names, Path)` uses `structureNames`.
- `Stores.attempt` / `resolveOption`; `resolve` throws `ResolveError` (`NotFound`, `Ambiguous`, `Leftover`, `Cycle`).
- `NumberedStores.findByName` also matches `number2names`; `By.Numbered(..., name2number, number2names)`.
- `NumberedStore.compare` never returns 0 for different parents.
- Breaking: `Selector.title: Option[String]` is `plural: Option[Names]` (`pluralOrNames`); catalog `<plural>` (was
  `title` attribute).
- Selector `lesson`.
- `resolve` may omit a `By` hop when the name uniquely matches a child of one axis; `Path.toUrl` still includes the hop.
  Several matching axes fail.
- Breaking: `By.Numbered(selector, min, max)(create)`; drop `By.numbered`.
  `NumberedStores.length` is abstract (`maxNumber` is derived).
- Breaking: `Path.parent` is the parent store (`init.last`).
  `lastAs[T]` throws `ClassCastException`; `lastOption[T]`.
  `getPaths` defaults `stop` to never.
- `NumberedStore.compare` orders by parent identity, then number.
- `Path.tail` / `init`.
- Breaking: `Numbered.equals` / `hashCode` are overridable; `NumberedStore` includes `oneOf`.
- Breaking: `Alias.to` is `Seq[String]`; `"/"` is illegal; aliases resolve from the resolve root; cycles fail.
  `getPaths` skips aliases.
  `Path.toUrl` encodes segments (`%20`).
- Breaking: `getPaths` omits the starting node and drops the `path` prefix parameter; `Path.toUrl` of a result resolves
  from the node you called.
- `Stores.indexOf` / `next` / `prev` / `distance`; `NumberedStores` caches children, `get(number)`, `indexOf` by parent
  and number.
- Breaking: `Path` is a class (`stores`, English `structureNames` / `toUrl`), not `Seq[Store]`.
- Breaking: drop `Terminal`; `Alias` is a `Store`.
- `By(selector, stores)` and `By.Numbered`.
- Breaking: `Named` is `HasNames` (`HasName` stays the default-name key).
- Breaking: drop `HasName.findByNames` (use `find`).
- Breaking: drop `Names.checkDisjoint`, `Names.isDisjoint`, `Names.isEmpty`, `Language.Spec.languageName`, and Hebrew
  `MAQAF` / `PASEQ` / `SOF_PASUQ`.
- `HasName.bind` / `mapByName` reject duplicate keys and extra metadata keys.
- `Numbered.equals` / `hashCode` require the same non-anonymous class, not just the same number.
- `XmlWriter` tokenizes mixed text instead of allocating AST text nodes; `chunkify` splits on whitespace and glues when
  the next element is `cling` or `unStack`.
  Preformat attributes are space-separated; empty preformat uses `selfClose`.
  Hidden newline is NUL.
  Attribute name and value stay one token.
  HTML `cling` is implied by `unStack`.
- Breaking: `XmlParser` is string + classpath.
  Drop File/URL `parseXml` / `parseHtml` / `parseXmlDocument`, `parseResource(String)`, `parseResourceDocument`, and
  `parseCatalog`.
  Catalogs are `loadCatalog`.
- `XmlDocument` keeps the XML declaration, doctype, and prolog/epilogue comments and PIs.
  `parseXml` still returns the root element; `parseXmlDocument` returns the document.
  `XmlWriter` emits a canonical `<?xml version="1.0" encoding="UTF-8"?>` for documents.
  `XmlDeclaration` / `XmlDoctype` / `XmlMisc` own their markup; `XmlDocument.prefix` / `suffix` join the envelope.
- Breaking: drop `XmlAst` element `qName` / `localName` / `getPrefix` / `getNamespace`.
  Compare with `isNamed` / `isElement`; read components from `getName`.
  Writer/error text uses `getName.qName`.
- `XmlName.is(XmlElement)` is local+prefix (today's `isElement`).
  `XmlName.is(XmlAttribute)` / `sameAs(XmlAttribute)` are Clark identity.
  `matchesAny` / `localNameIn` / `isInclude`.
  Catalog `XmlElement.matches` / `XmlAttribute.matches`.
- `set(XmlAttribute)` uses the expanded name (no stringify through `qName`).
  `get(String)` still matches attribute qName (`id` ≠ `xml:id`).
- Breaking: `XmlName` (was `XmlExpandedName`).
- Breaking: `XmlAst` `getName` / `getAttributes` / `setAttributes` / `withAttributes` take `XmlName` (were
  `getExpandedName` / `getExpandedAttributes` / `setExpandedAttributes` / `withExpandedAttributes`).
  Drop the string-list `getAttributes` / `setAttributes` / `withAttributes`.
  QName pairs are `XmlName.asPairs`.
  `parseDeclared` binds xmlns from expanded attributes.
- Breaking: `XmlAttribute.qName` / `XmlElement.qName` / `XmlAst` element `qName` (were `name` / `getName`);
  `XmlName.qName` (was `qualifiedName`); `parseQName` (was `parseQualified`).
- Breaking: `XmlName(localName, namespace: Option[XmlNamespace])`.
  Prefix and URI are `prefix` / `uri`.
  Prefix without a URI is `XmlNamespace.of`.
- Breaking: `XmlAttribute` and `XmlElement` hold `XmlName` (string auxiliary constructor remains).
  `get(XmlAttribute)` matches with `sameAs`.
  `isElement`/`isA` match local name and prefix (a default-namespace DocBook `a` is still `isA`).
  `xml:id` / `xml:base` / `xmlns*` are the xml/xmlns URIs.
- `XmlName` owns xmlns/xml tests, conversion to/from ZIO Blocks `XmlName`, and writer xmlns fill-in from URIs.
  `withAttribute` matches URI+local.
  Codec encode/decode uses expanded names.
  `isInclude` requires the XInclude namespace or `xi` prefix.
- Breaking: `XmlAttribute` / `XmlElement` constructor field is `name` (was `expanded`).
- `HtmlXmlWriterConfig.break` includes `br` (hard newline after the tag when a right-side break is allowed).
- `Xml2Html.elementName` / `attributeName` / `is` / `get` are the post-`convert` names (`Head` → `tei-head`, `Lang` →
  `tei-lang`).
  `get` prefers the prefixed attribute so HTML `class` from `renameKeepingClass` does not hide the original.
  `convert` rewrites expanded names in place (`setAttributes`).
- `XmlElement` adds `Html`, `P`, `Head`, `Body`, `Title`, `Div`, `Span`, `Ul`, `Ol`, `Li`, `Img`, `Pre`, `Table`, `Tr`,
  `Td`, `Th`, `Dl`, `Dt`, `Dd`, `Blockquote`, `Figure`, `Figcaption`, `Br`, `Em`.
  `localName` is on the catalog object.
  `HtmlTagSoup` strips `Html`/`Body` wrappers by catalog local name.
- `XmlAttribute` adds `Lang`, `XmlLang`, `Src`, `Type`, `Title`, `Alt`, `Target`, `Rel`, `Role`, `Frame`.
  `localName` is on the catalog object.
- Breaking: drop `Xml2Html.renameElement`.
  Stamp-old-name-as-class is `XmlAst` `renameKeepingClass` (plain `rename` does not add a class).
- `XmlAttribute(name, XmlNamespace)` auxiliary constructor; `xml:*` and default `xmlns` use it.
  Prefixed `xmlns` stays `Xmlns(prefix)` (apply: `extends` cannot call apply).
- Breaking: `given Html` and `given ScalaXml` are no longer in `org.podval.xml`.
  `given Xml` stays the default.
  Import `Html.given` / `ScalaXml.given`.
  `Html` / `ScalaXml` objects stay in `org.podval.xml`.
- Breaking: identity codec fields are `XmlTree` (alias of `Xml.Element`).
  Package given `xmlElementSchema` is in scope in `org.podval.xml`; other packages `import org.podval.xml.given`.
  Drop `import XmlCodec.xmlElementSchema`.
- Breaking: drop XInclude expansion (`XmlXInclude`, `xinclude` flags).
  `xi:include` stays in the tree.
  Codec leftover checks no longer ignore `xml:base`.
  Xerces still mishandles nested `xml:base` ([XERCESJ-1102](https://issues.apache.org/jira/browse/XERCESJ-1102)).
- Breaking: drop `XmlNode` and `XmlExtras`.
  Leftover parent content is always an error.
  Identity fields are canonical `XmlTree` (same-AST decode keeps the node; other ASTs convert).
- `XmlAst` node `fold` dispatches element/text/cdata/comment/PI/unknown.
  `converted`/`toNodes` and `XmlWriter.fromNode`/`preformat` use it.
- `XmlWriter.chunkify` walks `Nil` / `node :: tail`.
- Breaking: CSS class token is `CssClass`.
  `object HtmlClass` is only the HTML `class` attribute.
- `XmlBuilder` stacks open elements with a child buffer and builds each element once on `endElement`.
  Adjacent text merges in the buffer.
  `result` requires a document element.
- `XmlAst` is a mixin of core, walk, and HTML `class` helpers (public type unchanged).
  Record codec, field layout, and register load/store live in `XmlCodecRecord`.
- `Xml2Html`, `XmlWriterConfig` name sets, and `transform`/`gather` `stopAtCode` match local names (default `true`; pass
  `false` to walk into `<code>`).
  `xml:lang` / `xml:id` are not rewritten.
- `parseHtml` drops the XHTML namespace; `parseXml` keeps it.
  `parseXml` still keeps undeclared entities (`&nbsp;`).
- Breaking: drop `XmlUtil` and `XmlDecode`.
  Tree/codec helpers are `XmlAst` extensions; `XmlAst.toId`; `Xml2Html.renameElement`.
- Writer always encodes `&` and `<` in text and `&`, `<`, `"` in attributes.
  Ampersands that already start an entity (`&nbsp;`, `&lt;`, `&#x…;`) are left alone.
  Drop `XmlWriterConfig.encodeXmlSpecials`.
  HTML CDATA becomes ordinary text (encoded on write).
- Drop unused `FromUrl`.
- `XmlParser` / `XmlBuilder` are abstract over `XmlAst` (same as `XmlWriter` / `XmlCodec`).
  Catalog helpers still pin ZIO Blocks XML.
- Drop unused `XmlDialect`.
  Write-time dialect lives on `XmlWriterConfig`; document headers live on `XmlDocument` / `XmlDoctype`.
- `XmlNamespace` is a case class (`uri`, optional canonical `prefix`); well-known prefixes (`xml`, `xmlns`, `xi`,
  `xlink`) live on the instances.
  XHTML has no prefix.
- Drop `XmlParserStAX`.
  Parse is SAX only (`XmlParser`); `XmlParserSax` is package-private.
  `XmlBuilder` stays public.
- `XmlAst` can represent comments and processing instructions; HTML drops them.
  `element.to[TO]` copies them when both sides have them.
  `XmlWriter` emits `<!-- -->` and `<?target data?>`.
- Replace `Ast2Ast` with `element.to[TO]` (`converted` on the source `XmlAst`); drop `XmlUtil.xml2html`.

## [0.0.2] - 2026-09-03
- Drop duplicate `day` selector in `Selector.xml`.
- `zio-blocks-html` and `scala-xml` are `compileOnlyApi`; `tagsoup` is `compileOnly` (were `api` / `implementation`).

## [0.0.1] - 2026-09-02
- Initial release, extracted from [site-publisher](https://github.com/dubinsky/site-publisher).
- `XmlCodec`: Schema-derived document binder over any `XmlAst` (unwrapped sequences, leaf-record attributes, `XmlNode`
  identity, leftover `XmlExtras`).
- `XmlParser` loads from URL, file, and classpath; XInclude is off by default; `xinclude = true` expands includes and
  sets `xml:base` relative to the initial document.
- `XmlCodec.decodeCatalog` / `XmlParser.parseCatalog` load a named wrapper and decode each child.
- `parseCatalog` can expand XInclude.
- comments, PIs, and text outside the document element (catalog files with a prologue comment) are ignored.
- `XmlDecode` holds the hand-codec helpers.
- `org.podval.store` and `org.podval.metadata` (from OpenTorah `core`); store walks and `HasName.bind` are synchronous.
