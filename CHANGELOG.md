# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## Unreleased
- `XmlCodec.xmlElementSchema` is a given on `XmlCodec`; `import XmlCodec.given` is enough when deriving identity fields. Package given `xmlElementSchema` remains.
- `Numbered` has `companion`, `+` / `-` / `next` / `prev`, and `-(that)` distance.
- Parse/decode I/O is `Either[XmlError, _]`; `XmlError` wraps causes. `attemptCatalog` / `attemptResources`; `loadCatalog` / `loadResources` still throw.
- README: one artifact, not split into modules. `checkReadmeVersion` keeps Maven coordinates equal to `project.version`.
- `XmlCodec.derived[A, K](tagField, tag)` is used for nested `A` without `.instance(TypeId.of[A], …)`.
- `XmlParser.loadCatalog(from, name, codec, wrapperName)`; `loadResources(from, codec, names*)` for one document per file.
- `Name`, `Language`, and `Language.Spec` have `Schema` givens. Catalog DTOs use `Seq[Name]` (`Name.Data` is private).
- `XmlCodec.IgnoreUnknown` skips leftover attributes/elements/text. `XmlCodec.Include` on `Seq[String]` gathers `xi:include/@href`. Identity fields (`Xml.Element`) use the Scala field name as the child tag.
- `element.toHtml` (`import Html.toHtml`). `HtmlXmlWriterConfig.render` takes `Html.Element` without `Html.given`.
- `By.Numbered(name, min, max, name2number, number2names)` with catalog summon.
- `object Selectors extends Selectors` loads `Selector.xml` / `<Selector>` by default.
- Breaking: `By.Numbered` constructor params are `name2number` / `number2names` (were `fromName` / `toNames`).
- Breaking: `Selector` is data; catalogs are `Selectors` (`forName` / `loadCatalog`). Drop library `Selector.xml`. `By(name)` / `By.Numbered(name, …)` need `given Selectors`; unknown name fails. `By(selector, stores)` does not.
- `Stores.asStores` / `axes` / `storeAliases`; `Alias(names, Path)` uses `structureNames`.
- `Stores.attempt` / `resolveOption`; `resolve` throws `ResolveError` (`NotFound`, `Ambiguous`, `Leftover`, `Cycle`).
- `NumberedStores.findByName` also matches `number2names`; `By.Numbered(..., name2number, number2names)`.
- `NumberedStore.compare` never returns 0 for different parents.
- Selector `lesson`.
- `resolve` may omit a `By` hop when the name uniquely matches a child of one axis; `Path.toUrl` still includes the hop. Several matching axes fail.
- Breaking: `By.Numbered(selector, min, max)(create)`; drop `By.numbered`. `NumberedStores.length` is abstract (`maxNumber` is derived).
- Breaking: `Path.parent` is the parent store (`init.last`). `lastAs[T]` throws `ClassCastException`; `lastOption[T]`. `getPaths` defaults `stop` to never.
- `NumberedStore.compare` orders by parent identity, then number.
- Breaking: `Numbered.equals` / `hashCode` are overridable; `NumberedStore` includes `oneOf`.
- Breaking: `Alias.to` is `Seq[String]`; `"/"` is illegal; aliases resolve from the resolve root; cycles fail. `getPaths` skips aliases. `Path.toUrl` encodes segments (`%20`).
- Breaking: `getPaths` omits the starting node and drops the `path` prefix parameter; `Path.toUrl` of a result resolves from the node you called.
- `Path.tail` / `init`.
- `Stores.indexOf` / `next` / `prev` / `distance`; `NumberedStores` caches children, `get(number)`, `indexOf` by parent and number.
- Breaking: `Path` is a class (`stores`, English `structureNames` / `toUrl`), not `Seq[Store]`.
- Breaking: drop `Terminal`; `Alias` is a `Store`.
- `By(selector, stores)` and `By.Numbered`.
- Breaking: `Named` is `HasNames` (`HasName` stays the default-name key).
- Breaking: drop `HasName.findByNames` (use `find`).
- Breaking: drop `Names.checkDisjoint`, `Names.isDisjoint`, `Names.isEmpty`, `Language.Spec.languageName`, and Hebrew `MAQAF` / `PASEQ` / `SOF_PASUQ`.
- `HasName.bind` / `mapByName` reject duplicate keys and extra metadata keys.
- `Numbered.equals` / `hashCode` require the same non-anonymous class, not just the same number.
- `XmlWriter` tokenizes mixed text instead of allocating AST text nodes; `chunkify` splits on whitespace and glues when the next element is `cling` or `unStack`. Preformat attributes are space-separated; empty preformat uses `selfClose`. Hidden newline is NUL. Attribute name and value stay one token. HTML `cling` is implied by `unStack`.
- Breaking: `XmlParser` is string + classpath. Drop File/URL `parseXml` / `parseHtml` / `parseXmlDocument`, `parseResource(String)`, `parseResourceDocument`, and `parseCatalog`. Catalogs are `loadCatalog`.
- Breaking: drop `XmlAst` element `qName` / `localName` / `getPrefix` / `getNamespace`. Compare with `isNamed` / `isElement`; read components from `getName`. Writer/error text uses `getName.qName`.
- `XmlName.is(XmlElement)` is local+prefix (today's `isElement`). `XmlName.is(XmlAttribute)` / `sameAs(XmlAttribute)` are Clark identity. `matchesAny` / `localNameIn` / `isInclude`. Catalog `XmlElement.matches` / `XmlAttribute.matches`.
- `set(XmlAttribute)` uses the expanded name (no stringify through `qName`). `get(String)` still matches attribute qName (`id` ≠ `xml:id`).
- Breaking: `XmlName` (was `XmlExpandedName`).
- Breaking: `XmlAttribute` / `XmlElement` constructor field is `name` (was `expanded`).
- `HtmlXmlWriterConfig.break` includes `br` (hard newline after the tag when a right-side break is allowed).
- `Xml2Html.elementName` / `attributeName` / `is` / `get` are the post-`convert` names (`Head` → `tei-head`, `Lang` → `tei-lang`). `get` prefers the prefixed attribute so HTML `class` from `renameKeepingClass` does not hide the original. `convert` rewrites expanded names in place (`setAttributes`).
- Breaking: `XmlAst` `getName` / `getAttributes` / `setAttributes` / `withAttributes` take `XmlName` (were `getExpandedName` / `getExpandedAttributes` / `setExpandedAttributes` / `withExpandedAttributes`). Drop the string-list `getAttributes` / `setAttributes` / `withAttributes`. QName pairs are `XmlName.asPairs`. `parseDeclared` binds xmlns from expanded attributes.
- `XmlElement` adds `Html`, `P`, `Head`, `Body`, `Title`, `Div`, `Span`, `Ul`, `Ol`, `Li`, `Img`, `Pre`, `Table`, `Tr`, `Td`, `Th`, `Dl`, `Dt`, `Dd`, `Blockquote`, `Figure`, `Figcaption`, `Br`, `Em`. `localName` is on the catalog object. `HtmlTagSoup` strips `Html`/`Body` wrappers by catalog local name.
- Breaking: drop `Xml2Html.renameElement`. Stamp-old-name-as-class is `XmlAst` `renameKeepingClass` (plain `rename` does not add a class).
- Breaking: `XmlAttribute.qName` / `XmlElement.qName` / `XmlAst` element `qName` (were `name` / `getName`); `XmlName.qName` (was `qualifiedName`); `parseQName` (was `parseQualified`).
- Breaking: `XmlName(localName, namespace: Option[XmlNamespace])`. Prefix and URI are `prefix` / `uri`. Prefix without a URI is `XmlNamespace.of`.
- `XmlAttribute(name, XmlNamespace)` auxiliary constructor; `xml:*` and default `xmlns` use it. Prefixed `xmlns` stays `Xmlns(prefix)` (apply: `extends` cannot call apply).
- `XmlAttribute` adds `Lang`, `XmlLang`, `Src`, `Type`, `Title`, `Alt`, `Target`, `Rel`, `Role`, `Frame`. `localName` is on the catalog object.
- Breaking: identity codec fields are `XmlTree` (alias of `Xml.Element`). Package given `xmlElementSchema` is in scope in `org.podval.xml`; other packages `import org.podval.xml.given`. Drop `import XmlCodec.xmlElementSchema`.
- `XmlAst` node `fold` dispatches element/text/cdata/comment/PI/unknown. `converted`/`toNodes` and `XmlWriter.fromNode`/`preformat` use it.
- `XmlWriter.chunkify` walks `Nil` / `node :: tail`.
- Breaking: `XmlAttribute` and `XmlElement` hold `XmlName` (string auxiliary constructor remains). `get(XmlAttribute)` matches with `sameAs`. `isElement`/`isA` match local name and prefix (a default-namespace DocBook `a` is still `isA`). `xml:id` / `xml:base` / `xmlns*` are the xml/xmlns URIs.
- Breaking: CSS class token is `CssClass`. `object HtmlClass` is only the HTML `class` attribute.
- `XmlBuilder` stacks open elements with a child buffer and builds each element once on `endElement`. Adjacent text merges in the buffer. `result` requires a document element.
- `XmlAst` is a mixin of core, walk, and HTML `class` helpers (public type unchanged). Record codec, field layout, and register load/store live in `XmlCodecRecord`.
- Breaking: `given Html` and `given ScalaXml` are no longer in `org.podval.xml`. `given Xml` stays the default. Import `Html.given` / `ScalaXml.given`. `Html` / `ScalaXml` objects stay in `org.podval.xml`.
- `XmlName` owns xmlns/xml tests, conversion to/from ZIO Blocks `XmlName`, and writer xmlns fill-in from URIs. `withAttribute` matches URI+local. Codec encode/decode uses expanded names. `isInclude` requires the XInclude namespace or `xi` prefix.
- `Xml2Html`, `XmlWriterConfig` name sets, and `transform`/`gather` `stopAtCode` match local names. `xml:lang` / `xml:id` are not rewritten.
- `parseHtml` drops the XHTML namespace; `parseXml` keeps it. `parseXml` still keeps undeclared entities (`&nbsp;`).
- Breaking: drop `XmlUtil` and `XmlDecode`. Tree/codec helpers are `XmlAst` extensions; `XmlAst.toId`; `Xml2Html.renameElement`.
- Writer always encodes `&` and `<` in text and `&`, `<`, `"` in attributes. Ampersands that already start an entity (`&nbsp;`, `&lt;`, `&#x…;`) are left alone. Drop `XmlWriterConfig.encodeXmlSpecials`. HTML CDATA becomes ordinary text (encoded on write).
- Breaking: drop XInclude expansion (`XmlXInclude`, `xinclude` flags). `xi:include` stays in the tree. Codec leftover checks no longer ignore `xml:base`. Xerces still mishandles nested `xml:base` ([XERCESJ-1102](https://issues.apache.org/jira/browse/XERCESJ-1102)).
- Drop unused `XmlDialect`. Write-time dialect lives on `XmlWriterConfig`; document headers live on `XmlDocument` / `XmlDoctype`.
- Drop unused `FromUrl`.
- Breaking: drop `XmlNode` and `XmlExtras`. Leftover parent content is always an error. Identity fields are canonical `XmlTree` (same-AST decode keeps the node; other ASTs convert).
- `XmlNamespace` is a case class (`uri`, optional canonical `prefix`); well-known prefixes (`xml`, `xmlns`, `xi`, `xlink`) live on the instances. XHTML has no prefix.
- Drop `XmlParserStAX`. Parse is SAX only (`XmlParser`); `XmlParserSax` is package-private. `XmlBuilder` stays public.
- `XmlParser` / `XmlBuilder` are abstract over `XmlAst` (same as `XmlWriter` / `XmlCodec`). Catalog helpers still pin ZIO Blocks XML.
- `XmlAst` can represent comments and processing instructions; HTML drops them. `element.to[TO]` copies them when both sides have them. `XmlWriter` emits `<!-- -->` and `<?target data?>`.
- Replace `Ast2Ast` with `element.to[TO]` (`converted` on the source `XmlAst`); drop `XmlUtil.xml2html`.
- `XmlDocument` keeps the XML declaration, doctype, and prolog/epilogue comments and PIs. `parseXml` still returns the root element; `parseXmlDocument` returns the document. `XmlWriter` emits a canonical `<?xml version="1.0" encoding="UTF-8"?>` for documents. `XmlDeclaration` / `XmlDoctype` / `XmlMisc` own their markup; `XmlDocument.prefix` / `suffix` join the envelope.

## [0.0.2] - 2026-09-03
- cleanup
- drop duplicate `day` selector in `Selector.xml`

## [0.0.1] - 2026-09-02
- Initial release, extracted from [site-publisher](https://github.com/dubinsky/site-publisher).
- `XmlCodec`: Schema-derived document binder over any `XmlAst` (unwrapped sequences, leaf-record attributes, `XmlNode` identity, leftover `XmlExtras`).
- `XmlParser` loads from URL, file, and classpath; XInclude is off by default; `xinclude = true` expands includes and sets `xml:base` relative to the initial document.
- `XmlCodec.decodeCatalog` / `XmlParser.parseCatalog` load a named wrapper and decode each child.
- `parseCatalog` can expand XInclude.
- comments, PIs, and text outside the document element (catalog files with a prologue comment) are ignored.
- `XmlDecode` holds the hand-codec helpers.
- `org.podval.store` and `org.podval.metadata` (from OpenTorah `core`); store walks and `HasName.bind` are synchronous.

