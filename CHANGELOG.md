# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## Unreleased
- Breaking: `XmlAttribute` / `XmlElement` constructor field is `name` (was `expanded`).
- `HtmlXmlWriterConfig.break` includes `br` (hard newline after the tag when a right-side break is allowed).
- `Xml2Html.elementName` / `attributeName` / `is` / `get` are the post-`convert` names (`Head` → `tei-head`, `Lang` → `tei-lang`). `get` prefers the prefixed attribute so HTML `class` from `renameKeepingClass` does not hide the original. `convert` rewrites expanded names in place (`setAttributes`).
- Breaking: `XmlAst` `getName` / `getAttributes` / `setAttributes` / `withAttributes` take `XmlExpandedName` (were `getExpandedName` / `getExpandedAttributes` / `setExpandedAttributes` / `withExpandedAttributes`). Drop the string-list `getAttributes` / `setAttributes` / `withAttributes`. QName pairs are `XmlExpandedName.asPairs`. `parseDeclared` binds xmlns from expanded attributes.
- `XmlElement` adds `Html`, `P`, `Head`, `Body`, `Title`, `Div`, `Span`, `Ul`, `Ol`, `Li`, `Img`, `Pre`, `Table`, `Tr`, `Td`, `Th`, `Dl`, `Dt`, `Dd`, `Blockquote`, `Figure`, `Figcaption`, `Br`, `Em`. `localName` is on the catalog object. `HtmlTagSoup` strips `Html`/`Body` wrappers by catalog local name.
- Breaking: drop `Xml2Html.renameElement`. Stamp-old-name-as-class is `XmlAst` `renameKeepingClass` (plain `rename` does not add a class).
- Breaking: `XmlAttribute.qName` / `XmlElement.qName` / `XmlAst` element `qName` (were `name` / `getName`); `XmlExpandedName.qName` (was `qualifiedName`); `parseQName` (was `parseQualified`).
- Breaking: `XmlExpandedName(localName, namespace: Option[XmlNamespace])`. Prefix and URI are `prefix` / `uri`. Prefix without a URI is `XmlNamespace.of`.
- `XmlAttribute(name, XmlNamespace)` auxiliary constructor; `xml:*` and default `xmlns` use it. Prefixed `xmlns` stays `Xmlns(prefix)` (apply: `extends` cannot call apply).
- `XmlAttribute` adds `Lang`, `XmlLang`, `Src`, `Type`, `Title`, `Alt`, `Target`, `Rel`, `Role`, `Frame`. `localName` is on the catalog object.
- Breaking: identity codec fields are `XmlTree` (alias of `Xml.Element`). Package given `xmlElementSchema` is in scope in `org.podval.xml`; other packages `import org.podval.xml.given`. Drop `import XmlCodec.xmlElementSchema`.
- `XmlAst` node `fold` dispatches element/text/cdata/comment/PI/unknown. `converted`/`toNodes` and `XmlWriter.fromNode`/`preformat` use it.
- `XmlWriter.chunkify` walks `Nil` / `node :: tail`.
- Breaking: `XmlAttribute` and `XmlElement` hold `XmlExpandedName` (string auxiliary constructor remains). `get(XmlAttribute)` matches with `sameAs`. `isElement`/`isA` match local name and prefix (a default-namespace DocBook `a` is still `isA`). `xml:id` / `xml:base` / `xmlns*` are the xml/xmlns URIs.
- Breaking: CSS class token is `CssClass`. `object HtmlClass` is only the HTML `class` attribute.
- `XmlBuilder` stacks open elements with a child buffer and builds each element once on `endElement`. Adjacent text merges in the buffer. `result` requires a document element.
- `XmlAst` is a mixin of core, walk, and HTML `class` helpers (public type unchanged). Record codec, field layout, and register load/store live in `XmlCodecRecord`.
- Breaking: `given Html` and `given ScalaXml` are no longer in `org.podval.xml`. `given Xml` stays the default. Import `Html.given` / `ScalaXml.given`. `Html` / `ScalaXml` objects stay in `org.podval.xml`.
- `XmlExpandedName` owns xmlns/xml tests, ZIO `XmlName` conversion, and writer xmlns fill-in from URIs. `withAttribute` matches URI+local. Codec encode/decode uses expanded names. `isInclude` requires the XInclude namespace or `xi` prefix.
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
- `XmlDocument` keeps the XML declaration, doctype, and prolog/epilog comments and PIs. `parseXml` still returns the root element; `parseXmlDocument` / `parseResourceDocument` return the document. `XmlWriter` emits a canonical `<?xml version="1.0" encoding="UTF-8"?>` for documents. `XmlDeclaration` / `XmlDoctype` / `XmlMisc` own their markup; `XmlDocument.prefix` / `suffix` join the envelope.

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

