# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [0.0.2] - 2026-09-03
- cleanup
- drop duplicate `day` selector in `Selector.xml`

## [0.0.1] - 2026-09-02
- Initial release, extracted from [site-publisher](https://github.com/dubinsky/site-publisher).
- `XmlCodec`: Schema-derived document binder over any `XmlAst` (unwrapped sequences, leaf-record attributes, `XmlNode` identity, leftover `XmlExtras`).
- `XmlTag` binds a field from the element name.
- `XmlExtras` holds leftovers.
- `XmlParser` loads from URL, file, and classpath; XInclude is off by default; `xinclude = true` expands includes and sets `xml:base` relative to the initial document.
- `XmlCodec.decodeCatalog` / `XmlParser.parseCatalog` load a named wrapper and decode each child.
- `parseCatalog` can expand XInclude.
- comments, PIs, and text outside the document element (catalog files with a prologue comment) are ignored.
- `XmlDecode` holds the hand-codec helpers.
- `org.podval.store` and `org.podval.metadata` (from OpenTorah `core`); store walks and `HasName.bind` are synchronous.

