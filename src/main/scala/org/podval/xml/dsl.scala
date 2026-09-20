package org.podval.xml

/** Xml-backed construction DSL. Inspired by ZIO Blocks HTML.
  *
  * `import org.podval.xml.dsl.{*, given}`
  */
object dsl:
  given xml: Xml.type = Xml

  export Xml.{
    XmlMod,
    ToXmlMod,
    Attr,
    MultiAttr,
    element,
    a,
    article,
    blockquote,
    body,
    br,
    code,
    data,
    dd,
    details,
    div,
    dl,
    dt,
    em,
    figcaption,
    figure,
    footer,
    h1,
    h2,
    h3,
    head,
    header,
    html,
    img,
    input,
    label,
    li,
    link,
    main,
    meta,
    nav,
    ol,
    p,
    pre,
    script,
    span,
    style,
    summary,
    table,
    td,
    th,
    time,
    title,
    tr,
    ul,
    className,
    id,
    href,
    src,
    rel,
    titleAttr,
    contentAttr,
    langAttr,
    `type`,
    typeAttr,
    `for`,
    forAttr,
    target,
    role,
    name,
    charset,
    httpEquiv,
    itemProp,
    itemType,
    itemScope,
    hidden,
    datetime,
    xmlns,
    aria,
    attr,
    dataAttr
  }
  given xmlMod: Xml.ToXmlMod[Xml.XmlMod] = Xml.ToXmlMod.xmlMod
  given string: Xml.ToXmlMod[String] = Xml.ToXmlMod.string
  given node: Xml.ToXmlMod[Xml.Node] = Xml.ToXmlMod.node
  given option: [A] => Xml.ToXmlMod[A] => Xml.ToXmlMod[Option[A]] = Xml.ToXmlMod.option
  given seq: [A] => Xml.ToXmlMod[A] => Xml.ToXmlMod[Seq[A]] = Xml.ToXmlMod.seq
  given conversion: [A] => Xml.ToXmlMod[A] => Conversion[A, Xml.XmlMod] = Xml.ToXmlMod.conversion
