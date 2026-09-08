package org.podval.xml

import org.scalatest.funsuite.AnyFunSuite

final class XmlEncodeSpec extends AnyFunSuite:
  test("encodes raw & and <") {
    assert(XmlEncode.encodeXmlSpecials("a & b < c") == "a &amp; b &lt; c")
  }

  test("leaves existing entities") {
    assert(XmlEncode.encodeXmlSpecials("a&nbsp;b") == "a&nbsp;b")
    assert(XmlEncode.encodeXmlSpecials("a&lt;b") == "a&lt;b")
    assert(XmlEncode.encodeXmlSpecials("&amp;") == "&amp;")
    assert(XmlEncode.encodeXmlSpecials("&#8617;") == "&#8617;")
    assert(XmlEncode.encodeXmlSpecials("&#x21A9;") == "&#x21A9;")
  }

  test("encodes a lone ampersand") {
    assert(XmlEncode.encodeXmlSpecials("&") == "&amp;")
    assert(XmlEncode.encodeXmlSpecials("& foo") == "&amp; foo")
    assert(XmlEncode.encodeXmlSpecials("&;") == "&amp;;")
    assert(XmlEncode.encodeXmlSpecials("&amp") == "&amp;amp")
  }

  test("quote encodes attribute specials including \"") {
    assert(XmlEncode.quote("a & b < \"c\"") == "\"a &amp; b &lt; &quot;c&quot;\"")
    assert(XmlEncode.quote("a&nbsp;b") == "\"a&nbsp;b\"")
  }
