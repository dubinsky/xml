package org.podval.xml

object XmlEncode:
  /** Encode `&` and `<` in text (and, via [[quote]], `"` in attributes).
    *
    * Ampersands that already start an entity (`&nbsp;`, `&lt;`, `&#x21A9;`) are
    * left alone: SAX `skippedEntity` stores undeclared HTML names as those
    * characters (Markdown HTML-as-XML). A decoded `&lt;` in the tree (Markdown
    * source `&amp;lt;` outside `<pre>`) therefore stays `&lt;` and browsers
    * show `<`.
    *
    * Not taken: decode HTML names to Unicode at parse (`nbsp` → U+00A0), then
    * always encode `&`; or decode and re-emit names like `&nbsp;` when writing
    * HTML.
    */
  def encodeXmlSpecials(string: String): String =
    encodeAmpersands(string).replace("<", "&lt;")

  // Note: maybe use single quotes if the value contains double quote?
  def quote(value: String): String =
    "\"" + encodeXmlSpecials(value).replace("\"", "&quot;") + "\""

  private def encodeAmpersands(string: String): String =
    val out: StringBuilder = StringBuilder()
    var i: Int = 0
    while i < string.length do
      if string.charAt(i) != '&' then
        out.append(string.charAt(i))
        i += 1
      else
        entityEnd(string, i) match
          case Some(end) =>
            out.append(string.substring(i, end))
            i = end
          case None =>
            out.append("&amp;")
            i += 1
    out.toString

  /** Index after a well-formed entity that starts at `amp` (`string(amp) == '&'`). */
  private def entityEnd(string: String, amp: Int): Option[Int] =
    val start: Int = amp + 1
    if start >= string.length then None
    else
      val afterName: Option[Int] =
        val c: Char = string.charAt(start)
        if c == '#' then
          val i: Int = start + 1
          if i < string.length && (string.charAt(i) == 'x' || string.charAt(i) == 'X') then
            val digits: Int = run(string, i + 1, isHex)
            Option.when(digits > i + 1)(digits)
          else
            val digits: Int = run(string, i, _.isDigit)
            Option.when(digits > i)(digits)
        else if isNameStart(c) then
          Some(run(string, start + 1, isNameChar))
        else None
      afterName.filter(end => end < string.length && string.charAt(end) == ';').map(_ + 1)

  private def run(string: String, from: Int, pred: Char => Boolean): Int =
    var i: Int = from
    while i < string.length && pred(string.charAt(i)) do i += 1
    i

  private def isNameStart(c: Char): Boolean =
    c >= 'A' && c <= 'Z' || c >= 'a' && c <= 'z'

  private def isNameChar(c: Char): Boolean =
    isNameStart(c) || c.isDigit

  private def isHex(c: Char): Boolean =
    c.isDigit || c >= 'a' && c <= 'f' || c >= 'A' && c <= 'F'
