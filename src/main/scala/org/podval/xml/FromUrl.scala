package org.podval.xml

import java.net.URL

final class FromUrl(
  val url: URL
  // isInclude: Boolean
)

object FromUrl:
  trait With:
    def fromUrl: FromUrl
