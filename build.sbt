scalaVersion := "3.3.3"

enablePlugins(ScalaNativePlugin)

import scala.scalanative.build._

nativeConfig ~= { c =>
  c.withLTO(LTO.none)
    .withMode(Mode.debug)
    // .withMode(Mode.release) // NOTE: use for production build instead of debug
    .withGC(GC.immix)
}
