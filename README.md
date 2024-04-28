# Dwayne Scala Johnson

A task management app written in Scala

## Building with Scala Native

```
sbt nativeLink
```

The resulting binary will be in `target/scala-3.3.3/` folder


Note that you should use `Mode.release` for production builds and `Mode.debug` for development.
```scala
nativeConfig ~= { c =>
  c.withLTO(LTO.none)
    .withMode(Mode.release) // <--- here
    .withGC(GC.immix)
}
```
