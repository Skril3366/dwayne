scalaVersion := "3.3.3"

val zioVersion = "2.0.22"
val catsVersion = "2.6.1"
val monocleVersion = "3.1.0"

val dependencies = List(
  "dev.zio" %% "zio" % zioVersion,
  "dev.zio" %% "zio-test" % zioVersion % Test,
  "dev.zio" %% "zio-test-sbt" % zioVersion % Test,
  "org.typelevel" %% "cats-core" % catsVersion,
  "dev.optics" %% "monocle-core" % monocleVersion,
  "dev.optics" %% "monocle-macro" % monocleVersion
)

lazy val dwayne = project
  .in(file("."))
  .settings(
    semanticdbEnabled := true,
    semanticdbVersion := scalafixSemanticdb.revision,
    scalacOptions ++= Seq(
      "-Wunused:imports",
      "-deprecation",
      "-Werror",
    ),
    libraryDependencies ++= dependencies,
    testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework")
    // scalacOptions ++= Seq("-no-indent") // diallow indentation based syntax
  )
