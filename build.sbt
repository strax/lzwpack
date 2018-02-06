name := "lzwpack"

version := "0.1"

scalaVersion := "2.12.4"

scalacOptions += "-Ypartial-unification"
scalacOptions += "-language:higherKinds"

resolvers += Resolver.sonatypeRepo("releases")

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.4")

libraryDependencies += "org.typelevel" %% "cats-core" % "1.0.1"
libraryDependencies += "com.chuusai" %% "shapeless" % "2.3.3"
libraryDependencies += "com.github.scopt" %% "scopt" % "3.7.0"
libraryDependencies += "co.fs2" %% "fs2-core" % "0.10.0-RC1"
libraryDependencies += "co.fs2" %% "fs2-io" % "0.10.0-RC1"
libraryDependencies += "com.lihaoyi" %% "pprint" % "0.5.3"

libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.4"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.4" % "test"
libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.13.4" % "test"

// libraryDependencies += "com.github.alexarchambault" %% "scalacheck-shapeless_1.13" % "1.1.6" % "test"
libraryDependencies += "org.typelevel" %% "cats-laws" % "1.0.1" % "test"
libraryDependencies += "org.typelevel" %% "cats-testkit" % "1.0.0-RC1" % "test"


libraryDependencies += "com.lihaoyi" % "ammonite" % "1.0.3" % "test" cross CrossVersion.full

sourceGenerators in Test += Def.task {
  val file = (sourceManaged in Test).value / "amm.scala"
  IO.write(file, """object amm extends App { ammonite.Main.main(args) }""")
  Seq(file)
}.taskValue