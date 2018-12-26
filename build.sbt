name := "lzwpack"

version := "1.0.0"

scalaVersion := "2.12.8"

scalacOptions += "-Ypartial-unification"
scalacOptions += "-language:higherKinds"

autoAPIMappings := true

resolvers += Resolver.sonatypeRepo("releases")

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.9")

enablePlugins(JmhPlugin)

libraryDependencies += "org.typelevel" %% "cats-core" % "1.5.0"
libraryDependencies += "org.typelevel" %% "cats-effect" % "1.1.0"
libraryDependencies += "com.github.scopt" %% "scopt" % "3.7.1"
libraryDependencies += "co.fs2" %% "fs2-core" % "1.0.2"
libraryDependencies += "co.fs2" %% "fs2-io" % "1.0.2"
// libraryDependencies += "com.lihaoyi" %% "pprint" % "0.5.3"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.4" % "test"
libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.13.4" % "test"

// libraryDependencies += "com.github.alexarchambault" %% "scalacheck-shapeless_1.13" % "1.1.6" % "test"
libraryDependencies += "org.typelevel" %% "cats-laws" % "1.5.0" % "test"
libraryDependencies += "org.typelevel" %% "cats-testkit" % "1.5.0" % "test"