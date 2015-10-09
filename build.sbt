
name := "scalaReflect"

version := "1.0"

scalaVersion := "2.11.7"

resolvers +=  "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"
resolvers += Resolver.sonatypeRepo("releases")
addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0-M5" cross CrossVersion.full)
//dependencies
libraryDependencies += "org.scalameta" %% "scalameta"  % "0.1.0-SNAPSHOT"
libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value
libraryDependencies += "org.scala-lang" % "scala-compiler" % scalaVersion.value
libraryDependencies += "org.scala-lang" % "scala-library" % scalaVersion.value
libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.1.4"
libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.4" % "test"
//libraryDependencies += "org.scalamacros" %% "paradise" % "2.1.0-M5"
