
val scalaZVersion = "7.2.0"

name := "scalaReflect"

version := "1.0"

scalaVersion := "2.11.7"

//resolvers +=  "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"
resolvers += Resolver.sonatypeRepo("releases")
resolvers += Resolver.sonatypeRepo("snapshots")
addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)
//dependencies
libraryDependencies += "org.scalameta" %% "scalameta"  % "0.1.0-SNAPSHOT"
libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value
libraryDependencies += "org.scala-lang" % "scala-compiler" % scalaVersion.value
libraryDependencies += "org.scala-lang" % "scala-library" % scalaVersion.value

libraryDependencies += "org.scalaz" %% "scalaz-core" % scalaZVersion
libraryDependencies += "org.scalaz" %% "scalaz-effect" % scalaZVersion
// libraryDependencies += "org.scalaz" %% "scalaz-typelevel" % scalaZVersion
libraryDependencies += "org.scalaz" %% "scalaz-scalacheck-binding" % scalaZVersion % "test"

libraryDependencies += "com.github.mpilquist" %% "simulacrum" % "0.7.0"
libraryDependencies += "com.chuusai" %% "shapeless" % "2.3.0"



libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.6" % "test"

javaOptions += "-XX:+PrintFlagsFinal"
scalacOptions += "-feature" //:kind
scalacOptions += "-language:implicitConversions"
scalacOptions += "-language:higherKinds"

initialCommands in console := "import scalaz._, Scalaz._"
initialCommands in console in Test := "import scalaz._, Scalaz._, scalacheck.ScalazProperties._, scalacheck.ScalazArbitrary._,scalacheck.ScalaCheckBinding._"
mainClass in (Compile,run) := Some("entry")
//mainClass := Some("entry")