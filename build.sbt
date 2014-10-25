import aether.Aether._
import AssemblyKeys._

name := "cr_scala"

version := "r104"

organization := "de.tu_bs.cs.isf.cr"

scalaVersion := "2.10.2"

resolvers += "Local Maven Repository" at "file:///"+Path.userHome.absolutePath+"/.m2/repository"

resolvers += "isfcr" at "http://www.isf.cs.tu-bs.de/data/sanschul/cr/mvn"

libraryDependencies += "de.tu_bs.cs.isf.cr.typechef_custom" % "frontend_2.10" % "26286b3-v3"

publishTo := Some(Resolver.file("file", new File(Path.userHome.absolutePath + "/mvn_builds")))

publishMavenStyle := true

publishArtifact in (Compile, packageDoc) := false

publishArtifact in (Compile, packageSrc) := false

aetherSettings

assemblySettings

scalacOptions += "-target:jvm-1.7"