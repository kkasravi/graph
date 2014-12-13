name := "graph"

version := "0.1"

organization := "com.kasravi"

scalaVersion := "2.11.3"

resolvers ++= Seq(
  "maven-repo" at "http://repo.maven.apache.org/maven2",
  "maven1-repo" at "http://repo1.maven.org/maven2",
  "maven2-repo" at "http://mvnrepository.com",
  "sonatype" at "https://oss.sonatype.org/content/repositories/releases"
)

parallelExecution in Test := false

val scalaGraphVersion = "1.9.0-SNAPSHOT"
val scalaGraphJsonVersion = "1.9.2-SNAPSHOT"
val scalaVersionNumber = "2.11.3"
val slf4jVersion = "1.7.5"

libraryDependencies ++= Seq(
  "org.slf4j" % "slf4j-api" % slf4jVersion,
  "org.slf4j" % "slf4j-log4j12" % slf4jVersion,
  "org.slf4j" % "jul-to-slf4j" % slf4jVersion,
  "org.slf4j" % "jcl-over-slf4j" % slf4jVersion,
  "org.scala-lang" % "scala-reflect" % scalaVersionNumber,
  "com.assembla.scala-incubator" %% "graph-core" % scalaGraphVersion,
  "com.assembla.scala-incubator" %% "graph-json" % scalaGraphJsonVersion,
  "com.assembla.scala-incubator" %% "graph-dot" % scalaGraphVersion
)
