name := "scala-either-extras"

organization := "org.lafros"

version := "1.0"

scalaVersion := "2.9.2"

libraryDependencies += "org.scalatest" %% "scalatest" % "1.7.2" % "test"

useGpg := true

publishMavenStyle := true

publishTo <<= version { (v: String) =>
  val nexus = "https://oss.sonatype.org/"
  if (v.trim.endsWith("SNAPSHOT"))
    Some("snapshots" at nexus + "content/repositories/snapshots")
  else
    Some("releases"  at nexus + "service/local/staging/deploy/maven2")
}

publishArtifact in Test := false

pomIncludeRepository := { _ => false }

pomExtra := (
  <url>https://github.com/robcd/scala-either-extras</url>
  <licenses>
    <license>
      <name>Apache v2.0</name>
      <url>http://www.apache.org/licenses/LICENSE-2.0</url>
      <distribution>repo</distribution>
    </license>
  </licenses>
  <scm>
    <url>git@github.com:robcd/scala-either-extras.git</url>
    <connection>scm:git:git@github.com:robcd/scala-either-extras.git</connection>
  </scm>
  <developers>
    <developer>
      <id>robcd</id>
      <name>Rob Dickens</name>
      <url>http://lafros.com/home</url>
    </developer>
  </developers>)
