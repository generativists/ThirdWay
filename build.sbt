name := "ThirdWay"

organization := "com.generativists"

version := "0.0.2-SNAPSHOT"

publishMavenStyle := true

publishTo := {
  val nexus = "https://oss.sonatype.org/"
  if (isSnapshot.value)
    Some("snapshots" at nexus + "content/repositories/snapshots")
  else
    Some("releases"  at nexus + "service/local/staging/deploy/maven2")
}

publishArtifact in Test := false

scalaVersion := "2.11.7"

libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.4" % "test"

libraryDependencies += "org.apache.commons" % "commons-math3" % "3.5" % "test"

scalacOptions ++= Seq(
  "-deprecation",
  "-encoding", "UTF-8",
  "-feature",
  "-language:existentials",
  "-language:higherKinds",
  "-language:implicitConversions",
  "-unchecked",
  "-Xfatal-warnings",
  "-Xlint",
  "-Yno-adapted-args",
  "-Ywarn-dead-code",
  "-Ywarn-numeric-widen",
  "-Ywarn-value-discard",
  "-Xfuture",
  "-Ywarn-unused-import"
)

pomExtra := (
    <url>https://github.com/generativists/ThirdWay</url>
    <licenses>
      <license>
        <name>MIT</name>
        <url>https://opensource.org/licenses/MIT</url>
        <distribution>repo</distribution>
      </license>
    </licenses>
    <scm>
      <url>git@github.com:generativists/ThirdWay.git</url>
      <connection>scm:git:git@github.com:generativists/ThirdWay.git</connection>
    </scm>
    <developers>
      <developer>
        <id>jbn</id>
        <name>John Bjorn Nelson</name>
        <url>http://johnbnelson.com</url>
      </developer>
    </developers>
 )
