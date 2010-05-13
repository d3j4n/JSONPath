import sbt._

class Project(info: ProjectInfo) extends DefaultProject(info) {

    val scalaTools = "Scala-Tools Maven2 Snapshots Repository" at "http://scala-tools.org/repo-snapshots"
    val json = "org.codehaus.jackson" % "jackson-mapper-asl" % "1.5.2"
    val scalaz = "com.googlecode.scalaz" % "scalaz-core_2.8.0.RC2" % "5.0-SNAPSHOT"
}

// vim: set ts=4 sw=4 et:
