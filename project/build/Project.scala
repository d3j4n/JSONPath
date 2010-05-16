import sbt._

class Project(info: ProjectInfo) extends DefaultProject(info) {

  val scalaTools = ScalaToolsSnapshots

  override def libraryDependencies = Set(
    "org.codehaus.jackson" % "jackson-mapper-asl" % "1.5.2",
    "com.googlecode.scalaz" % "scalaz-core_2.8.0.RC2" % "5.0-SNAPSHOT",
    "org.scalatest" % "scalatest" % "1.2-for-scala-2.8.0.RC2-SNAPSHOT" % "test"
  )
}

