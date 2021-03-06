import sbt._

class SBTConfig(info: ProjectInfo) extends DefaultProject(info) {
  val scalaSwing = "org.scala-lang" % "scala-swing" % "2.8.1"
  val sqliteJDBC = "org.xerial" % "sqlite-jdbc" % "3.6.16"
  val scalatest = "org.scalatest" % "scalatest" % "1.3"
  val liftjson = "net.liftweb" % "lift-json_2.8.0" % "2.3"

  override def mainClass = Some("org.hinz.septa.Main")
}

// vim: set ts=4 sw=4 et:
