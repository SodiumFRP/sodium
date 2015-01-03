libraryDependencies += "com.novocode" % "junit-interface" % "0.8" % "test->default"

javacOptions ++= Seq("-source", "1.8", "-target", "1.8", "-Xlint")

initialize := {
  val _ = initialize.value
  if (sys.props("java.specification.version") != "1.8")
    sys.error("Java 8 is required for this project.")
}

lazy val root = (project in file(".")).
  settings(
    name := "sodium",
    version := "1.0",
    scalaVersion := "2.11.4"
  )
