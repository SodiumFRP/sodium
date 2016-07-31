libraryDependencies += "com.novocode" % "junit-interface" % "0.8" % "test->default"

lazy val root = (project in file(".")).
  settings(
    name := "sodium",
    version := "1.0",
    scalaVersion := "2.11.8"
  )
