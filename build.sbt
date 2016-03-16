import de.johoop.ant4sbt.Ant4Sbt._

name := "JSAF"

version := "1.0"

scalaVersion := "2.9.2"

libraryDependencies += "junit" % "junit" % "4.11" % "test"


parallelExecution in Test := false

antSettings

// import ant tasks
addAntTasks("compile", "clean", "testTyping")

// add ant's compile as a dependency to sbt's compile task.
// ie. 'sbt compile' should build the whole project
compile <<= (compile in Compile) dependsOn antTaskKey("compile")

// a nightly test task
lazy val nightly = taskKey[Unit]("Runs assorted JUnit tests.")

nightly := {
	(test in Test).value
	antTaskKey("testTyping").value
}
