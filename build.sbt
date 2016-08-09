import de.johoop.ant4sbt.Ant4Sbt._

name := "JSAF"

version := "1.0"

scalaVersion := "2.10.6"

libraryDependencies += "junit" % "junit" % "4.11" % "test"

libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % "test"

libraryDependencies += "org.rogach" %% "scallop" % "2.0.1"

libraryDependencies += "org.scalactic" %% "scalactic" % "2.2.6"

libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.6" % "test"


parallelExecution in Test := false

antSettings

// import ant tasks
addAntTasks("compile", "clean", "testTyping", "testNightly")

// add ant's compile as a dependency to sbt's compile task.
// ie. 'sbt compile' should build the whole project
compile <<= (compile in Compile) dependsOn antTaskKey("compile")

// a nightly test task
lazy val nightly = taskKey[Unit]("Runs assorted JUnit tests.")

nightly := {
	(test in Test).value
	antTaskKey("testNightly").value
}

nightly <<= nightly dependsOn compile

// quick test task
addCommandAlias("quick", ";compile;test;antRun testTyping")

