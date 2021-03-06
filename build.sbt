name := "lancet"

version := "0.2"

//scalaVersion := "2.10.0-M1-virtualized"

scalaVersion := "2.10.0"

//scalaBinaryVersion := "2.10.0"

scalaOrganization := "org.scala-lang.virtualized"

scalacOptions += "-Yvirtualize"

// tests are not thread safe
parallelExecution in Test := false

resolvers += "Dropbox" at "http://dl.dropbox.com/u/1186811/scala-virtualized"

libraryDependencies += "org.scala-lang.virtualized" % "scala-compiler" % "2.10.0"

//libraryDependencies += "org.scala-lang" % "scala-reflect" % "2.10.0-M1-virtualized"

//libraryDependencies += "org.scalatest" % "scalatest_2.9.0" % "1.6.1" % "test"

//libraryDependencies += "org.scalatest" %% "scalatest" % "2.0.M5b" % "test"

//libraryDependencies += "org.scalatest" %% "scalatest" % "1.6.1-SNAPSHOT" % "test"

libraryDependencies += "EPFL" %% "lms" % "0.3-SNAPSHOT"

libraryDependencies += "stanford-ppl" %% "framework" % "0.1-SNAPSHOT"

libraryDependencies += "stanford-ppl" %% "runtime" % "0.1-SNAPSHOT"

libraryDependencies += "stanford-ppl" %% "optiml" % "0.1-SNAPSHOT"

libraryDependencies += "com.google.protobuf" % "protobuf-java" % "2.4.1"


libraryDependencies += "junit" % "junit" % "4.8.1" % "test"

retrieveManaged := true

unmanagedClasspath in Compile <++= baseDirectory map { base =>
  val graal = new File(System.getenv("JAVA_HOME")) / ".." / ".." / "graal"
  Seq(
    "com.oracle.graal.alloc",
    "com.oracle.graal.api",
    "com.oracle.graal.api.code",
    "com.oracle.graal.api.interpreter",
    "com.oracle.graal.api.meta",
    "com.oracle.graal.api.runtime",
    "com.oracle.graal.api.test",
    "com.oracle.graal.boot",
    "com.oracle.graal.boot.test",
    "com.oracle.graal.bytecode",
    "com.oracle.graal.compiler",
    "com.oracle.graal.debug",
    "com.oracle.graal.examples",
    "com.oracle.graal.graph",
    "com.oracle.graal.graph.test",
    "com.oracle.graal.hotspot",
    "com.oracle.graal.hotspot.server",
    "com.oracle.graal.interpreter",
    "com.oracle.graal.java",
    "com.oracle.graal.jtt",
    "com.oracle.graal.lir",
    "com.oracle.graal.lir.amd64",
    "com.oracle.graal.nodes",
    "com.oracle.graal.printer",
    "com.oracle.graal.snippets",
    "com.oracle.graal.snippets.test",
    "com.oracle.graal.tests",
    "com.oracle.max.asm",
    "com.oracle.max.cri",
    "com.oracle.max.criutils",
    "com.oracle.truffle",
    "com.oracle.truffle.bf",
    "com.oracle.truffle.c",
    "com.oracle.truffle.c.serial",
    "com.oracle.truffle.c.test",
    "com.oracle.truffle.compiler",
    "com.oracle.truffle.debugger.model",
    "com.oracle.truffle.debugger.ui",
    "com.oracle.truffle.java",
    "com.oracle.truffle.java.test",
    "com.oracle.truffle.javac",
    "com.oracle.truffle.jlang",
    "com.oracle.truffle.js",
    "com.oracle.truffle.js.test",
    "com.oracle.truffle.jxinterface",
    "com.oracle.truffle.py",
    "com.oracle.truffle.serial",
    "com.oracle.truffle.serial.test"
  ) map (graal / _ / "bin")
}

unmanagedClasspath in Test <++= (unmanagedClasspath in Compile)
