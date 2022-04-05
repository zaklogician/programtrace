addCompilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.1")
scalacOptions += "-Ypartial-unification"

lazy val root = project
  .in(file("."))
  .settings(
    name := "ProgramTraceOptimization",
    description := "ProgramTraceOptimization",
    version := "0.1.1-SNAPSHOT",
    scalaVersion := "2.12.15",
    libraryDependencies ++= Seq(
        "org.scalanlp" %% "breeze" % "2.0.1-RC1"
        , "org.typelevel" %% "cats-core" % "2.3.0"
        , "org.typelevel" %% "cats-free" % "2.3.0"        
        , "com.novocode" % "junit-interface" % "0.8" % "test->default"        
        , "com.cra.figaro" %% "figaro" % "5.0.0.0"
    )
)

// End ///////////////////////////////////////////////////////////////

