lazy val root = (project in file("."))
  .configs(Test)
  .settings(
    Defaults.itSettings,
    name         := "simulations",
    organization := "me.dan",
    scalaVersion := "2.11.8",
    version      := "0.1-SNAPSHOT",
    fork         := true,
    parallelExecution := false,
    libraryDependencies := Seq(
      "com.github.wookietreiber" %% "scala-chart" % "latest.integration"
    )
  )
