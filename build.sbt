lazy val designpattern = project
lazy val `benchmark-asyncio-scalaz` = project

lazy val `benchmark-naive` = project.dependsOn(designpattern)

lazy val `benchmark-Main` = project.dependsOn(`benchmark-asyncio-scalaz`, `benchmark-naive`)

lazy val unidoc = project
  .enablePlugins(StandaloneUnidoc, TravisUnidocTitle)
  .settings(
    UnidocKeys.unidocProjectFilter in ScalaUnidoc in UnidocKeys.unidoc := {
      inAggregates(LocalRootProject)
    },
    addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.3"),
    scalacOptions += "-Xexperimental"
  )
