enablePlugins(JmhPlugin)

//scalacOptions += "-Ypartial-unification"

enablePlugins(Optimization)

scalacOptions += "-Ydelambdafy:inline"

scalacOptions += "-Xexperimental" // Enable SAM type for Scala 2.11.11
