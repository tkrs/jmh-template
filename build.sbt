version := "2.12.7"
name := "loop-benches-on-scala"
enablePlugins(JmhPlugin)

scalacOptions ++= Seq(
  "-opt-inline-from:**",
  "-opt:l:inline",
  "-opt-warnings",
)