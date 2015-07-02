val snapshots = "Sonatype Snapshots"  at "https://oss.sonatype.org/content/repositories/snapshots"

val algebraVersion = "0.2.0-SNAPSHOT"
val catsVersion    = "0.1.0-SNAPSHOT"

val algebra    = "org.spire-math" %% "algebra" % algebraVersion
val algebraStd = "org.spire-math" %% "algebra-std" % algebraVersion

val cats       = "org.spire-math" %% "cats-core" % catsVersion
val catsStd    = "org.spire-math" %% "cats-std" % catsVersion

scalaVersion := "2.11.7"

libraryDependencies ++=
  Seq(
    algebra, algebraStd,
    cats, catsStd
  )

resolvers += snapshots
