val scala3Version = "3.1.2"

val scalaTestVersion  = "3.2.12"
val catsVersion       = "2.8.0"
val catsEffectVersion = "3.3.12"

lazy val root = project
  .in(file("."))
  .settings(
    name         := "equations",
    version      := "0.1.0-SNAPSHOT",
    scalaVersion := scala3Version,
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-core"   % catsVersion,
      "org.typelevel" %% "cats-effect" % catsEffectVersion,
      "org.scalatest" %% "scalatest"   % scalaTestVersion % Test,
    ),
  )
