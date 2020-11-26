import Settings._
import Dependencies._

lazy val eff_result = project.in(file("modules/result"))
  .settings(
    CommonSettings,
    name := "effect-result",
    libraryDependencies += ScalaTest.Root % Test)

lazy val eff_trans = project.in(file("modules/transformation"))
  .dependsOn(eff_result)
  .settings(
    CommonSettings,
    name := "effect-transformation",
    libraryDependencies ++= Seq(
        ScalaTest.Root % Test,
        ScalaCheck.Root % Test))

lazy val root = project.in(file("."))
  .aggregate(eff_result, eff_trans)
  .settings(NoPublishSettings)