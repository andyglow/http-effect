import Versions._
import sbt._
import sbt.Keys._

object Settings {

  object ScalaC {

    val generic = Seq(
      "-encoding", "UTF-8",
      "-feature",
      "-unchecked",
      "-deprecation",
      "-Xfatal-warnings",
      "-Ywarn-numeric-widen")

    val pre213 = Seq(
      "-language:higherKinds",
      "-Yno-adapted-args",
      "-Xfuture")

    val post213 = Seq.empty
  }

  val CommonSettings = Seq(
    scalaVersion := _2_11,
    crossScalaVersions := Seq(_2_11, _2_12, _2_13),
    Compile / unmanagedSourceDirectories ++= {
      val bd = baseDirectory.value
      def extraDirs(suffix: String): Seq[File] = Seq(bd / "src" / "main" / s"scala$suffix")
      CrossVersion.partialVersion(scalaVersion.value) match {
        case Some((2, y)) if y <= 12 => extraDirs("-2.12-")
        case Some((2, y)) if y >= 13 => extraDirs("-2.13+")
        case _                       => Nil
      }
    },
    Test / unmanagedSourceDirectories ++= {
      val bd = baseDirectory.value
      def extraDirs(suffix: String): Seq[File] = Seq(bd / "src" / "test" / s"scala$suffix")
      CrossVersion.partialVersion(scalaVersion.value) match {
        case Some((2, y)) if y <= 12 => extraDirs("-2.12-")
        case Some((2, y)) if y >= 13 => extraDirs("-2.13+")
        case _                       => Nil
      }
    },
    scalacOptions ++= ScalaC.generic,
    scalacOptions ++= {
      CrossVersion.partialVersion(scalaVersion.value) match {
        case Some((2, y)) if y <= 12 => ScalaC.pre213
        case Some((2, y)) if y >= 13 => ScalaC.post213
        case _                       => Seq.empty
      }
    }
  )

  val NoPublishSettings = Seq(
    crossScalaVersions := Nil,
    publish / skip := true,
    publishArtifact := false,
    aggregate in update := false)
}
