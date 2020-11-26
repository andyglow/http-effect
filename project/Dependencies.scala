import sbt._

object Dependencies {

  object ScalaTest {
    private val v = "3.2.3"
    val Root = "org.scalatest" %% "scalatest" % v
  }

  object ScalaCheck {
    private val v = "3.2.3.0"
    val Root = "org.scalatestplus" %% "scalacheck-1-15" % v
  }
}
