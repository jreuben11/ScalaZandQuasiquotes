package learningScalaZ

/**
 * Created by joshr on 20/10/2015.
 */
import org.scalatest.{Matchers, FlatSpec}
//import scala.language.higherKinds
//import scala.language.implicitConversions
//import scala.language.postfixOps

import scalaz._
import Scalaz._

class Day11_Lenses extends FlatSpec with Matchers {
  "ScalaZ Lens" should "lensu" in {
    case class Point(x: Double, y: Double)
    case class Color(r: Byte, g: Byte, b: Byte)
    case class Turtle( position: Point, heading: Double, color: Color)

    val turtlePosition = Lens.lensu[Turtle, Point] (
      (a, value) => a.copy(position = value), _.position
    )
    val pointX = Lens.lensu[Point, Double] (
      (a, value) => a.copy(x = value), _.x
    )
    val turtleX = turtlePosition >=> pointX

    val t0 = Turtle(Point(2.0, 3.0), 0.0, Color(255.toByte, 255.toByte, 255.toByte))
    val t1 = Turtle(Point(5.0, 3.0), 0.0, Color(255.toByte, 255.toByte, 255.toByte))
    val t2 = Turtle(Point(3.0, 3.0), 0.0, Color(255.toByte, 255.toByte, 255.toByte))
    turtleX.get(t0) should be (2.0)
    turtleX.set(t0, 5.0) should be (t1)
    turtleX.mod(_ + 1.0, t0) should be (t2)
    val incX = turtleX =>= {_ + 1.0}
    incX(t0) should be (t2)

    val turtleHeading = Lens.lensu[Turtle, Double] ( (a, value) => a.copy(heading = value), _.heading)
    val pointY = Lens.lensu[Point, Double] ( (a, value) => a.copy(y = value), _.y )
    val turtleY = turtlePosition >=> pointY
    def forward(dist: Double) = for {
      heading <- turtleHeading
      x <- turtleX += dist * math.cos(heading)
      y <- turtleY += dist * math.sin(heading)
    } yield (x, y)
    forward(10.0)(t0)
    forward(10.0) exec (t0)
  }
}
