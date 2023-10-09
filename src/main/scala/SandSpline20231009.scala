import cats.syntax.all.*
import cats.effect.unsafe.implicits.global
import doodle.core.*
import doodle.random.*
import doodle.syntax.all.*
import doodle.java2d.*

object SandSpline20231009 {
  val splinesPerLine = 40
  val linesPerPicture = 20
  val stdDevMultiplier = 0.125

  val points = List(Point.zero, Point(0, 40), Point(0, 80), Point(0, 120), Point(0, 160), Point(0, 200))

  def noise(y: Double): Random[Point] =
    Random.normal(0.0, y * stdDevMultiplier).map(offset => Point(offset, y))

  val line = points
    .traverse(pt => noise(pt.y))
    .map(pts => interpolatingSpline(pts).strokeColor(Color.lightGrey.alpha(0.2.normalized)))
    .replicateA(splinesPerLine)
    .map(_.allOn)

  val lines =
    line.replicateA(linesPerPicture).map(_.allBeside)

  @main def sandSpline20231009Main(): Unit = {
    import doodle.core.format.*

    val picture = lines.run
    picture.write[Png]("20231009-sandspline.png")
  }
}
