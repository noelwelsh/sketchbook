import cats.syntax.all.*
import cats.effect.unsafe.implicits.global
import doodle.core.*
import doodle.random.*
import doodle.syntax.all.*
import doodle.java2d.*

// Genuary 2022 prompt 8
// Single curve only.
object SingleCurve20220108 {
  val nPoints = 17
  val size = 600
  val width = 30
  val palette =
    List(Color.navy, Color.yellow, Color.black, Color.red, Color.cyan)
  val xOffset = -7
  val yOffset = -7

  val randomPoint: Random[Point] =
    (Random.double, Random.double).mapN((x, y) => Point(x * size, y * size))

  val randomPoints: Random[List[Point]] =
    randomPoint.replicateA(nPoints)

  val randomCurve: Random[Picture[Unit]] =
    randomPoints.map(points =>
      OpenPath(PathElement.interpolatingSpline(points)).path
    )

  val curve: Random[Picture[Unit]] =
    randomCurve
      .map(p =>
        palette.zipWithIndex.map((c, i) =>
          p.strokeColor(c).at(xOffset * i, yOffset * i)
        )
      )
      .map(_.allOn.strokeWidth(width))

  @main def singleCurve20220108Main(): Unit = {
    import doodle.effect.Writer.*

    val picture = curve.run
    picture.draw()
    picture.write[Png]("20220108-single-curve.png")
  }
}
