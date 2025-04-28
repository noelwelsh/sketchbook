import cats.effect.unsafe.implicits.global
import cats.syntax.all.*
import doodle.core.*
import doodle.random.*
import doodle.syntax.all.*
import doodle.java2d.*
import doodle.core.format.Png

object ChaosGame28042025 {
  val nSamples = 600
  val factor = 0.5
  val size = 600

  val vertices = List(
    Point(-size / 2.0, 0),
    Point(size / 2.0, 0),
    Point(0, Math.sqrt(3) * size / 2.0)
  )

  val vertex = Random.oneOf(vertices: _*)

  def nextPoint(currentPoint: Point): Random[Point] =
    vertex.map(v => currentPoint + ((v - currentPoint) * factor))

  val start =
    Point.zero

  def points(start: Point, nSamples: Int): Random[List[Point]] =
    (0.until(nSamples))
      .foldLeft(Random.always((start, List.empty[Point]))) { (accum, _) =>
        accum.flatMap { case (currentPt, pts) =>
          val nextPt = nextPoint(currentPt)

          nextPt.map(pt => (pt, pt :: pts))
        }
      }
      .map { case (_, pts) => pts }

  def triangle(points: List[Point]): Picture[Unit] =
    points.map(pt => Picture.circle(3).at(pt)).allOn

  def go(): Unit = {
    val triangles = List(400, 800, 1600)
      .map(nSamples => points(start, nSamples).map(triangle))
      .sequence
      .run()
    val picture = triangles.map(_.margin(20)).allBeside
    val frame = Frame.default
    picture.drawWithFrame(frame)
    picture.write[Png]("chaos-game-28042025.png", frame)
  }
}
