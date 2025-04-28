import cats.effect.unsafe.implicits.global
import cats.syntax.all.*
import doodle.core.*
import doodle.random.*
import doodle.syntax.all.*
import doodle.java2d.*
import doodle.core.format.Png

// Genuary 2025 prompt: Vertical or horizontal lines only.
object OrderDisorder28042025 {
  val size = 600
  val nLines = 7
  val nControlPoints = 6
  val pointSkip = size.toDouble / (nControlPoints + 2).toDouble
  val disorderRate = (size.toDouble / nLines) / nLines / 3

  def line(disorder: Double): Random[Picture[Unit]] =
    for {
      cps <- controlPoint(disorder).replicateA(nControlPoints)
    } yield Picture.path(
      OpenPath.interpolatingSpline(
        // Add points for the start and end of the line that are not perturbed
        (Point.zero +: cps :+ Point.zero).zipWithIndex.map { case (pt, idx) =>
          pt + Vec(idx * pointSkip, 0.0)
        }
      )
    )

  def controlPoint(disorder: Double): Random[Point] =
    Random.normal(0, disorder).map((y) => Point(0.0, y))

  val lines =
    (for (i <- 0.until(nLines)) yield {
      line(disorderRate * i).map(_.at(0, -i * (size / nLines)))
    }).toList.sequence.map(_.allOn.strokeWidth(7).strokeColor(Color.white))

  def go(): Unit = {
    val picture = lines.run()
    val frame = Frame.default.withBackground(
      Color.midnightBlue.darkenBy(0.7.normalized)
    )
    picture.drawWithFrame(frame)
    picture.write[Png]("order-disorder-28042025.png", frame)
  }
}
