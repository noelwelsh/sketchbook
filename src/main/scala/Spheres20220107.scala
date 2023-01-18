import cats.syntax.all.*
import cats.effect.unsafe.implicits.global
import doodle.core.*
import doodle.random.*
import doodle.syntax.all.*
import doodle.java2d.*
import doodle.interact.syntax.interpolation.*

// Genuary 2022 prompt 1
// "Draw 10,000 of something."
object Spheres20220107 {
  val nLines = 100
  val nSpheresPerLine = 100
  val radius = 512
  val circleMean = 7.0
  val circleStdDev = 3.5
  val baseColor = Color.hsla(0.degrees, 1.0, 0.7, 0.8)
  val colorStdDev = 0.1

  val randomCircle: Random[Picture[Unit]] =
    Random.normal(circleMean, circleStdDev).map(r => circle(r))

  def randomColor(x: Normalized): Random[Color] = {
    for {
      s <- Random.normal(
        (baseColor.saturation - x).max(0.1),
        colorStdDev
      )
      l <- Random.normal(
        (baseColor.lightness - x).max(0.1),
        colorStdDev
      )
    } yield baseColor.saturate(s.normalized).lightness(l.normalized)
  }

  def lineWidth(angle: Angle): Double = Math.abs(angle.cos * radius)

  def line(angle: Angle): Random[Picture[Unit]] = {
    val width = lineWidth(angle)

    (-width)
      .upToIncluding(width)
      .map { x =>
        val normed = ((x + width) / (width * 2.0)).normalized
        for {
          picture <- randomCircle
          color <- randomColor(normed)
        } yield picture.strokeColor(color).at(x, 0.0)
      }
      .forSteps(nSpheresPerLine)
      .toList
      .sequence
      .map(_.allOn)
  }

  val lines: Random[Picture[Unit]] = {
    (-178.0
      .upToIncluding(178.0)
      .forSteps(nLines))
      .map(a => a.degrees)
      .map(a => line(a).map(_.at(0.0, a.sin * radius)))
      .toList
      .sequence
      .map(_.allOn)
  }

  val sphere = lines

  @main def spheres20220107Main(): Unit = {
    import doodle.core.format.*

    val picture = sphere.run
    val frame = Frame.default
      .withSize(radius * 2 + 40, radius * 2 + 40)
      .withBackground(Color.hsl(240.degrees, 0.0, 0.0))
    picture.drawWithFrame(frame)
    picture.write[Png]("20220107-spheres.png", frame)
  }
}
