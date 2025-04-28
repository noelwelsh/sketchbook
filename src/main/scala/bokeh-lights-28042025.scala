import cats.effect.unsafe.implicits.global
import cats.syntax.all.*
import doodle.core.*
import doodle.random.*
import doodle.syntax.all.*
import doodle.java2d.*
import doodle.core.format.Png

// Genuary 2025 prompt: Vertical or horizontal lines only.
object BokehLights28042025 {
  val size = 600

  val lineWidth = 50

  val palette = List(
    // CrayolaColors.wisteria,
    CrayolaColors.shockingPink,
    // CrayolaColors.navyBlue,
    CrayolaColors.blueViolet,
    CrayolaColors.periwinkle,
    CrayolaColors.hotMagenta,
    CrayolaColors.unmellowYellow
  )

  def pointLight(color: Color): Random[Picture[Unit]] =
    for {
      skip <- Random.double.map(s => (s * lineWidth) + lineWidth / 16.0)
      m <- Random.double.map(m => m * 1.2)
      n <- Random.int(2, 7)
      spin <- Random.double.map(a => (a * 15.0).degrees)
      fade <- Random.double.map(d => d / 8.0)
    } yield {
      (for (i <- 0.to(n))
        yield Picture
          .circle(lineWidth)
          .noStroke
          .fillColor(
            color
              .spin(spin * i)
              .alpha((0.7 - (i * fade)).normalized)
          )
          .at(i * skip * Math.pow(m, i), 0)).toList.allOn
    }

  val nLights = 60

  val coord = Random.int(-size / 2, size / 2)
  val color = Random.oneOf(palette: _*)
  val light =
    for {
      x <- coord
      y <- coord
      c <- color
      p <- pointLight(c).map(_.at(x, y))
    } yield p
  val lights = light.replicateA(nLights).map(_.allOn)

  def go(): Unit = {
    val picture = lights.run()
    val frame = Frame.default.withBackground(
      Color.midnightBlue.darkenBy(0.7.normalized)
    )
    picture.drawWithFrame(frame)
    picture.write[Png]("bokeh-lights-28042025.png", frame)
  }
}
