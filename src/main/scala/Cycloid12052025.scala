import cats.Monoid
import cats.syntax.all.*
import cats.effect.unsafe.implicits.global
import doodle.core.*
import doodle.random.*
import doodle.syntax.all.*
import doodle.java2d.*
import doodle.interact.syntax.interpolation.*
import doodle.core.format.Png
import doodle.interact.easing.Easing

@main def cycloid120250512(): Unit = {
  given Monoid[Vec] with {
    def combine(a: Vec, b: Vec): Vec = a + b

    val empty: Vec = Vec.zero
  }

  given Monoid[Angle => Vec] with {
    def combine(a: Angle => Vec, b: Angle => Vec): Angle => Vec =
      angle => a(angle) + b(angle)

    val empty: Angle => Vec = angle => Vec.zero
  }

  /** Reverse the rolling direction of the cycloid. */
  val reverse: Angle => Angle = angle => -angle

  /** Multiply the angle by the given speed, which determines how rapidly the
    * cycloid rotates.
    */
  def speed(speed: Double): Angle => Angle =
    angle => angle * speed

  /** Increment the angle by the given amount. In other words move it out of
    * phase.
    */
  def phase(p: Angle): Angle => Angle =
    angle => p - angle

  /** Set the radius of the cycloid */
  def radius(r: Double): Angle => Vec =
    angle => Vec(r, angle)

  /** Cycloid is speed of rotation and radius (+ve or -ve) */
  def cycloid(v: Double, r: Double): Angle => Vec =
    speed(v).andThen(radius(r))

  // Nine-fold symmetry
  def c1(amplitude: Double) =
    cycloid(1, amplitude) |+| cycloid(9, 0.66 * amplitude) |+| reverse.andThen(
      cycloid(18, 0.25 * amplitude)
    )

  // Inspired by "Creating Symmetry" by Frank Farris.
  def c2(amplitude: Double) =
    cycloid(1.0, amplitude) |+| cycloid(6.0, 0.5 * amplitude) |+| (speed(14.0)
      .andThen(phase(90.degrees))
      .andThen(radius(0.33 * amplitude)))

  def drawCycloid(
      cycloid: Angle => Vec,
      start: Angle = 0.degrees,
      stop: Angle = 720.degrees,
      steps: Int = 1000
  ): Picture[Unit] =
    interpolatingSpline[Algebra](
      (start)
        .upTo(stop)
        .forSteps(steps)
        .map(angle => cycloid(angle).toPoint)
        .toList
    )

  val curve =
    (0.0)
      .upTo(1.0)
      .withEasing(Easing.quadratic)
      .forSteps(50)
      .map { m =>
        drawCycloid(c2(350 * m + 100)).rotate(30.degrees * m)
      }
      .toList
      .allOn

  // val curve = 0.degrees
  //   .upTo(360.degrees)
  //   .forSteps(3)
  //   .map(angle => drawCycloid(c2).at(25, angle).rotate(angle))
  //   .toList
  //   .allOn
  // val curve = drawCycloid(c1)
  val picture = curve.on(curve.rotate(30.degrees))
  //drawCycloid(c1).on(drawCycloid(phase(20.degrees).andThen(c1)))
  val frame = Frame.default

  picture.drawWithFrame(frame)
  picture.write[Png]("20250512-cycloid.png", frame)
}
