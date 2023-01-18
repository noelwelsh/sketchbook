import cats.Monoid
import cats.syntax.all.*
import cats.effect.unsafe.implicits.global
import doodle.core.*
import doodle.random.*
import doodle.syntax.all.*
import doodle.java2d.*
import doodle.interact.syntax.interpolation.*

object Cycloid27062022 {
  implicit val vecMonoid: Monoid[Vec] =
    new Monoid[Vec] {
      def combine(a: Vec, b: Vec): Vec = {
        a + b
      }

      val empty: Vec =
        Vec.zero
    }

  /** Reverse the rolling direction of the cycloid. */
  val reverse: Angle => Angle = angle => -angle

  /** Multiply the angle by the given speed, which determines how rapidly the
    * cycloid rotates.
    */
  def rotationSpeed(speed: Double): Angle => Angle =
    angle => angle * speed

  /** Increment the angle by the given amount. In other words move it out of
    * phase.
    */
  def phase(p: Angle): Angle => Angle =
    angle => p - angle

  /** Set the radius of the cycloid */
  def radius(r: Double): Angle => Vec =
    angle => Vec(r, angle)

  /** Add together two cycloids, creating a new cycloid */
  def add(c1: Angle => Vec, c2: Angle => Vec): Angle => Vec =
    angle => c1(angle) + c2(angle)

  /** Cycloid is speed of rotation and radius (+ve or -ve) */
  def cycloid(speed: Double, r: Double): Angle => Vec =
    rotationSpeed(speed).andThen(radius(r))

  val c1 = add(
    rotationSpeed(1.0).andThen(radius(50.0)),
    add(
      rotationSpeed(4.0).andThen(radius(25.0)),
      rotationSpeed(5.0).andThen(reverse).andThen(radius(-15.0))
    )
  )

  // Reproduced from "Creating Symmetry" by Frank Farris.
  val c2 =
    cycloid(1.0, 100.0) |+| cycloid(6.0, 50.0) |+| (rotationSpeed(14.0)
      .andThen(phase(90.degrees))
      .andThen(radius(33.0)))

  // val c2 = (angle: Angle) =>
  //   Vec(
  //     (angle.cos * 100) + ((angle * 6).cos * 50) + ((angle * 14).sin * 33),
  //     (angle.sin * 100) + ((angle * 6).sin * 50) + ((angle * 14).cos * 33)
  //   )

  val c3 = add(
    rotationSpeed(1.0).andThen(radius(200)),
    rotationSpeed(40.0).andThen(radius(-200))
  )

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

  val picture =
    drawCycloid(c3, stop = 60.degrees, steps = 100)
      .strokeColor(Color.magenta)
      .on(
        drawCycloid(c3, start = 60.degrees, stop = 120.degrees, steps = 100)
          .strokeColor(Color.cyan)
      )
      .on(
        drawCycloid(c3, start = 120.degrees, stop = 180.degrees, steps = 100)
          .strokeColor(Color.cornflowerBlue)
      )
      .on(
        drawCycloid(c3, start = 180.degrees, stop = 240.degrees, steps = 100)
          .strokeColor(Color.deepPink)
      )
      .on(
        drawCycloid(c3, start = 240.degrees, stop = 300.degrees, steps = 100)
          .strokeColor(Color.violet)
      )
      .on(
        drawCycloid(c3, start = 300.degrees, stop = 360.degrees, steps = 100)
          .strokeColor(Color.deepSkyBlue)
      )
      .strokeWidth(3.0)

  @main def runCycloid27062022(): Unit = {
    import doodle.core.format.Png
    picture.draw()
    picture.write[Png]("cycloid27062022.png")
  }
}
