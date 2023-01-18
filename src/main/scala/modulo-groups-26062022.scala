import cats.syntax.all.*
import cats.effect.unsafe.implicits.global
import doodle.core.*
import doodle.random.*
import doodle.syntax.all.*
import doodle.java2d.*
import doodle.interact.syntax.interpolation.*

object ModuloGroups26062022 {
  def grid(
      width: Int,
      height: Int,
      cellSize: Int
  ): ((Int, Int) => Picture[Unit]) => Picture[Unit] =
    (draw: (Int, Int) => Picture[Unit]) => {
      (for {
        y <- 0.until(height)
        x <- 0.until(width)
      } yield draw(x, y).at(x * cellSize, y * cellSize)).toList.allOn
    }

  def countingGrid(
      width: Int,
      height: Int,
      cellSize: Int
  ): (Int => Picture[Unit]) => Picture[Unit] =
    (draw: Int => Picture[Unit]) => {
      grid(width, height, cellSize)((x, y) => draw(y * height + x))
    }

  extension [A,B, C](f: (A, B) => C) {
    def andThen[D](g: C => D): (A, B) => D =
      (a, b) => g(f(a, b))
  }

  def add(x: Int, y: Int): Int = x + y

  def modulo(m: Int): Int => Picture[Unit] =
    (count: Int) =>
      square(20).fillColor(if count % m == 0 then Color.black else Color.white)

  val baseGrid = countingGrid(10, 10, 20)

  val consecutive =
    baseGrid(modulo(2))
      .beside(baseGrid(modulo(3)))
      .beside(baseGrid(modulo(4)))
      .beside(baseGrid(modulo(5)))
      .beside(baseGrid(modulo(6)))
      .beside(baseGrid(modulo(7)))
      .beside(baseGrid(modulo(8)))
      .beside(baseGrid(modulo(9)))

  val primes =
    baseGrid(modulo(7))
      .beside(baseGrid(modulo(11)))
      .beside(baseGrid(modulo(13)))
      .beside(baseGrid(modulo(17)))
      .beside(baseGrid(modulo(19)))
      .beside(baseGrid(modulo(23)))
      .beside(baseGrid(modulo(29)))
      .beside(baseGrid(modulo(31)))

  val simple = consecutive.above(primes)

  val modulo151 = grid(400, 400, 5)(add.andThen(modulo(151)))

  @main def runModuloGroups26062022(): Unit = {
    modulo151.draw()
  }
}
