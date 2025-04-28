import cats.syntax.all.*
import cats.effect.unsafe.implicits.global
import doodle.core.*
import doodle.random.*
import doodle.syntax.all.*
import doodle.java2d.*
import doodle.interact.syntax.interpolation.*

object TriangleGrid28032023 {

  /** Render a triangular grid.
    *
    *   - size is the side length of the equilateral triangles
    *   - width is the number of triangles wide
    *   - height is the number of triangles deep
    */
  final case class TriangleGrid(size: Int, width: Int, height: Int) {
    // The center of the equilateral triangle is half way between the top and
    // bottom, but equidistant from each vertex. This is a fine definition but
    // not what we want here. Shift the origin to be half way between top and
    // bottom so when we flip the triangle it tesselates.
    //
    // height = (sqrt(3) / 2) * size
    // origin is as (0, size/3)
    // So we want to shift up by
    //   (sqrt(3) / 4) * size - (size / 3)
    //  =(3 * sqrt(3) - 4) * size / 12
    val shift = (3 * Math.sqrt(3) - 4) * size.toDouble / 12.0
    val triangle = Picture.equilateralTriangle(size).originAt(0, shift)
    val rotated =
      Picture
        .equilateralTriangle(size)
        .rotate(180.degrees)
        .originAt(0, -shift * 2)
    def render(f: (Int, Int, Picture[Unit]) => Picture[Unit]): Picture[Unit] =
      (0.until(width)).foldLeft(Picture.empty) { (picture, x) =>
        0.until(height).foldLeft(picture) { (picture, y) =>
          val flipped = {
            val row = (x % 2 == 1)
            if y % 2 == 1 then !row else row
          }
          val t = if flipped then rotated else triangle
          f(x, y, t).at(x * size / 2, y * size * Math.sqrt(3) / 2.0).on(picture)
        }
      }
  }

  val colors = Array(
    Color.turquoise,
    Color.goldenrod,
    Color.mediumSpringGreen,
    Color.navy,
    Color.antiqueWhite
  )
  val picture =
    TriangleGrid(50, 20, 10).render { (x, y, triangle) =>
      triangle.fillColor(colors(((x * 37) + (y * 9)) % 5))
    }

  @main def runTriangleGrid28032023(): Unit = {
    import doodle.core.format.Png
    picture.draw()
    picture.write[Png]("triangle-grid-28032023.png")
  }
}
