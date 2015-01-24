import scala.util.Random

/**
 * Created by a-saitoh on 2014/12/31.
 */
object COMMON {

  val rand = new Random

  //自分の城が左上と仮定したときの座標をかえす
  def location(p : Int) = scala.math.abs(p - 100)

  def distance(ay : Int, ax : Int, by : Int, bx : Int) = {
    math.abs(ay - by) + math.abs(ax - bx)
  }

  def getInt(max : Int) = {
    rand.nextInt(max)
  }

}
