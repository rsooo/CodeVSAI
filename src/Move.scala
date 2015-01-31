/**
 * Created by a-saitoh on 2015/01/31.
 */
import scala.math._

object Move {
  //def nextDirection() : ORDER
  def rightFirst(yfrom : Int, xfrom : Int, ydest : Int, xdest : Int) : ORDER = {
    (xdest - xfrom) match {
      case m if m > 0 => ORDER.RIGHT
      case m if m < 0 => ORDER.LEFT
      case _ => ydest - yfrom match {
        case n if n > 0 => ORDER.DOWN
        case n if n < 0 => ORDER.UP
        case _ =>
//          field.cells(ydest)(xdest).myUnitsGoing.remove(id)
          ORDER.NONE
      }
    }
  }

  def downFirst(yfrom : Int, xfrom : Int, ydest : Int, xdest : Int) : ORDER = {
    (ydest - yfrom) match {
      case n if n > 0 => ORDER.DOWN
      case n if n < 0 => ORDER.UP
      case _ => (xdest - xfrom) match {
        case m if m > 0 => ORDER.RIGHT
        case m if m < 0 => ORDER.LEFT
        case _ =>
          //          field.cells(ydest)(xdest).myUnitsGoing.remove(id)
          ORDER.NONE
      }
    }
  }

  def longerFirst(yfrom : Int, xfrom : Int, ydest : Int, xdest : Int) : ORDER = {
    if (abs(xdest - xfrom) >= abs(ydest - yfrom)) {
      (xdest - xfrom) match {
        case m if m > 0 => ORDER.RIGHT
        case m if m < 0 => ORDER.LEFT
        case _ => ORDER.NONE
      }
    }else{
      (ydest - yfrom) match {
        case n if n > 0 => ORDER.DOWN
        case n if n < 0 => ORDER.UP
        case _ => ORDER.NONE
      }
    }
  }

}

