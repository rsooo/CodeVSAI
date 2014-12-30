
/**
 * Created by a-saitoh on 2014/12/28.
 */
trait Command {
  def generateOrder() : ORDER
  def isCommandFinished() : Boolean


}

class SimpleMoveCommand(field : FieldInfo, id : Int, ydest : Int, xdest : Int) extends Command{


  override def generateOrder(): ORDER = {
    val self = field.myUnitMap.get(id) .get
//    System.err.println("xdest:" + xdest + "selfx " + self.x)

    (xdest - self.x) match {
      case m if m > 0 => ORDER.RIGHT
      case m if m < 0 => ORDER.LEFT
      case _ => ydest - self.y match{
        case n if n > 0 => ORDER.DOWN
        case n if n < 0 => ORDER.UP
        case _ => ORDER.NONE
      }
    }
  }

  override def isCommandFinished(): Boolean = {
    val self = field.myUnitMap.get(id) .get
    if(self.x == xdest && self.y == ydest) true else false
  }
}

object FreeCommand extends Command {
  override def generateOrder(): ORDER = ORDER.NONE
  override def isCommandFinished(): Boolean = false
}
//
//object FreeCommand extends Command{
//  override def generateOrder(): ORDER = null
//
//  override def isCommandFinished(): Boolean = false
//}

class KeepGenerateCommand(field : FieldInfo, order : ORDER, threshold : Int ) extends Command {
  override def generateOrder(): ORDER = {
    if(field.currentResource >= threshold) order else ORDER.NONE
  }
  override def isCommandFinished(): Boolean = false
}
