
/**
 * Created by a-saitoh on 2014/12/28.
 */
trait Command {
  def generateOrder() : ORDER
  def isCommandFinished() : Boolean
  def changeCommand : Unit


}

class SimpleMoveCommand(field : FieldInfo, id : Int, ydest : Int, xdest : Int) extends Command {

  field.cells(ydest)(xdest).myUnitsGoing.add(id)

  override def generateOrder(): ORDER = {
    val self = field.myUnitMap.get(id).get
    //    System.err.println("xdest:" + xdest + "selfx " + self.x)

    (xdest - self.x) match {
      case m if m > 0 => ORDER.RIGHT
      case m if m < 0 => ORDER.LEFT
      case _ => ydest - self.y match {
        case n if n > 0 => ORDER.DOWN
        case n if n < 0 => ORDER.UP
        case _ =>
          field.cells(ydest)(xdest).myUnitsGoing.remove(id)
          ORDER.NONE
      }
    }
  }

  override def isCommandFinished(): Boolean = {
    val self = field.myUnitMap.get(id).get
    if (self.x == xdest && self.y == ydest) true else false
  }

  override def changeCommand: Unit = {
    field.cells(ydest)(xdest).myUnitsGoing.remove(id)
  }
}

class EarnResourceCommand(field : FieldInfo, id : Int, ydest : Int, xdest : Int) extends Command{

  field.cells(ydest)(xdest).myUnitsGoing.add(id)

  override def generateOrder(): ORDER = {
    val self = field.myUnitMap.get(id) .get
    //    System.err.println("xdest:" + xdest + "selfx " + self.x)

    (xdest - self.x) match {
      case m if m > 0 => ORDER.RIGHT
      case m if m < 0 => ORDER.LEFT
      case _ => ydest - self.y match{
        case n if n > 0 => ORDER.DOWN
        case n if n < 0 => ORDER.UP
        case _ =>
          field.cells(ydest)(xdest).myUnitsGoing.remove(id)
          ORDER.NONE
      }
    }
  }

  override def isCommandFinished(): Boolean = {
    false
  }

  override def changeCommand: Unit = ()

}


object FreeCommand extends Command {
  override def generateOrder(): ORDER = ORDER.NONE
  override def isCommandFinished(): Boolean = false
  override def changeCommand: Unit = ()

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
  override def changeCommand: Unit = ()

}
