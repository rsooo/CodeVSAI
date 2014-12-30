/**
 * Created by a-saitoh on 2014/12/28.
 */
class UnitAI {

  def think(field : FieldInfo) = {

    for(tunit <- field.myUnitMap){
      val unit = tunit._2
//      System.err.println("unitx:" + unit.x + " unity:" + unit.y)
      unit.unitType match {
        case UNIT_TYPE.WORKER if unit.command == FreeCommand => unit.command = new SimpleMoveCommand(field, unit.id , if(field.isTopLeft) 90 else 10, if(field.isTopLeft) 90 else 10)
        case UNIT_TYPE.CASTLE if unit.command == FreeCommand => unit.command = new KeepGenerateCommand(field, ORDER.C_WORKER, 40)
        case _ =>
      }

      unit.order = unit.command.generateOrder()
      if (unit.command.isCommandFinished()) unit.command = FreeCommand
    }

  }
}
