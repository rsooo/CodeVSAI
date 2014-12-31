/**
 * Created by a-saitoh on 2014/12/28.
 */
class UnitAI {
  var searchx = 0

  def think(field : FieldInfo) = {

    workerResourceAllocate(field)

    for(tunit <- field.myUnitMap){
      val unit = tunit._2
//      System.err.println("unitx:" + unit.x + " unity:" + unit.y)
      unit.unitType match {
        case UNIT_TYPE.WORKER if unit.command == FreeCommand => unit.setCommand(workerCommand(field, unit.id))
        case UNIT_TYPE.CASTLE if unit.command == FreeCommand => unit.setCommand(new KeepGenerateCommand(field, ORDER.C_WORKER, 40))
        case _ =>
      }

      unit.order = unit.command.generateOrder()
      if (unit.command.isCommandFinished()) unit.setCommand(FreeCommand)
    }

  }

  def workerCommand(f : FieldInfo, id : Int) : Command = {
//    System.err.println(f.resSet)
//    for(resPoint <- f.resSet){
//      System.err.println(f.countUnitInCell(resPoint._1, resPoint._2, UNIT_TYPE.WORKER) +  ", " + f.countUnitGoingToCell(resPoint._1, resPoint._2, UNIT_TYPE.WORKER))

//      if(f.countUnitInCell(resPoint._1, resPoint._2, UNIT_TYPE.WORKER) + f.countUnitGoingToCell(resPoint._1, resPoint._2, UNIT_TYPE.WORKER) < 6){
//        return new EarnResourceCommand(f, id, resPoint._1, resPoint._2)
//      }
//    }
    searchx = (searchx + 2) % 100
    new SimpleMoveCommand(f, id, if(f.isTopLeft) 99 else 0, if(f.isTopLeft) searchx else math.abs(searchx - 99))
  }

  def workerResourceAllocate(f : FieldInfo) = {
    for(resPoint <- f.resSet) {
      val workingCount = f.countUnitInCell(resPoint._1, resPoint._2, UNIT_TYPE.WORKER) + f.countUnitGoingToCell(resPoint._1, resPoint._2, UNIT_TYPE.WORKER)
      if(workingCount < 5){
        val unitList = f.findUnit(resPoint._1, resPoint._2, 5 - workingCount, UNIT_TYPE.WORKER)
        for(unit <- unitList){
          unit.setCommand(new EarnResourceCommand(f, unit.id, resPoint._1, resPoint._2))
        }
      }
    }
  }
}
