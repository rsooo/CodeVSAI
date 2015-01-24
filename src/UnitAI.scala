/**
 * Created by a-saitoh on 2014/12/28.
 */
class UnitAI {
  var searchx = 0
  var searchy = 0

  def init = {
    searchx = 0
    searchy = 0
  }

  def think(field : FieldInfo) = {

    field.workerEnough  = workerResourceAllocate(field)

    for(tunit <- field.myUnitMap){
      val unit = tunit._2
//      System.err.println("unitx:" + unit.x + " unity:" + unit.y)
      unit.unitType match {
        case UNIT_TYPE.WORKER if unit.command == FreeCommand =>
            unit.setCommand(workerCommand(field, unit))
        case UNIT_TYPE.CASTLE if unit.command == FreeCommand => unit.setCommand(new KeepGenerateCommand(field, ORDER.C_WORKER, (f:FieldInfo) =>
        { if(f.currentTurn > 50 && f.countUnitInField(UNIT_TYPE.BARRACK) < 1 && f.currentResource < 500)  false
          else if(f.currentTurn > 100 && field.workerEnough) false
          else f.currentResource > 40}, 100000
        ))
        case UNIT_TYPE.BARRACK => unit.setCommand(new KeepGenerateCommand(field, ORDER.C_KNIGHT, (f:FieldInfo) =>
        {
                f.currentResource > 100}, 100000
        ))

        case _ =>
      }

      unit.order = unit.command.generateOrder()
      if (unit.command.isCommandFinished()) unit.setCommand(FreeCommand)
    }

  }

  def workerCommand(f : FieldInfo, unit : FieldUnit) : Command = {
    if(f.currentResource >= 500 && unit.y == f.myCastle._1 && unit.x == f.myCastle._2){
      System.err.println("GEN BARRACK:unit.id" + unit.id + " y:" + unit.y + " x:" + unit.x)
//      f.myBarrackNum = f.myBarrackNum + 1
      return new KeepGenerateCommand(f, ORDER.C_BARRACK, (f:FieldInfo) => true, 1 )
    }
    else


//    System.err.println(f.resSet)
//    for(resPoint <- f.resSet){
//      System.err.println(f.countUnitInCell(resPoint._1, resPoint._2, UNIT_TYPE.WORKER) +  ", " + f.countUnitGoingToCell(resPoint._1, resPoint._2, UNIT_TYPE.WORKER))

//      if(f.countUnitInCell(resPoint._1, resPoint._2, UNIT_TYPE.WORKER) + f.countUnitGoingToCell(resPoint._1, resPoint._2, UNIT_TYPE.WORKER) < 6){
//        return new EarnResourceCommand(f, id, resPoint._1, resPoint._2)
//      }
//    }

    if(unit.id < 5 || unit.y == 0 || unit.y == 99){
      return new SimpleMoveCommand(f, unit.id, COMMON.getInt(100), COMMON.getInt(100))
    }

    searchx = (searchx + 4) % 100
/*    searchx = (searchx + 4)
    if(searchx >= 100) {
      searchx = searchx % 100
      searchy = if(searchy == 0) 99 else 0
    }
*/

    new SimpleMoveCommand(f, unit.id, if(f.isTopLeft) 99 else 0, if(f.isTopLeft) searchx else math.abs(searchx - 99))
  }

  /**
   *
   * @param f
   * @return lack of worker false, else true
   */
  def workerResourceAllocate(f : FieldInfo) : Boolean = {
    for(resPoint <- f.resSet) {
      val workingCount = f.countUnitInCell(resPoint._1, resPoint._2, UNIT_TYPE.WORKER) + f.countUnitGoingToCell(resPoint._1, resPoint._2, UNIT_TYPE.WORKER)
      System.err.println(f.currentTurn+ ": " + resPoint._1 + ". "+ resPoint._2 + ", count:" +  f.countUnitInCell(resPoint._1, resPoint._2, UNIT_TYPE.WORKER) + "," + f.countUnitGoingToCell(resPoint._1, resPoint._2, UNIT_TYPE.WORKER))
      if(workingCount < 5){
        val unitList = f.findUnit(resPoint._1, resPoint._2, 5 - workingCount, UNIT_TYPE.WORKER)
        for(unit <- unitList){
          unit.setCommand(new EarnResourceCommand(f, unit.id, resPoint._1, resPoint._2))
        }
        if(unitList.size < 5 - workingCount){
          System.err.println("worker lack")
          return false
        }
      }
    }
    return true;
  }
}
