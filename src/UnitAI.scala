//import _root_.COMMON._

/**
 * Created by a-saitoh on 2014/12/28.
 */
class UnitAI {
  var searchx = 0
  var searchy = 0
  val KNIGHT_ATTACK_COUNT = 10

  var searchOpx  = 0

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
        { if(f.currentResource >= 540) true
          else if(f.currentTurn > 50 && f.countUnitInField(UNIT_TYPE.BARRACK) < 1 && f.currentResource < 500)  false
          else if(f.currentTurn > 100 && field.workerEnough) false
          else f.currentResource > 40}, 100000
        ))
        case UNIT_TYPE.BARRACK => unit.setCommand(new KeepMultiGenerateCommand(field, (f:FieldInfo) =>
        {
          if(f.currentTurn % 3 == 0 && f.currentResource > 20) ORDER.C_KNIGHT
          else if(f.currentResource > 60) ORDER.C_ASSASSIN
          else ORDER.NONE}, 100000
        ))
        case UNIT_TYPE.ASSASSIN if unit.command == FreeCommand => unit.setCommand(armyCommand(field, unit))
        case UNIT_TYPE.KNIGHT if unit.command == FreeCommand => unit.setCommand(armyExplorer(field, unit))

        case _ =>
      }

      if(isPrepareAttacking(field) && field.opCastle != null){
        unit.unitType match {
            //とりあえずdefendeCommandにしている
          case UNIT_TYPE.ASSASSIN if unit.command.isInstanceOf[DefendeCommand] => unit.setCommand(new DefendeCommand(field, unit.id, field.opCastle._1, field.opCastle._2))
          case _ =>

        }
      }
      if(field.countUnitInCell(field.myCastle._1 + 3, field.myCastle._2 , UNIT_TYPE.KNIGHT) > KNIGHT_ATTACK_COUNT){
        val resPoint = getNearestTargetResource(field, unit)
        if(resPoint._1 != -1){
          val workingCount = field.countUnitInCell(resPoint._1, resPoint._2, UNIT_TYPE.WORKER)
          val defendingCount =  field.countUnitInCell(resPoint._1, resPoint._2, UNIT_TYPE.KNIGHT)
          if(workingCount < 5 && defendingCount < KNIGHT_ATTACK_COUNT){
            unit.unitType match {
              //とりあえずdefendeCommandにしている
              case UNIT_TYPE.KNIGHT if unit.command.isInstanceOf[DefendeCommand] => unit.setCommand(new DefendeResourceCommand(field, unit.id, resPoint._1, resPoint._2))
              case _ =>
            }
          }
        }
      }

      unit.order = unit.command.generateOrder()
      if (unit.command.isCommandFinished()) unit.setCommand(FreeCommand)
    }

  }

  def armyCommand(f : FieldInfo, unit : FieldUnit) : Command = {
    if(f.currentTurn % 4 != 0 || f.opCastle != null) {
      for (around <- COMMON.Around) {
        if (!f.isInrange(around._1 + f.myCastle._1, around._2 + f.myCastle._2)) "do nothing"
        else if (f.countUnitInCell(around._1 + f.myCastle._1, around._2 + f.myCastle._2, UNIT_TYPE.ASSASSIN) + f.countUnitGoingToCell(around._1 + f.myCastle._1, around._2 + f.myCastle._2, UNIT_TYPE.ASSASSIN) < 10) {
          return new DefendeCommand(f, unit.id, around._1 + f.myCastle._1, around._2 + f.myCastle._2)
        }
      }
    }
    //まずは全力で拠点をつぶしに行く
    /*
    for(opBarrack <- f.opBarrackSet){
      return new SimpleMoveCommand(f, unit.id, opBarrack._1, opBarrack._2)
    }
    */



    if(f.opCastle != null)
      return new SimpleMoveCommand(f, unit.id, f.opCastle._1, f.opCastle._2, Move.rightFirst)
      else{
      val desty = if(unit.y == 0 || f.isTopLeft) 99 else 0
      searchOpx = (searchOpx + 4) % 40
      return new SimpleMoveCommand(f, unit.id, desty, if(f.isTopLeft) searchOpx + 60 else searchOpx, Move.rightFirst)
    }
  }

  def isPrepareAttacking(f : FieldInfo) : Boolean= {
    var skip : Int = 0

    for(i <- 0 to 4){
        while(!f.isInrange(COMMON.Around(i + skip)._1 + f.myCastle._1, COMMON.Around(i + skip)._2 + f.myCastle._2)){
          skip = skip + 1
        }
      val diffPoint = COMMON.Around(i + skip)
      if (f.countUnitInCell(f.myCastle._1 + diffPoint._1, f.myCastle._2 + diffPoint._2, UNIT_TYPE.ASSASSIN) < 10){
        return false
      }
      
    }

    return true


  }


  def armyExplorer(f : FieldInfo, unit : FieldUnit) : Command = {
    searchx = (searchx + 4) % 100
/*    if(unit.y == 0 || unit.y == 99){

      //capture empty resource

      for(resPoint <- f.resSet) {
        val workingCount = f.countUnitInCell(resPoint._1, resPoint._2, UNIT_TYPE.WORKER)
        val defendingCount =  f.countUnitInCell(resPoint._1, resPoint._2, UNIT_TYPE.KNIGHT) + f.countUnitGoingToCell(resPoint._1, resPoint._2, UNIT_TYPE.KNIGHT)
        if(workingCount < 5 && defendingCount < 5){
          return new DefendeResourceCommand(f, unit.id, resPoint._1, resPoint._2)
        }
      }
       return new SimpleMoveCommand(f, unit.id, COMMON.getInt(100), COMMON.getInt(100), Move.longerFirst)
    }*/

    if(f.explolerRequired && f.currentTurn % 2 == 0) {
      val expCell = explolerNewCell(f, unit)
      if (expCell._1 != -1) {
        return new ExplolerCommand(f, unit.id, expCell._1, expCell._2)
      }
    }else{
      if(unit.y == f.myCastle._1 && unit.x == f.myCastle._2){
        return new DefendeCommand(f, unit.id, f.myCastle._1 + 3, f.myCastle._2)
      }else{
        val resPoint = getNearestTargetResource(f, unit)
        if(resPoint._1 != -1){
          val workingCount = f.countUnitInCell(resPoint._1, resPoint._2, UNIT_TYPE.WORKER)
          val defendingCount =  f.countUnitInCell(resPoint._1, resPoint._2, UNIT_TYPE.KNIGHT) + f.countUnitGoingToCell(resPoint._1, resPoint._2, UNIT_TYPE.KNIGHT)
          if(workingCount < 5 && defendingCount < 5){
            return new DefendeResourceCommand(f, unit.id, resPoint._1, resPoint._2)
          }
        }
      }
    }


//    if(f.countUnitInCell(f.myCastle._1 + 3, f.myCastle._2, UNIT_TYPE.KNIGHT) < 10){
//
/*      }else{
        val resPoint = getNearestTargetResource(f, unit)
        if(resPoint._1 != -1){

        }*/


    return new SimpleMoveCommand(f, unit.id, if(COMMON.getInt(2) == 0) 99 else 0, if(f.isTopLeft) searchx else math.abs(searchx - 99),Move.longerFirst)


    //      return new SimpleMoveCommand(f, unit.id, if(COMMON.getInt(2) == 0) 99 else 0, if(f.isTopLeft) searchx else math.abs(searchx - 99),Move.longerFirst)

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
      return new SimpleMoveCommand(f, unit.id, COMMON.getInt(100), COMMON.getInt(100),Move.longerFirst)
    }

    searchx = (searchx + 4) % 100
/*    searchx = (searchx + 4)
    if(searchx >= 100) {
      searchx = searchx % 100
      searchy = if(searchy == 0) 99 else 0
    }
*/

    if(f.explolerRequired){
      val expCell = explolerNewCell(f,unit)
      if(expCell._1 != -1){
        return new ExplolerCommand(f, unit.id, expCell._1, expCell._2)
      }
    }

    new SimpleMoveCommand(f, unit.id, if(f.isTopLeft) 99 else 0, if(f.isTopLeft) searchx else math.abs(searchx - 99), Move.longerFirst)
  }

  /**
   *
   * @param f
   * @return lack of worker false, else true
   */
  def workerResourceAllocate(f : FieldInfo) : Boolean = {
    for(resPoint <- f.resSet) {
      val workingCount = f.countUnitInCell(resPoint._1, resPoint._2, UNIT_TYPE.WORKER) + f.countUnitGoingToCell(resPoint._1, resPoint._2, UNIT_TYPE.WORKER)
//      System.err.println(f.currentTurn+ ": " + resPoint._1 + ". "+ resPoint._2 + ", count:" +  f.countUnitInCell(resPoint._1, resPoint._2, UNIT_TYPE.WORKER) + "," + f.countUnitGoingToCell(resPoint._1, resPoint._2, UNIT_TYPE.WORKER))
      if(workingCount < 5){
        val unitList = f.findUnit(resPoint._1, resPoint._2, 5 - workingCount, UNIT_TYPE.WORKER)
        for(unit <- unitList){
          unit.setCommand(new EarnResourceCommand(f, unit.id, resPoint._1, resPoint._2))
        }
        if(unitList.size < 5 - workingCount){
//          System.err.println("worker lack")
          return false
        }
      }
    }
    return true;
  }

  def explolerNewCell(f : FieldInfo, unit : FieldUnit) = {
    var shortestDistance = 200
    var retPoint : (Int, Int) = (-1,-1)
    for(j <- 0 to 24){
      for(i <- 0 to 24){
        val y : Int = j * 4 + 1
        val x : Int = i * 4 + 1
//      System.err.println("count" + f.cells(y)(x).see + y + " " + x + " " +  f.countUnitGoingToCell(y,x, UNIT_TYPE.ALL) + " " + COMMON.distance(unit.y,unit.x,y,x))
      if(!f.cells(y)(x).see && f.countUnitGoingToCell(y,x, UNIT_TYPE.ALL) == 0 && COMMON.distance(unit.y,unit.x,y,x) < shortestDistance){
          shortestDistance = COMMON.distance(unit.y,unit.x,y,x)
//          System.err.println("point:" + y + " " + x)
          retPoint = (y,x)

        }
      }
    }
//    System.err.println("return" + retPoint._1 + " " + retPoint._2)
    retPoint
  }

  def getNearestTargetResource(f : FieldInfo, unit : FieldUnit) ={
    var shortestDistance = 200
    var retPoint : (Int, Int) = (-1,-1)

    for(resPoint <- f.resSet){
      val workingCount = f.countUnitInCell(resPoint._1, resPoint._2, UNIT_TYPE.WORKER)
      val defendingCount =  f.countUnitInCell(resPoint._1, resPoint._2, UNIT_TYPE.KNIGHT) + f.countUnitGoingToCell(resPoint._1, resPoint._2, UNIT_TYPE.KNIGHT)
      if(workingCount < 5 && defendingCount < 5 && COMMON.distance(unit.y,unit.x, resPoint._1, resPoint._2) < shortestDistance){
        shortestDistance = COMMON.distance(unit.y,unit.x, resPoint._1, resPoint._2)
        retPoint = resPoint
      }
    }
    retPoint

  }
}
