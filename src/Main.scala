import java.io.File


import scala.sys.process._

/**
 * Created by a-saitoh on 2014/12/26.
 */
object Main extends App{

  // 70NDExnt7xen2fbtXObGFwD9pFyqrV9b

  var isFirstPrint = true
  var field : FieldInfo = null
//    print ("test")
//    println( FIELD_DATA.field(10)(20).x)
  val unitAI = new UnitAI
  println("rsoAI")
 val sc = new java.util.Scanner(System.in)

// val sc = new java.util.Scanner(new File("input.txt"))

  while(sc.hasNext){
    val is = new InputScanner(sc)
    val inputData = is.input()
    updateField(inputData)
    unitAI.think(field)
    for(tunit <- field.myUnitMap) {
      val unit = tunit._2
//      System.err.println("##unitx:" + unit.x + " unity:" + unit.y)
    }


    printOrder(field)
  }



//    test()

//    var source = Source.fromFile("input.txt")
//    var source = Source.fromInputStream(System.in)
//    val lines = source.getLines()
//    lines.foreach(println)

//    source.close


  def updateField( in : InputData) = {
    if(in.currentTurn == 0) {
      System.err.println("Stage: " + in.currentStage)
      field = new FieldInfo
    }
    for (cell1 <- field.cells){
      cell1.foreach(cell => {cell.myUnits.clear();cell.opUnits.clear()})
    }


    var newMyUnitMap = scala.collection.mutable.Map[Int,FieldUnit]()

    for(unitData <- in.myUnitList) {
      newMyUnitMap += (field.myUnitMap.get(unitData._1) match {
        case Some(v) => unitData._1 -> new FieldUnit(unitData._1, unitData._2, unitData._3, unitData._4, UNIT_TYPE.getUnitType(unitData._5), v.order, v.command)
        case _ => unitData._1 -> new FieldUnit(unitData._1, unitData._2, unitData._3, unitData._4, UNIT_TYPE.getUnitType(unitData._5), ORDER.NONE, FreeCommand)
      })
      val unit = newMyUnitMap.get(unitData._1) match {
        case Some(v) => v
        case _ => throw new IllegalStateException
      }
      field.cells(unit.y)(unit.x).myUnits.add(unit.id)

      unit.unitType match {
        case UNIT_TYPE.CASTLE =>
          field.myCastle = (unit.y, unit.x);
          field.isTopLeft = if(unit.y < 50) true else false
        case _ =>
      }
    }


      //update new unitMap
      field.myUnitMap = newMyUnitMap
//      System.err.println(field.myUnitMap.toString())

      var newOpUnitMap = scala.collection.mutable.Map[Int,FieldUnit]()


      for(unitData <- in.opUnitList) {
//        newOpUnitMap += (field.opUnitMap.get(unitData._1) match {
//          case Some(v) => (unitData._1 -> v)
//          case _ => (unitData._1 -> new FieldUnit(unitData._1, unitData._2, unitData._3, unitData._4, UNIT_TYPE.getUnitType(unitData._5)))
//        })
        //敵のUnit情報は前ターンからとりあえず何も引き継がない
        newOpUnitMap += (unitData._1 -> new FieldUnit(unitData._1, unitData._2, unitData._3, unitData._4, UNIT_TYPE.getUnitType(unitData._5), ORDER.NONE, null))
        val unit = newOpUnitMap.get(unitData._1) match {
          case Some(v) => v
          case _ => throw new IllegalStateException
        }
        field.cells(unit.y)(unit.x).opUnits.add(unit.id)

        unit.unitType match {
          case UNIT_TYPE.CASTLE => field.opCastle = (unit.y, unit.x)
          case _ =>
        }
      }
        //update new unitMap
        field.opUnitMap = newOpUnitMap

    field.resNum = in.resNum
//    System.err.println(in.resList)
    for(resData <- in.resList){
      field.cells(resData._1)(resData._2).resource = true
      field.resSet.add((resData._1, resData._2))
    }
    field.currentResource = in.currentResource

        //      newUnitMap += (unitData._1 -> (if(field.unitMap.contains(unitData._1)) field.unitMap.get(unitData._1) else new FieldUnit(1,1,1,UNIT_TYPE.ASSASSIN)))
//        newUnitMap += (if(field.unitMap.contains(unitData._1)) (unitData._1 -> field.unitMap.get(unitData._1)) else (unitData._1 -> new FieldUnit(1,1,1,1, UNIT_TYPE.WORKER)))
  }

  def printOrder(field : FieldInfo) : Unit =  {
//    if(isFirstPrint){
//      System.out.println("rsoAI")
//      isFirstPrint = false
//    }
    println(field.countUnitOrder())
    for(unit <- field.myUnitMap ){
      if(unit._2.order != ORDER.NONE)println(unit._2.toOrderString())
    }
  }

}
