

/**
 * Created by a-saitoh on 2014/12/28.
 */
class FieldInfo {
//  var field  = Array.ofDim[Cell](100,100)
  var currentTurn : Int = 0
  var myUnitMap = scala.collection.mutable.Map[Int, FieldUnit]()
  var opUnitMap = scala.collection.mutable.Map[Int, FieldUnit]()
  //Field はf(y)(x)の形でアクセスする
  var cells = Array.tabulate[Cell](100, 100)((y,x) => new Cell(y,x))
  var remainingTime : Int = 0
  var currentResource : Int = 0
  var workerEnough = true
  //y座標、x座標のタプル
  var myCastle : (Int, Int) = null
  var opCastle : (Int, Int) = null
  var isTopLeft = false;
  var resNum = 0;
  var resSet = scala.collection.mutable.Set[(Int, Int)]()

//  var myBarrackNum : Int = 0

  def countUnitOrder() = {
    myUnitMap.size - myUnitMap.count((recode :(Int, FieldUnit)) => {recode._2.order == ORDER.NONE})
  }

  def countUnitInCell(_y : Int, _x : Int, unitType : UNIT_TYPE) = {
    cells(_y)(_x).myUnits.count(id =>
      myUnitMap.get(id) match {
        case Some(u) if u.unitType == unitType => true
        case _ => false
      }
    )
  }
  def countUnitGoingToCell(_y : Int, _x : Int, unitType : UNIT_TYPE) = {
    cells(_y)(_x).myUnitsGoing.count(id =>
      myUnitMap.get(id) match {
        case Some(u) if u.unitType == unitType => true
        case _ => false
      }
    )
  }

  def countUnitInField(unitType : UNIT_TYPE) = {
    myUnitMap.values.count(_.unitType == unitType)
  }

  def findUnit(fromy : Int, fromx : Int, num : Int, unitType: UNIT_TYPE) : Iterable[FieldUnit] = {
    //myUnitMap.va
    //val distances = for(tunit <- myUnitMap if tunit._2.unitType == unitType) yield ((tunit._2, COMMON.distance(tunit._2.y, tunit._2.x, fromy, fromx)))
    val distance: Iterable[(FieldUnit, Int)] = for(unit <- myUnitMap.values if unit.unitType == unitType && !unit.command.isInstanceOf[EarnResourceCommand] && !unit.command.isInstanceOf[KeepGenerateCommand]) yield (unit, COMMON.distance(unit.y, unit.x, fromy, fromx))
    val nearestTupleSeq: Seq[(FieldUnit, Int)] = distance.toSeq.sortWith((t1:(FieldUnit,Int), t2:(FieldUnit, Int)) => {t1._2 < t2._2}).take(num)
    for(nearestTuple <- nearestTupleSeq) yield nearestTuple._1
  }
}
