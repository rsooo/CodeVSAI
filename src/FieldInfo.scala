

/**
 * Created by a-saitoh on 2014/12/28.
 */
class FieldInfo {
//  var field  = Array.ofDim[Cell](100,100)
  var myUnitMap = scala.collection.mutable.Map[Int, FieldUnit]()
  var opUnitMap = scala.collection.mutable.Map[Int, FieldUnit]()
  //Field はf(y)(x)の形でアクセスする
  var cells = Array.tabulate[Cell](100, 100)((y,x) => new Cell(x,y))
  var remainingTime : Int = 0
  var currentResource : Int = 0

  //y座標、x座標のタプル
  var myCastle : (Int, Int) = null
  var opCastle : (Int, Int) = null
  var isTopLeft = false;
  var resNum = 0;

  def countUnitOrder() = {
    myUnitMap.size - myUnitMap.count((recode :(Int, FieldUnit)) => {recode._2.order == ORDER.NONE})
  }

}
