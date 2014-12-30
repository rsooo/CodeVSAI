/**
 * Created by a-saitoh on 2014/12/28.
 */
class Cell(_x : Int,_y : Int) {
  val x = _x
  val y = _y
  var resource = false
  var see = false
  var myUnits = scala.collection.mutable.Set[Int]()
  var opUnits = scala.collection.mutable.Set[Int]()



}
