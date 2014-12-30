/**
 * Created by a-saitoh on 2014/12/28.
 */


case class FieldUnit(id : Int, y : Int, x : Int, hp : Int, unitType: UNIT_TYPE, var order : ORDER, var command : Command) {

  //default order
//  var order : ORDER = ORDER.NONE

//  def this(id : Int, y : Int, x : Int, hp : Int, unitType: UNIT_TYPE, order : ORDER) = {
//    this(id, y, x, hp, unitType)
//  }

  def toOrderString() = {
    id + " " + order.getVal
  }

}


