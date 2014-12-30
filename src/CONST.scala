/**
 * Created by a-saitoh on 2014/12/28.
 */
object CONST {
}

object UNIT_TYPE{
  case object WORKER extends UNIT_TYPE(0, 4, 40)
  case object KNIGHT extends UNIT_TYPE(1, 4, 20)
  case object FIGHTER extends UNIT_TYPE(2, 4, 40)
  case object ASSASSIN extends  UNIT_TYPE(3, 4, 60)
  case object CASTLE extends UNIT_TYPE(4, 10, 9999999)
  case object SETTLEMENT extends UNIT_TYPE(5, 10, 100)
  case object BARRACK extends UNIT_TYPE(6, 4, 500)

  def getUnitType(n : Int) = {
    n match  {
      case 0 => WORKER
      case 1 => KNIGHT
      case 2 => FIGHTER
      case 3 => ASSASSIN
      case 4 => CASTLE
      case 5 => SETTLEMENT
      case 6 => BARRACK
    }
  }
}

sealed class UNIT_TYPE(val id : Int, val vision: Int, val cost : Int){
  def getId = id
  def getVision = vision
  def getCost = cost
}

object ORDER{
  case object NONE extends ORDER("NONE")
  case object UP extends ORDER("U")
  case object DOWN extends ORDER("D")
  case object LEFT extends ORDER("L")
  case object RIGHT extends ORDER("R")
  case object C_WORKER extends ORDER("0")
  case object C_KNIGHT extends ORDER("1")
  case object C_FIGHTER extends ORDER("2")
  case object C_ASSASSIN extends ORDER("3")
  case object C_SETTLEMENT extends ORDER("5")
  case object C_BARRACK extends ORDER("6")
}

sealed abstract class ORDER(val value : String){
  def getVal = value
}