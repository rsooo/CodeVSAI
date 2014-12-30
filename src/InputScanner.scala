import java.io.File

import scala.io.Source

/**
 * Created by a-saitoh on 2014/12/28.
 */
class InputScanner(sc : java.util.Scanner) {

//  val sc = new java.util.Scanner(System.in)
//  val sc = new java.util.Scanner(new File("input.txt"))

    def input() = {
      val remainingTime, currentStage, currentTurn, currentResource, myNum = sc.nextInt()
      val myUnitList = List.fill[(Int,Int,Int,Int,Int)](myNum)((sc.nextInt(),sc.nextInt(),sc.nextInt(),sc.nextInt(), sc.nextInt()))
      val opNum = sc.nextInt()
      val opUnitList = List.fill[(Int,Int,Int,Int,Int)](opNum)((sc.nextInt(),sc.nextInt(),sc.nextInt(),sc.nextInt(), sc.nextInt()))
      val resNum = sc.nextInt()
      val resList = List.fill[(Int,Int)](resNum)((sc.nextInt(), sc.nextInt()))
      val END = sc.next()
      if(END != "END") System.err.println("parse error: Found: " + END + ", Expect : END")

      //println(new InputData(remainingTime, currentStage, currentTurn, currentResource, myNum, myUnitList, opNum, opUnitList, resNum, resList).toString);
      val in = new InputData(remainingTime, currentStage, currentTurn, currentResource, myNum, myUnitList, opNum, opUnitList, resNum, resList)
//      println (in.toString)
//      for(unitdata <- in.myUnitList){
//        println (unitdata)
//      }
      in

  }



}

case class InputData(remainingTime : Int, currentStage : Int, currentTurn : Int, currentResource : Int,
                     myNum : Int, myUnitList : List[(Int, Int, Int, Int, Int)], opNum : Int, opUnitList : List[(Int, Int, Int, Int, Int)],
                     resNum : Int, resList : List[(Int,Int)]
                      ){

}
