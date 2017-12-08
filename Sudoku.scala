import cmpsci220.hw.sudoku._
import scala.util.control.Breaks._

object Solution extends SudokuLike {
  type T = Board

  def parse(str: String): Board = {
   val startMap=(0.to(80).toList map{x=>(x/9,x%9)->1.to(9).toList}).toMap
   val origRem=((0.to(80).toList zip str.toList).filter {x=>x._2!='.'} map{x=>(x._1/9,x._1%9)->(List(x._2.toString.toInt))}).toMap

   def pHelp(b:Map[(Int, Int), List[Int]],count:Int): Map[(Int, Int), List[Int]]={
      if(str(count)!='.'){
      val thisCell=Map((count/9, count%9)->List(str(count).toString.toInt))
      val ps=peers(count/9, count%9)
      val upB1=(ps map{x=>x->b(x).diff(List(str(count).toString.toInt))}).toMap
      val upB=thisCell++upB1++(b--thisCell.keys--upB1.keys)
      if(count==80) upB else pHelp(upB,count+1)
      }else if(count==80) b else pHelp(b,count+1)
    }

  def pHelp2(updatedMap:Map[(Int,Int),List[Int]],removeMap:Map[(Int,Int),List[Int]]): Map[(Int,Int),List[Int]]={
      val deferToRem= removeMap ++ (updatedMap -- removeMap.keys)
      val remPath=(removeMap.keys map{x=>peers(x._1,x._2) map{y=> y->removeMap(x)}}).flatten.toMap
      val peersNotLen1=remPath filterKeys {x=> deferToRem(x).length>1}
      val removeValue=(peersNotLen1.keys map{x=>x->deferToRem(x).diff(remPath(x))}).toMap
      val newLenOneCell= removeValue filterKeys {x=>removeValue(x).length==1}
      val udMap2=newLenOneCell++removeValue++(deferToRem-- peersNotLen1.keys)
      if(newLenOneCell.keys.toList.length==0) udMap2
        else pHelp2(udMap2, newLenOneCell)
      }

   val remMap=pHelp(startMap,0)
   val remFilt=remMap filterKeys {x=>remMap(x).length==1}
   new Board(pHelp2(remMap, remFilt--origRem.keys))   
  }


  def peers(row:Int, col:Int):List[(Int,Int)]={
    val b1=0.to(2).toList
    val b2=3.to(5).toList
    val b3=6.to(8).toList

    val boundList=List(b1,b2,b3)

    val intToBoundMap=(boundList map{x=>x map{y=>y->x}}).flatten.toMap

    val bp = for{i<-intToBoundMap(row)
           j<-intToBoundMap(col)}yield (i,j)

    val cp=(0.to(8).toList zip List.fill(9)(col)).toSet
    val rp=(List.fill(9)(row) zip 0.to(8).toList).toSet

    (cp ++ rp ++ bp.toSet -- Set((row,col))).toList
  }
}  
// Top-left corner is (0,0). Bottom-right corner is (8,8).
// You don't have to have a field called available. Feel free to change it.
class Board(val available: Map[(Int, Int), List[Int]]) extends BoardLike[Board] {

  def availableValuesAt(row: Int, col: Int): List[Int] = {
    // Assumes that a missing value means all values are available. Feel
    // free to change this.
    available.getOrElse((row, col), 1.to(9).toList)
  }

  def valueAt(row: Int, col: Int): Option[Int] = {
    if(availableValuesAt(row,col).length==1)Some(availableValuesAt(row,col)(0)) else None
  }

  def isSolved(): Boolean = {
    available.values.forall{x=>x.length==1}
  }

  def isUnsolvable(): Boolean = {
     available.values.toList.contains(Nil)
  }

  def place(row: Int, col: Int, value: Int): Board = {
    require(availableValuesAt(row, col).contains(value))
    val newVal=Map((row,col)->List(value))
   
    def boardPeers(tup:(Int,Int)):List[(Int,Int)]=Solution.peers(tup._1,tup._2)
    
    def placeHelp(updatedMap:Map[(Int,Int),List[Int]],removeMap:Map[(Int,Int),List[Int]]): Map[(Int,Int),List[Int]]={
      val deferToRem= removeMap ++ (updatedMap -- removeMap.keys)
      val remPath=(removeMap.keys map{x=>boardPeers(x) map{y=> y->removeMap(x)}}).flatten.toMap

      val peersNotLen1=remPath filterKeys {x=> deferToRem(x).length>1}
      val removeValue=(peersNotLen1.keys map{x=>x->deferToRem(x).diff(remPath(x))}).toMap

      val peersLen1SameVal=remPath filterKeys {x=> deferToRem(x)==remPath(x)}
      val removeValueSame=(peersLen1SameVal.keys map{x=>x->deferToRem(x).diff(remPath(x))}).toMap

      val newLenOneCell= removeValue filterKeys {x=>removeValue(x).length==1}
      val udMap2=removeValueSame++newLenOneCell++removeValue++((deferToRem-- peersNotLen1.keys)--removeValueSame.keys)
      if(newLenOneCell.keys.toList.length==0) udMap2
        else placeHelp(udMap2, newLenOneCell)
      }
    new Board(placeHelp(available,newVal))
  }

  // You can return any Iterable (e.g., Stream)
  def nextStates(): List[Board] = {
    val nextStatesUnsorted=for{x<-0.to(80).toList
                              if valueAt(x/9,x%9)==None
                              y=availableValuesAt(x/9,x%9).toSet
                              c<-y
                              }yield place(x/9,x%9,c)               
    val sorted = nextStatesUnsorted.sortWith{(a,b)=>
        ((a.available filterKeys (x=>available(x).length==1)).values.toList.length > (b.available filterKeys (x=>available(x).length==1)).values.toList.length)}
    sorted
  }
  def nextStatesHelp():List[Board]={
    val nextStates1=available filterKeys{x=>available(x).length>1}
    val nextStatesKey=nextStates1.keys.toList(0)
    val nextStatesOneCell=List(nextStatesKey) map{x=> availableValuesAt(x._1,x._2) map {y=> place(x._1,x._2,y)}}
    nextStatesOneCell.flatten
  }

  def solve(): Option[Board] = {

    if(this.isSolved()) return Some(this)
    if(this.isUnsolvable()) return None
    else{
      val next= nextStatesHelp()
      for(i<-next){
        i.solve match{
          case Some(sol)=>return Some(sol)
          case None=>()
        }
      } 
    None }
  }     
}  