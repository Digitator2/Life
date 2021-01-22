import scala.collection.mutable

object LifeModel {

  val numBoards = 5
  private val que = new mutable.Queue[Board]()

  var size:Int =0

  var isStopped = false

  def board = que.last

  def init(size:Int) = {
    this.size = size
    (1 to numBoards).foreach( _ => que.addOne( Board(size).init()) )
    this
  }

  def setCell(x:Int, y:Int, v:Boolean) = {
    board.setCell(x,y,v)
    this
  }

  def getCell(x:Int, y:Int):Boolean = {
    board.getCell(x,y)
  }

  def doIteration() ={
    val newHm = board.hm.clone()
    val b = Board(newHm, size).doIteration()
    que.addOne(b)
    que.removeHead()
    this
  }

  def forEach( f:((Int, Int), (Boolean,Boolean,Boolean,Boolean,Boolean)) => Unit ) = {
    for {
      x <- 0 until size
      y <- 0 until size
    }{
      val v0 = que(0).hm((x,y))
      val v1 = que(1).hm((x,y))
      val v2 = que(2).hm((x,y))
      val v3 = que(3).hm((x,y))
      val v4 = que(4).hm((x,y))

      f((x,y),(v0,v1,v2,v3,v4))
    }
    this
  }

}



object Board {
  type HashMap = mutable.HashMap[(Int, Int), Boolean]
  type ListPos = List[(Int, Int)]

  def apply(size:Int): Board ={
    Board(new Board.HashMap, size).init()
  }
}

case class Board(hm: Board.HashMap, size:Int) {

  def init(size:Int =this.size): Board = {
    hm.clear()
    for {
      x <- 0 until size
      y <- 0 until size
    } {
      hm.put( (x,y), false )
    }
    this
  }

  private def filter(hm:Board.HashMap):Board.HashMap = {
      hm.filter{
        case ((x,y), _) => x<size && y<size
      }
  }

  def setListPos(listPos: Board.ListPos):Board = {
    listPos.foreach {
      case (x,y) => hm.put((x,y), true)
    }
    this
  }

  def setCell(x:Int, y:Int, v:Boolean):Board = {
    hm.put((x,y), v)
    this
  }

  def getCell(x:Int, y:Int):Boolean = {
    hm.getOrElse((x,y), false)
  }

  private def getNumberNeighborsForPos(x:Int, y:Int) = {
    val lst = Vector(
      (x-1,y-1), (x, y-1), (x+1, y-1),
      (x-1,y),             (x+1, y),
      (x-1,y+1), (x, y+1), (x+1, y+1)
    )

    lst.map{ case (x,y)   => hm.get(x, y)
    case _       => None
    }.map { case Some(x) if x   => 1
    case _              => 0
    }.sum
  }

  def doIteration(): Board = {
    val newPositions = for{ ((x,y),nonEmpty) <- hm
                            st = getNumberNeighborsForPos(x,y)
                            if (st == 3 || (st == 2 && nonEmpty))
                            if (x<size && y<size)} yield ((x,y),true)


    init()
    hm.addAll( newPositions )
    this
  }

  def setBoardFromString(s:String):Unit = {
    init()
    s.split(System.lineSeparator()).foreach( e => {
      val arr = e.split(",").map( _.toInt )
      arr match {
        case Array(x,y) => hm.put((x,y),true)
        case _ =>
      }
    } )
  }

  def getBoardToString():String = {
    var str =""
    for{ ((x,y),cellOn) <- hm  if cellOn }{ str += "" + x + "," + y + System.lineSeparator() }
    str
  }

}



