import java.awt.Color
import scala.collection.mutable
import scala.util.Try


case class Pref(
    var delay:Int = 30,
    var transitionBW:Boolean = false,
    var drawGrid :Boolean = true,

    var colorGrid:Color = new Color(30,30,30),

    var colorNew:Color = new Color(0,255,0),
    var colorRegular:Color = new Color(255,255,255),
    var colorOld:Color = new Color(255,150,0),
    var colorDead1:Color = new Color(127,127,127),
    var colorDead2:Color = new Color(50,50,50),
    var colorDead3:Color = new Color(20,20,20),
    var colorDead4:Color = new Color(7,7,7),
    var colorEmpty:Color = new Color(0,0,0)
  ) {

  val hashMap = new mutable.HashMap[String, String]

  private def colorToString(c:Color):String = {
    val r = c.getRed
    val g = c.getGreen
    val b = c.getBlue
    "Color(" + r + "," + g + "," + b + ")"
  }

  private def colorFromString(v:String):Color = {
    val arr = v.stripPrefix("Color(").stripSuffix(")").split(',')
    new Color(arr(0).toInt, arr(1).toInt, arr(2).toInt)
  }

  def load() = {

    varsToHash()

    val s:String = Try { Sys.loadFile(Sys.appCatalog + Sys.appIniFileName) }.getOrElse("")
    val arrLines:Array[String] = s.split(System.lineSeparator)
    for { kv <- arrLines if !kv.isEmpty }{
      val arrKV = kv.split("=")
      hashMap.put(arrKV(0).trim, arrKV(1).trim)
    }

    hashToVars()
  }

  def save() = {

    varsToHash()

    val str = hashMap.toString().stripPrefix("HashMap(").stripSuffix(")").
      replace(", ", System.lineSeparator).replace("->", "=")
    Sys.saveFile( Sys.appCatalog + "app.ini", str)
  }

  private def varsToHash():Unit = {
    hashMap.put("delay", delay.toString)
    hashMap.put("transitionBW", transitionBW.toString)
    hashMap.put("drawGrid", drawGrid.toString)
    hashMap.put("colorGrid", colorToString(colorGrid))
    hashMap.put("colorNew", colorToString(colorNew))
    hashMap.put("colorRegular", colorToString(colorRegular))
    hashMap.put("colorOld", colorToString(colorOld))
    hashMap.put("colorDead1", colorToString(colorDead1))
    hashMap.put("colorDead2", colorToString(colorDead2))
    hashMap.put("colorDead3", colorToString(colorDead3))
    hashMap.put("colorDead4", colorToString(colorDead4))
    hashMap.put("colorEmpty", colorToString(colorEmpty))
  }

  private def hashToVars():Unit = {
    delay = hashMap("delay").toInt
    transitionBW = hashMap("transitionBW").toBoolean
    drawGrid = hashMap("drawGrid").toBoolean
    colorGrid = colorFromString(hashMap("colorGrid"))
    colorNew = colorFromString(hashMap("colorNew"))
    colorRegular = colorFromString(hashMap("colorRegular"))
    colorOld = colorFromString(hashMap("colorOld"))
    colorDead1 = colorFromString(hashMap("colorDead1"))
    colorDead2 = colorFromString(hashMap("colorDead2"))
    colorDead3 = colorFromString(hashMap("colorDead3"))
    colorDead4 = colorFromString(hashMap("colorDead4"))
    colorEmpty = colorFromString(hashMap("colorEmpty"))
  }

}
