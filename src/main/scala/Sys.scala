import java.awt.event.ActionEvent
import java.io.File

import scala.io.Source
import javax.swing.AbstractAction

import scala.swing.Reactions
import scala.swing.event.Event
import scala.util.Try

object Sys {

  val appCatalog:String = System.getProperty("user.dir") + File.separator
  val presetsCatalog:String = System.getProperty("user.dir") + File.separator + "presets" + File.separator

  val appIniFileName = "app.ini"

  var pref = Pref()

  def onStartApp() = {
    pref.load()
  }

  def onExitApp() = {
    pref.save()
  }

  def loadFile(fullFileName:String):String = {
    val buf = Source.fromFile(fullFileName)
    val str = buf.getLines().mkString(System.lineSeparator())
    buf.close()
    str
  }

  def saveFile(fullFileName:String, data:String ) = {
    import java.nio.file.{Paths, Files}
    import java.nio.charset.StandardCharsets

    Files.write(Paths.get(fullFileName), data.getBytes(StandardCharsets.UTF_8))
  }

  def savePreset(name:String, s:String ) = {
    saveFile( presetsCatalog + name + ".txt", s)
  }

  def loadPreset(name:String):String = {
    loadFile(presetsCatalog + name + ".txt")
  }

  def deletePreset(name:String) = {
    val file = new File(presetsCatalog + name + ".txt")
    file.delete()
  }

  def getListPresets():List[String] = {
    val file = new File(presetsCatalog)
    Try { file.listFiles.map( _.getName.stripSuffix(".txt") ).toList }.getOrElse(List.empty[String])
  }
}


case class Tick(source: Timer) extends Event
case class Timeout(source: Timer) extends Event

abstract class Timer {
  private val timer = this
  private var counter = 0
  private val tick = new AbstractAction(){def actionPerformed(e:ActionEvent) = {
    reactions(Tick(timer))
    if(_repeats > 0){
      counter -= 1
      if(counter == 0){
        run = false; reactions(Timeout(timer))
      }
    }
  }
  }
  val reactions: Reactions = new Reactions.Impl

  private var _interval = 1000
  def interval:Int = _interval
  def interval_=(i:Int):Unit = {_interval = i; peer.setDelay(i)}

  private var _repeats = -1
  def repeats:Int = _repeats
  def repeats_=(i:Int):Unit = {_repeats = i; counter = i}

  private var _run = false
  def run:Boolean = _run
  def run_=(f:Boolean):Unit = { _run = f; runStop(f)}

  private def runStop(f:Boolean) = f match{
    case true =>
      counter = _repeats
      if(counter != 0){peer.start()}else{reactions(Timeout(timer))}

    case false => peer.stop()}
  val peer = new javax.swing.Timer(_interval, tick); peer.setRepeats(true)
}