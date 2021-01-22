//import javafx.application.Application
import scala.swing.Swing._
import scala.swing._
import scala.swing.event._
import scala.util.Try

import scala.concurrent.blocking

object LifeForm extends SimpleSwingApplication {

  LifeModel.init(120).setCell(14,15, true).setCell(15,15, true).setCell(16,15, true)

  val timer = new Timer{
    interval = Sys.pref.delay //In milliseconds
    //repeats = 10   //Repeats 10 times
    reactions += {
      case Tick(_) =>
        blocking {
          if(!LifeModel.isStopped) LifeModel.doIteration()
          interval = Sys.pref.delay
        }
        workPlacePanel.repaint()

      case Timeout(_) => //println("timeout")
    }
    run = true
  }


  lazy val workPlacePanel: Panel = new Panel {

    preferredSize = (800, 600)

    focusable = true
    listenTo(mouse.clicks, mouse.moves, keys)

    reactions += {
      case e: MousePressed =>
        requestFocusInWindow()
        val (x, y) = getXYFromScreenPoint(e.point)
        val v = LifeModel.getCell(x, y)
        LifeModel.setCell(x, y, !v)


      case e: MouseDragged =>
      case e: MouseReleased =>
      case KeyTyped(_, 'c', _, _) =>

      case _: FocusLost => repaint()
    }

    def getXYFromScreenPoint(p:Point):(Int,Int) ={
      val sizeCell = size.height / LifeModel.size
      val offsetX = (size.width - (LifeModel.size * sizeCell))/2
      val offsetY = 0
      val x = (p.x - offsetX)/sizeCell
      val y = (p.y - offsetY)/sizeCell
      (x,y)
    }

    private def getColorCell(v0:Boolean, v1:Boolean, v2:Boolean, v3:Boolean, v4:Boolean):Color = { // на основе истории

      val NEW = Sys.pref.colorNew  // зеленый
      val REGULAR = Sys.pref.colorRegular  // белый
      val OLD = Sys.pref.colorOld  // оранжевый
      val DEAD1 = Sys.pref.colorDead1 // серый
      val DEAD2 = Sys.pref.colorDead2
      val DEAD3 = Sys.pref.colorDead3
      val DEAD4 = Sys.pref.colorDead4
      val EMPTY = Sys.pref.colorEmpty

      (v0, v1, v2, v3, v4) match {
        case (_, _, _, false, true) => NEW // есть только в последнем - цвет только что появившегося
        case (_, _, true ,true, true) => OLD // был долгое время
        case (_, _, false, true, true) => REGULAR // есть в последнем и предыдущем - цвет обычного
        case (_, _, _, true, false) => DEAD1
        case (_, _, true, false, false) => DEAD2
        case (_, true, false, false, false) => DEAD3
        case (true, false, false, false, false) => DEAD4
        case _ => EMPTY
      }
    }

    private def getColorCellBW(v0:Boolean, v1:Boolean, v2:Boolean, v3:Boolean, v4:Boolean):Color = { // на основе истории
      val REGULAR = Sys.pref.colorRegular  // белый
      val EMPTY = Sys.pref.colorEmpty // черный
      if (v4) REGULAR else EMPTY
    }

    override def paintComponent(g: Graphics2D): Unit = {
      super.paintComponent(g)

      val h = size.height

      val sizeCell = h / LifeModel.size
      val offsetX = (size.width - (LifeModel.size * sizeCell))/2

      blocking {
        LifeModel.forEach{
          case ((x, y), (v0, v1, v2, v3, v4)) =>
            val color = if(Sys.pref.transitionBW) getColorCellBW(v0,v1,v2,v3,v4) else getColorCell(v0,v1,v2,v3,v4)
            g.setColor(color)
            g.fillRect(offsetX + x * sizeCell, y * sizeCell, sizeCell, sizeCell)

            if(Sys.pref.drawGrid) {
              g.setColor(Sys.pref.colorGrid)

              g.fillRect(offsetX + x * sizeCell, y * sizeCell, sizeCell, 1)
              g.fillRect(offsetX + x * sizeCell, y * sizeCell, 1, sizeCell)
            }
        }
      }

    }
  }

  val toolBar = new BoxPanel(Orientation.Horizontal) {

    border = Swing.EmptyBorder(4)

    contents += Swing.HStrut(16)
    contents += new Label {
      text = "  "
      horizontalAlignment = Alignment.Right
    }
    contents += new Button("Start/Stop"){


      reactions += { case _ =>
        LifeModel.isStopped = !LifeModel.isStopped
      }

    }
    contents += new Label("  ")
    contents += new Button("+") {
      reactions += { case _ => if(Sys.pref.delay>5) Sys.pref.delay = ( Sys.pref.delay / 1.2 ).toInt }
    }
    contents += new Button("–") {
      reactions += { case _ => if(Sys.pref.delay<2000) Sys.pref.delay = ( Sys.pref.delay * 1.2 ).toInt }
    }
    contents += new Label("  ")
    contents += new Button("Preferences..") {
      reactions += {
        case _ =>
          val st = LifeModel.isStopped
          LifeModel.isStopped = true
          pref.open()
          LifeModel.isStopped = st
      }

    }

    contents += Swing.HGlue

    contents += new Button("Save") {
      reactions += { case _ =>
        LifeModel.isStopped = true
        val r = Dialog.showInput(contents.head, "New preset name:", initial = "")
        r match {
          case Some(namePreset) =>
            // сохраняем в файл с указанным именем включенные точки на поле
            val str = LifeModel.board.getBoardToString()
            Sys.savePreset(namePreset, str)
            // обновляем список в ComboBox
            val count = comboBoxPresets.peer.getItemCount
            var seq:Seq[String] = for{ i <- 0 until count} yield comboBoxPresets.peer.getItemAt(i)
            seq = seq :+ namePreset
            comboBoxPresets.peer.setModel(ComboBox.newConstantModel(seq))
          case _ =>
        }

      }
    }

    contents += new Label("  ")
    val comboBoxPresets = new ComboBox[String](Seq("one","two","three")) {
    }

    val lstPresets = Sys.getListPresets()
    comboBoxPresets.peer.setModel(ComboBox.newConstantModel(lstPresets))


    listenTo(comboBoxPresets.selection)
    reactions += {
      case SelectionChanged(`comboBoxPresets`) =>
        val i = comboBoxPresets.peer.getSelectedIndex()
        if(i != -1) {
          val name = comboBoxPresets.peer.getItemAt(i)
          val str = Sys.loadPreset(name)
          LifeModel.board.setBoardFromString(str)
          LifeModel.isStopped = false
        }
    }

    contents += comboBoxPresets

    contents += new Label("  ")
    contents += new Button("Delete") {
      reactions += { case _ =>
        val se = comboBoxPresets.peer.getSelectedIndex
        if(se != -1){
          val v = comboBoxPresets.peer.getItemAt( se )
          if (Dialog.showConfirmation(message = "Delete preset \"" + v + "\" ?") == Dialog.Result.Yes)
          {
            Sys.deletePreset(v)
            comboBoxPresets.peer.setModel(ComboBox.newConstantModel(Sys.getListPresets()))
          }
        }
      }
    }

  }



  def pref = new Dialog() {

    val _pref:Pref = Sys.pref.copy()

    preferredSize = (480,480)
    resizable = false
    size = (480,480)
    title = "Preferences"
    modal = true
    peer.setLocationRelativeTo(null) // центрируем диалог

    trait KeyFirer {
      reactions += {
        case KeyPressed(_, Key.Escape, _, _) => println("Key press")
      }
    }

    val transitionBW = new CheckBox("Transition Black/White")
    {
      reactions += {
        case _ => _pref.transitionBW = selected
      }
    }
    transitionBW.selected = _pref.transitionBW

    val drawGrid = new CheckBox("Draw grid")
    {
      reactions += {
        case _ => _pref.drawGrid = selected
      }
    }
    drawGrid.selected = _pref.drawGrid

    val delay = new TextField (_pref.delay.toString, 4) {
      reactions += {
        case _ => _pref.delay = Try{ text.toInt }.getOrElse(30)
      }
    }

    def restrictHeight(s: Component) = s.maximumSize = new Dimension(Short.MaxValue, s.preferredSize.height)

    class ButtonColor(hintName:String, get : => Color, set: Color => () ) extends Button {
      text = "Choose"; background = get
      reactions += { case _ =>
        val oldColor = get
        val newColor = ColorChooser.showDialog(this, hintName, oldColor).getOrElse(oldColor)
        background = newColor
        set(newColor)
      }
    }

    class BoxPanelColor(labelText:String, get : => Color, set: Color => () ) extends BoxPanel(Orientation.Horizontal) {
      contents += new ButtonColor(labelText, get, set )
      contents += Swing.HStrut(8)
      contents += new Label(labelText)
      contents += Swing.HGlue
    }

    restrictHeight(delay)

    contents = new BoxPanel(Orientation.Vertical) {
      focusable = true
      listenTo(this.keys, keys)
      requestFocus
      reactions += {
        case KeyPressed(_, Key.Escape, _, _) =>
          close()
      }

      contents += Swing.VStrut(4)

      contents += new BoxPanel(Orientation.Horizontal) {
        contents += transitionBW
        contents += Swing.HGlue
      }
      contents += new BoxPanel(Orientation.Horizontal) {
        contents += drawGrid
        contents += Swing.HGlue
      }

      contents += new BoxPanel(Orientation.Horizontal) {
        contents += new Label("Delay (ms):")
        contents += Swing.HStrut(5)
        contents += delay
        contents += Swing.HGlue
      }

      contents += Swing.VStrut(5)

      contents += new BoxPanelColor("Grid color", { _pref.colorGrid }, { _pref.colorGrid = _ })
      contents += new BoxPanelColor("New color", { _pref.colorNew }, { _pref.colorNew = _ })
      contents += new BoxPanelColor("Regular color", { _pref.colorRegular }, { _pref.colorRegular = _ })
      contents += new BoxPanelColor("Old color", { _pref.colorOld }, { _pref.colorOld = _ })
      contents += new BoxPanelColor("Empty color", { _pref.colorEmpty }, { _pref.colorEmpty = _ })
      contents += new BoxPanelColor("Dead1 color", { _pref.colorDead1 }, { _pref.colorDead1 = _ })
      contents += new BoxPanelColor("Dead2 color", { _pref.colorDead2 }, { _pref.colorDead2 = _ })
      contents += new BoxPanelColor("Dead3 color", { _pref.colorDead3 }, { _pref.colorDead3 = _ })
      contents += new BoxPanelColor("Dead4 color", { _pref.colorDead4 }, { _pref.colorDead4 = _ })

      contents += Swing.VStrut(16)
      contents += Swing.VGlue

      contents += new BoxPanel(Orientation.Horizontal) {
        contents += Button("  Ok  ") {
          Sys.pref = _pref
          close()
        }
        contents += Swing.HGlue
        contents += Button("Cancel") {
          close()
        }
      }

      //for (e <- contents)  e.xLayoutAlignment = 0.0
      border = Swing.EmptyBorder(10, 10, 10, 10)
    }

  }


  def top: Frame = new MainFrame {

    title = "Life"

    contents = new BorderPanel {
      add( toolBar, BorderPanel.Position.North)
      add( workPlacePanel, BorderPanel.Position.Center)
    }


  }

}


