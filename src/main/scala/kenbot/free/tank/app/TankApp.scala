package kenbot.free.tank.app

import scala.swing.MainFrame
import scala.swing.Panel
import scala.swing.SimpleSwingApplication
import scala.swing.Swing.ActionListener
import scala.swing.Swing.pair2Dimension
import scala.swing.BorderPanel
import scala.swing.FlowPanel
import scala.swing.Button
import scala.swing.ButtonGroup
import scala.swing.event.ButtonClicked
import kenbot.free.tank.ai.{EasyTankAI, HardTankAI}


object TankApp extends SimpleSwingApplication {
      
  var game = StartingState.game

  lazy val top = new MainFrame {
    contents = new BorderPanel {
      val tankPanel = new Panel with PaintWorld {
        def world = game.world
      }
      
      val buttonPanel = new FlowPanel {
        val easyButton = new Button("Easy")
        val hardButton = new Button("Hard")
        contents += (easyButton, hardButton)
        
        val buttonGroup = new ButtonGroup(easyButton, hardButton)
        
        listenTo(easyButton)
        listenTo(hardButton)
        
        reactions += {
          case ButtonClicked(`easyButton`) => game = game withInterpreter EasyTankAI
          case ButtonClicked(`hardButton`) => game = game withInterpreter HardTankAI
        }
      }
      
      add(buttonPanel, BorderPanel.Position.North)
      add(tankPanel, BorderPanel.Position.Center)
    }
    title = "Free Monad Tanks"
    centerOnScreen()
    size = (1024, 768)
    
  }
  
  val gameTimer = new javax.swing.Timer(40, ActionListener {_ => 
    game = game.runFrame
    top.repaint()
  })
  
  override def startup(args: Array[String]): Unit = {
    super.startup(args)
    gameTimer.start()
  }
  
  override def shutdown(): Unit = {
    gameTimer.stop()
    super.shutdown()
  }
}
