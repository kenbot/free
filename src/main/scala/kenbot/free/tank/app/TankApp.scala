package kenbot.free.tank.app

import scala.swing.MainFrame
import scala.swing.Panel
import scala.swing.SimpleSwingApplication
import scala.swing.Swing.ActionListener
import scala.swing.Swing.pair2Dimension


object TankApp extends SimpleSwingApplication {
      
  var game = StartingState.game

  lazy val top = new MainFrame {
    contents = new Panel with PaintWorld {
      def world = game.world
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
