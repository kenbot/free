package free.tank

import scala.swing.MainFrame
import scala.swing.SimpleSwingApplication
import java.awt.Dimension
import scala.swing.Panel
import java.awt.Graphics2D
import java.awt.Color
import scala.swing.Component
import scala.swing.Swing._


object StartingState {
  import TankMoves._
  
  val tanks = List(
    Tank("1", Vec.Zero) withAI (for {
      _ <- rotateTank(Angle.degrees(20)) 
      _ <- accelerate
      _ <- rotateTank(Angle.degrees(20))
      _ <- fire
    } yield ()),
    
    Tank("2", Vec(4, 7)), 
    Tank("3", Vec(2,9)))
      
  val world = TankWorld(tanks)
  val game = TankGame(world, EasyTankAI, 0)
}


object TankApp extends SimpleSwingApplication {
      
  var game = StartingState.game

  lazy val top = new MainFrame {
    contents = new Panel with PaintTanks {
      def tanks = game.world.tanks
      background = Color.white
    }
    title = "Free Monad Tanks"
    size = (1024, 768)
    centerOnScreen()
    
  }
  
  val gameTimer = new javax.swing.Timer(50, ActionListener {_ => 
    game = game.runFrame
    println("RAN FRAME " + game.frame)
    top.repaint()
  })
  
  override def startup(args: Array[String]): Unit = {
    super.startup(args)
    gameTimer.start()
  }
  
  override def shutdown(): Unit = gameTimer.stop()


}

trait PaintTanks extends Component {
    
  def tanks: Seq[Tank]
  
  override def paintComponent(g: Graphics2D): Unit = {
    super.paintComponent(g)
    g.setColor(Color.red)
    tanks foreach paintTank(g)
  }
  
  def paintTank(g: Graphics2D)(t: Tank): Unit = {
    g.drawRect(t.pos.x.toInt, t.pos.y.toInt, 10, 30)
  }
}





object Repl {
  val tankA0 = Tank("a", Vec(5,4))
  val tankB0 = Tank("b", Vec(9,8))
      
  val world0 = TankWorld(List(tankA0, tankB0)) 
  
  import TankMoves._
  
  val ai = for {
    _ <- rotateTank(Angle.Quarter / 2)
    _ <- rotateTank(Angle.Quarter / 2)
    _ <- rotateTank(Angle.Quarter / 2)
  } yield ()
  
  implicit class TankPrinter(tank: Tank) {
    def toStr = s"Tank(${tank.pos.toTuple}, ${tank.facing.degrees})"
    def print(): Unit = println(toStr)
  }
  
  implicit class WorldPrinter(world: TankWorld) {
    def print(): Unit = {
      println(world.tanks.map(_.toStr) mkString ", ")
    }
  }
  
}