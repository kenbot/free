package free.tank

import java.awt.Color
import java.awt.Graphics2D
import scala.swing.Component
import scala.swing.MainFrame
import scala.swing.Panel
import scala.swing.SimpleSwingApplication
import scala.swing.Swing.ActionListener
import scala.swing.Swing.pair2Dimension
import Moves.AI

import free.tank.Moves.AI
import scala.util.Random


object StartingState {
  import Moves._
  
  def tankAI: AI[Unit] = for {
    _ <-  rotateTank(Angle.degrees(10)) 
    _ <- accelerate * 5
    _ <- delay * 2
    _ <- fire 
    _ <- tankAI
  } yield ()
  
  val tanks = List(
    Tank("1", Vec(100,100)) withAI tankAI)
    
   // Tank("2", Vec(200, 200)), 
   // Tank("3", Vec(300,300)))
      
  val world = World(Dim(500,500), tanks)
  val game = TankGame(world, EasyTankAI, 0)
}


object TankApp extends SimpleSwingApplication {
      
  var game = StartingState.game

  lazy val top = new MainFrame {
    contents = new Panel with PaintWorld {
      def world = game.world
    }
    title = "Free Monad Tanks"
    size = (1024, 768)
    centerOnScreen()
    
  }
  
  val gameTimer = new javax.swing.Timer(40, ActionListener {_ => 
    game = game.runFrame
    val t1 = game.world.find(EntityId("1")).get
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

trait PaintWorld extends Component {
    
  def world: World
  
  background = Color.lightGray
  
  override def paintComponent(g: Graphics2D): Unit = {
    super.paintComponent(g)
    g.setColor(Color.red)
    
    paintWorld(g)
  }
  
  def paintWorld(g: Graphics2D): Unit = {
    
    g.setColor(Color.white)
    g.fillRect(world.bounds.x1.toInt, world.bounds.y1.toInt, world.bounds.width.toInt, world.bounds.height.toInt)
    
    g.setColor(Color.black)
    g.drawRect(world.bounds.x1.toInt, world.bounds.y1.toInt, world.bounds.width.toInt, world.bounds.height.toInt)
    
    world.entities foreach paintEntity(g)
  }
  
  def paintEntity(g: Graphics2D)(e: Entity): Unit = e match {
    case t: Tank => paintTank(g, t)
    case m: Missile => paintMissile(g, m)
    case x => sys.error("Unexpected entity: " + x)
  }
  
  
  def paintTank(g: Graphics2D, t: Tank): Unit = {
    val (x1, y1) = t.pos.toIntTuple
    g.drawRect(x1 - 5, y1 - 5, 10, 10)
    val pt1 = t.pos
    val vec = Vec.fromAngle(t.facing, 20.0)
    
    val pt2 = t.pos + vec
    val (x2, y2) = pt2.toIntTuple
    g.drawLine(x1, y1, x2, y2)
  }
  
  
  def paintMissile(g: Graphics2D, m: Missile): Unit = {
    val (x,y) = m.pos.toIntTuple
    g.setColor(Color.red)
    g.fillOval(x-3, y-3, 6, 6)
  }
}

