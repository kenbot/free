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
  
  val tanks = List(
    Tank("1", Vec(100,100)) withAI loop(for {
      t <- findNearestTank
      _ <- aimAtTank(t)
      _ <- fire
      _ <- accelerate 
    } yield ()),
    
    Tank("2", Vec(200, 200)) withAI loop(for {
      t <- findNearestTank
      _ <- aimAwayFrom(t)
      _ <- accelerate * 20
    } yield ()),
    
    
    Tank("3", Vec(300,300)) withAI loop(for {
      _ <- moveTo(Vec(10,10))
      _ <- fire
      _ <- moveTo(Vec(300, 10))
      _ <- fire
      _ <- moveTo(Vec(300, 300))
      _ <- fire
      _ <- moveTo(Vec(10, 300))
      _ <- fire
    } yield ()))
      
  val world = World(Dim(1000,700), tanks)
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
    val scrW = size.width.toDouble
    val scrH = size.height.toDouble
    val worldW = world.bounds.width
    val worldH = world.bounds.height
    g.translate(scrW/2 - worldW/2, scrH/2 - worldH/2)
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
    val (rectX, rectY, rectW, rectH) = t.bounds.toSizeIntTuple
    
    g.fillRect(rectX, rectY, rectW, rectH)
    
    val (x1, y1) = t.pos.toIntTuple
    val vec = Vec.fromAngle(t.facing, 20.0)
    
    val (x2, y2) = (t.pos + vec).toIntTuple
    g.drawLine(x1, y1, x2, y2)
  }
  
  
  def paintMissile(g: Graphics2D, m: Missile): Unit = {
    val (x,y, width, height) = m.bounds.toSizeIntTuple
    g.setColor(Color.red)
    g.fillOval(x, y, width, height)
  }
}

