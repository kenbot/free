package kenbot.free.tank.app

import java.awt.Color
import java.awt.Graphics2D
import scala.swing.Component
import kenbot.free.tank.model.Tank
import kenbot.free.tank.maths.Vec
import kenbot.free.tank.model.Missile
import kenbot.free.tank.model.World
import kenbot.free.tank.model.Entity


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

