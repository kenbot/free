package kenbot.free.tank.app

import java.awt.Color
import java.awt.Graphics2D
import scala.swing.Component
import kenbot.free.tank.model.Tank
import kenbot.free.tank.maths.Vec
import kenbot.free.tank.model.Missile
import kenbot.free.tank.model.World
import kenbot.free.tank.model.Entity
import kenbot.free.tank.maths.Rect


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
    fillRect(g, world.bounds)
    
    g.setColor(Color.black)
    paintRect(g, world.bounds)

    world.entities foreach paintEntity(g)
  }
  
  def paintRect(g: Graphics2D, r: Rect) {
    val (x, y, w, h) = r.toSizeIntTuple
    g.drawRect(x, y, w, h)
  }
  
  def fillRect(g: Graphics2D, r: Rect) {
    val (x, y, w, h) = r.toSizeIntTuple
    g.fillRect(x, y, w, h)
  }
  
  def paintEntity(g: Graphics2D)(e: Entity): Unit = e match {
    case t: Tank => paintTank(g, t)
    case m: Missile => paintMissile(g, m)
    case x => sys.error("Unexpected entity: " + x)
  }

  
  def paintTank(g: Graphics2D, t: Tank): Unit = {
    g.setColor(if (t.dead) Color.gray else Color.black)
    
    fillRect(g, t.bounds)
    
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

