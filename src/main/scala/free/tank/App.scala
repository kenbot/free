package free.tank

import scala.swing.MainFrame
import scala.swing.SimpleSwingApplication
import java.awt.Dimension
import scala.swing.Panel
import java.awt.Graphics2D
import java.awt.Color


object TankMain extends SimpleSwingApplication {
  val tanks = List(
      Tank("1", Vec.Zero), 
      Tank("2", Vec(4, 7)), 
      Tank("3", Vec(2,9)))
      
  val world = TankWorld(tanks) 

  val top = new MainFrame {
    size = new Dimension(500,500)
    contents = new Panel {
      background = Color.white
      
      override def paintComponent(g: Graphics2D): Unit = {
        super.paintComponent(g);
        g setColor Color.red
        world.tanks foreach { t => 
          g.drawRect(t.pos.x.toInt, t.pos.y.toInt, 10, 30)
        }
        
      }
    }
    pack()
  }
}