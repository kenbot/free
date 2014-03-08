package free.tank

import scala.math._
import scala.annotation.tailrec


object Percentage {
  
  val One = Percentage(1.0)
  val Zero = Percentage(0.0)
  
  def apply(amount: Double): Percentage = new Percentage(normalize(amount))
  
  def normalize(amount: Double): Double = {
    if (amount < 0) 0
    else if (amount > 1) 1
    else amount
  }
}

class Percentage(val amount: Double) extends AnyVal {
  def +(other: Percentage) = Percentage(amount + other.amount)
  def *(other: Percentage) = Percentage(amount * other.amount)
  def complement = Percentage(1.0 - amount)
}


object Angle {
  
  val TwoPi = 2 * Pi
  val PiOver2 = Pi / 2
  
  val Zero = Angle(0)
  val Quarter = Angle(Pi/2)
  val Half = Angle(Pi)
  val ThreeQuarters = Angle(3*Pi/2)
  val Full = Angle(TwoPi)
  
  
  def degrees(degs: Double) = Angle(degs * Pi / 180.0)
  
  @tailrec 
  def normalize(radians: Double): Double = {
    if (radians < 0) normalize(radians + TwoPi)
    else if (radians >= TwoPi) normalize(radians - TwoPi)
    else radians
  }
  
  def apply(radians: Double) = new Angle(normalize(radians))
}

class Angle(val radians: Double) extends AnyVal {
  def sin: Double = math.sin(radians)
  def cos: Double = math.cos(radians)
  def tan: Double = math.tan(radians)
  def opposite: Angle = (this + Angle.Half)
  def degrees: Double = radians * 180.0 / Pi
  def unary_- = Angle(radians + Pi)
  def +(other: Angle) = Angle(radians + other.radians)
  def -(other: Angle) = Angle(radians - other.radians)
  def *(factor: Double) = Angle(radians * factor)
  def /(factor: Double) = Angle(radians / factor)
    
  override def toString() = s"Angle($radians)"
}

case class Dim(width: Double, height: Double) {
  def toTuple = (width, height)
}

case class Rect(x1: Double, y1: Double, x2: Double, y2: Double) {
  def width = x2 - x1
  def height = y2 - y1
  def bounds = Dim(width, height)
  
  def topLeft = Vec(x1, y1)
  def topRight = Vec(x2, y1)
  def bottomRight = Vec(x2, y2)
  def bottomLeft = Vec(x1, y2)
  def center = Vec(x1 + width/2, y1 + height/2)
  
  def containsX(x: Double) = x >= x1 && x < x2
  def containsY(y: Double) = y >= y1 && y < y2
  def containsPt(pt: Vec) = containsX(pt.x) && containsY(pt.y)
  def containsRect(rect: Rect) = (containsX(rect.x1) && containsX(rect.x2) 
                               && containsY(rect.y1) && containsY(rect.y2))
                          
  def cropX(x: Double) = if (x >= x2) x2-1
                         else if (x < x1) x1
                         else x
                         
  def cropY(y: Double) = if (y >= y2) y2-1
                         else if (y < y1) y1
                         else y
                         
  def cropPt(pt: Vec) = Vec(cropX(pt.x), cropY(pt.y))
  
  def wrapX(x: Double) = x % width + x1
  def wrapY(y: Double) = y % height + y1
  def wrapPt(pt: Vec) = Vec(wrapX(pt.x), wrapY(pt.y))
  
  def centreInRect(outerRect: Rect): Rect = {
    val center = outerRect.center
    val newX1 = center.x - width/2
    val newY1 = center.y - height/2
    Rect(newX1, newY1, newX1 + width, newY1 + height)
  }
}


object Vec {
  def fromAngle(angle: Angle, length: Double) = Vec(length * angle.cos, length * angle.sin)
  val Zero = Vec(0,0)
}

case class Vec(x: Double, y: Double) {
  def angle: Angle = {
    if (x > 0) Angle(atan(y/x)) 
    else if (x < 0) Angle(atan(y/x) + Pi)
    else {
      if (y <= 0) Angle.Zero
      else Angle.Quarter
    }
  }
    
  def length: Double = sqrt(x*x + y*y)
  def unary_- = Vec(-x, -y)
  def +(vec: Vec) = Vec(x + vec.x, y + vec.y)
  def -(vec: Vec) = Vec(x - vec.x, y - vec.y)
  def withLength(len: Double) = Vec.fromAngle(angle, len)
  def *(factor: Double) = Vec.fromAngle(angle, length * factor)
  def rotate(by: Angle) = Vec.fromAngle(angle + by, length)
  def /(factor: Double): Vec = this * (1.0 / factor)           
  def toTuple: (Double, Double) = (x,y)
  def toIntTuple: (Int, Int) = (x.round.toInt, y.round.toInt)
  def between(vec: Vec) = this - vec
  def distanceTo(pt: Vec) = between(pt).length
  def angleTo(pt: Vec) = between(pt).angle
}
