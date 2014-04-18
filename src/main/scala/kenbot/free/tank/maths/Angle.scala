package kenbot.free.tank.maths

import scala.annotation.tailrec
import scala.math.floor

object Angle {
  
  val Pi = math.Pi
  val TwoPi = 2 * Pi
  val PiOver2 = Pi / 2
  
  val Zero = Angle(0)
  val Quarter = Angle(Pi/2)
  val Half = Angle(Pi)
  val ThreeQuarters = Angle(3*Pi/2)
  val Full = Angle(TwoPi)
  
  implicit class DoubleOps(d: Double) {
    def degrees = Angle.degrees(d)
    def radians = Angle(d)
  }
  
  def degrees(degs: Double) = Angle(degs * Pi / 180.0)
  
  def normalize(radians: Double): Double = {
    val fullCycles = (radians / TwoPi).asInstanceOf[Int]
    val possiblyNegative = radians - TwoPi * fullCycles
    
    if (possiblyNegative < 0) possiblyNegative + TwoPi 
    else possiblyNegative
  }
  
  def apply(radians: Double) = new Angle(normalize(radians))
}

class Angle private (val radians: Double) extends AnyVal with Ordered[Angle] {
  import Angle.{Pi, Zero, Half}
  
  def sin: Double = math.sin(radians)
  def cos: Double = math.cos(radians)
  def tan: Double = math.tan(radians)
  def opposite: Angle = Angle(radians + Pi)
  def degrees: Double = radians * 180.0 / Pi
  def unary_- = Angle(-radians)
  def +(other: Angle) = Angle(radians + other.radians)
  def -(other: Angle) = Angle(radians - other.radians)
  def *(factor: Double) = Angle(radians * factor)
  def /(factor: Double) = Angle(radians / factor)
  def compare(a: Angle): Int = {
    if (this == a) 0 
    else if (this.radians < a.radians) -1
    else +1
  }
  
  private def shiftSin(x: Double) = math.sin(x - radians - Pi)
  
  def isLeftOf(a: Angle): Boolean = 
    (a == opposite) || (a != this && shiftSin(a.radians) < 0)
  
  def isRightOf(a: Angle): Boolean = 
    (a == opposite) || (a != this && shiftSin(a.radians) > 0)
  
  def distanceTo(a: Angle): Angle = {
    val diff = a - this
    if (diff < Angle.Half) diff else -diff
  }
  
  def addUpTo(add: Angle, upTo: Angle): Angle = {
    val added = this + add
    if (isLeftOf(upTo) != added.isLeftOf(upTo)) upTo else added
  }
  
  override def toString() = s"Angle($radians)"
}