package kenbot.free.tank.maths

import scala.math.Pi
import scala.annotation.tailrec

object Angle {
  
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
  
  @tailrec 
  def normalize(radians: Double): Double = {
    if (radians < 0) normalize(radians + TwoPi)
    else if (radians >= TwoPi) normalize(radians - TwoPi)
    else radians
  }
  
  def apply(radians: Double) = new Angle(normalize(radians))
}

class Angle(val radians: Double) extends AnyVal  {
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
  
  def isLeftOf(a: Angle): Boolean =  (a.radians + 3 * Pi) - radians + Angle.TwoPi < Pi
  def isRightOf(a: Angle): Boolean = !isLeftOf(a)
  
  def addUpTo(add: Angle, upTo: Angle): Angle = {
    val upToDist = upTo - this
    if (upToDist.radians < add.radians) upTo else this + add
  }
  
  override def toString() = s"Angle($radians)"
}