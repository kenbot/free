package kenbot.free.tank.maths

import scala.math.{atan, sin, cos, sqrt, Pi}

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
  def between(vec: Vec) = vec - this
  def distanceTo(pt: Vec) = between(pt).length
  def angleTo(pt: Vec) = between(pt).angle
}
