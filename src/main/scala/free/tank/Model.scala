package free.tank

import scala.math._


object Angle {
  val Zero = Angle(0)
  val Quarter = Angle(Pi/2)
  val Half = Angle(Pi)
  val ThreeQuarters = Angle(3*Pi/2)
  val Full = Angle(2*Pi)
}

case class Angle(radians: Double) extends AnyVal {
  def sin: Double = math.sin(radians)
  def cos: Double = math.cos(radians)
  def tan: Double = math.cos(radians)
  def opposite: Angle = this + Angle.Half
  def degrees: Double = radians * 180.0 / Pi
  def +(other: Angle) = Angle(radians + other.radians)
  def -(other: Angle) = Angle(radians - other.radians)
  def *(factor: Double) = Angle(radians * factor)
  def normalize = Angle(radians % (2*Pi))
}


object Vec {
  def apply(angle: Angle, length: Double): Vec = Vec(length * angle.cos, length * angle.sin)
  val Zero = Vec(0,0)
}

case class Vec(x: Double, y: Double) {
  def angle: Angle = if (x != 0) Angle(atan(y/x)) else Angle(Pi)
  def length: Double = sqrt(x*x + y*y)
  def +(vec: Vec) = Vec(x + vec.x, y + vec.y)
  def -(vec: Vec) = Vec(x - vec.x, y - vec.y)
  def between(vec: Vec) = this - vec
  def distanceTo(pt: Vec) = between(pt).length
  def angleTo(pt: Vec) = between(pt).angle
}


case class TankId(name: String) extends AnyVal

object Tank {
  val Acceleration = 5.0
  val Friction = 1.0
  
  def apply(id: String, pos: Vec): Tank = 
    Tank(TankId(id), Angle.Zero, Angle.Zero, pos, Vec.Zero, Vec.Zero)
}

case class Tank(id: TankId, facing: Angle, gunFacing: Angle, pos: Vec, vel: Vec, acc: Vec) {
  def update = Tank(id, facing, gunFacing, pos + vel, vel + acc, Vec.Zero)
  
  def distanceTo(other: Tank) = pos distanceTo other.pos
  def sameTankAs(other: Tank): Boolean = other.id == id
  def accelerate: Tank = copy(acc = Vec(facing, Tank.Acceleration))
  def moveTo(newPos: Vec): Tank = copy(pos = newPos)
  def rotate(a: Angle): Tank = copy(facing = facing + a)
  def fire
}


case class TankWorld(tanks: List[Tank] = Nil) {
  def tankNearestTo(tank: Tank): Option[Tank] = {
    val result = tanks.foldLeft(None: Option[(Tank, Double)]) {
      case (acc @ Some((_, bestDist)), t) => 
        val dist = t distanceTo tank
        if (!(t sameTankAs tank) && dist < bestDist) {
          Some((t, dist))
        }
        else acc
      case (None, t) => Some((t, t distanceTo tank))
    }
    result.map(_._1)
    
  }
  def withTank(tank: Tank) = TankWorld(tanks.map(t => if (t.id == tank.id) tank else t))
  def update: TankWorld = TankWorld(tanks map (_.update))
  def addTank(tank: Tank) = TankWorld(tank :: tanks)
}
