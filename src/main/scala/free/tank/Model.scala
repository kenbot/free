package free.tank

import scala.math._

import TankMoves.{AI, AIDone}
import scala.collection.SortedMap
import scala.collection.immutable.TreeMap
import scala.math.Ordered


object Angle {
  def degrees(degs: Double) = Angle(degs * Pi / 180.0)
  val Zero = Angle(0)
  val Quarter = Angle(Pi/2)
  val Half = Angle(Pi)
  val ThreeQuarters = Angle(3*Pi/2)
  val Full = Angle(2*Pi)
}

case class Angle(radians: Double) extends AnyVal {
  def sin: Double = math.sin(radians)
  def cos: Double = math.cos(radians)
  def tan: Double = math.tan(radians)
  def opposite: Angle = this + Angle.Half
  def degrees: Double = radians * 180.0 / Pi
  def +(other: Angle) = Angle(radians + other.radians)
  def -(other: Angle) = Angle(radians - other.radians)
  def *(factor: Double) = Angle(radians * factor)
  def /(factor: Double) = Angle(radians / factor)
  def normalize = Angle(radians % (2*Pi))
}


object Vec {
  def fromAngle(angle: Angle, length: Double): Vec = Vec(length * angle.cos, length * angle.sin)
  val Zero = Vec(0,0)
}

case class Vec(x: Double, y: Double) {
  def angle: Angle = if (x != 0) Angle(atan(y/x)) else Angle(Pi)
  def length: Double = sqrt(x*x + y*y)
  def +(vec: Vec) = Vec(x + vec.x, y + vec.y)
  def -(vec: Vec) = Vec(x - vec.x, y - vec.y)
  def toTuple: (Double, Double) = (x,y)
  def between(vec: Vec) = this - vec
  def distanceTo(pt: Vec) = between(pt).length
  def angleTo(pt: Vec) = between(pt).angle
}

case class TankId(name: String) extends AnyVal with Ordered[TankId] {
  override def compare(that: TankId): Int = name compare that.name
}

object Tank {
  val Acceleration = 5.0
  val Friction = 1.0
  
  def apply(id: String, pos: Vec): Tank = 
    Tank(TankId(id), AIDone, Angle.Zero, Angle.Zero, pos, Vec.Zero, Vec.Zero)
}


case class Tank(id: TankId, ai: AI[Unit], facing: Angle, gunFacing: Angle, pos: Vec, vel: Vec, acc: Vec) {
  def updatePhysics = Tank(id, ai, facing, gunFacing, pos + vel, vel + acc, Vec.Zero)

  def withAI(newAI: AI[Unit]): Tank = copy(ai = newAI)
  
  def distanceTo(other: Tank) = pos distanceTo other.pos
  def sameTankAs(other: Tank): Boolean = other.id == id
  def accelerate: Tank = copy(acc = Vec.fromAngle(facing, Tank.Acceleration))
  def moveTo(newPos: Vec): Tank = copy(pos = newPos)
  def rotate(a: Angle): Tank = copy(facing = facing + a)
}

case class TankGame(world: TankWorld, interpreter: MoveInterpreter, frame: Int) {
  def runFrame = TankGame(world.updatePhysics.updateAI(interpreter), interpreter, frame + 1)
}


object TankWorld {
  def apply(tanks: Seq[Tank]): TankWorld = 
    new TankWorld(TreeMap(tanks.map(t => t.id -> t): _*))
}

class TankWorld private (private val tankMap: SortedMap[TankId, Tank]) {
  
  lazy val tanks: Seq[Tank] = tankMap.values.toIndexedSeq
  
  def findTank(id: TankId): Option[Tank] = tankMap.get(id)
  
  def tankNearestTo(tank: Tank): Option[Tank] = {
    if (tankMap.size > 1)
      Some((tankMap - tank.id).values.minBy(_ distanceTo tank))
    else
      None
  }

  def updateTank(tank: Tank) = new TankWorld(tankMap + (tank.id -> tank))
  
  def updatePhysics: TankWorld = new TankWorld(tankMap mapValues (_.updatePhysics))
  
  def updateAI(interpreter: MoveInterpreter) = (this /: tanks)(interpreter)
}
