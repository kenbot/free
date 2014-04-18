package kenbot.free.tank.model

import kenbot.free.tank.maths.Vec
import kenbot.free.tank.maths.Dim
import kenbot.free.tank.maths.Percentage
import kenbot.free.tank.maths.Angle

case class Physics(facing: Angle, pos: Vec, vel: Vec, acc: Vec, size: Dim, friction: Percentage, maxSpeed: Double) {
  def run(allowMove: Vec => Boolean) = {
    val nextPos = constrainPos(allowMove, pos + vel)
    val nextVel = constrainVel((vel * frictionMult.amount) + acc)
    Physics(facing, nextPos, nextVel, Vec.Zero, size, friction, maxSpeed)
  }
  
  def nextFrameAllowed(allowMove: Vec => Boolean): Boolean = allowMove(pos + vel)
  
  lazy val bounds = size centeredAt pos
  
  def accelerateForward(acc: Double): Physics = accelerate(Vec.fromAngle(facing, acc))
  
  def accelerate(acc: Vec): Physics = copy(acc = this.acc + acc)
  
  def rotateTo(a: Angle): Physics = copy(facing = a)
  def rotateBy(by: Angle, upTo: Option[Angle] = None): Physics = 
    copy(facing = upTo.fold(facing + by)(facing.addUpTo(by, _)))
    
  private def frictionMult = friction.complement
  
  private def constrainPos(allowMove: Vec => Boolean, newPos: Vec): Vec = if (allowMove(newPos)) newPos else pos
  private def constrainVel(newVel: Vec): Vec = if (newVel.length > maxSpeed) vel.withLength(maxSpeed) else newVel
}
