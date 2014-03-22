package kenbot.free.tank.model

import kenbot.free.tank.ai.Moves.{AI, AIDone}
import kenbot.free.tank.maths.Vec
import kenbot.free.tank.maths.Dim
import kenbot.free.tank.maths.Percentage
import kenbot.free.tank.maths.Angle

object Missile {
  val Friction = Percentage.Zero
  val MaxSpeed = 100.0
  val Size = Dim(6,6)
  
  def fireToward(fromPos: Vec, angle: Angle, speed: Double, range: Double): Missile = {
    val physics = Physics(angle, fromPos, Vec.fromAngle(angle, speed), Vec.Zero, Missile.Size, Friction, MaxSpeed)
    Missile(EntityId.Auto, physics, range, true)
  }
}

case class Missile(id: EntityId, physics: Physics, range: Double, alive: Boolean) extends Entity {
  type This = Missile
  
  override def ai = AIDone
  override def kill = copy(alive = false)
  override def updatePhysics(f: Physics => Physics): Missile = copy(physics = f(physics))
  override def withAI(newAI: AI[Unit]): This = this
  override def run(allowMove: Vec => Boolean): This = {
    Missile(id, onlyIfAlive(physics)(_ run allowMove), range - vel.length, physics.nextFrameAllowed(allowMove))
  }
  override def replaceId(newId: EntityId): This = copy(id = newId)
}