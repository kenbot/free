package kenbot.free.tank.model

import kenbot.free.tank.ai.Moves.AI
import kenbot.free.tank.maths.Vec
import kenbot.free.tank.maths.Angle

object EntityId {
  val Auto = EntityId("[AUTO]")
}

case class EntityId(name: String) extends AnyVal with Ordered[EntityId] {
  override def compare(that: EntityId): Int = name compare that.name
}

trait Alive {
  type This <: Alive
  
  def kill: This
  def alive: Boolean
  final def onlyIfAlive[A](a: A)(f: A => A): A = if (alive) f(a) else a
  final def dead = !alive
}


trait Entity extends Physical with AIControlled with Alive {
  type This <: Entity
  
  def id: EntityId
  def replaceId(newId: EntityId): This
  final def sameAs(other: Entity): Boolean = other.id == id
}


trait AIControlled {
  type This <: AIControlled
  
  def ai: AI[Unit]
  def withAI(newAI: AI[Unit]): This
}

trait Physical {
  type This <: Physical
  
  def physics: Physics
  def updatePhysics(f: Physics => Physics): This
  
  def run(allowMove: Vec => Boolean): This = updatePhysics(_ run allowMove)
  
  final def pos = physics.pos
  final def vel = physics.vel
  final def acc = physics.acc
  final def facing = physics.facing
  final def bounds = physics.bounds
  
  final def accelerateForward(acc: Double): This = updatePhysics(_ accelerateForward acc)
  final def distanceTo(other: Physical) = pos distanceTo other.pos
  final def rotateBy(a: Angle, upTo: Option[Angle] = None): This = updatePhysics(_.rotateBy(a, upTo))
  final def rotateTo(a: Angle): This = updatePhysics(_ rotateTo a)
}


