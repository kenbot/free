package free.tank

import Moves.{AI, AIDone}
import scala.collection.SortedMap
import scala.collection.immutable.TreeMap
import scala.math.Ordered
import scala.annotation.tailrec
import scalaz._
import Scalaz._
import scala.reflect.ClassTag

object EntityId {
  def Auto = EntityId("[AUTO]")
}

case class EntityId(name: String) extends AnyVal with Ordered[EntityId] {
  override def compare(that: EntityId): Int = name compare that.name
}

sealed trait Entity extends Physical with AIControlled {
  type This <: Entity
  
  def id: EntityId
  def replaceId(newId: EntityId): This
  final def sameAs(other: Entity): Boolean = other.id == id
}

object Missile {
  val Friction = Percentage.Zero
  val MaxSpeed = 100.0
  
  def fireToward(fromPos: Vec, angle: Angle, speed: Double, range: Double): Missile = {
    val physics = Physics(angle, fromPos, Vec.fromAngle(angle, speed), Vec.Zero, Friction, MaxSpeed)
    Missile(EntityId.Auto, physics, range)
  }
}

case class Missile(id: EntityId, physics: Physics, range: Double) extends Entity {
  type This = Missile
  
  def ai = AIDone
  def updatePhysics(f: Physics => Physics): Missile = copy(physics = f(physics))
  def withAI(newAI: AI[Unit]): This = this
  override def run(constrainPos: Vec => Vec): This = Missile(id, physics run constrainPos, range - vel.length)
  def replaceId(newId: EntityId): This = copy(id = newId)
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
  
  def run(constrainPos: Vec => Vec): This = updatePhysics(_ run constrainPos)
  
  final def pos = physics.pos
  final def vel = physics.vel
  final def acc = physics.acc
  final def facing = physics.facing
  
  
  final def accelerateForward(acc: Double): This = updatePhysics(_ accelerateForward acc)
  final def distanceTo(other: Physical) = pos distanceTo other.pos
  final def rotate(by: Angle): This = updatePhysics(_ rotate by)
  
}

case class Physics(facing: Angle, pos: Vec, vel: Vec, acc: Vec, friction: Percentage, maxSpeed: Double) {
  def run(constrainPos: Vec => Vec) = {
    Physics(facing, constrainPos(pos + vel), constrainVel((vel * frictionMult.amount) + acc), Vec.Zero, friction, maxSpeed)
  }
  
  def accelerateForward(acc: Double): Physics = accelerate(Vec.fromAngle(facing, acc))
  
  def accelerate(acc: Vec): Physics = copy(acc = this.acc + acc)
  
  def rotate(by: Angle): Physics = copy(facing = facing + by)
  
  private def frictionMult = friction.complement
  
  private def constrainVel(vel: Vec): Vec = if (vel.length > maxSpeed) vel.withLength(maxSpeed) else vel
}

object Tank {
  val Acceleration = 3.0
  val Friction = Percentage(0.5)
  val GunRange = 10000.0
  val MissileSpeed = 10.0
  val MaxSpeed = 100.0
  
  def apply(id: String, pos: Vec): Tank = {
    val physics = Physics(Angle.Zero, pos, Vec.Zero, Vec.Zero, Friction, MaxSpeed)
    new Tank(EntityId(id), AIDone, physics, Angle.Zero)
  }  
  
  def unapply(e: Entity): Option[Tank] = if (e.isInstanceOf[Tank]) Some(e.asInstanceOf[Tank]) else None
}


case class Tank(id: EntityId, ai: AI[Unit], physics: Physics, gunAngle: Angle) extends Entity {

  type This = Tank

  def replaceId(newId: EntityId): Tank = copy(id = newId)
  
  def gunFacing: Angle = physics.facing + gunAngle
  
  def accelerate: Tank = accelerateForward(Tank.Acceleration)
  
  def withAI(newAI: AI[Unit]): Tank = copy(ai = newAI)
  
  def fire = Missile.fireToward(pos, facing, Tank.MissileSpeed, Tank.GunRange)
  
  def updatePhysics(f: Physics => Physics): Tank = copy(physics = f(physics))
}


object World {
  def apply(bounds: Dim, entities: Seq[Entity]): World = 
    new World(bounds, TreeMap(entities.map(t => t.id -> t): _*), 1)
}


class World private (dimensions: Dim, private val entityMap: SortedMap[EntityId, Entity], nextId: Int) {
  lazy val entities: Seq[Entity] = entityMap.values.toIndexedSeq
   
  val bounds = Rect(0.0, 0.0, dimensions.width, dimensions.height)
  
  def find(id: EntityId): Option[Entity] = entityMap.get(id)
  
  def nearestTankTo(e: Entity): Option[Tank] = {
    if (entityMap.size > 1) {
      val otherTanks = (entityMap - e.id).values.collect { case Tank(t) => t }
      Some(otherTanks.minBy(_ distanceTo e))
    } else None
  }
  
  private def sanitizeId(e: Entity): Entity = 
    if (e.id == EntityId.Auto) e.replaceId(EntityId(s"[AUTO-$nextId]"))
    else e
  
  def withEntity(e: Entity) = {
    val e2 = sanitizeId(e)
    new World(dimensions, entityMap + (e2.id -> e2), nextId + 1)
  }
   
  def runPhysics: World = new World(dimensions, entityMap mapValues (_ run bounds.cropPt), nextId) 
  
  def runAI(interpreter: MoveInterpreter): World = (this /: entities)(interpreter)
 
}


case class TankGame(world: World, interpreter: MoveInterpreter, frame: Int) {
  def runFrame = TankGame(world.runPhysics.runAI(interpreter), interpreter, frame + 1)
}

