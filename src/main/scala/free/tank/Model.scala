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

trait Alive {
  type This <: Alive
  
  def kill: This
  def alive: Boolean
  final def onlyIfAlive[A](a: A)(f: A => A): A = if (alive) f(a) else a
  final def dead = !alive
}


sealed trait Entity extends Physical with AIControlled with Alive {
  type This <: Entity
  
  def id: EntityId
  def replaceId(newId: EntityId): This
  final def sameAs(other: Entity): Boolean = other.id == id
}

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
  final def rotateBy(a: Angle): This = updatePhysics(_ rotateBy a)
  final def rotateTo(a: Angle): This = updatePhysics(_ rotateTo a)
  final def rotateUpTo(by: Angle, upTo: Angle) = updatePhysics(_.rotateUpTo(by, upTo))
}

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
  
  def rotateBy(a: Angle): Physics = copy(facing = facing + a)
  def rotateTo(a: Angle): Physics = copy(facing = a)
  def rotateUpTo(by: Angle, upTo: Angle): Physics = copy(facing = facing.addUpTo(by, upTo))
  
  private def frictionMult = friction.complement
  
  private def constrainPos(allowMove: Vec => Boolean, newPos: Vec): Vec = if (allowMove(newPos)) newPos else pos
  private def constrainVel(newVel: Vec): Vec = if (newVel.length > maxSpeed) vel.withLength(maxSpeed) else newVel
}

object Tank {
  val Acceleration = 3.0
  val Friction = Percentage(0.5)
  val GunRange = 10000.0
  val MissileSpeed = 10.0
  val MaxSpeed = 100.0
  val RotationRate = Angle.degrees(10)
  val Size = Dim(10,10)
  
  def apply(id: String, pos: Vec): Tank = {
    val physics = Physics(Angle.Zero, pos, Vec.Zero, Vec.Zero, Tank.Size, Friction, MaxSpeed)
    new Tank(EntityId(id), AIDone, physics, Angle.Zero, true)
  }  
  
  def unapply(e: Entity): Option[Tank] = if (e.isInstanceOf[Tank]) Some(e.asInstanceOf[Tank]) else None
}


case class Tank(id: EntityId, ai: AI[Unit], physics: Physics, gunAngle: Angle, alive: Boolean) extends Entity {

  type This = Tank

  def replaceId(newId: EntityId): Tank = copy(id = newId)
  
  def gunFacing: Angle = physics.facing + gunAngle
  
  def accelerate: Tank = accelerateForward(Tank.Acceleration)
  
  def withAI(newAI: AI[Unit]): Tank = copy(ai = newAI)
  
  def kill = copy(alive = false, ai = AIDone)
  
  def fire = Missile.fireToward(pos, facing, Tank.MissileSpeed, Tank.GunRange)
  
  def updatePhysics(f: Physics => Physics): Tank = copy(physics = f(physics))
}


object World {
  def apply(bounds: Dim, entities: Seq[Entity]): World = 
    new World(bounds, TreeMap(entities.map(t => t.id -> t): _*), 1)
}


class World private (dimensions: Dim, entityMap: SortedMap[EntityId, Entity], nextId: Int) {
  lazy val entities: Seq[Entity] = entityMap.values.toIndexedSeq
   
  val bounds = Rect(0.0, 0.0, dimensions.width, dimensions.height)
  
  def find(id: EntityId): Option[Entity] = entityMap.get(id)
  
  def nearestTankTo(e: Entity): Option[Tank] = {
    if (entityMap.size > 1) {
      val otherTanks = (entityMap - e.id).values.collect { case Tank(t) => t }
      Some(otherTanks.minBy(_ distanceTo e))
    } else None
  }
  
  
  def collidingEntities(rect: Rect): Seq[Entity] = entities.filter(_.bounds intersects rect)

  
  private def sanitizeId(e: Entity): Entity = 
    if (e.id == EntityId.Auto) e.replaceId(EntityId(s"[AUTO-$nextId]"))
    else e
  
  def withEntity(e: Entity) = {
    val e2 = sanitizeId(e)
    new World(dimensions, entityMap + (e2.id -> e2), nextId + 1)
  }
   
  def runFrame(interpreter: MoveInterpreter): World = 
    removeDead.runPhysics.runAI(interpreter)
    
  private def killCollidingEntities: World = {
    val newlyKilled = for {
      e <- entities 
      x <- collidingEntities(e.bounds) if !e.sameAs(x)
    } yield x.id -> x.kill
    
    new World(dimensions, entityMap ++ newlyKilled, nextId)
  }
  
  private def runPhysics: World = new World(dimensions, entityMap mapValues (_ run bounds.containsPt), nextId)
  
  private def removeDead: World = new World(dimensions, removeEntitiesIf(_.dead), nextId)
  
  private def runAI(interpreter: MoveInterpreter): World = (this /: entities)(interpreter)
  
  private def removeEntitiesIf(p: Entity => Boolean) = entityMap.filterNot(kv => p(kv._2))
}


case class TankGame(world: World, interpreter: MoveInterpreter, frame: Int) {
  def withInterpreter(newInterp: MoveInterpreter) = copy(interpreter = newInterp)
  def runFrame = TankGame(world.runFrame(interpreter), interpreter, frame + 1)
}

