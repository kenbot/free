package free.tank

import scalaz.Functor

import Moves._
import scalaz.Free._
import scalaz._
import Scalaz._


sealed trait Move[+A]
case class Accelerate[A](next: A) extends Move[A] 
//case class RotateGun[A](angle: Angle, next: A) extends Move[A]
case class RotateTank[A](angle: Angle, next: A) extends Move[A]
case class Delay[A](next: A) extends Move[A]
case class Fire[A](next: A) extends Move[A]
case class FindNearestTank[A](onFoundTank: Tank => A) extends Move[A]
case class AngleTo[A](toPos: Vec, onAngle: Angle => A) extends Move[A]
case class At[A](pos: Vec, at: Boolean => A) extends Move[A]
  

object Move {
  
  implicit def functor: Functor[Move] =  new Functor[Move] {
    def map[A,B](move: Move[A])(f: A => B): Move[B] = move match {
      case Accelerate(next) => Accelerate(f(next))
      case RotateTank(angle, next) => RotateTank(angle, f(next))
      case Delay(next) => Delay(f(next))
      case Fire(next) => Fire(f(next))
      case FindNearestTank(onFoundTank) => FindNearestTank(onFoundTank andThen f) 
      case AngleTo(toPos, onAngle) => AngleTo(toPos, onAngle andThen f)
      case At(pos, at) => At(pos, at andThen f)
    }
  }
}


object Moves {

  type AI[A] = Free[Move, A]
  
  implicit class AIOps(ai: AI[Unit]) {
    def *(times: Int): AI[Unit] = 
      if (times == 1) ai else ai >> this * (times - 1)
  }
  
  val AIDone: AI[Unit] = Return(())

  private implicit def liftMove[A](move: Move[A]): AI[A] = liftF(move)
  
  def accelerate: AI[Unit] = Accelerate(())
  
  def rotateTank(angle: Angle): AI[Unit] = RotateTank(angle, ())
  //def rotateGun(angle: Angle): AI[Unit] = RotateGun(angle, ())
  
  def delay: AI[Unit] = Delay(())
  
  def fire: AI[Unit] = Fire(())
  
  def at(pos: Vec): AI[Boolean] = At(pos, identity)
  
  def findNearestTank: AI[Tank] = FindNearestTank(identity)
  
  def aimAtTank(tank: Tank): AI[Unit] = for {
    angle <- angleTo(tank.pos)
    _ <- rotateTank(angle)
  } yield ()
  
  def fireAtTank(tank: Tank): AI[Unit] = aimAtTank(tank) >> fire

  def angleTo(pos: Vec): AI[Angle] = AngleTo(pos, identity)
  
  def moveTo(pos: Vec): AI[Unit] = for {
    angle <- angleTo(pos)
    _ <- rotateTank(angle)
    _ <-  accelerate
  } yield ()
  
  def searchAndDestroy: AI[Unit] = for {
    tank <- findNearestTank
    angle <- angleTo(tank.pos)
    _ <- fire
  } yield ()
}

trait MoveInterpreter extends ((World, Entity) => World) {
  
  final def apply(world: World, entity: Entity): World = {
    entity.ai.resume.fold(interpretMove(world, entity, _), _ => world)
  }
  
  protected def interpretMove(world: World, entity: Entity, move: Move[AI[Unit]]): World

  
  protected def updateAI(world: World, entity: Entity, nextAI: AI[Unit]): World = {
    world withEntity (entity withAI nextAI)
  }
  
  protected def updateEntity(world: World, e: Entity, nextAI: AI[Unit], f: Entity => Entity): World = {
    world withEntity f(e).withAI(nextAI)
  }
}


object EasyTankAI extends MoveInterpreter {
  def interpretMove(world: World, e: Entity, move: Move[AI[Unit]]): World = move match {
    case Accelerate(next) => updateEntity(world, e, next, _ accelerateForward Tank.Acceleration)
    case RotateTank(angle, next) => updateEntity(world, e, next, _ rotate angle)
    case Delay(next) => updateAI(world, e, next)
    
    case Fire(next) => 
      val maybeMissile = Tank.unapply(e).map(_.fire)
      val updatedWorld = maybeMissile.fold(world)(world.withEntity)
      updateAI(updatedWorld, e, next)
      
    case FindNearestTank(onFoundTank) => 
      val maybeNearest = world nearestTankTo e
      val nextAI = maybeNearest.fold(e.ai)(onFoundTank)
      updateAI(world, e, nextAI)
      
    case AngleTo(toPos, onAngle) => updateAI(world, e, onAngle(e.pos angleTo toPos))
    case At(pos, at) => updateAI(world, e, at(e.pos.distanceTo(pos) <= 0.01))
  }
}
  

