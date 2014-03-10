package free.tank

import Moves._
import scalaz._
import scalaz.Free._
import scalaz.Scalaz._


sealed trait Move[+A]
case class Accelerate[A](next: A) extends Move[A] 
case class RotateLeft[A](upTo: Option[Angle], next: A) extends Move[A]
case class RotateRight[A](upTo: Option[Angle], next: A) extends Move[A]
case class Delay[A](next: A) extends Move[A]
case class Fire[A](next: A) extends Move[A]
case class FindNearestTank[A](onFoundTank: Tank => A) extends Move[A]
case class AngleTo[A](toPos: Vec, onAngle: Angle => A) extends Move[A]
case class IsAt[A](pos: Vec, onAt: Boolean => A) extends Move[A]
case class IsFacing[A](angle: Angle, onFacing: Boolean => A) extends Move[A]
  

object Move {
  implicit def functor: Functor[Move] =  new Functor[Move] {
    def map[A,B](move: Move[A])(f: A => B): Move[B] = move match {
      case Accelerate(next) => Accelerate(f(next))
      case RotateLeft(upTo, next) => RotateLeft(upTo, f(next))
      case RotateRight(upTo, next) => RotateRight(upTo, f(next))
      case Delay(next) => Delay(f(next))
      case Fire(next) => Fire(f(next))
      case FindNearestTank(onFoundTank) => FindNearestTank(onFoundTank andThen f) 
      case AngleTo(toPos, onAngle) => AngleTo(toPos, onAngle andThen f)
      case IsAt(pos, onAt) => IsAt(pos, onAt andThen f)
      case IsFacing(angle, onFacing) => IsFacing(angle, onFacing andThen f)
    }
  }
}

object Moves extends AdvancedMoves


trait BasicMoves {
  type AI[A] = Free[Move, A]
  
  val AIDone: AI[Unit] = Return(())
  
  protected implicit def liftMove[A](move: Move[A]): AI[A] = liftF(move)
  
  def accelerate: AI[Unit] = Accelerate(())

  def rotateLeft: AI[Unit] = RotateLeft(None, ())
  
  def rotateRight: AI[Unit] = RotateRight(None, ())
  
  def rotateLeftUpTo(upTo: Angle): AI[Unit] = RotateLeft(Some(upTo), ())
  
  def rotateRightUpTo(upTo: Angle): AI[Unit] = RotateRight(Some(upTo), ())
  
  def delay: AI[Unit] = Delay(())
  
  def fire: AI[Unit] = Fire(())
  
  def findNearestTank: AI[Tank] = FindNearestTank(identity)
  
  def angleTo(pos: Vec): AI[Angle] = AngleTo(pos, identity)
  
  def isAt(pos: Vec): AI[Boolean] = IsAt(pos, identity)
  
  def isFacing(angle: Angle): AI[Boolean] = IsFacing(angle, identity)
  
}


trait AdvancedMoves extends BasicMoves {
  
  implicit class AIOps(ai: AI[Unit]) {
    def *(times: Int): AI[Unit] = 
      if (times == 1) ai else ai >> this * (times - 1)
  }
  
  def loop(ai: AI[Unit]): AI[Unit] = ai >> loop(ai)
  
  def when(b: Boolean)(ai: => AI[Unit]): AI[Unit] = b whenM ai
  
  def unless(b: Boolean)(ai: => AI[Unit]): AI[Unit] = b unlessM ai
  
  def aimAtTank(tank: Tank): AI[Unit] = for {
    angle <- angleTo(tank.pos)
    _ <- rotateTowards(angle)
  } yield ()
  
  def aimAwayFrom(tank: Tank): AI[Unit] = for {
    angle <- angleTo(tank.pos)
    _ <- rotateTowards(angle.opposite)
  } yield ()
  
  def fireAtTank(tank: Tank): AI[Unit] = aimAtTank(tank) >> fire
  
  def rotateTowards(angle: Angle): AI[Unit] = for {
    ok <- isFacing(angle)
    _ <- ok.unlessM(rotateLeftUpTo(angle) >> rotateTowards(angle))
  } yield ()
  
  def moveTo(pos: Vec): AI[Unit] = for {
    arrived <- isAt(pos)
    _ <- unless(arrived)(for {
      angle <- angleTo(pos)
      _ <- rotateTowards(angle)
      _ <- accelerate
      _ <- moveTo(pos)
    } yield ())
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

  
  protected def updateAI(world: World, e: Entity, nextAI: AI[Unit]): World = {
    world withEntity (e withAI nextAI)
  }
}


object EasyTankAI extends MoveInterpreter {
  def interpretMove(world: World, e: Entity, move: Move[AI[Unit]]): World = move match {
    
    case Accelerate(next) => 
      updateAI(world, e accelerateForward Tank.Acceleration, next)
      
    case RotateLeft(upTo, next) => 
      val rotatedEntity: Entity = (upTo.fold
          (e rotateBy Tank.RotationRate)
          (e.rotateUpTo(Tank.RotationRate, _)))
      updateAI(world, rotatedEntity, next)
   
    case RotateRight(upTo, next) => 
      val rotatedEntity: Entity = (upTo.fold
          (e rotateBy -Tank.RotationRate)
          (e.rotateUpTo(-Tank.RotationRate, _)))
      updateAI(world, rotatedEntity, next)
      
    case Delay(next) => 
      updateAI(world, e, next)
    
    case Fire(next) => 
      val maybeMissile = Tank.unapply(e).map(_.fire)
      val updatedWorld = maybeMissile.fold(world)(world.withEntity)
      updateAI(updatedWorld, e, next)
      
    case FindNearestTank(onFoundTank) => 
      val maybeNearest = world nearestTankTo e
      val nextAI = maybeNearest.fold(e.ai)(onFoundTank)
      updateAI(world, e, nextAI)
      
    case AngleTo(toPos, onAngle) => 
      updateAI(world, e, onAngle(e.pos angleTo toPos))
      
    case IsAt(pos, onAt) => 
      updateAI(world, e, onAt(e.pos.distanceTo(pos) <= 4))
    
    case IsFacing(angle, onFacing) => 
      updateAI(world, e, onFacing((e.facing - angle).radians <= 0.01))
  }
}
  

