package free.tank

import scalaz.Functor

import TankMoves._
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


object TankMoves {

  type AI[A] = Free[Move, A]
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

trait MoveInterpreter extends ((TankWorld, Tank) => TankWorld) {
  
  final def apply(world: TankWorld, tank: Tank): TankWorld = {
    tank.ai.resume.fold(interpretMove(world, tank, _), _ => world)
  }
  
  protected def interpretMove(world: TankWorld, tank: Tank, move: Move[AI[Unit]]): TankWorld
    
  protected def updateAI(world: TankWorld, tank: Tank, nextAI: AI[Unit]): TankWorld = {
    world updateTank (tank withAI nextAI)
  }
  
  protected def updateTank(world: TankWorld, tank: Tank, nextAI: AI[Unit], f: Tank => Tank): TankWorld = {
    world updateTank f(tank).withAI(nextAI)
  }
}

object EasyTankAI extends MoveInterpreter {
  def interpretMove(world: TankWorld, tank: Tank, move: Move[AI[Unit]]): TankWorld = move match {
    case Accelerate(next) => updateTank(world, tank, next, _.accelerate)
    case RotateTank(angle, next) => updateTank(world, tank, next, _ rotate angle)
    case Delay(next) => println("waiting..."); world
    case Fire(next) => println("FIRE!"); world
    case FindNearestTank(onFoundTank) => 
      val maybeNearest = world tankNearestTo tank
      val nextAI = maybeNearest.fold(tank.ai)(onFoundTank)
      updateAI(world, tank, nextAI)
    case AngleTo(toPos, onAngle) => updateAI(world, tank, onAngle(tank.pos angleTo toPos))
    case At(pos, at) => updateAI(world, tank, at(tank.pos.distanceTo(pos) <= 0.01))
  }
}
  

