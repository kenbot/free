package free.tank

import scalaz.Functor

import TankMoves._
import scalaz.Free._
import scalaz._
import Scalaz._

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

sealed trait Move[+A]
case class Accelerate[A](next: A) extends Move[A] 
//case class RotateGun[A](angle: Angle, next: A) extends Move[A]
case class RotateTank[A](angle: Angle, next: A) extends Move[A]
case class Delay[A](next: A) extends Move[A]
case class Fire[A](next: A) extends Move[A]
case class FindNearestTank[A](onFoundTank: Tank => A) extends Move[A]
case class AngleTo[A](toPos: Vec, onAngle: Angle => A) extends Move[A]
case class At[A](pos: Vec, at: Boolean => A) extends Move[A]
  
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
    _ <- accelerate
  } yield ()
  
  def searchAndDestroy: AI[Unit] = for {
    tank <- findNearestTank
    angle <- angleTo(tank.pos)
    _ <- fire
  } yield ()
}



object TankAiInterpreter {
  import TankMoves._
  
  def interpret[A](tank: Tank, ai: AI[A], world: TankWorld): (AI[A], TankWorld) = {
    def updateTank(f: Tank => Tank): TankWorld = world withTank f(tank)
    ai match {
      case Suspend(move) => move match {
        case Accelerate(next) => (next, updateTank(_.accelerate))
        case RotateTank(angle, next) => (next, updateTank(_ rotate angle))
        case Delay(next) => (next, world)
        case Fire(next) => (next, world)
        case FindNearestTank(onFoundTank) => (onFoundTank(???), world)
        case AngleTo(toPos, onAngle) => (onAngle(tank.pos angleTo toPos), world)
        case At(pos, at) => (at(false), world)
      }
      case Return(_) => (ai, world)
    }
  }
  
}
