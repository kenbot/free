package kenbot.free.tank.ai

import Moves._
import scalaz._
import scalaz.Free._
import scalaz.Scalaz._
import kenbot.free.tank.model.Tank
import kenbot.free.tank.maths.Vec
import kenbot.free.tank.maths.Angle
import kenbot.free.tank.model.Entity

// ADT for tank moves
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
case class Me[A](onMe: Entity => A) extends Move[A]
  

// Functor definition
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
      case Me(onMe) => Me(onMe andThen f)
    }
  }
}

object Moves extends AdvancedMoves

// Lifting functions.  Note that liftF is implicit within the trait.
trait BasicMoves {
  type AI[A] = Free[Move, A]
  
  val AIDone: AI[Unit] = Return(())
  
  private implicit def liftMove[A](move: Move[A]): AI[A] = liftF(move)
  
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
  
  def me: AI[Entity] = Me(identity)
  
}

// Fancy composite tank moves
trait AdvancedMoves extends BasicMoves {
  
  implicit class AIOps(ai: AI[Unit]) {
    def *(times: Int): AI[Unit] = {
      require(times >= 1)
      if (times == 1) ai else ai >> this * (times - 1)
    }
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
    tank <- me
    _ <- unless(ok) {
           val left = tank.facing isLeftOf angle
           val rotationAction = if (left) rotateLeftUpTo(angle)
                                else rotateRightUpTo(angle)
           rotationAction >> rotateTowards(angle)
         }
  } yield ()
  
  def distanceTo(e: Entity): AI[Double] = me.map(_ distanceTo e)
  
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

  

