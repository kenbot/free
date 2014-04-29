package kenbot.free.tank.ai

import kenbot.free.tank.model.Tank
import kenbot.free.tank.maths.Angle
import scala.util.Random
import kenbot.free.tank.ai.Moves.{AI, AIDone}


object HardTankAI extends DefaultTankAI 

object EasyTankAI extends DefaultTankAI {
  import Angle._
  
  override def interpretMove(move: Move[AI[Unit]]): WorldChange = move match {
    case Fire(next) => 
      if (Random.nextBoolean) doNothing(next)
      else super.interpretMove(move)
    
    case AngleTo(toPos, onAngle) => 
      observeEntity(onAngle)(_.pos.angleTo(toPos) + (Random.nextInt(40) - 20).degrees)
      
    case _ => super.interpretMove(move)
  }
}

object TruceTankAI extends DefaultTankAI {
  override def interpretMove(move: Move[AI[Unit]]): WorldChange = move match {
    case Fire(next) => doNothing(next)
    case _ => super.interpretMove(move)
  }
}

trait DefaultTankAI extends MoveInterpreter {
  def interpretMove(move: Move[AI[Unit]]): WorldChange = move match {
    
    case Accelerate(next) => 
      updateEntity(next)(_ accelerateForward Tank.Acceleration)
      
    case RotateLeft(upTo, next) => 
      updateEntity(next)(_.rotateBy(Tank.RotationRate, upTo))
   
    case RotateRight(upTo, next) => 
      updateEntity(next)(_.rotateBy(-Tank.RotationRate, upTo))
      
    case Delay(next) => 
      doNothing(next)
    
    case Fire(next) => 
      updateEntityInWorld(next) { (world, e) => 
        val maybeMissile = Tank.unapply(e).map(_.fire)
        maybeMissile.fold(world)(world.withEntity)
      }
      
    case FindNearestTank(onFoundTank) => 
      def onMaybeFoundTank(t: Option[Tank]): AI[Unit] = 
        t.fold(AIDone)(onFoundTank)
        
      observeEntityInWorld(onMaybeFoundTank) { (world, e) => 
        world nearestTankTo e 
      }
      
    case AngleTo(toPos, onAngle) => 
      observeEntity(onAngle)(_.pos angleTo toPos)
      
    case IsAt(pos, onAt) => 
      observeEntity(onAt)(_.pos.distanceTo(pos) <= 4)
    
    case IsFacing(angle, onFacing) => 
      observeEntity(onFacing)(e => (e.facing - angle).radians <= 0.01)
      
    case Me(onMe) => 
      observeEntity(onMe)(identity)
      
  }
}