package kenbot.free.tank.ai

import Moves._
import kenbot.free.tank.model.Tank
import kenbot.free.tank.model.World
import kenbot.free.tank.model.Entity
import kenbot.free.tank.maths.Angle
import scala.util.Random

trait MoveInterpreter extends ((World, Entity) => World) {
  
  final def apply(world: World, entity: Entity): World = {
    entity.ai.resume.fold(interpretMove(world, entity, _), _ => world)
  }
  
  protected def interpretMove(world: World, entity: Entity, move: Move[AI[Unit]]): World

  
  protected def updateAI(world: World, e: Entity, nextAI: AI[Unit]): World = 
    world withEntity (e withAI nextAI)
}


object HardTankAI extends DefaultTankAI 

object EasyTankAI extends DefaultTankAI {
  import Moves._
  import Angle._
  
  override def interpretMove(world: World, e: Entity, move: Move[AI[Unit]]): World = move match {
    case Fire(next) => 
      if (Random.nextBoolean) updateAI(world, e, next) // Don't fire
      else super.interpretMove(world, e, move) // Do fire
    
    case AngleTo(toPos, onAngle) => 
      updateAI(world, e, onAngle((e.pos angleTo toPos) + (Random.nextInt(40) - 20).degrees))
      
    case _ => super.interpretMove(world, e, move)
  }
}

trait DefaultTankAI extends MoveInterpreter {
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
      
    case Me(onMe) => 
      updateAI(world, e, onMe(e))
      
  }
}