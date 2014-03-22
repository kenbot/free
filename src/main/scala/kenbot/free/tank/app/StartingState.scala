package kenbot.free.tank.app
import kenbot.free.tank.model.Tank
import kenbot.free.tank.model.World
import kenbot.free.tank.model.TankGame
import kenbot.free.tank.ai.EasyTankAI
import kenbot.free.tank.maths.Vec
import kenbot.free.tank.maths.Dim
import kenbot.free.tank.ai.Moves

object StartingState {
  import Moves._
  
  val tanks = List(
    Tank("1", Vec(100,100)) withAI loop(for {
      t <- findNearestTank
      _ <- aimAtTank(t)
      _ <- fire * 3
      _ <- accelerate * 10
      _ <- delay * 30
    } yield ()),
    
    Tank("2", Vec(200, 200)) withAI loop(for {
      t <- findNearestTank
      _ <- aimAwayFrom(t)
      _ <- accelerate * 20
    } yield ()),
    
    
    Tank("3", Vec(400,400)) withAI loop(for {
      _ <- moveTo(Vec(200,200))
      _ <- fire
      _ <- moveTo(Vec(400, 200))
      _ <- fire
      _ <- moveTo(Vec(400, 400))
      _ <- fire
      _ <- moveTo(Vec(200, 400))
      _ <- fire
    } yield ()),
    
    Tank("4", Vec(250,400)) withAI loop(for {
      _ <- rotateLeft
      _ <- fire
      _ <- accelerate * 4
    } yield ()))
      
  val world = World(Dim(1000,700), tanks)
  val game = TankGame(world, EasyTankAI, 0)
}

