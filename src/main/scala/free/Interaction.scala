package free.interaction

/*
import scalaz._
import Scalaz._
import Free._
import scala.annotation.tailrec

sealed trait Direction
case object Forward extends Direction
case object Backward extends Direction
case object Left extends Direction
case object Right extends Direction

class Image

sealed trait Interaction[Next]
case class Look[Next](dir: Direction, f: Image => Next) extends Interaction[Next]
case class Fire[Next](dir: Direction, next: Next) extends Interaction[Next]
case class ReadLine[Next](f: String => Next) extends Interaction[Next]
case class WriteLine[Next](line: String, f: Boolean => Next) extends Interaction[Next]

object Interaction {
  
  implicit def functor: Functor[Interaction] = new Functor[Interaction] {
    def map[A, B](fa: Interaction[A])(f: A => B): Interaction[B] = fa match {
      case Look(dir, g) => Look(dir, f compose g)
      case Fire(dir, next) => Fire(dir, f(next))
      case ReadLine(g) => ReadLine(f compose g)
      case WriteLine(line, g) => WriteLine(line, f compose g)
    }
  }
  
  def EmptyProgram: Program[Unit] = Monad[Program].pure(())
  
  type Program[A] = Free[Interaction, A]
  
  def look(dir: Direction): Program[Image] = liftF(Look(dir, identity))
  def fire(dir: Direction): Program[Unit] = liftF(Fire(dir, ()))
  def readLine: Program[String] = liftF(ReadLine(identity))
  def writeLine(line: String): Program[Boolean] = liftF(WriteLine(line, identity))
}

object Interpret {
  import Program._
  import Interaction._
  
  case class Game[S](state: S) {
    def flatMap[B](f: S => Game[B]): Game[B] = f(state)
    def map[B](f: S => B): Game[B] = Game(f(state))
  }
  
  def collectImage(dir: Direction): Game[Image] = Game(new Image)
  
  def sendBullet(dir: Direction): Game[Unit] = Game(())
  
  def getChatLine: Game[String] = Game("chat blah blah")
  
  def putChatLine(str: String): Game[Unit] = Game(println("blah"))
  
  def interpret[R](program: Program[R]): Game[R] = program match {
    case Suspend(Look(dir, f)) => 
      for {
        img <- collectImage(dir)
        next <- interpret(f(img))
      } yield next
      
    case Suspend(Fire(dir, next)) => 
      for {
        _ <- sendBullet(dir)
        next <- interpret(next)
      } yield next
      
    case Suspend(ReadLine(f)) => 
      for {
        str <- getChatLine
        next <- interpret(f(str))
      } yield next
      
    case Suspend(WriteLine(str, f)) => 
      for {
        _ <- putChatLine(str)
        next <- interpret(f(true))
      } yield next
      
    case Return(r) => Game(r)
  }
}


object Program {
  import Interaction._
  
  def reactAngrily: Program[Unit] = for {
    str <- readLine
    _ <- (str == "no").whenM {
      for {
        _ <- fire(Forward)
        _ <- writeLine("Take that!")
      } yield ()
    }
  } yield ()
  
  def keepReactingAngrily: Program[Unit] = forever(reactAngrily)


  
  def when[M[_]](p: Boolean)(then: => M[Unit])(implicit m: Applicative[M]): M[Unit] = if (p) then else m.pure(())
  
  def forever[M[_]: Monad, A, B](ma: M[A]): M[B] = ma >> forever(ma)

  
}*/