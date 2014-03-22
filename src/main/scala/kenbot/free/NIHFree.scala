package kenbot.free

import scalaz._
import Scalaz._
import NIHFree._

// NIH reimplementation of Free.  This was a helpful exercise to boost my understanding of the structure.
// It may be helpful as a more minimal illustration of the mechanism than scalaz.Free.

object NIHFree {
  implicit def monad[F[+_]](implicit F: Functor[F]): Monad[({type f[x] = NIHFree[F,x]})#f] = new Monad[({type f[x] = NIHFree[F,x]})#f] {
    def point[A](a: => A): NIHFree[F,A] = NIHPure[F, A](a)
    
    def bind[A, B](free: NIHFree[F,A])(f: A => NIHFree[F,B]): NIHFree[F,B] = 
      free flatMap f
  }
  
  def liftF[F[_]: Functor, A](fa: => F[A]): NIHFree[F, A] = 
    NIHSuspend(fa map NIHPure.apply)
}




sealed trait NIHFree[F[_], A] {
  
  def flatMap[B](f: A => NIHFree[F, B])(implicit F: Functor[F]): NIHFree[F, B] = this match {
    case NIHSuspend(ffa) => NIHSuspend(ffa.map(_ flatMap f))
    case NIHPure(a) => f(a)
  }
  
  def map[B](f: A => B)(implicit functor: Functor[F]): NIHFree[F, B] = this match {
    case NIHSuspend(ffa) => NIHSuspend(ffa.map(_ map f))
    case NIHPure(a) => NIHPure(f(a))
  }

}
case class NIHSuspend[F[_], A](f: F[NIHFree[F, A]]) extends NIHFree[F, A]
case class NIHPure[F[_], A](a: A) extends NIHFree[F, A]
