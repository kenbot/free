package free

import scalaz._
import Scalaz._
import Free._

sealed trait KVS[+Next]
case class Put[Next](key: String, value: String, next: Next) extends KVS[Next]
case class Get[Next](key: String, onResult: String => Next) extends KVS[Next]
case class Delete[Next](key: String, next: Next) extends KVS[Next]

object KVS {
  type KVSScript[A] = Free[KVS, A]
  
  implicit def functor: Functor[KVS] = new Functor[KVS] {
    def map[A,B](kvs: KVS[A])(f: A => B): KVS[B] = kvs match {
      case Put(key, value, next) => Put(key, value, f(next))
      case Get(key, onResult) => Get(key, onResult andThen f)
      case Delete(key, next) => Delete(key, f(next))
    }
  }
  
  def put(key: String, value: String): Free[KVS, Unit] = liftF( Put(key, value, ()) )
  def get(key: String): Free[KVS, String] = liftF(Get(key, identity))
  def delete(key: String): Free[KVS, Unit] = liftF(Delete(key, ()))
  
  def modify(key: String, f: String => String): Free[KVS, Unit] = for { 
    v <- get(key)
    _ <- put(key, f(v))
  } yield ()

    
  def runKVS[A](kvs: Free[KVS, A], table: Map[String, String] = Map.empty): Map[String, String] = kvs match {
    case Suspend(Get(key, onResult)) => runKVS(onResult(table(key)), table)
    case Suspend(Put(key, value, next)) => runKVS(next, table + (key -> value))
    case Suspend(Delete(key, next)) => runKVS(next, table - key)
    case Return(_) => table
  }
  
}


