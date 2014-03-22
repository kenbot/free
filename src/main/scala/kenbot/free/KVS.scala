package kenbot.free

import scalaz._
import Scalaz._
import Free._
import scala.collection.mutable

// This example is based off the one in Runar Bjarnason's "Dead Simple Dependency Injection" talk.
// http://www.youtube.com/watch?v=ZasXwtTRkio


// 0. Fantasy API
// def put(key: String, value: String): Unit
// def get(key: String): String
// def delete(key: String): Unit


// 1. ADT
sealed trait KVS[+Next]
case class Put[Next](key: String, value: String, next: Next) extends KVS[Next]     // <----  def put(key: String, value: String): Unit
case class Get[Next](key: String, onResult: String => Next) extends KVS[Next]      // <----  def get(key: String): String
case class Delete[Next](key: String, next: Next) extends KVS[Next]                 // <----  def delete(key: String): Unit


object KVS {
  type Script[A] = Free[KVS, A]
  
  // 2. Functor definition
  implicit val functor: Functor[KVS] = new Functor[KVS] {
    def map[A,B](kvs: KVS[A])(f: A => B): KVS[B] = kvs match {
      case Put(key, value, next) => Put(key, value, f(next))
      case Get(key, onResult) => Get(key, onResult andThen f)
      case Delete(key, next) => Delete(key, f(next))
    }
  }
  
  // 3. Lifting functions
  def put(key: String, value: String): Script[Unit] = liftF( Put(key, value, ()) )
  
  def get(key: String): Script[String] = liftF(Get(key, identity))
  
  def delete(key: String): Script[Unit] = liftF(Delete(key, ()))
  
  
  // 4. Composite functions
  def modify(key: String, f: String => String): Free[KVS, Unit] = for { 
    v <- get(key)
    _ <- put(key, f(v))
  } yield ()

  
  // 5. Write scripts
  val script: Free[KVS, Unit] = for {
    id <- get("swiss-bank-account-id")
    _ <- modify(id, (_ + 1000000))
    _ <- put("bermuda-airport", "getaway car")
    _ <- delete("tax-records")
  } yield ()

  
  // 6. Interpreters
  
  // Building an immutable structure
  def interpretPure(kvs: Script[Unit], table: Map[String, String] = Map.empty): Map[String, String] = kvs.resume.fold({
    case Get(key, onResult) => interpretPure(onResult(table(key)), table)
    case Put(key, value, next) => interpretPure(next, table + (key -> value))
    case Delete(key, next) => interpretPure(next, table - key)
  }, _ => table)

  
  // Directly running effects
  def interpretImpure(kvs: Script[Unit], table: mutable.Map[String, String]): Unit = kvs.go {
    case Get(key, onResult) => onResult(table(key))
    case Put(key, value, next) => table += (key -> value); next
    case Delete(key, next) => table -= key; next
  }
  
}


