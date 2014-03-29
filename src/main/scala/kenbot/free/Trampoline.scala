package kenbot.free

import scalaz._
import Scalaz._
import Free._
import scala.concurrent.Future


object NaughtyRecursion extends EvenProgram {
  
  def odd[A](list: Stream[A]): Boolean = list match {
    case a #:: as => even(as)
    case Stream.Empty => false
  }
  
  def even[A](list: Stream[A]): Boolean = list match {
    case a #:: as => odd(as)
    case Stream.Empty => true
  }
  
  def runTest(range: Stream[Int]) = even(range)
}




object Trampoline extends EvenProgram {
  def odd[A](list: Stream[A]): Trampoline[Boolean] = list match {
    case a #:: as => Suspend(() => even(as))
    case Stream.Empty => Return(false)
  }
  
  def even[A](list: Stream[A]): Trampoline[Boolean] = list match {
    case a #:: as => Suspend(() => odd(as))
    case Stream.Empty => Return(true)
  }

  def runTest(range: Stream[Int]) = even(range).run
}



trait EvenProgram {
  val bigRange = (1 to 10000).toStream

  def runTest(range: Stream[Int]): Boolean
  
  def main(args: Array[String]): Unit = {
    val isEven = runTest(bigRange)
    println(s"Even: $isEven")
  }
}


