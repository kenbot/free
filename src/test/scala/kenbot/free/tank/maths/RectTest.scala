package kenbot.free.tank.maths

import org.scalacheck.Arbitrary
import org.scalacheck.Gen
import org.scalacheck.Prop
import org.scalacheck.Prop.forAll
import org.scalatest.PropSpec
import org.scalatest.Matchers
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import scalaz._
import Scalaz._


class RectTest extends PropSpec with GeneratorDrivenPropertyChecks with Matchers {
  val genDouble = Gen.chooseNum(-999999.0, 999999.0, -1.0, 0.0, 1.0)
  
  implicit val genRect: Gen[Rect] = for {
    x1 <- genDouble
    y1 <- genDouble
    x2 <- genDouble
    y2 <- genDouble
  } yield Rect(x1, y1, x2, y2)
  
  sealed trait Empty
  
  val genEmptyRect: Gen[Rect @@ Empty] = for {
    x <- genDouble
    y <- genDouble
  } yield Tag(Rect(x, y, x, y))
  
  implicit val arbRect = Arbitrary(genRect)
  implicit val arbEmptyRect = Arbitrary(genEmptyRect)
  
  property("Intersection is reflexive, when non-empty") {
    forAll { (r: Rect) =>
      whenever(!r.isEmpty) { 
        r intersects r should be (true)
      }
    }
  }
  
  property("Empty rectangles shouldn't intersect anything") {
    forAll { (emptyRect: Rect @@ Empty, r: Rect) =>
      whenever(emptyRect.isEmpty) { 
        emptyRect intersects r should be (false)
      }
    }
  }
  
  property("Intersection is symmetric") {
    forAll { (r1: Rect, r2: Rect) =>
      r1 intersects r2 should equal (r2 intersects r1)
    }
  }
  
  property("Concentric rectangles intersect") {
    forAll { (r: Rect) =>
      whenever(!r.isEmpty) {
        val r2 = modifyRect(r)(_ + _, _ + _, _ - _, _ - _)
        r intersects r2 should be (true)
      }
    }
  }
  
  property("Criss-crossing rects intersect") {
    forAll { (r: Rect) =>
      whenever(!r.isEmpty) {
        val r2 = modifyRect(r)(_ + _, _ - _, _ - _, _ + _)
        r intersects r2 should be (true)
      }
    }
  }
  
  property("Overlapping rects intersect") {
    forAll { (r: Rect) =>
      whenever(!r.isEmpty) {
        val r2 = modifyRect(r)(_ + _, _ + _, _ + _, _ + _)
        r intersects r2 should be (true)
      }
    }
  }
  
  property("Horizontally adjacent rects don't intersect") {
    forAll { (r: Rect) =>
      whenever(!r.isEmpty) {
        val r2 = Rect(r.x2, r.y1, r.x2 + 5, r.y2)
        r intersects r2 should be (false)
      }
    }
  }
  
  property("Vertically adjacent rects don't intersect") {
    forAll { (r: Rect) =>
      whenever(!r.isEmpty) {
        val r2 = Rect(r.x1, r.y2, r.x2, r.y2 + 5)
        r intersects r2 should be (false)
      }
    }
  }
  
  type BinOp = (Double, Double) => Double
  
  private def modifyRect(r: Rect)(x1f: BinOp, y1f: BinOp, x2f: BinOp, y2f: BinOp): Rect = {
    val Rect(x1, y1, x2, y2) = r
    val hw = r.width/4
    val hh = r.height/4
    
    Rect(x1f(x1, hw), y1f(y1, hh), 
         x2f(x2, hw), y2f(y2, hh)) 
  }
}