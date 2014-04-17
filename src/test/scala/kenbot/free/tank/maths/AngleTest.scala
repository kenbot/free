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


class AngleTest extends PropSpec with GeneratorDrivenPropertyChecks with Matchers {
  import Angle._
  
  val genDouble = Gen.chooseNum(-10000.0, 10000.0, 0.0, 
      Pi, TwoPi, -Pi, -TwoPi, TwoPi + 0.0001, TwoPi - 0.0001, 0.0001, -0.0001)
  
  implicit val genAngle: Gen[Angle] = for (d <- genDouble) yield Angle(d)
  implicit val arbAngle = Arbitrary(genAngle)
  
  
  val tolerance = 0.00001

  property("Adding a full circle gives the same angle") {
    forAll { (a: Angle) =>
      shouldBeRoughlyEqual(a + Full, a)
    }
  }
  
  property("Adding zero gives the same angle") {
    forAll { (a: Angle) =>
      shouldBeRoughlyEqual(a + Zero, a)
    }
  }

  property("An angle equals itself") {
    forAll { (a: Angle) =>
      a should equal (a)
    }
  }
    
  property("An angle equals an angle with the same radians") {
    forAll { (a: Angle) =>
      a should equal (Angle(a.radians))
    }
  }
  
  property("An angle translated to degrees and back shouldn't change") {
    forAll { (a: Angle) =>
      shouldBeRoughlyEqual(degrees(a.degrees), a)
    }
  }

  property("An angle added to its negative should equal zero") {
    forAll { (a: Angle) =>
      a - a should equal (Zero)
    }
  }
  
  property("'Add up to' will add if it doesn't cross the limit") {
    forAll { (a: Angle) =>
      val limit = a + Angle(1.1)
      shouldBeRoughlyEqual(a.addUpTo(Angle(1), limit), a + Angle(1))
    }
  }
  
  property("'Add up to' will return limit if it crosses the limit") {
    forAll { (a: Angle) =>
      val limit = a + Angle(0.5)
      shouldBeRoughlyEqual(a.addUpTo(Angle(1), limit), limit)
    }
  }
  
  property("Angle radians should never be >= 2 pi") {
    forAll { (a: Angle) =>
      a.radians should be < (TwoPi)
    }
  }
  
  property("Angle radians should never be < 0") {
    forAll { (a: Angle) =>
      a.radians should be >= (0.0)
    }
  }
  
  property("An angle's distance to itself should be 0") {
    forAll { (a: Angle) =>
      shouldBeRoughlyEqual(a, a)
    }
  }
  
  property("The distance between any two angles should be <= pi") {
    forAll { (a: Angle, b: Angle) =>
      a.distanceTo(b).radians should be <= (Pi)
    }
  }
  
  property("Angle is left of anything on the right side") {
    forAll { (a: Angle) =>
      a.isLeftOf(a + Angle(Pi - 0.001)) should be (true)
      a.isLeftOf(a + Angle(0.001)) should be (true)
    }
  }
  
  property("Angle is right of anything on the left side") {
    forAll { (a: Angle) =>
      a.isRightOf(a - Angle(Pi - 0.001)) should be (true)
      a.isRightOf(a - Angle(0.001)) should be (true)
    }
  }
  
  property("Angle isn't right of anything on the right side") {
    forAll { (a: Angle) =>
      a.isRightOf(a + Angle(Pi - 0.001)) should be (false)
      a.isRightOf(a + Angle(0.001)) should be (false)
    }
  }
  
  property("Angle isn't left of anything on the left side") {
    forAll { (a: Angle) =>
      a.isLeftOf(a - Angle(Pi - 0.001)) should be (false)
      a.isLeftOf(a - Angle(0.001)) should be (false)
    }
  }
  
  property("Angle is right of the opposite side") {
    forAll { (a: Angle) =>
      a.isRightOf(a + Angle.Half) should be (true)
    }
  }
  
  property("Angle is left of the opposite side") {
    forAll { (a: Angle) =>
      a.isLeftOf(a + Angle.Half) should be (true)
    }
  }
  
  property("Angle is not left of itself") {
    forAll { (a: Angle) =>
      a isLeftOf a should be (false)
    }
  }
  
  property("Angle is not right of itself") {
    forAll { (a: Angle) =>
      a isRightOf a should be (false)
    }
  }
  
  private def shouldBeRoughlyEqual(a: Angle, b: Angle) {
    (b/2).sin - (a/2).sin should be (0.0 +- tolerance)
  }
}