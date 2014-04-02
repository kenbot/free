package kenbot.free.tank.maths

import org.scalacheck.Arbitrary
import org.scalacheck.Gen
import org.scalacheck.Prop
import org.scalacheck.Prop.forAll
import org.scalatest.PropSpec
import org.scalatest.Matchers


class VecTest extends PropSpec with Matchers {
  val genPositiveDouble = Gen.chooseNum(0.0, Double.MaxValue)
  
  implicit val genVec: Gen[Vec] = for {
    x <- genPositiveDouble
    y <- genPositiveDouble
  } yield Vec(x, y)
  
  implicit val arbVec = Arbitrary(genVec)
  
  property("Full rotation should be back where you started") {
    forAll { (v: Vec) =>
      (v rotate Angle.Full).angle == v.angle
    }
  }
}