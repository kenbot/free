package kenbot.free.tank.maths

object Percentage {
  
  val One = Percentage(1.0)
  val Zero = Percentage(0.0)
  
  def apply(amount: Double): Percentage = new Percentage(normalize(amount))
  
  def normalize(amount: Double): Double = {
    if (amount < 0) 0
    else if (amount > 1) 1
    else amount
  }
}

class Percentage(val amount: Double) extends AnyVal {
  def +(other: Percentage) = Percentage(amount + other.amount)
  def *(other: Percentage) = Percentage(amount * other.amount)
  def complement = Percentage(1.0 - amount)
}
