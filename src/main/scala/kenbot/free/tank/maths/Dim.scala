package kenbot.free.tank.maths

case class Dim(width: Double, height: Double) {
  def positionedAt(topLeft: Vec) = Rect(topLeft.x, topLeft.y, topLeft.x + width, topLeft.y + height)
  def centeredAt(centre: Vec): Rect = {
    val halfW = width/2
    val halfH = height/2
    Rect(centre.x - halfW, centre.y - halfH, centre.x + halfW, centre.y + halfH)
  }
  
  def toTuple = (width, height)
  def toIntTuple = (width.round.toInt, height.round.toInt)
}