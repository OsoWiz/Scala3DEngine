package world3D
import scala.math._

class Camera(initialPos:Coordinate,initialDir:Coordinate) {
  var pos = initialPos
  var dir = initialDir
  var up = new Coordinate(0,1,0)
  //Array of all blocking planes
  var blockers = Array[(Triangle,Coordinate)]()
  
  /**Moves the camera to specified direction*/
  def move(dir:Coordinate):Unit = if(blockers.forall(_._2*dir>=0)) this.pos = this.pos+dir 
  
  /**Moves the camera horisontally*/
  def yav(angle:Double):Unit = {
    val rad = (angle/180)*Pi
    val eka = new Coordinate(cos(rad),0,-sin(rad)) * dir
    val toka = new Coordinate(sin(rad),0,cos(rad)) * dir
    
    dir= new Coordinate(eka,dir.y,toka) 
  }

  
  
}