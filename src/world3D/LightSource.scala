package world3D
import scala.math._
import java.awt.Color
class LightSource(power:Int,val location:Coordinate) {
  
  val luminance = if(power<0) 0 else if(power>100) 100 else power
  
  def shade(tri:(Triangle,Coordinate)):Color = {
    val pointLight = (this.location-tri._1.centroid).normalize
    val samanlaisuus = max((1+pointLight*tri._2)/2,0.03)
    val teho = samanlaisuus*luminance/100
    new Color((tri._1.color.getRed*teho).toInt,(tri._1.color.getGreen*teho).toInt,(tri._1.color.getBlue*teho).toInt)
  }
  
}