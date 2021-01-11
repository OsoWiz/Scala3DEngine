
  package world3D
import java.io._
object ModelLoader {
  
  /**Loads a 3D model from a .obj file*/
  def loadModel(filename:String):Option[Array[(Triangle,Coordinate)]] = {
    
  var pisteet = Array[Coordinate]()
  var kolmiot = Array[(Triangle,Coordinate)]()
  var normaalit = Array[Coordinate]()
  var normals = false
  var tulos:Option[Array[(Triangle,Coordinate)]] = None
  try{
    val file = new FileReader(filename)
    val luku = new BufferedReader(file)
    var line:String = luku.readLine()
    
    while(line != null){
      val tulos = line.split(' ')
      tulos.head match{
        case "v" => pisteet = pisteet:+ new Coordinate(tulos(1).toDouble,tulos(2).toDouble,tulos(3).toDouble)
        case "vn" => {
          normals = true
          normaalit = normaalit:+new Coordinate(tulos(1).toDouble,tulos(2).toDouble,tulos(3).toDouble)
        }
        case "f" =>{ 
          val ekat = tulos(1).split("//").map(_.toInt)
          val tokat = tulos(2).split("//").map(_.toInt)
          val kolmannet = tulos(3).split("//").map(_.toInt)
          if(normals){
          kolmiot = kolmiot:+(new Triangle(pisteet(ekat.head-1),pisteet(tokat.head-1),pisteet(kolmannet.head-1)),normaalit(ekat.last-1))
          } else kolmiot = kolmiot:+(new Triangle(pisteet(ekat.head-1),pisteet(tokat.head-1),pisteet(kolmannet.head-1)),null)
          
        }
        case default => 
          }
       line = luku.readLine()
      }
    
    if(!kolmiot.isEmpty ){ 
      
      tulos = Some(kolmiot)
    }
    
  }catch{
      case e1:FileNotFoundException => println("File not found"); 
      case e2:IOException => println("File opening failed"); 
    }
    
    
  tulos
    }
  
}
