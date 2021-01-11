package gui3D

import world3D._
import javax.swing._
import scala.math._
import java.awt._


class gui(var leveys:Int,var korkeus:Int,var fov:Int,filename:String,lights:Option[LightSource],NC:Boolean) extends JPanel {
  
  if(lights.isEmpty) println("No light") else lights.get.location.printThis
  //Intializes the viewer
  val katsoja = new Camera(new Coordinate(0,0,0),new Coordinate(0,0,1))
 
 val katse = new Coordinate(0,0,1)
 
   
  
  def levSka = leveys/2
  def korSka = korkeus/2
  
  def a = korkeus*1.0/leveys
  
  def angleToRad(angle:Double) = (angle*1.0/180)*Pi
  def f = 1/tan(angleToRad(fov)/2)
  
  val zFar = 1000
  val zNear = 0.3
  var border = 0.5
  def q = zFar*1.0/(zFar-zNear)
  
  var existNormals = false
  var noclip = NC
  var dynamicLight = false
  
  //Clipping planes
  val xVasP = new Coordinate(0,0,0)
  def xVasN = new Coordinate(1,0,0)
  
  val xOikP = new Coordinate(leveys-1,0,0)
  def xOikN = new Coordinate(-1,0,0)
  
  val yTopP = xVasP
  def yTopN = new Coordinate(0,1,0)
  
  val yBotP = new Coordinate(0,korkeus-1,0)
  def yBotN = new Coordinate(0,-1,0)
  
  def zNearPoint = new Coordinate(0,0,zNear)
  
  
    //Luodaan seuraavaksi matriisit
  val viewMatrix = new Matrix(4,4)
  //This one value is just for preserving the w component
 viewMatrix.setSingleValue(3, 3, 1)
  
  //Projection matrix
  val projectionMatrix = new Matrix(4,4)
    projectionMatrix.setSingleValue(0, 0, a*f)
    projectionMatrix.setSingleValue(1, 1, f)
    projectionMatrix.setSingleValue(2, 2, q)
    projectionMatrix.setSingleValue(2, 3, 1)
    projectionMatrix.setSingleValue(3, 2, -zNear*q)
    
 

  
  val skaalaus = new Matrix(4,4)
  skaalaus.setSingleValue(0, 0, levSka)
  skaalaus.setSingleValue(1, 1, korSka)
  skaalaus.setSingleValue(2,2,1)
  var i = 0
  
  
  //Useful functions under here
  
  /**scales the color*/
  def scaleColor(vahvuus:Double) ={ 
    val vahv =if(vahvuus<0) 0 else min(1,vahvuus)
    new Color((vahv*255).toInt,(255*vahv).toInt,(255*vahv).toInt)
  }
  
  /**Clips the triangles used here*/
  def clip(tris:Array[(Triangle,Coordinate)],tasoP:Coordinate,tasoN:Coordinate) = {
    var lopullinen = Array[(Triangle,Coordinate)]()
    for(tri<-tris){
      val klipatut = tri._1.clipAgainstPlane(tasoP, tasoN)
      if(!klipatut.isEmpty){
       lopullinen = lopullinen++klipatut.map(_->tri._2) 
      }  
    }
    lopullinen
  } 
  
  /**updates the blockers aka. directions where the camera cannot move*/
  def updateBlockers(tris:Array[(Triangle,Coordinate)]):Unit = {
    val close = tris.filter(x=>x._1.isBlocker(x._2,katsoja.pos,border))
    katsoja.blockers = close
  }
  

    
    //vakiokuution koordinaatit
    val p1 = new Coordinate(1,1,1)
    val p2 = new Coordinate(1,1,-1)
    val p3 = new Coordinate(-1,1,-1)
    val p4 = new Coordinate(-1,1,1)
    
    val p1a = new Coordinate(1,-1,1)
    val p2a = new Coordinate(1,-1,-1)
    val p3a = new Coordinate(-1,-1,-1)
    val p4a = new Coordinate(-1,-1,1)
    
  val t1 = new Triangle(p1,p1a,p4)
  val t2 = new Triangle(p4,p1a,p4a)
  
  val t3 = new Triangle(p2,p2a,p1)
  val t4 = new Triangle(p1,p2a,p1a)   
  
   val t5 = new Triangle(p2,p3,p2a)
  val t6 = new Triangle(p2a,p3,p3a)  
  
   val t7 = new Triangle(p3,p4,p4a)
  val t8 = new Triangle(p3a,p3,p4a) 
  
  val t9 = new Triangle(p1,p3,p2)
  val t10 = new Triangle(p1,p4,p3)
  
  val t11 =  new Triangle(p2a,p3a,p1a)
  val t12 = new Triangle(p3a,p4a,p1a) 
    
  //Ladataan jotain siltä varalta,että mitään ei saada ladattua
  var kuutio =  Array(t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,t12).reverse zip Array(t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,t12).reverse.map(_.surfaceNormal)
 kuutio = kuutio.map(x=>(x._1.translate(new Coordinate(0,0,5))->x._2))
  
 val ladattu = ModelLoader.loadModel(filename)
  if(!ladattu.isEmpty){ 
    kuutio = ladattu.get
    println("Succesfully loaded the model")
    }else{
      println("invalid file, trying to load the default one...")
      val uus = ModelLoader.loadModel("models/huoneisto.obj")
      if(!uus.isEmpty){
        println("Succesfully loaded the default model")
        kuutio = uus.get
      }else println("Failed to load the default model, using test cube instead.")
    }
  
  //Check if normaldata exists
  if(kuutio.forall(_._2!=null)){
  existNormals = true
  println("Normaldata found") 
  }else{ 
    println("normal data missing, calculating normals..")
     kuutio = kuutio.map(x=>(x._1->x._1.surfaceNormal))
    }
  
  //Check that everything in the obj file is actually useful. This filters out non-triangles
  kuutio = kuutio.filter(_._1.isTriangle)
  
  //Check lighting
  if(lights.isEmpty) dynamicLight = true
  else{
    val valo = lights.getOrElse(new LightSource(100,new Coordinate(0,10,0)))
     //Shading the triangles
    for(kolmio <- kuutio){
   kolmio._1.color = valo.shade(kolmio)
   }
  }
  
  
  
  
  ////////////////////////////////////////////////// Painter under this line////////////////////////////////////////////////////////////////////////
  
   //Painter
   override def paintComponent(grafiikka:Graphics){
    var kolmiot = kuutio
     
    
    //Piirtää taustan
   grafiikka.setColor(Color.BLACK)
   grafiikka.fillRect(0, 0, leveys, korkeus)
   
   
     
   //Updating the matrix:
   viewMatrix.updateViewMatrix(katsoja)
   
   //Filter all the normals that wont point at the camera
   kolmiot =kolmiot.filter(x=>(x._1.centroid-katsoja.pos)*x._2<0)
   //Update the directions where the camera cannot move
  if(!noclip) updateBlockers(kolmiot)
   
   //Bring to view Space:
   kolmiot = kolmiot.map(x=>x._1.transform(viewMatrix)->x._2)
   
  
   
   //After viewing, I need to clip them against the near plane
    kolmiot = clip(kolmiot,zNearPoint, katse)
   
    
   //bring triangles to the projection space:
  kolmiot = kolmiot.map(x=>(x._1.project(projectionMatrix),x._2))
  
  
  //Let's sort these projected triangles now
  kolmiot = kolmiot.sortBy(x=> x._1.asArray.map(_.z).sum).reverse
  
  
  //And we should invert x and y coordinates of the triangles
  kolmiot = kolmiot.map(x=>x._1.invertXY->x._2)
  
  
  //For the offset one can use this, or centering. We are using offset so this is commented off
  //kolmiot = kolmiot.map(_.translate(new Coordinate(1,1,0)))
  
  //scale them to screen space
  kolmiot = kolmiot.map(x=>x._1.transform(skaalaus)->x._2)
  
  
  //And offset them to the center of the screen. One can use this or the outcommented scale offset above
  kolmiot = kolmiot.map(x=>x._1.centering(levSka, korSka)->x._2)
  
   
  
  //Clipping by all other fustrum planes
  for(luku<-0 until 4){
    luku match{
      case 0 => kolmiot = clip(kolmiot ,xVasP, xVasN)
      case 1 => kolmiot = clip(kolmiot,yTopP, yTopN)
      case 2 => kolmiot = clip(kolmiot,xOikP, xOikN)
      case 3 => kolmiot = clip(kolmiot,yBotP, yBotN)
    }
  }
  
 

 
   
   
   //And finally, let's draw these badboys
   for(tri<-kolmiot){
     
    val norm = tri._2
    val kolmio = tri._1
     val skaala = 1-pow(kolmio.zArray.sum/3,10)
     //If dynamic lighting is turned on, this calculation will take effect
     if(dynamicLight)kolmio.color = scaleColor(skaala)
     
     //Color has to be correct
     grafiikka.setColor(kolmio.color)
    
   //Fills the triangle 
      grafiikka.fillPolygon(kolmio.xArray.map(_.toInt), kolmio.yArray.map(_.toInt), 3)
     
   }
     
     //location and direction info
   grafiikka.setColor(Color.GREEN)
   grafiikka.drawString("Position: "+katsoja.pos.textData, 10, 10)
   grafiikka.drawString("Direction: "+katsoja.dir.textData, 10, 25)
   
   
     
    }
  
  
  
}