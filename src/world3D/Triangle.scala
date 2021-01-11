package world3D
import java.awt.Color
import scala.math.abs
/**This class represents triangles that can be found in 3D meshes. The points MUST BE GIVEN IN CLOCKWISE ORDER*/
class Triangle(var first:Coordinate,var second:Coordinate,var third:Coordinate) {
  
  //All triangles are white by default
  var color = new Color(255,255,255)
  
  /**This triangle, as an array*/
  def asArray = Array(first,second,third)
  
 /**Gives all the x coordinates in order*/
  def xArray = Array(first.x,second.x,third.x)
  
  /**Gives all the y coordinates in order*/
  def yArray = Array(first.y,second.y,third.y)

  /**Gives all the z coordinates in order*/
  def zArray = Array(first.z,second.z,third.z)
  
  /**Sounds really cool ;) but it is just the mass center of this triangle*/
  def centroid:Coordinate = new Coordinate( (this.xArray.sum)/3,(this.yArray.sum)/3,(this.zArray.sum)/3)
  
 
  
  /**inverts all the x and y points of this triangle ,and returns that triangle*/
  def invertXY:Triangle = {
    def invert(cor:Coordinate) = new Coordinate(-1*cor.x,-1*cor.y,cor.z)
    val inv = this.asArray.map(invert(_))
    val pal = new Triangle(inv(0),inv(1),inv(2))
    pal.color = this.color
    pal
  }
  
  /**returns a triangle with added centering to each of the x and y coordinates of this triangle*/
  def centering(xCentering:Int,yCentering:Int):Triangle = {
    def center(cor:Coordinate) = new Coordinate(cor.x+xCentering,cor.y+yCentering,cor.z)
    val cen = this.asArray.map(center(_))
    val pal =new Triangle(cen(0),cen(1),cen(2))
  pal.color = this.color
  pal
  }
  
  /**returns a new triangle in reverse winding order*/
  def reverse:Triangle ={val pal = new Triangle(this.third,this.second,this.first)
  pal.color = this.color
  pal
  }
  
  /**Clips this triangle against given plane and returns clipped triangles (0-2 triangles)*/
  def clipAgainstPlane(planePoint:Coordinate,planeNormal:Coordinate):Array[Triangle] = {
    
    def distance(point:Coordinate):Double = {planeNormal*point-planeNormal*planePoint} 
    
    var insideCount = 0
    var outsideCount = 0
    val outsidePoints = Array.ofDim[Coordinate](3)
    val insidePoints = Array.ofDim[Coordinate](3)
    
    //Distances
    val d0 = distance(this.first)
    val d1 = distance(this.second)
    val d2 = distance(this.third)
    
    if(d0>=0){insidePoints(0) = this.first; insideCount+=1}
    else{outsidePoints(0) = this.first; outsideCount+=1}
    if(d1>=0){insidePoints(1) = this.second; insideCount+=1}
    else{outsidePoints(1) = this.second; outsideCount+=1}
    if(d2>=0){insidePoints(2) = this.third; insideCount+=1}
    else{outsidePoints(2) = this.third; outsideCount+=1}
    
    if(insideCount == 0){
      //println("this one is completely outside the plane")
      return Array()
    }else if(insideCount == 3){
      //println("this one is completely inside the plane")
      Array(this)
    }else if(insideCount == 1 && outsideCount == 2){
      
      val sisa = insidePoints.filter(_!=null).head
    //println("1 sisällä, kaks ulkona, sisällä olevan pisteen data on: "+sisa.textData)
      val eka = planeIntersect(planePoint,planeNormal,sisa,outsidePoints.filter(_!=null).head)
      val toka = planeIntersect(planePoint,planeNormal,sisa,outsidePoints.filter(_!=null).last)
      
      val uuskolmio = new Triangle(sisa , eka, toka )
      uuskolmio.color = this.color
       Array(uuskolmio)
    }
    else if(insideCount == 2 && outsideCount == 1){
      
      val sisa = insidePoints.filter(_!=null)
      val ulkoP = outsidePoints.filter(_!=null)
      //println("2 sisällä, yks ulkona, sisälläolevien koko on: "+ sisa.size +" ja ulkonaolevien: "+ulkoP.size)
      val ekaInter = planeIntersect(planePoint,planeNormal,sisa.head,ulkoP.head)
      val tokaInter = planeIntersect(planePoint,planeNormal,sisa.last,ulkoP.head)
      
      val eka = new Triangle(sisa.head,sisa.last,ekaInter)
      val toka = new Triangle(sisa.last,ekaInter,tokaInter)
      
      eka.color = this.color
      toka.color = this.color
      
      Array(eka,toka)
      
       
    }else Array()
    
    
  }
  
  /**Checks wether this triangle is blocking the movement of the camera*/
  def isBlocker(nor:Coordinate,cPos:Coordinate,border:Double):Boolean = {
    //(cPos-this.centroid).proj(nor).printThis
    
     if((cPos-this.centroid).proj(nor).length<border){
      val piste = cPos-cPos.proj(nor)
      val ekak = this.second-this.first
      val tokak = this.third-this.second
      val kolmask = this.first-this.third
      
      val t1 = (this.first+ekak*0.5)-this.centroid
      val t2 = (this.second+tokak*0.5)-this.centroid
      val t3 = (this.third+kolmask*0.5)-this.centroid
      
      val n1 = t1-t1.proj(ekak)
      val n2 = t2-t2.proj(tokak)
      val n3 = t3-t3.proj(kolmask)
      
      val p1 = piste-this.first
      val p2 = piste-this.second
      val p3  = piste-this.third
      
      val b1 = p1*n1<0
      val b2 = p2*n2<0
      val b3 = p3*n3<0
      
      ((b1==b2) && (b2 == b3))
      }else false
    
    
  }
 
  
  /**Returns a point where a line crosses a plane*/
  private def planeIntersect(planePoint:Coordinate,planeNormal:Coordinate,startPoint:Coordinate,endPoint:Coordinate):Coordinate = {
    
    val planeDir = -(planePoint*planeNormal)
    val ekaDir = (startPoint*planeNormal)
    val tokaDir = (endPoint*planeNormal)
    val t = (-planeDir-ekaDir)/(tokaDir-ekaDir)
    val startToEnd = endPoint-startPoint
    val pointToIntersect = startToEnd*t
    startPoint+pointToIntersect
  }
  
   
  /**Moves the mesh by a vector*/
  def translate(vector:Coordinate) = {
       val array =this.asArray.map(x=>x+vector)
    val pal =new Triangle(array(0),array(1),array(2))
    pal.color = this.color
       pal
  }
  
  /**Calls the 4D product for each of this triangles points*/
  def project(matriisi:Matrix):Triangle = {
    val array =this.asArray.map(x=>x.product4D(matriisi))
    
   val pal = new Triangle(array(0),array(1),array(2))
  pal.color = this.color
  pal
  }
  
  /**Applies the given matrix transform to all of this triangles points*/
  def transform(matriisi:Matrix):Triangle = {
    val array =this.asArray.map(x=>x.generalProduct(matriisi))
    
    val pal =new Triangle(array(0),array(1),array(2))
  pal.color = this.color
  pal
  }

  /**Checks wether this is actually a triangle we have here*/
  def isTriangle:Boolean = {
    def diffCheck(eka:Coordinate,toka:Coordinate) = {
      val xSam = abs(toka.x-eka.x)<0.01
      val ySam = abs(toka.y-eka.y)<0.01
      val zSam = abs(toka.z -eka.z)<0.01
     !(xSam && ySam&& zSam)
    }
    diffCheck(this.first,this.second) && diffCheck(this.second,this.third) && diffCheck(this.third,this.first)
  }
  
  /**Returns the surface normal of this triangle*/
  def surfaceNormal:Coordinate = {
    val eka = second-first
    val toka = third-first
    
    (eka X toka).normalize
  }
  
  def printInfo:Unit = {
    println("This triangle consists of these points:")
    this.asArray.foreach(_.printThis)
    println("with a centroid of:")
    this.centroid.printThis
  }
  
}