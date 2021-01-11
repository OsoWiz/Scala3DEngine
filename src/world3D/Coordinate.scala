package world3D
import scala.math.sqrt

class Coordinate(var x:Double,var y:Double,var z:Double) {
  
  var w:Double = 1
 
  /**Tells how similar 2 vectors are. 0 is neutral, negative is opposite and positive is similar*/
  def dotProduct(another:Coordinate):Double = this.x*another.x+this.y*another.y+this.z*another.z
  def *(another:Coordinate) = this.dotProduct(another)
  
  /**returns a normal vector of a span of two vectors: this and the "another"*/
  def crossProduct(another:Coordinate):Coordinate =  new Coordinate(this.y*another.z-this.z*another.y,this.z*another.x-this.x*another.z,this.x*another.y-this.y*another.x)
  def X(another:Coordinate) = this.crossProduct(another)
   
  /**Sum of two vectors*/
  def sum(another:Coordinate):Coordinate = new Coordinate(this.x+another.x,this.y+another.y,this.z+another.z)
  def +(another:Coordinate) = this.sum(another)
  def -(another:Coordinate) = new Coordinate(this.x-another.x,this.y-another.y,this.z-another.z)
  
  /**Scalar multiplication*/
  def *(t:Double) = new Coordinate(this.x*t,this.y*t,this.z*t)
  
  /*returns inverse of this vector**/
  def inverse:Coordinate = new Coordinate(-this.x,-this.y,-this.z)
  def !(another:Coordinate):Coordinate= another.inverse
 
  /**Projects this into given coordinate*/
  def proj(onto:Coordinate):Coordinate = onto * ((this*onto)/(onto*onto))
  
    /**returns the length of this vector*/
  def length:Double = sqrt(this.x*this.x+this.y*this.y+this.z*this.z)
  
  /**Length in yz plane. Useful for painters*/
  def yzLength:Double = sqrt(this.y*this.y+this.z*this.z)
  
  /**A Method to turn this into an array*/
  def as3DArray:Array[Double] = Array(this.x,this.y,this.z)
  
  /**A method to  turn this into 4D to use the 4x4 matrices*/
  def as4DArray:Array[Double] = Array(this.x,this.y,this.z,w)
  
 
  
  /** For projection matrix and projection onto the screen*/
  def product4D(matriisi:Matrix):Coordinate={
    if(matriisi.columns == 4){
    val ari = this.as4DArray
    val simo =new Coordinate(matriisi.column(0).zip(ari).map(x=>x._1*x._2).sum,matriisi.column(1).zip(ari).map(x=>x._1*x._2).sum,matriisi.column(2).zip(ari).map(x=>x._1*x._2).sum)
    simo.w = matriisi.column(3).zip(ari).map(x=>x._1*x._2).sum
    
    if(simo.w > 0){
    simo.x = simo.x/simo.w
    simo.y = simo.y/simo.w
    simo.z = simo.z/simo.w
    }
    simo
    }
    else this
  }

  
  /**For matrices other than projection, where you must divide by z. Here, you won't*/
  def generalProduct(matriisi:Matrix):Coordinate = {
   if(matriisi.columns == 4){
    val ari = this.as4DArray
    val simo =new Coordinate(matriisi.column(0).zip(ari).map(x=>x._1*x._2).sum,matriisi.column(1).zip(ari).map(x=>x._1*x._2).sum,matriisi.column(2).zip(ari).map(x=>x._1*x._2).sum)
    simo.w = matriisi.column(3).zip(ari).map(x=>x._1*x._2).sum
    simo
    }
    else this
  } 
  
  /**normalizes this vector*/
  def normalize:Coordinate = {
    val length = sqrt(this.x*this.x+this.y*this.y+this.z*this.z)
    new Coordinate(this.x/length,this.y/length,this.z/length)
  }
  
  
  
  /**Prints the coordinates*/
  def printThis:Unit = println("X: "+this.x+" Y: "+this.y+" Z: "+this.z+" and the fourth member: "+ this.w)
  
  /**textual information about this vector*/
  def textData:String = "X: "+this.x+" Y: "+this.y+" Z: "+this.z+" and the fourth member: "+ this.w
  
}