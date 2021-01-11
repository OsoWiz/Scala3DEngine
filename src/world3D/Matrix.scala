package world3D
import scala.math._
/**Creates an empty matrix with given row-,and column sizes*/
class Matrix(val rows: Int,val columns:Int) {
 
  var matrix = Array.ofDim[Double](rows, columns)
  
  def apply(i:Int)(j:Int) = this.matrix(i)(j)
  
  /**Sets values specified in the 2d array. Does nothing if dimensions are incorrect*/
  def setValues(arvot:Array[Array[Double]]):Unit = {
    if(arvot.length==rows && arvot.map(_.length).forall(_ == columns)){
      for(index<-arvot.indices){  this.matrix(index) = arvot(index)   }  
      }
    }
  
  /**Sets a single value to a certain spot*/
  def setSingleValue(rowIndex:Int,colIndex:Int,value:Double):Unit = this.matrix(rowIndex)(colIndex) = value
  /**Sets a certain Coordinate to specified column. Works only for 4x4 matrices*/
  def setColumn(i:Int,vctr:Coordinate):Unit = {
    if(i<this.columns && this.rows >3){
      val array = vctr.as4DArray
      for(ind<-array.indices){
        this.matrix(ind)(i) = array(ind)
      }
      
    }
  }
  /**Sets a certain Coordinate to specified row. Works only for 4x4 matrices*/
  def setRow(i:Int,vctr:Coordinate):Unit = {
    if(i<this.rows && this.columns >3){
      this.matrix(i) = vctr.as4DArray
    }
  }
  
  /**Gets a column given by the index*/
  def column(indeksi:Int):Array[Double] = {
   if(indeksi<columns){ 
    val kolumni =Array.ofDim[Double](rows)
    for(i<-0 until rows){kolumni(i)=matrix(i)(indeksi)}
    kolumni
    }
   else Array.ofDim[Double](columns)
  }
  
  /**Gets a row given by the index*/
  def row(indeksi:Int):Array[Double] = {
    if(indeksi<rows) matrix(indeksi)
    else Array.ofDim[Double](rows)
  }
  
  
  /**product with another matrix*/
  def product(another:Matrix):Matrix = {
    if(this.columns == another.rows){
      val uusi = new Matrix(this.rows,another.columns)
      for(row<-0 until this.rows){
        for(col<-0 until another.columns){
          val arvo = this.row(row).zip(another.column(col)).map(x=>x._1*x._2).sum
          uusi.setSingleValue(row, col, arvo)
        }
      }
      uusi
    }else this
  }
  
  def * (another:Matrix) = this.product(another)
  
  /**For updating viewmatrix. Updates THIS matrix. This should be only used for the viewMatrix*/
  def updateViewMatrix(kamera:Camera):Unit = {
    if(this.rows == 4 && this.columns == 4){
      val t = kamera.pos
      val c = kamera.dir
      val b = kamera.up
      val a = (b X c).normalize
      val minusTA = -(t*a)
      val minusTB = -(t*b)
      val minusTC = -(t*c)
      
      this.setColumn(0, a)
      this.setColumn(1, b)
      this.setColumn(2, c)
      this.setSingleValue(3, 0, minusTA)
      this.setSingleValue(3, 1, minusTB)
      this.setSingleValue(3, 2, minusTC)
      
    }
  //metodi päättyy tähän
  }
  
  
 
  /**Prints out this matrix*/
  def printMat:Unit = {
    println("This matrix has these values:")
    for(i<-0 until this.rows){
      for(j<- 0 until this.columns-1){
        print(this.matrix(i)(j)+" ")
        
      }
      print(this.matrix(i)(this.columns-1)+"\n")
    }
    
  }
  
  private def angleToRad(kulma:Double) = kulma/180*Pi
  
  //luokka päättyy tähän
}


