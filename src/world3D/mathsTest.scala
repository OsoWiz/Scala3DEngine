package world3D
import scala.math._
object mathsTest extends App {
 
  /**val perusmatriisi = new Matrix(2,2)
  for(i<-perusmatriisi.matrix.indices){
    for(j<-perusmatriisi.matrix(0).indices) println(perusmatriisi.matrix(i)(j))
    
  }
    
  perusmatriisi.setValues(Array(Array(1,2),Array(0,1)))
  for(i<-perusmatriisi.matrix.indices){
    for(j<-perusmatriisi.matrix(0).indices) println(perusmatriisi.matrix(i)(j))
    
  }
  val kertomamatriisi = perusmatriisi.product(perusmatriisi)
   for(i<-perusmatriisi.matrix.indices){
    for(j<-perusmatriisi.matrix(0).indices) println(kertomamatriisi.matrix(i)(j))
  }
  val matriisiA = new Matrix(2,4)
  val matriisiB = new Matrix(4,2)
  matriisiA.setValues(Array( Array(1,0,-1,2),Array(3,1,0,-2) ))
  matriisiB.setValues( Array( Array(0,1),Array(1,1),Array(0,2),Array(1,2) ) )
  val kertoma2 = matriisiA.product(matriisiB)
  for(i<-kertoma2.matrix.indices){
    for(j<-kertoma2.matrix(0).indices) println(kertoma2(i)(j))
  }
  var testiarvo = 5
  def testi = testiarvo
  
  val pysyykovartestimatriisi = new Matrix(2,2)
  pysyykovartestimatriisi.setValues(Array(Array(1,2),Array(6,testi)))
  println("Arvot ovat nyt:")
   for(i<-pysyykovartestimatriisi.matrix.indices){
    for(j<-pysyykovartestimatriisi.matrix(0).indices) println(pysyykovartestimatriisi(i)(j))
  }
  testiarvo = 10
  //pysyykovartestimatriisi.setSingleValue(1,1,15)
  println("Testiarvo on nyt "+testiarvo)
  println("Uudet arvot ovat nyt:")
   for(i<-pysyykovartestimatriisi.matrix.indices){
    for(j<-pysyykovartestimatriisi.matrix(0).indices) println(pysyykovartestimatriisi(i)(j))
  }
  
  println("Luodaan uusi kamera")
  val kamera = new Camera(new Coordinate(0,0,0),new Coordinate(1,0,0))
  println("Kamera on nyt:" )
  kamera.pos.printThis
  println("siirretään kameraa x akselin suunnassa kahdella")
  kamera.move(new Coordinate(2,0,0))
   println("Kamera on siirron jälkeen:" )
  kamera.pos.printThis
  
  val eteenz = new Coordinate(0,0,1)
  val ylos = new Coordinate(0,-1,0)
  val suunta1 = eteenz.crossProduct(ylos)
  val suunta2 = ylos.crossProduct(eteenz)
  println("eteen kertaa ylös on: ")
  suunta1.printThis
  println("ylös kertaa eteen on: ")
  suunta2.printThis
  */
  
 
  /**
   val äksä = new Coordinate(1,0,0)
   val yy = new Coordinate(0,1,0)
   val zeta = new Coordinate(0,0,1)
   val ygy = new Coordinate(1,1,1)
   val crossi1 = zeta X yy
   val crossi2 = yy X zeta
   crossi1.printThis
   crossi2.printThis
   val iso = ygy*12
   iso.normalize.printThis
   
   val koordinaatti = new Coordinate(10,1,50)
   val koordinaatti2 = new Coordinate(20,1,50)
   val kolmaskolmio = new Triangle(koordinaatti,koordinaatti2,new Coordinate(30,1,40))
   val eripain = new Triangle(new Coordinate(30,1,40),koordinaatti2,koordinaatti)
   kolmaskolmio.surfaceNormal.printThis
   eripain.surfaceNormal.printThis
   
   val uusMatriisi = new Matrix(4,4)
   uusMatriisi.setRow(0, koordinaatti)
   uusMatriisi.setRow(1, koordinaatti2)
   val uusMatriisi2 = new Matrix(4,4)
   uusMatriisi2.setColumn(0, koordinaatti)
   uusMatriisi2.setColumn(1, koordinaatti2)
   
   println("eka\n")
   uusMatriisi.printMat
   println("toka\n")
   uusMatriisi2.printMat
   */
  /**
  val plane = new Coordinate(0,0,1)
  val point1 = new Coordinate(2,0,2)
  val point2 = new Coordinate(-2,0,2)
  val origo = new Coordinate(0,0,0)
  val klippi = new Triangle(point1,point2,origo)
  val ari = klippi.clipAgainstPlane(plane, plane)
  
  println(ari.size)
  ari.foreach(x=>x.asArray.foreach(_.printThis))
  
  klippi.planeIntersect(plane, plane, origo, point2).printThis
  */
  val eka =  new Coordinate(1,1,-1)
  val toka  =  new Coordinate(1,-1,-1)
  val kolmas  =  new Coordinate(1,1,1)
  val kolmio = new Triangle(eka,toka,kolmas)
  val raja = 2
  val testCam = new Camera(new Coordinate(2,0.5,0),new Coordinate(-1,0,0))

 // println(kolmio.isBlocker(new Coordinate(1,0,0), testCam.pos, raja))
  //testCam.move(testCam.dir)
  println(kolmio.isTriangle)
  println("luodaan uusi testikolmio, joka ei oikeasti edes ole kolmio")  
  val kolmio2 = new Triangle(eka,toka,toka)
  
  println(kolmio2.isTriangle)
  val kolmio3 = new Triangle(eka,new Coordinate(28.99,0.6,0.7999),new Coordinate(28.99,0.6,0.7999) )
  println(kolmio3.isTriangle)
  
  val kulma = 45
  println(kulma)
  println(kulma.toRadians)
  
}