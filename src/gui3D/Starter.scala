package gui3D
import world3D._
import javax.swing._
import java.util.{Timer,TimerTask}
import java.awt.Color
import java.awt.event._
import javax.swing.filechooser.FileSystemView
import javax.swing.event._


class Starter(width:Int,height:Int,fov:Int,filename:String,valot:Option[LightSource],noclip:Boolean) extends App {
  
  val liittyma = new JFrame
  
  val paneeli = new gui(width,height,fov,filename,valot,noclip)
  //Kameran kanssa kikkailu helpottuu hakemalla viittaus siitä muuttujaan
  val kamera = paneeli.katsoja
  
  
  
  println(paneeli.leveys)
  println(paneeli.korkeus)
  println(paneeli.a)
  println(paneeli.f)
  println(paneeli.levSka)
  println(paneeli.korSka)
 
  
  liittyma.addKeyListener(new KeyListener(){
    def keyTyped(e:KeyEvent){}
    
    def keyPressed(e:KeyEvent){
      val keyCode = e.getKeyCode()
      keyCode match  {
        case KeyEvent.VK_RIGHT => kamera.move(kamera.dir.crossProduct(kamera.up).normalize*0.1)
        case KeyEvent.VK_LEFT => kamera.move(kamera.up.crossProduct(kamera.dir).normalize*0.1)
        case KeyEvent.VK_DOWN => kamera.move(kamera.dir.inverse*0.1)
        case KeyEvent.VK_UP => kamera.move(kamera.dir*0.1)
        case KeyEvent.VK_W => kamera.move(kamera.up*0.1)
        case KeyEvent.VK_S => kamera.move(kamera.up.inverse*0.1)
        case KeyEvent.VK_SHIFT => kamera.move(kamera.up.inverse*0.1)
        case KeyEvent.VK_SPACE => kamera.move(kamera.up*0.1)
        case KeyEvent.VK_D => kamera.yav(2)
        case KeyEvent.VK_A => kamera.yav(-2)
      }
  
    }
    
    def keyReleased(e:KeyEvent){}
  })
  
  
  val task = new TimerTask{
    def run() = {
      
      paneeli.repaint()
      
    }
  }
 
  
  liittyma.setBackground(Color.BLACK)
  liittyma.setSize(paneeli.leveys,paneeli.korkeus)
  
  liittyma.add(paneeli)
  
  liittyma.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
  
  
  liittyma.show()
  new Timer().schedule(task, 0, 1)
  
}

object start extends App{
  
  var valo = new LightSource(100,new Coordinate(0,10,0))
  var lights:Option[LightSource] = None
  var filepath = "models/huoneisto.obj"
  var noclip = true
  
  var width = 900
  var height = 900
  
  val init = new SpinnerNumberModel(900,200,2000,20)
  val init2 = new SpinnerNumberModel(900,200,2000,20)
  
  val freimi = new JFrame("3DLauncher")
  freimi.setSize(800, 800)
  val nappi = new JButton
  val valitsija = new JFileChooser(FileSystemView.getFileSystemView())
  val filenappi = new JButton("Open a file")
  val valot = new JToggleButton("Dynamic Lighting ON")
  val klippi = new JToggleButton("Noclip ON")
  val polku = new JLabel(filepath)
  val dimi1 = new JLabel("Width")
  val dimi2 = new JLabel("Height")
  
  val levSpin = new JSpinner(init)
  val korSpin = new JSpinner(init2)
  
  
  nappi.setText("Start the program") 
  levSpin.setBounds(300,170,100,50)
  korSpin.setBounds(400, 170, 100, 50)
  dimi1.setBounds(330,120,200,50)
  dimi2.setBounds(420,120,200,50)
  filenappi.setBounds(300,240,200,50)
  nappi.setBounds(300, 600, 200, 100)
  valot.setBounds(300, 500, 200, 50)
  klippi.setBounds(300,400,200,50)
  polku.setBounds(300, 300, 350, 50)
  
  levSpin.addChangeListener(new ChangeListener(){
    def stateChanged(e:ChangeEvent) = {
      width = levSpin.getValue().asInstanceOf[Int]
    }
  })
  
  korSpin.addChangeListener(new ChangeListener(){
    def stateChanged(e:ChangeEvent) = {
      height = korSpin.getValue().asInstanceOf[Int]
    }
  })
  
  valot.addActionListener(new ActionListener(){
    def actionPerformed(e:ActionEvent) ={
      if(valot.isSelected()){ 
        lights = Some(valo)
        valot.setText("Dynamic Lighting OFF")
      }
      else{ lights = None
        valot.setText("Dynamic Lighting ON")
      }
    }
  })
  
  klippi.addActionListener(new ActionListener(){
    def actionPerformed(e:ActionEvent) ={
      if(klippi.isSelected()){ 
        noclip = false
        klippi.setText("Noclip OFF")
      }
      else{ noclip = true
        klippi.setText("Noclip ON")
      }
    }
  })
  
  filenappi.addActionListener(new ActionListener(){
    def actionPerformed(e:ActionEvent) = {
      val tulos =  valitsija.showOpenDialog(null)
       if (tulos == JFileChooser.APPROVE_OPTION) 
            { 
                polku.setText(valitsija.getSelectedFile().getAbsolutePath()); 
                filepath = valitsija.getSelectedFile().getAbsolutePath()
            } 
    }
  })
  
  nappi.addActionListener(new ActionListener(){
    def actionPerformed(e:ActionEvent) = {
       val mesta = new Starter(width,height,90,filepath,lights,noclip)
     mesta.main(args)
    }
  })
  
  freimi.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
  freimi.add(nappi)
  freimi.add(valot)
  freimi.add(polku)
  freimi.add(filenappi)
  freimi.add(valitsija)
  freimi.add(klippi)
  freimi.add(levSpin)
  freimi.add(korSpin)
  freimi.add(dimi1)
  freimi.add(dimi2)
  freimi.setLayout(null)
  freimi.setVisible(true)
  freimi.setBackground(Color.CYAN)
  freimi.show()
  
 
}