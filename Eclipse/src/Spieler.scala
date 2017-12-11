
import scala.collection.mutable.ArrayBuffer;

class Spieler(id: Int, st: Int) {
  
  val spielerId = id;
  
  var karten = ArrayBuffer[Int]();
  var start = ArrayBuffer[String]();
  var ziel = ArrayBuffer[(String, Int)]();
  
  var startPos = st; 
  
  def getId(): Int = {
    return spielerId;
  }
  
 def getName(): String = {
     spielerId match{
        case 1 => return "B"
        case 2 => return "R"
        case 3 => return "G"
        case 4 => return "S"
      
    }
  }  
  
  def setStart(){
    for (i <- 1 to 4){
      spielerId match{
        case 1 => start += "B" + i;
        case 2 => start += "R" + i;  
        case 3 => start += "G" + i;
        case 4 => start += "S" + i;
      } 
    }
  }
  
  def getStart(): ArrayBuffer[String] = {
    return start;
  }
  
  def setKarte(kar: Int){
    karten += kar
  }
  
  def getAnzKart(): Int = {
    return karten.length;
  }
  
  def getKarten(): ArrayBuffer[Int] = {
    return karten;
  }
  
  def delKarte(kar:Int){
    karten -= kar
  }
}