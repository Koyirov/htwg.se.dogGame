
import scala.collection.mutable.ArrayBuffer;

class Spieler(id: Int, st: Int) {

  val spielerId = id;

  var karten = ArrayBuffer[Int]();
  var start = collection.mutable.Map[String, Int]();
  var ziel = collection.mutable.Map[String, Int]();

  var startPos = st;

  def getId(): Int = {
    return spielerId;
  }

  def getName(): String = {
    spielerId match {
      case 1 => return "B"
      case 2 => return "R"
      case 3 => return "G"
      case 4 => return "S"

    }
  }

  def setStart() {
    for (i <- 1 to 4) {
      spielerId match {
        case 1 => start += (("B" + i, i))
        case 2 => start += (("R" + i, i))
        case 3 => start += (("G" + i, i))
        case 4 => start += (("S" + i, i))
      }
    }
  }

  def getStart(): collection.mutable.Map[String, Int] = {
    return start;
  }

  def setKarte(kar: Int) {
    karten += kar
  }

  def getAnzKart(): Int = {
    return karten.length;
  }

  def getKarten(): ArrayBuffer[Int] = {
    return karten;
  }

  def delKarte(kar: Int) {
    karten -= kar
  }

  def delFigur(fig: String): Boolean = {

    var check = false
    if (start.contains(fig))
      check = true
      start -= (fig)

    return check

  }
  
  def getStartPos():Int = {
    return startPos
  }
  
  def getFigPos(fig: String): Int ={
    var pos = 1 
    for(f <- start){
      if(f._2 != pos){
        return pos
      }
      pos += 1
    }
    return pos
  }
}