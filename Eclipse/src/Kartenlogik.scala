import scala.collection.mutable.ArrayBuffer;

case class Kartenlogik() {
 
  
  
  
  def ausfuehren(lF: ArrayBuffer[(String, Int)], karte: Int, spieler: Spieler){
    // Lauffeld uebersichtlich ausgeben
    karte match{
      case 1 => { // ASS
        var check1 = false
        var fig = 0;
        while(check1 == false){
          println("Waehlen Sie eine Figur!")
          var figStr = scala.io.StdIn.readLine()
          fig = figStr.toInt
          if(fig >= 1 && fig <= 4){
            check1 = true;
          }
          if(!check1){
            println("Diese Figur haben Sie nicht.")
          }
          
        }
        
        var opt = 0
        var check2 = false
        while(check2 == false){
          println("Waehlen Sie eine Option aus!")
          var optStr = scala.io.StdIn.readLine()
          opt = optStr.toInt
          if(opt >= 1 && fig <= 4){
            check2 = true;
          }
          if(!check2){
            println("Diese Option haben Sie nicht.")
          }
          
        }
      }
      
      
      
    }
  }
}