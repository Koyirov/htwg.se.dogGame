import scala.collection.mutable.ArrayBuffer;

case class Kartenlogik() {

  //case 1 => start += "B" + i;
  //case 2 => start += "R" + i;
  //case 3 => start += "G" + i;
  //case 4 => start += "S" + i;

  def figur_waehlen(): Int = {
    var check1 = false
    var fig = 0;
    while (check1 == false) {
      println("Waehlen Sie eine Figur!")
      var figStr = scala.io.StdIn.readLine()
      fig = figStr.toInt
      if (fig >= 1 && fig <= 4) {
        check1 = true;
      }
      if (!check1) {
        println("Diese Figur haben Sie nicht.")
      }

    }
    return fig
  }

  def opt_waehlen(anz: Int): Int = {
    var opt = 0
    var check2 = false
    while (check2 == false) {
      println("Waehlen Sie eine Option aus!")
      var optStr = scala.io.StdIn.readLine()
      opt = optStr.toInt
      if (opt >= 1 && opt <= anz) {
        check2 = true;
      }
      if (!check2) {
        println("Diese Option haben Sie nicht.")
      }

    }
    return opt
  }

  def laufen(lF: collection.mutable.Map[String, Int], figur: String, opt: Int, spieler: Spieler,alleSp:ArrayBuffer[Spieler]) {

    opt match {
      case 0 => {
        if (!spieler.delFigur(figur)){
          println("etwas lief falsch.")
          //todo fehlerbehandlung
        }
        if(posBelegt(lF, spieler.getStartPos())){
          //nachhause schicken
          schickStart(lF, alleSp, spieler.getStartPos())
        }
        
        //aus dem start gehen
        lF += ((figur, spieler.getStartPos()))
        
      }
    }
    
    // im normalfall : 1-13
        if(lF.contains(figur)){
          var pos = lF.get(figur)
          lF.remove(figur)
          var erg = pos.get + opt
          lF += ((figur, erg))
        }else{
           lF += ((figur, 0))
        }
      

    //lF.insert(0, (figur, 2))
    //lF.insert(0, (figur, 4))
    //lF.remove(0)

  }

  def ausfuehren(lF: collection.mutable.Map[String, Int], karte: Int, spieler: Spieler,alleSp:ArrayBuffer[Spieler]) {
    // Lauffeld uebersichtlich ausgeben

    //Fall:joker
    var karteE = karte
    if (karte == 14) {
      var kart = 0
      var check2 = false
      while (check2 == false) {
        println("Waehlen Sie eine Karte aus die der Joker sein soll.")
        var optStr = scala.io.StdIn.readLine()
        kart = optStr.toInt
        if (kart >= 1 && kart <= 13) {
          check2 = true;
        }
        if (!check2) {
          println("Diese Karte gibt es nicht.")
        }

      }
      karteE = kart
    }

    karteE match {
      case 1 => { // ASS
        var fig = spieler.getName() + figur_waehlen()
        // optionen bestimmen
        println("0 -> Aus Startfeld rausgehen.")
        println("1 -> 1 Schritt im Lauffeld weiter laufen.")
        println("11 -> 11 Schritte im Lauffeld weiter laufen.")
        var opt = opt_waehlen(3)
        //todo
        laufen(lF, fig, opt, spieler,alleSp)
      }
      case 2 => {
        var fig = figur_waehlen()
        // optionen bestimmen
        var opt = 2
        //todo
      }
      case 3 => {
        var fig = figur_waehlen()
        // optionen bestimmen
        var opt = 3
        //todo
      }
      case 4 => {
        var fig = figur_waehlen()
        // optionen bestimmen
        println("14 -> 4 Schritte im Lauffeld Rueckwaerts laufen.")
        println("4 -> 4 Schritte im Lauffeld weiter laufen.")
        var opt = opt_waehlen(2)
        //todo
      }
      case 5 => {
        var fig = figur_waehlen()
        // optionen bestimmen
        var opt = 5
        //todo
      }
      case 6 => {
        var fig = figur_waehlen()
        // optionen bestimmen
        var opt = 6
        //todo
      }
      case 7 => {
        var fig = figur_waehlen()
        // optionen bestimmen
        for (i <- 1 to 7) {
          var opt = 1
          //todo
        }
      }
      case 8 => {
        var fig = figur_waehlen()
        // optionen bestimmen
        var opt = 8
        //todo
      }
      case 9 => {
        var fig = figur_waehlen()
        // optionen bestimmen
        var opt = 9
        //todo
      }
      case 10 => {
        var fig = figur_waehlen()
        // optionen bestimmen
        var opt = 10
        //todo
      }
      case 11 => { // Bube
        var fig = figur_waehlen()
        // optionen bestimmen
        println("Figur von andere Spieler waehlen!");
        var fig2 = figur_waehlen()
        var opt = 15
        //todo
      }
      case 12 => { // Dame
        var fig = figur_waehlen()
        var opt = 12
        //todo

      }
      case 13 => { // Koenig
        var fig = figur_waehlen()
        // optionen bestimmen
        println("0 -> Aus Startfeld rausgehen.")
        println("13 -> 13 Schritte im Lauffeld weiter laufen.")
        var opt = opt_waehlen(2)
        //todo

      }
    }
  }
  
  def posBelegt(lF: collection.mutable.Map[String, Int], pos: Int): Boolean ={
    var check = false
    for ((k,v) <- lF){
      if(v == pos){
        check = true
      } 
    }
    return check
  }
  
  def schickStart(lF: collection.mutable.Map[String, Int], alleSp: ArrayBuffer[Spieler], pos: Int) = {
    // get figur
    var fig = "0"
    for ((k,v) <- lF){
      if(v == pos){
        fig = k
      }
    }
    //todo fehlerbehandlung
    // loesche im lF
    lF -= ((fig))
    
    // setze in zielSpieler.start
    for(sp <- alleSp){
      if(fig.startsWith(sp.getName())){
        sp.start += ((fig, sp.getFigPos(fig)))
      }
    }
    
    
  }
}