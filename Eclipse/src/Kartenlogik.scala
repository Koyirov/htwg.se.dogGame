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
      var s = StrToIntK(figStr)

      while (s == None) {
        println("Waehlen Sie eine Figur!")
        figStr = scala.io.StdIn.readLine()
        s = StrToIntK(figStr)

      }
      fig = s.get
      if (fig >= 1 && fig <= 4) {
        check1 = true;
      }
      if (!check1) {
        println("Diese Figur haben Sie nicht.")
      }

    }
    return fig
  }

  def spFigur_waehlen(): String = {

    return scala.io.StdIn.readLine()
  }

  def opt_waehlen(): Int = {
    var opt = -1
    var check2 = false
    while (check2 == false) {
      println("Waehlen Sie eine Option aus!")
      var optStr = scala.io.StdIn.readLine()
      var s = StrToIntK(optStr)

      while (s == None) {
        println("Waehlen Sie eine Option aus!")
        optStr = scala.io.StdIn.readLine()
        s = StrToIntK(optStr)

      }
      opt = s.get

      if (opt >= 0 && opt <= 15) {
        check2 = true;
      }
      if (!check2) {
        println("Diese Option haben Sie nicht.")
      }

    }
    return opt
  }

  def laufen(lF: collection.mutable.Map[String, Int], figur: String, opt: Int, spieler: Spieler, alleSp: ArrayBuffer[Spieler]) {

    opt match {
      case 0 => {
        if (!spieler.delFigur(figur)) {
          println("etwas lief falsch.")
          //TODO fehlerbehandlung
        }
        if (posBelegt(lF, spieler.getStartPos())) {
          //nachhause schicken
          schickStart(lF, alleSp, spieler.getStartPos())
        }

        //aus dem start gehen
        lF += ((figur, spieler.getStartPos()))

      }
      case 7 => {
        // 7
        if (lF.contains(figur)) {
          var pos = lF.get(figur)
          var erg = (pos.get + opt) % 64 
          
          
          // TODO was wenn blockiert
          
         
          
          
          lF.remove(figur)
          
          if (pos.get == spieler.startPos){
            // Ins Ziel laufen
            val zielPos = erg - spieler.startPos
            spieler.ziel += ((figur, zielPos))
          } else {
            // noch eine runde laufen
            if (posBelegt(lF, erg))
              //figur schlagen
              schickStart(lF, alleSp, erg)
            lF += ((figur, erg))
          }
        } else {
          // Figur nicht im Lauffeld -> Figur ist im Zielfeld
          if (spieler.ziel.contains(figur)) {
            var pos = spieler.ziel.get(figur)
            var schritt = 4 - pos.get
            var l = spieler.ziel.map(_.swap)

            if (opt <= schritt) {
              var belegt = false
              for (i <- pos.get + 1 to 4) {
                if (l.contains(i)) {
                  belegt = true
                }

                if (!belegt) {
                  spieler.ziel.remove(figur)
                  spieler.ziel += ((figur, pos.get + schritt))
                } else {
                  println("Figur kann sich nicht mehr laufen!")
                }
              }
            }
          }
        }
      }
      case 14 => {
        // 4 zurueck
        if (lF.contains(figur)) {
          var pos = lF.get(figur)
          lF.remove(figur)
          var erg = (pos.get - 4) 
       
          if (erg < 0){
            erg = 64+erg
          }
          
          erg = erg % 64
          println("TEST." + erg)

          if (posBelegt(lF, erg))
            //figur schlagen
            schickStart(lF, alleSp, erg)
          lF += ((figur, erg))
        }
      }
      case 15 => {
        println("Figur von andere Spieler waehlen! (z.B. R1");
        var fig2 = spFigur_waehlen()

        // Check falls tauschbar
        while (!lF.contains(fig2) && !fig2.startsWith(spieler.getName())) {
          println("Diese Figur kann man nicht tauschen.")
          println("Figur von anderem Spieler waehlen! (z.B. R1");
          fig2 = spFigur_waehlen()
        }

        var pos = lF.get(figur).get
        lF.remove(figur)

        var pos2 = lF.get(fig2).get
        lF.remove(fig2)

        lF += ((figur, pos2))
        lF += ((fig2, pos))

      }
      case _ => {
        // TODO was wenn blockiert
        // im normalfall : 1-13
        if (lF.contains(figur)) {
          var pos = lF.get(figur)
          var erg = (pos.get + opt) % 64 
          
          //was wenn blockiert
          
          
          
         
          lF.remove(figur)
          
          if ((pos.get < spieler.startPos || (spieler.getId() == 1 && pos.get > 4)) && 1 <= (erg - spieler.startPos) % 64 && (erg - spieler.startPos) % 64 <= 4) {
            // Ins Ziel laufen
            val zielPos = erg - spieler.startPos
            spieler.ziel += ((figur, zielPos))
          } else {
            // noch eine runde laufen
            if (posBelegt(lF, erg))
              //figur schlagen
              schickStart(lF, alleSp, erg)
            lF += ((figur, erg))
          }
        } else {
          // Figur nicht im Lauffeld -> Figur ist im Zielfeld
          if (spieler.ziel.contains(figur)) {
            var pos = spieler.ziel.get(figur)
            var schritt = 4 - pos.get
            var l = spieler.ziel.map(_.swap)

            if (opt <= schritt) {
              var belegt = false
              for (i <- pos.get + 1 to 4) {
                if (l.contains(i)) {
                  belegt = true
                }

                if (!belegt) {
                  spieler.ziel.remove(figur)
                  spieler.ziel += ((figur, pos.get + schritt))
                } else {
                  println("Figur kann sich nicht mehr laufen!")
                }
              }
            }
          }
        }
      }
    }
  }

  def ausfuehren(lF: collection.mutable.Map[String, Int], karte: Int, spieler: Spieler, alleSp: ArrayBuffer[Spieler]) {

    //Fall:joker
    var karteE = karte
    if (karte == 14) {
      var kart = 0
      var check2 = false
      while (check2 == false) {
        println("Waehlen Sie eine Karte aus die der Joker sein soll.")
        var optStr = scala.io.StdIn.readLine()
        var s = StrToIntK(optStr)

        while (s == None) {
          println("Waehlen Sie eine Karte aus die der Joker sein soll.")
          optStr = scala.io.StdIn.readLine()
          s = StrToIntK(optStr)

        }
        kart = s.get
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
      case 0 => { // keine moegliche karte

        println("Es gibt keine ausspielbare Karte.")
      }
      case 1 => { // ASS
        var fig = spieler.getName() + figur_waehlen()
        // optionen bestimmen
        println("0 -> Aus Startfeld rausgehen.")
        println("1 -> 1 Schritt im Lauffeld weiter laufen.")
        println("11 -> 11 Schritte im Lauffeld weiter laufen.")
        var opt = opt_waehlen()

        laufen(lF, fig, opt, spieler, alleSp)
      }
      case 2 => {
        var fig = spieler.getName() + figur_waehlen()
        // optionen bestimmen
        var opt = 2
        laufen(lF, fig, opt, spieler, alleSp)
      }
      case 3 => {
        var fig = spieler.getName() + figur_waehlen()
        // optionen bestimmen
        var opt = 3
        laufen(lF, fig, opt, spieler, alleSp)
      }
      case 4 => {
        var fig = spieler.getName() + figur_waehlen()
        // optionen bestimmen
        println("14 -> 4 Schritte im Lauffeld Rueckwaerts laufen.")
        println("4 -> 4 Schritte im Lauffeld weiter laufen.")
        var opt = opt_waehlen()

        laufen(lF, fig, opt, spieler, alleSp)
      }
      case 5 => {
        var fig = spieler.getName() + figur_waehlen()
        // optionen bestimmen
        var opt = 5

        laufen(lF, fig, opt, spieler, alleSp)
      }
      case 6 => {
        var fig = spieler.getName() + figur_waehlen()
        // optionen bestimmen
        var opt = 6

        laufen(lF, fig, opt, spieler, alleSp)
      }
      case 7 => {
        for (i <- 1 to 7) {
          var fig = spieler.getName() + figur_waehlen()
          // optionen bestimmen
          // TODO zuege verfallen falls fehler
          
          var opt = 7

          laufen(lF, fig, opt, spieler, alleSp)
        }
      }
      case 8 => {
        var fig = spieler.getName() + figur_waehlen()
        // optionen bestimmen
        var opt = 8

        laufen(lF, fig, opt, spieler, alleSp)
      }
      case 9 => {
        var fig = spieler.getName() + figur_waehlen()
        // optionen bestimmen
        var opt = 9

        laufen(lF, fig, opt, spieler, alleSp)
      }
      case 10 => {
        var fig = spieler.getName() + figur_waehlen()
        // optionen bestimmen
        var opt = 10

        laufen(lF, fig, opt, spieler, alleSp)
      }
      case 11 => { // Bube
        var fig = spieler.getName() + figur_waehlen()
        // optionen bestimmen

        var opt = 15

        laufen(lF, fig, opt, spieler, alleSp)
      }
      case 12 => { // Dame
        var fig = spieler.getName() + figur_waehlen()
        var opt = 12

        laufen(lF, fig, opt, spieler, alleSp)

      }
      case 13 => { // Koenig
        var fig = spieler.getName() + figur_waehlen()
        // optionen bestimmen
        println("0 -> Aus Startfeld rausgehen.")
        println("13 -> 13 Schritte im Lauffeld weiter laufen.")
        var opt = opt_waehlen()

        laufen(lF, fig, opt, spieler, alleSp)

      }
      case _ => { // etwas lief schief
        println("Etwas lief falsch: Error match-ausfuehren")
        System.exit(1)
      }
    }
  }

  def posBelegt(lF: collection.mutable.Map[String, Int], pos: Int): Boolean = {
    var check = false
    for ((k, v) <- lF) {
      if (v == pos) {
        check = true
      }
    }
    return check
  }

  def schickStart(lF: collection.mutable.Map[String, Int], alleSp: ArrayBuffer[Spieler], pos: Int) = {
    // get figur
    var fig = "0"
    for ((k, v) <- lF) {
      if (v == pos) {
        fig = k
      }
    }
    //todo fehlerbehandlung
    // loesche im lF
    lF -= ((fig))

    // setze in zielSpieler.start
    for (sp <- alleSp) {
      if (fig.startsWith(sp.getName())) {
        sp.start += ((fig, sp.getFigPos(fig)))
      }
    }

  }

  def StrToIntK(s: String): Option[Int] = {
    try {
      Some(s.toInt)
    } catch {
      case e: Exception => None
    }
  }
}