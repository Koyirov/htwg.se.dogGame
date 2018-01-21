
object StepOne {

  import scala.collection.mutable.ArrayBuffer

  var spPublicLf = collection.mutable.Map[String, Int]()
  var revert7 = false
  private var spLf = collection.mutable.Map[String, Int]()
  private var spPl = ArrayBuffer[Spieler]();

  def main(args: Array[String]) {

    var kartenStapel = ArrayBuffer[Int]()
    var laufFeld = collection.mutable.Map[String, Int]()
    var players = ArrayBuffer[Spieler]();
    //val kartFiguren = Array("Kreuz", "Pik", "Herz", "Karo")
    var turn = 1
    var karGrenz = 6

    // init spieler
    println("Wieviele Spieler spielen?")

    var anzSpieler = scala.io.StdIn.readLine()
    var s = StrToInt(anzSpieler)

    while (s == None) {
      println("Bitte eine Zahl eingeben. (4)")
      anzSpieler = scala.io.StdIn.readLine()
      s = StrToInt(anzSpieler)

    }

    if (s.get != 4) {
      println("Not implemented yet!");
      return ;
    }

    // default- 1 to 4
    for (i <- 1 to anzSpieler.toInt) {

      i match {
        case 1 => {
          var p1 = new Spieler(i, (i - 1) * 16);
          players += p1;

        }
        case 2 => {
          var p2 = new Spieler(i, (i - 1) * 16);
          players += p2;

        }
        case 3 => {
          var p3 = new Spieler(i, (i - 1) * 16);
          players += p3;

        }
        case 4 => {
          var p4 = new Spieler(i, (i - 1) * 16);
          players += p4;
        }
      }
    }

    // init
    for (x <- players) {
      x.setStart();
    }

    //TEST
    /*
    println("Schnell Start aktiviert")

    laufFeld += (("B1", 0))
    laufFeld += (("R1", 16))
    laufFeld += (("G1", 32))
    laufFeld += (("S1", 48))

    players(0).start -= "B1"
    players(1).start -= "R1"
    players(2).start -= "G1"
    players(3).start -= "S1"
*/
    //test
    tui_v1(laufFeld, players)

    //println("Init:")
    //for (x <- players)
    //  println(x.getStart())

    var win = 0
    while (win == 0) {

      // kartenstapel auffuellen
      kartenStapel.clear()
      for (wert <- 1 to 14) {
        for (anz <- 1 to 4) {
          kartenStapel += wert

        }

      }
      //println(kartenStapel);

      //karten verteilen

      val r = scala.util.Random

      // spieler - default 4

      for (sp <- 1 to players.length) {

        // kartengrenze - default 6
        for (j <- 1 to karGrenz) {
          val zufall = r.nextInt(kartenStapel.length - 1)
          players(sp - 1).setKarte(kartenStapel(zufall))
          kartenStapel.remove(zufall)
        }
      }

      // karten mit partner tauschen

      //init
      var tauschKarten = Array[Int](0, 0, 0, 0)
      //jeder spieler
      for (sp <- 0 to players.length - 1) {

        var tKart = "0"
        var tk = StrToInt(tKart)
        var check = false
        while (check == false) {
          println(players(sp).getName() + ", Waehlen Sie eine Karte zum tauschen aus!")
          println(players(sp).getKartenAusgabe())
          tKart = scala.io.StdIn.readLine()

          tk = StrToInt(tKart)

          while (tk == None) {
            println(players(sp).getName() + ", Waehlen Sie eine Karte zum tauschen aus!")
            println(players(sp).getKartenAusgabe())
            tKart = scala.io.StdIn.readLine()
            tk = StrToInt(tKart)

          }

          //checken ob karte da ist
          for (k <- players(sp).getKarten()) {

            if (k == tk.get) {
              check = true
            }
          }
          if (!check) {
            println("Diese Karte haben Sie nicht.")
          }

        }
        tauschKarten((sp + 2) % 4) = tk.get
        players(sp).delKarte(tk.get)
      }

      for (sp <- 1 to players.length) {
        players(sp - 1).setKarte(tauschKarten(sp - 1))
      }

      // spiel ...
      var rundenEnde = false
      while (!rundenEnde) {
        for (sp <- 0 to players.length - 1) {

          // Alle Felder uebersichtlich ausgeben
          //println("**alle felder ausgeben**")
          tui_v1(laufFeld, players)

          // checken ob mind. eine karte moeglich

          //startkarte
          var checkSt = false
          for (kar <- players(sp).getKarten()) {

            if ((kar == 1 || kar == 13 || kar == 14))
              checkSt = true

            if (players(sp).getStart().size + players(sp).getZiel().size != 4)
              checkSt = true

          }
          if (!checkSt) {
            println("Keine mögliche Startkarte vorhanden.")
          }

          // Blokierte Spielfelder
          var besteFig = 0
          var checkBl = false
          for (fig <- laufFeld) {
            if (fig._1.startsWith(players(sp).getName())) {

              var break = false
              for (i <- 1 to 13) {

                if (lFIstFrei(laufFeld, players, i + fig._2)) {

                  if ((i > besteFig) && !break)
                    besteFig = i

                } else {
                  break = true

                }
              }
            }
          }

          if (!(players(sp).getAnzKart() == 0)) {
            val klKart = players(sp).getkleinsteKarte()
            println("besteFig: " + besteFig + "klKart: " + klKart)
            if (besteFig >= klKart)
              //blockiert
              checkBl = true

            if (!checkBl) {
              println("Keine mögliche Figur vorhanden!(Blockiert)")
            }
          }

          // Nur Buben , ohne moeglichkeit
          var km = false
          var allB = true
          for (k <- players(sp).getKarten()) {
            if (k != 11) {
              allB = false
            }
          }

          if (allB && (players(sp).getStart().size + players(sp).getZiel().size == 4)) {
            km = true
          }

          var andere = 0
          for (fig <- laufFeld) {
            if (!fig._1.startsWith(players(sp).getName()))
              andere += 1
          }

          if (km || andere == 0) {
            println("Keine mögliche Figur vorhanden!(alle Buben)")
          }

          // im ziel blockiert
          var bl = false

          if (!players(sp).ziel.isEmpty) {
            // Figur nicht im Lauffeld -> Figur ist im Zielfeld

            if (!(players(sp).getAnzKart() == 0)) {
              val klKart = players(sp).getkleinsteKarte()

              for (fig <- players(sp).ziel) {
                var pos = fig._2
                var schritt = 4 - pos

                // ------------
                var l = players(sp).ziel.map(_.swap)

                if (klKart <= schritt) {
                  var belegt = false
                  for (i <- pos + 1 to 4) {
                    if (l.contains(i)) {
                      belegt = true
                    }
                  }
                  if (!belegt) {
                    bl = true
                  }
                }
              }
            }
          }

          if (bl)
            println("Keine mögliche Figur vorhanden!(alle blockiert)")

          // evtl andere faelle ?

          // Keine moegliche Zuege erkannt:

          if (!checkSt && !checkBl && (km || andere == 0) && bl) {
            players(sp).delAllKarte()
            println("Keine Karte zum Spielen: alle Karten abgeworfen")
          }

          var legit = false
          while (!legit) {

            // karten anzeigen
            println("Du bist dran: " + players(sp).getName())
            println("Ass = 1, Bube = 11, Dame = 12, Koenig = 13, Joker = 14, 0 = keine moegliche")
            println(players(sp).getKartenAusgabe())

            if (!(players(sp).getAnzKart() == 0)) {
              //auswaehlen + loeschen
              println("Waehle eine Karte zum Spielen aus.")
              var spKart = scala.io.StdIn.readLine()
              var sK = StrToInt(spKart)

              while (sK == None) {
                println("Waehle eine Karte zum Spielen aus.")
                spKart = scala.io.StdIn.readLine()
                sK = StrToInt(spKart)
              }

              if (sK.get == 0) {
                println("Keine Karte zum Spielen.")
                // Karten loeschen und naechster spieler
                players(sp).delAllKarte()

              } else {

                // kartenlogik ausfuehren
                if (Kartenlogik().ausfuehren(laufFeld, sK.get, players(sp), players)) {

                  players(sp).delKarte(sK.get)
                  legit = true

                } else if (revert7) {
                  var tup = ladeDaten()
                  laufFeld = tup._1
                  players = tup._2
                  revert7 = false
                  legit = false
                  println("Jetzt eine neue Karte angeben.(7)")
                } else {
                  legit = false
                  println("Jetzt eine neue Karte angeben.")
                }

              }
            }
            legit = true

          }
          // zug ende
          turn = (turn + 1) % 4
        }

        //testen ob runden ende
        var anzKar = 0;
        for (sp <- 1 to players.length) {
          anzKar += players(sp - 1).getAnzKart()
        }

        if (anzKar == 0) {
          // alle spieler haben keine karten mehr - runden ende!
          rundenEnde = true

          if (karGrenz > 2)
            karGrenz -= 1
          else
            karGrenz = 6
        }

      } // while rundenende

      if (players(1).alleImZiel() && players(3).alleImZiel())
        win = 1
      if (players(0).alleImZiel() && players(2).alleImZiel())
        win = 2

    } // while win ende
    println(win + " hat mit seinem Teampartner gewonnen :)")
  }
  def tui_v1(lF: collection.mutable.Map[String, Int], p: ArrayBuffer[Spieler]) {

    var l = lF.map(_.swap)

    println("Spielfeld:")
    println("                                                            " + sz(p, 2, 0, 1) + "  " + sz(p, 2, 0, 2) + "  " + sz(p, 2, 0, 3) + "  " + sz(p, 2, 0, 4) + "           ")
    println("                                                                        ")
    println("        " + pos(l, p, 0) + "  " + pos(l, p, 1) + "  " + pos(l, p, 2) + "  " + pos(l, p, 3) + "  " + pos(l, p, 4) + "  " + pos(l, p, 5) + "  " + pos(l, p, 6) + "  " + pos(l, p, 7) + "  " + pos(l, p, 8) + "  " + pos(l, p, 9) + "  " + pos(l, p, 10) + "  " + pos(l, p, 11) + "  " + pos(l, p, 12) + "  " + pos(l, p, 13) + "  " + pos(l, p, 14) + "  " + pos(l, p, 15) + "  " + pos(l, p, 16) + "      ")
    println("                                                                        ")
    println("    " + sz(p, 1, 0, 1) + "  " + pos(l, p, 63) + "      " + sz(p, 1, 1, 1) + "                                              " + sz(p, 2, 1, 1) + "      " + pos(l, p, 17) + "        ")
    println("                                                                         ")
    println("    " + sz(p, 1, 0, 2) + "  " + pos(l, p, 62) + "          " + sz(p, 1, 1, 2) + "                                      " + sz(p, 2, 1, 2) + "          " + pos(l, p, 18) + "        ")
    println("                                                                         ")
    println("    " + sz(p, 1, 0, 3) + "  " + pos(l, p, 61) + "              " + sz(p, 1, 1, 3) + "                              " + sz(p, 2, 1, 3) + "              " + pos(l, p, 19) + "        ")
    println("                                                                         ")
    println("    " + sz(p, 1, 0, 4) + "  " + pos(l, p, 60) + "                  " + sz(p, 1, 1, 4) + "                      " + sz(p, 2, 1, 4) + "                  " + pos(l, p, 20) + "        ")
    println("                                                                         ")
    println("        " + pos(l, p, 59) + "                                                              " + pos(l, p, 21) + "        ")
    println("                                                                         ")
    println("        " + pos(l, p, 58) + "                                                              " + pos(l, p, 22) + "        ")
    println("                                                                        ")
    println("        " + pos(l, p, 57) + "                                                              " + pos(l, p, 23) + "        ")
    println("                                                                        ")
    println("        " + pos(l, p, 56) + "                                                              " + pos(l, p, 24) + "        ")
    println("                                                                              ")
    println("        " + pos(l, p, 55) + "                                                              " + pos(l, p, 25) + "        ")
    println("                                                                        ")
    println("        " + pos(l, p, 54) + "                                                              " + pos(l, p, 26) + "        ")
    println("                                                                        ")
    println("        " + pos(l, p, 53) + "                                                              " + pos(l, p, 27) + "        ")
    println("                                                                        ")
    println("        " + pos(l, p, 52) + "                  " + sz(p, 4, 1, 4) + "                      " + sz(p, 3, 1, 4) + "                  " + pos(l, p, 28) + "  " + sz(p, 3, 0, 4) + "     ")
    println("                                                                        ")
    println("        " + pos(l, p, 51) + "              " + sz(p, 4, 1, 3) + "                              " + sz(p, 3, 1, 3) + "              " + pos(l, p, 29) + "  " + sz(p, 3, 0, 3) + "     ")
    println("                                                                        ")
    println("        " + pos(l, p, 50) + "          " + sz(p, 4, 1, 2) + "                                      " + sz(p, 3, 1, 2) + "          " + pos(l, p, 30) + "  " + sz(p, 3, 0, 2) + "     ")
    println("                                                                        ")
    println("        " + pos(l, p, 49) + "      " + sz(p, 4, 1, 1) + "                                              " + sz(p, 3, 1, 1) + "      " + pos(l, p, 31) + "  " + sz(p, 3, 0, 1) + "     ")
    println("                                                                        ")
    println("        " + pos(l, p, 48) + "  " + pos(l, p, 47) + "  " + pos(l, p, 46) + "  " + pos(l, p, 45) + "  " + pos(l, p, 44) + "  " + pos(l, p, 43) + "  " + pos(l, p, 42) + "  " + pos(l, p, 41) + "  " + pos(l, p, 40) + "  " + pos(l, p, 39) + "  " + pos(l, p, 38) + "  " + pos(l, p, 37) + "  " + pos(l, p, 36) + "  " + pos(l, p, 35) + "  " + pos(l, p, 34) + "  " + pos(l, p, 33) + "  " + pos(l, p, 32) + "      ")
    println("                                                              ")
    println("            " + sz(p, 4, 0, 1) + "  " + sz(p, 4, 0, 2) + "  " + sz(p, 4, 0, 3) + "  " + sz(p, 4, 0, 4) + "                                         ")
    println("")
  }

  def pos(l: collection.mutable.Map[Int, String], p: ArrayBuffer[Spieler], pos: Int): String = {
    if (l.contains(pos))
      return l.get(pos).get
    else
      return "■ "
  }

  //AUFRUF: sz(ArrayMitSpielern, SpielerID, 0=start/1=ziel, PositionImFeld)
  def sz(p: ArrayBuffer[Spieler], sID: Int, feldID: Int, posID: Int): String = {
    if (feldID == 0) {
      //START
      var st: collection.mutable.Map[Int, String] = p(sID - 1).getStart().map(_.swap)
      if (st.contains(posID))
        return st.get(posID).get
      else
        return "■ "

    } else {
      //ziel
      var st: collection.mutable.Map[Int, String] = p(sID - 1).getZiel().map(_.swap)
      if (st.contains(posID))
        return st.get(posID).get
      else
        return "■ "
    }

  }

  def StrToInt(s: String): Option[Int] = {
    try {
      Some(s.toInt)
    } catch {
      case e: Exception => None
    }
  }

  def lFIstFrei(lF: collection.mutable.Map[String, Int], p: ArrayBuffer[Spieler], pos: Int): Boolean = {

    var l = lF.map(_.swap)
    for (einer <- p) {
      //auf einer startposition
      if (pos == einer.getStartPos()) {
        if (l.get(pos) != None) {
          //die richtige figur auf dieser startPos
          if (l.get(pos).get.startsWith(einer.getName())) {
            println("Block erkannt.")
            return false
          }
        }
      }
    }

    return true
  }

  def speicherDaten(laufFeld: collection.mutable.Map[String, Int], players: ArrayBuffer[Spieler]) = {
    spLf = laufFeld.clone()
    spPl = players.clone()
  }

  def speicherFigDaten(laufFeld: collection.mutable.Map[String, Int]) = {
    spPublicLf = laufFeld.clone()
    //println("etwas2 : "+ spPublicLf)
  }

  def ladeDaten(): (collection.mutable.Map[String, Int], ArrayBuffer[Spieler]) = {

    return (spLf, spPl)
  }
}



