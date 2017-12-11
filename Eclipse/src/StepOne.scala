
object StepOne {
  
  import scala.collection.mutable.ArrayBuffer;
  
  def main(args: Array[String]){
    
    var kartenStapel = ArrayBuffer[Int]();
    var laufFeld = ArrayBuffer[(String, Int)]();
    var players = ArrayBuffer[Spieler]();
    //val kartFiguren = Array("Kreuz", "Pik", "Herz", "Karo");
    var turn = 1
    var karGrenz = 6;
    
    // init spieler
    println("Wieviele Spieler spielen?");
    
    var anzSpieler = scala.io.StdIn.readLine();
    
    if(anzSpieler != 4.toString()){
      println("Not implemented yet!");
      return;
    }
    
    // default- 1 to 4
    for ( i <- 1 to anzSpieler.toInt){
      
      
      i match{
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
    for (x <- players){
      x.setStart();
    }
    
    //test
    println("test");
    for (x <- players)
      println(x.getStart());
    
    // kartenstapel auffuellen
    kartenStapel.clear()
    for (wert <- 1 to 14){
      for (anz <- 1 to 4){
        kartenStapel += wert
        
      }
      
      
      
    }
    println(kartenStapel);
    
    //karten verteilen
    
    val r = scala.util.Random
    
    // spieler - default 4
    
    for (sp <- 1 to players.length){
    
      // kartengrenze - default 6
      for (j <- 1 to karGrenz){
        val zufall = r.nextInt(kartenStapel.length - 1)
        players(sp - 1).setKarte(kartenStapel(zufall))
        kartenStapel.remove(zufall)
      }
    }
    
    // karten mit partner tauschen
    
    //init
    var tauschKarten = Array[Int](0,0,0,0)
    //jeder spieler
    for(sp <- 0 to players.length - 1){
      
      var tKart = "0"
      var check = false
      while(check == false){
        println(players(sp).getName() + ", Waehlen Sie eine Karte zum tauschen aus!")
        println(players(sp).getKarten())
        tKart = scala.io.StdIn.readLine()
         
        //checken ob karte da ist
        for(k <- players(sp).getKarten()){
          if(k == tKart.toInt){
            check = true
          }
        }
        if(!check){
          println("Diese Karte haben Sie nicht.")
        }
      
        
      }
      tauschKarten((sp + 2) % 4) = tKart.toInt
      players(sp).delKarte(tKart.toInt)
    }
    
    for(sp <- 1 to players.length){
      players(sp - 1).setKarte(tauschKarten(sp - 1))
    }
    
    // spiel ... 
    for(sp <- 0 to players.length - 1){
      // checken ob mind. eine karte moeglich
      // ToDo
      
      // karten anzeigen
      println("Ass = 1, Bube = 11, Dame = 12, Koenig = 13, Joker = 14, 0 = keine moegliche")
      println(players(sp).getKarten())
      
      //auswaehlen + loeschen
      println("Waehle eine Karte zum Spielen aus.")
      var spKart = scala.io.StdIn.readLine()
      if(spKart == 0){
        println("Keine Karte zum Spielen.")
      }else{
      
      
        players(sp).delKarte(spKart.toInt)
      
        // kartenlogik ausfuehren
        Kartenlogik().ausfuehren(laufFeld, spKart.toInt, players(sp))
      
        // zug ende
        turn = (turn + 1) % 4
        
      }
    }
    
    //testen ob runden ende
    var anzKar = 0;
    for(sp <- 1 to players.length){
      anzKar += players(sp - 1).getAnzKart()
    }
    
    if(anzKar == 0){
        // runden ende!
      
    }
    
    //println(kartenStapel.length);
    
    
  }
  
  
  
}

