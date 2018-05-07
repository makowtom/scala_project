import java.io._

import sys.process._
import scala.sys.process.ProcessLogger
import java.lang.String
import java.util.ArrayList

import scala.collection.mutable.HashMap

object Hello{

  //funkcja sluzy do pobierania danych ze strony z danego miesiaca
  def pobierz(rok : String, mies : String)  {
    var cos: String = "wget -m -np --load-cookies cookies.txt ftp://guest:guest@ted.europa.eu/daily-packages/" + rok + "/" + mies !!
  }

  //funkcja wczytuje sane z konkretnego katalogu zawierajacego pliki xml
  // oraz wybiera tylko dane o krajach i wartosciach zamówień
  def ogarnij_katalog(sciezka : String): (HashMap[String, Double], HashMap[String, (Double,Int)]) = {
    var xmle : String = "ls " + sciezka !!
    var xmls : Array[String] = xmle.split("\n")
    var krajeCount : HashMap[String, Double] = HashMap.empty[String, Double] //wystapienie kraju w xml-u
    var sumaKasy : HashMap[String, (Double,Int)] = HashMap.empty[String, (Double,Int)] //wystapienie kraju wraz z wartoscia za zlecenie z waluta
    for (x <- xmls) {
      val xmlWpis = scala.xml.XML.loadFile(sciezka+"/"+x)
      val kraj = (xmlWpis \ "CODED_DATA_SECTION" \ "NOTICE_DATA" \ "ISO_COUNTRY" \ "@VALUE").map(_.text).mkString
      var kasa = xmlWpis \ "CODED_DATA_SECTION" \ "NOTICE_DATA" \ "VALUES" \ "VALUE"
      val currency = (kasa \ "@CURRENCY").map(_.text).mkString
      var kasaText = kasa.map(_.text)
      if (kasaText.isEmpty) { //nie dodajemy do sumaKasy
      } else if (kasaText.size == 1) {
        //dodajemy do sumaKasy tylko jesli wartosc >100, inaczej mozemy zaburzyc wyniki (sa przypadki, gdy ktos wpisal wartosc rowna np. 1)
        val kasaWartosc = kasaText.mkString.toDouble
        if (kasaWartosc > 100){
          val key = kraj + " " + currency
          if (sumaKasy.contains(key)) { //trzeba zaktualizować istniejący wpis
            sumaKasy.put(key,(sumaKasy(key)._1 + kasaWartosc, sumaKasy(key)._2 + 1))
          }
          else { //trzeba utworzyc nowy pusty wpis
            sumaKasy.put(key,(kasaWartosc, 1))
          }
        }
      }
      else { //nie dodajemy do sumaKasy, bo niewiadomo ktora wartosc
      }
      //aktualizacja licznika wystapien kraji
      if (krajeCount.contains(kraj)) {
        krajeCount.put(kraj,krajeCount(kraj) + 1)
      }
      else {
        krajeCount.put(kraj, 1)
      }
    }
    return (krajeCount,sumaKasy)
  }

  //funkcja laczy tabele wynikow uzyskane z poszczegolnych katalogow
  def polacz_dane( A : (HashMap[String, Double], HashMap[String, (Double,Int)]),
                       result1 : HashMap[String, Double],
                       result2 : HashMap[String, (Double,Int)]):
                      (HashMap[String, Double], HashMap[String, (Double,Int)])= {
    //funkcja dolacza do result1 i result2 odpowiednie czesci tupli A tak zeby unikalne wyniki dodawac, a powtarzajace sie w obu sumowac
    for ((k,v) <- A._1) {
      if (result1.contains(k)) {
        result1.put(k,result1(k)+v)
      }
      else {
        result1.put(k,v)
      }
    }
    for ((k,v) <- A._2) {
      if (result2.contains(k)) {
        result2.put(k,(result2(k)._1+v._1,result2(k)._2+v._2))
      }
      else {
        result2.put(k,v)
      }
    }
    return (result1,result2)
  }
  //wersja druga dla innych dannych wejsciowych
  def polacz_dane2( A : (HashMap[String, Double], HashMap[String, Double]),
                   result1 : HashMap[String, Double],
                   result2 : HashMap[String, Double]):
  (HashMap[String, Double], HashMap[String, Double])= {
    //funkcja dolacza do result1 i result2 odpowiednie czesci tupli A tak zeby unikalne wyniki dodawac, a powtarzajace sie w obu sumowac
    for ((k,v) <- A._1) {
      if (result1.contains(k)) {
        result1.put(k,result1(k)+v)
      }
      else {
        result1.put(k,v)
      }
    }
    for ((k,v) <- A._2) {
      if (result2.contains(k)) {
        result2.put(k,(result2(k)+v))
      }
      else {
        result2.put(k,v)
      }
    }
    return (result1,result2)
  }

  //funkcja znajduje unikalne wystapienia walut zawarte w plikach xml
  //tak, zeby mozna bylo zrobic liste przelicznikow na euro (tylko pomocnicza)
  def zlicz_waluty(result2 : HashMap[String, (Double,Int)]) : Array[String] = {
    var waluty : Array[String] = Array.empty[String]
    for ((k,v) <- result2) {
      val cur = k.substring(3)
      if (waluty.contains(cur)) {}
      else {
        waluty = waluty :+ cur}
    }
    return (waluty)
  }

  //funkcja liczy srednia wartosc zamowienia danego kraju w euro wykorzystujac przeliczniki z innych walut na euro
  def oblicz_srednia(result2 : HashMap[String, (Double,Int)]) : HashMap[String, Double] = {
    var krajeKasa : HashMap[String, Double] = HashMap.empty[String, Double]
    var krajeLiczba : HashMap[String, Int] = HashMap.empty[String, Int]
    val waluty : HashMap[String, Double] = HashMap(("EUR", 1), ("USD", 1.2107), ("GBP", 0.8784), ("BGN", 1.9558),
      ("CHF", 1.1967), ("NOK", 9.6506), ("MKD", 61.566), ("DKK", 7.4504), ("PLN", 4.2165), ("HUF", 312.8411),
      ("SKK", 30.126), ("SEK", 10.5074), ("HRK", 7.4172), ("CZK", 25.4744), ("MLT", 0.4293), ("MTL", 0.4293), ("RON",4.6598),
      ("ISK",121.51), ("EEK",15.6466), ("LTL",3.45280))
    //dane na dzien 27.04 (tylko korona slowacka kurs jak jeszcze istniala)
    for ((k,v) <- result2) {
      val cur = k.substring(3)
      val country = k.substring(0,2)
      var srednia = 0.0
      if (waluty.contains(cur)) {
        var kwota = v._1 / waluty(cur)
        if (krajeKasa.contains(country)) {
          krajeKasa.put(country,krajeKasa(country) + kwota)
          krajeLiczba.put(country,krajeLiczba(country) + v._2)
        }
        else {
          krajeKasa.put(country,kwota)
          krajeLiczba.put(country,v._2)
        }
      }
      else {
        println("Nie znaleziono waluty: " + cur)
      }
    }
    //liczenie średnich
    for ((k,v) <- krajeKasa) {
      krajeKasa.put(k,v/krajeLiczba(k))
    }
    return(krajeKasa)
  }

  //funkja robi wszystkie potrzebne operacje na plikach xml z danego miesiaca
  def dane_miesiaca(rok : String, mies : String): (HashMap[String, Double], HashMap[String, Double]) =  {
    //katalog do danych tymczasowych
    "mkdir tmp" ! ;
    var sciezka = "ls ted.europa.eu/daily-packages/" + rok + "/" + mies
    var pliki : String = sciezka !!
    var pliks : Array[String] = pliki.split("\n")
    for (x <- pliks) {
      "tar -zxvf ted.europa.eu/daily-packages/" + rok + "/" + mies + "/" + x + " -C tmp" !
    }
    var katalogi : String = "ls tmp" !!
    var katalogs : Array[String] = katalogi.split("\n")
    var result1 : HashMap[String, Double] = HashMap.empty[String, Double] //krajeCount
    var result2 : HashMap[String, (Double,Int)] = HashMap.empty[String, (Double,Int)] //sumaKasy
    for (x <- katalogs) {
      val A = ogarnij_katalog("tmp/"+x)
      val results = polacz_dane(A,result1,result2)
      result1 = results._1; result2 = results._2
    }
    // usuniecie smieci po miesiacu
    "rm -r tmp" ! ;
    var srednia = oblicz_srednia(result2)
    return (result1,srednia)
  }

  //funkcja zapisuje dane obliczone w programie do plikow .csv
  def zapisz(do_zapisania : HashMap[String, Double], nazwa_pliku : String) {
    val pw = new PrintWriter(new File("dane/" + nazwa_pliku + ".csv"))
    pw.write("Kraj,Wartosc\n")
    for ((k,v) <- do_zapisania) {
      pw.write(k + "," + v.toString() + "\n")
    }
    pw.close()
  }

  //przebieg programu
  def main(args: Array[String]){
    //pobieranie danych z serweru z lat 2011-2017
    for (i <- 2017 to 2017) {
      for (j <- 12 to 12) {
        var jString = j.toString
        if (jString.size==1) {jString = "0" + jString} //dodanie 0 do jednocyfrowych nazw
        //pobierz(i.toString, jString)
      }
    }
    //dane szukane w programie
    var krajeLiczbaZamowien : Array[HashMap[String, Double]] = Array.empty[HashMap[String, Double]]
    var krajeSredniaWartoscZamowien : Array[HashMap[String, Double]] = Array.empty[HashMap[String, Double]]
    var krajeLiczbaPodsumowanie : HashMap[String, Double] = HashMap.empty[String, Double]
    var krajeSredniaWartoscPodsumowanie : HashMap[String, Double] = HashMap.empty[String, Double]

    //do zapisywania danych
    //"mkdir dane" !;

    for (i <- 2017 to 2017) {
      for (j <- 1 to 12) {
        var jString = j.toString
        if (jString.size==1) {jString = "0" + jString} //dodanie 0 do jednocyfrowych nazw
        val wynik = dane_miesiaca(i.toString, jString)
        krajeLiczbaZamowien = krajeLiczbaZamowien :+ wynik._1
        krajeSredniaWartoscZamowien = krajeSredniaWartoscZamowien :+ wynik._2
        val podsumowanie = polacz_dane2(wynik,krajeLiczbaPodsumowanie,krajeSredniaWartoscPodsumowanie)
        krajeLiczbaPodsumowanie = podsumowanie._1
        krajeSredniaWartoscPodsumowanie = podsumowanie._2

        //zapis danych czesciowych
        zapisz(wynik._1,"Liczba_"+i.toString()+"_"+j.toString)
        zapisz(wynik._2,"Wartosc_"+i.toString()+"_"+j.toString)
      }
    }
    // zapis danych całowściowych
    zapisz(krajeLiczbaPodsumowanie,"Liczba_podsumowanie")
    zapisz(krajeSredniaWartoscPodsumowanie, "Wartosc_podsumowanie")
  }
}

