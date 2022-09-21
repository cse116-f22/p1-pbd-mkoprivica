package tests

import org.scalatest._
import pbd.PaleBlueDot

class Task3 extends FunSuite {

  val countriesFile: String = "data/countries.txt"
  val citiesFilename: String = "data/cities.csv"

  test("test 1 - cityPopulations test") {
    val Trinidad_and_TobagoList: List[String] = List("trinidad and tobago", "TRINIDAD AND TOBAGO", "TrInIdAd AND tObAgO")
    val TTcode1: String = "02"
    val AndorraList: List[String] = List("andorra", "ANDORRA", "AnDoRrA", "aNdOrRa")
    val ADcode1 : String = "04"

    for (trinidad <- Trinidad_and_TobagoList){
      assert(PaleBlueDot.cityPopulations(countriesFile, citiesFilename, trinidad, TTcode1) == Map("chaguanas" -> 72158, "couva" -> 5178, "tabaquite" -> 3314), trinidad)
    }

    for (andorra <- AndorraList) {
      assert(PaleBlueDot.cityPopulations(countriesFile , citiesFilename , andorra , ADcode1) == Map("la massana" -> 7211), andorra)
    }
  }

  test("test 2 - aboveAverageCities test") {
    val AnguillaList: List[String] = List("anguilla", "ANGUILLA", "AnGuIlLa", "aNgUiLla")
    val AndorraList: List[String] = List("andorra", "ANDORRA", "AnDoRrA", "aNdOrRa")


    for (anguilla <- AnguillaList) {
      assert(PaleBlueDot.aboveAverageCities(countriesFile, citiesFilename, anguilla).sorted == List(), anguilla)
    }
    for (andorra <- AndorraList) {
      assert(PaleBlueDot.aboveAverageCities(countriesFile, citiesFilename, andorra).sorted == List("les escaldes").sorted, andorra)
    }
    }
}

