package tests

import org.scalatest._
import pbd.PaleBlueDot


class Task2 extends FunSuite {

  val countriesFile: String = "data/countries.txt"
  val citiesFilename: String = "data/cities.csv"

  test("1- multiple different countries "){
    val AnguillaList : List[String] = List( "anguilla", "ANGUILLA", "AnGuIlLa", "aNgUiLla")
    val AndorraList : List[String] = List("andorra", "ANDORRA", "AnDoRrA", "aNdOrRa")
    val United_Arab_EmiratesList : List[String] = List("united arab emirates", "UNITED ARAB EMIRATES", "UnItEd ArAb EmIrAtEs", "uNiTeD aRaB eMiRaTeS")
    val AIavg : Double = 1379.0
    val ADavg : Double = 8409.5
    val AEavg : Double = 761668.333
    val epsilon : Double = .001


    for (anguilla <- AnguillaList){
      assert(Math.abs(PaleBlueDot.averagePopulation(countriesFile, citiesFilename,anguilla)-AIavg) < epsilon , anguilla)
    }
    for (andorra <- AndorraList){
      assert(Math.abs(PaleBlueDot.averagePopulation(countriesFile, citiesFilename,andorra) - ADavg ) < epsilon , andorra)
    }
    for (uae <- United_Arab_EmiratesList) {
      assert(Math.abs(PaleBlueDot.averagePopulation(countriesFile, citiesFilename, uae) - AEavg) < epsilon, uae)
    }



  }

}
