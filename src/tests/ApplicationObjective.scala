package tests

import org.scalatest._
import pbd.PaleBlueDot

class ApplicationObjective extends FunSuite {

  val countriesFile: String = "data/countries.txt"
  val citiesFilename: String = "data/cities.csv"

  test("Haversine Formula"){

    val NorthPole : List[Double] = List(90.0,0)
    val SouthPole : List[Double] = List(-90,0)
    val location_near_Buffalo: List[Double] = List(42.889, -78.8789)


    assert(PaleBlueDot.closestCity(citiesFilename, NorthPole) == List("sj", "ny-alesund", "00"))
    assert(PaleBlueDot.closestCity(citiesFilename, SouthPole) == List("ar", "ushuaia", "23"))
    assert(PaleBlueDot.closestCity(citiesFilename, location_near_Buffalo) == List("us", "buffalo", "NY"))


  }



}
