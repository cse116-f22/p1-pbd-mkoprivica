package pbd

import java.awt.Desktop
import java.net.URI
import scala.io.{BufferedSource, Source}


object PaleBlueDot {


  /**
   * Task 1
   *
   * Given a country name using a mix of case (upper/lower), return the country code in all lowercase letters
   *
   * Ex. If "Heard Island and McDonald Islands#HM" is a line countriesFilename and the countryName input
   * of your method is "hEaRd IsLaNd AnD mCdOnAlD iSlAnDs" the returned value is "hm"
   *
   * If countryName is not in the file, return the empty String: ""
   *
   * @param countriesFilename Name of the file containing country names and codes
   * @param countryName       The name of the country to lookup in the file with any mix of upper/lower-case
   * @return The two letter country code for countryName in lowercase letters
   */
  def getCountryCode(countriesFilename: String, countryName: String): String = {
    val countriesFile: BufferedSource = Source.fromFile(countriesFilename)
    var countrycode: String = ""
    for (line <- countriesFile.getLines()){
      var SplitLine: Array[String] = line.split("#")
      var country : String = SplitLine(0)
      var LCcountry : String = country.toLowerCase()
      if (LCcountry == countryName.toLowerCase()){
        countrycode = SplitLine(1)
      }
    }
    var rtrn : String = countrycode.toLowerCase()
    rtrn
  }


  /**
   * Task 2
   *
   * Find the average population of cities in a country
   * regardless.
   *
   * @param countriesFilename Name of the file containing country names and codes
   * @param citiesFilename    Name of the file containing city name, population, and location data
   * @param countryName       The name of the country with any mix of upper/lower-case
   * @return The average population of cities in the given country
   */
  def averagePopulation(countriesFilename: String, citiesFilename: String, countryName: String): Double = {
    var country: String = getCountryCode(countriesFilename, countryName)
    var population: Double = 0.0
    var number_of_cities: Double = 0.0
    val citiesFile: BufferedSource = Source.fromFile(citiesFilename)
    for (line <- citiesFile.getLines().drop(1).map(_.split(","))){
      if (country == line(0)){
        population += line(3).toDouble
        number_of_cities += 1.0
      }else{
        0.0
      }
    }
    var average : Double = population/number_of_cities
    average
  }

  /**
   * Task 3
   */

  /**
   * Returns a Map[cityName -> population] for all cities in the given county. The name of each
   * city should match exactly how it appears in citiesFilename and the population is read from the file
   * and converted to an Int. The country name may contain any mix of upper/lower-case letters.
   *
   * @param countriesFilename Name of the file containing country names and codes
   * @param citiesFilename    Name of the file containing city name, population, and location data
   * @param countryName       The name of the country with any mix of upper/lower-case
   * @param regionCode        Two digit region code with case matching the case from the cities file
   * @return A Map containing the name and population of every city in the given country
   */
  def cityPopulations(countriesFilename: String, citiesFilename: String, countryName: String, regionCode: String): Map[String, Int] = {
    var returnMap : Map[String,Int] = Map()
    var country: String = getCountryCode(countriesFilename, countryName)
    val citiesFile: BufferedSource = Source.fromFile(citiesFilename)
    for (line <- citiesFile.getLines().drop(1).map(_.split(","))){
      if (country == line(0) && regionCode == line(2)){
        returnMap = returnMap + ( line(1) -> line(3).toInt )

      }else{
        Map()
      }
    }
    returnMap
  }


  /**
   * Returns a List of city names in the given county and with above average population for that country
   *
   * @param countriesFilename Name of the file containing country names and codes
   * @param citiesFilename    Name of the file containing city name, population, and location data
   * @param countryName       The name of the country with any mix of upper/lower-case
   * @return All city names in given country with a population > the average populations of cities in that country
   */
  def aboveAverageCities(countriesFilename: String, citiesFilename: String, countryName: String): List[String] = {
    var returnList : List[String] = List()
    var country: String = getCountryCode(countriesFilename, countryName)
    var avgpop: Double = averagePopulation(countriesFilename,citiesFilename, countryName)
    val citiesFile: BufferedSource = Source.fromFile(citiesFilename)
    for (line <- citiesFile.getLines().drop(1).map(_.split(","))){
      if (country == line(0) && (line(3).toDouble > avgpop)){
        returnList = returnList :+ line(1)
        }
      }
    returnList
  }


  /**
   * Application Objective
   *
   * You find yourself stranded in an unfamiliar place with no signs of civilization. You don't have much with you,
   * but you do have a locator that gives your current latitude/longitude, a csv file of cities, and your final
   * submission to the PaleBlueDot assignment from CSE116 (What luck!). You decide that finding and walking
   * directly to the closest city will give you the best chance to survive.
   *
   * Return the closest city to the given location in terms of greater circle distance which is the shortest distance
   * needed to walk along the surface of the Earth to reach a city.
   *
   * @param citiesFilename Name of the file containing city name, population, and location data
   * @param location       A location on Earth given as a List containing latitude and longitude coordinates
   * @return The city closest to the given location as a List containing country code, city name, and region
   *         exactly as they appear in the cities file (ie. the List should have exactly 3 values to return
   *         a single city
   */

  def HaversineFormula(pointA: List[Double], pointB: List[Double]): Double ={
    val EarthRadius = 6371
    val latDistance = Math.toRadians(pointA(0) - pointB(0))
    val lngDistance = Math.toRadians(pointA(1) - pointB(1))
    val sinLat = Math.sin(latDistance / 2)
    val sinLng = Math.sin(lngDistance / 2)
    val a = sinLat * sinLat +
      (Math.cos(Math.toRadians(pointA(0))) *
        Math.cos(Math.toRadians(pointB(0))) *
        sinLng * sinLng)
    val c = 2 * Math.atan2(Math.sqrt(a), Math.sqrt(1 - a))
    val d: Double = (EarthRadius * c)
    d
  }

   def closestCity(citiesFilename: String, location: List[Double]): List[String] = {
    var Answer: List[String] = List()
    var SmallestDistance: Double = 100000.0
    val citiesFile: BufferedSource = Source.fromFile(citiesFilename)
    for (line <- citiesFile.getLines().drop(1).map(_.split(","))) {
      var LineLocation: List[Double] = List(line(4).toDouble, line(5).toDouble)
      val distance: Double = HaversineFormula(location, LineLocation)
      if (distance < SmallestDistance) {
        SmallestDistance = distance
        Answer = List(line(0), line(1), line(2))
      }
    }
    Answer
  }


  /**
   * Helper Method
   *
   * Opens Google Maps at a specific location. The location is a List containing the latitude then longitude as Doubles
   *
   * @param location The location to open in the format List(Latitude, Longitude)
   */
  def openMap(location: List[Double]): Unit = {
    if (Desktop.isDesktopSupported && Desktop.getDesktop.isSupported(Desktop.Action.BROWSE)) {
      val url: String = "http://maps.google.com/maps?t=m&q=loc:" + location.head.toString + "+" + location(1).toString
      Desktop.getDesktop.browse(new URI(url))
    } else {
      println("Opening the browser not supported")
    }
  }


  def main(args: Array[String]): Unit = {
    openMap(List(43.002743, -78.7874136))

  }

}
