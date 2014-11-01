package com.zgagnon.jsonAdapter

import org.json4s._
import org.scalacheck.Gen
import org.specs2.matcher.MatchResult
import org.specs2.specification.Fragments
import org.specs2.{ ScalaCheck, Specification }
import spray.json._

/**
 * Created by Zoe on 10/31/2014.
 */
class SprayTo4sSpec extends Specification with ScalaCheck {

  import com.zgagnon.jsonAdapter.sprayAdapter._

  val obGen = for (
    string <- Gen.alphaStr
  ) yield { JsObject("name" -> JsString(string)) }

  override def is: Fragments =
    s2"""
        The Spray converter provides a method to convert Spray JSON objects to Json4s objects.

        In order to do this, a mapping between the types of the abstract syntax tree must be establish:
          A Spray JsString must produce a Json4s JString with the same value.                       $str
          A Spray JsNumber must produce a Json4s JDecimal with the same value.                      $dec
          A Spray JsBoolean must produce a Json4s JBool with the same value.                        $bool
          A Spray JsObject must produce a Json4s JObject with the same values.                      $obj
          A Spray JsArray must produce a Json4s JArray with the same values.                        $array
          A Sapray JsNull must produce a Json4s JNull.                                              $nul
      """

  def str = prop {
    (string: String) =>
      val spray = JsString(string)
      val forS: JString = spray
      forS.values === string
  }

  def dec = prop {
    (value: BigDecimal) =>
      val spray = JsNumber(value)
      val forS: JDecimal = spray
      forS.values === value
  }

  def bool = prop {
    (value: Boolean) =>
      val spray = JsBoolean(value)
      val forS: JBool = spray
      forS.values === value
  }

  def obj = prop {
    (num: BigDecimal, string: String, bool: Boolean) =>
      val spray = JsObject(
        "num" -> JsNumber(num),
        "string" -> JsString(string),
        "bool" -> JsBoolean(bool)
      )
      val forS: JObject = spray
      ((forS \\ "num").values === num) and
        ((forS \\ "string").values === string) and
        ((forS \\ "bool").values === bool)
  }

  def array = prop {
    (string1: String, string2: String, string3: String) =>
      val first = JsObject("name" -> JsString(string1))
      val second = JsObject("name" -> JsString(string2))
      val third = JsObject("name" -> JsString(string3))
      val sprArray = JsArray(first, second, third)
      val forS: JArray = sprArray
      ((forS(0) \\ "name").values === string1) and
        ((forS(1) \\ "name").values === string2) and
        ((forS(2) \\ "name").values === string3)
  }

  def nul = {
    val forS: JValue = JsNull
    forS === JNull
  }

  def combineResults(results: Seq[MatchResult[_]]): MatchResult[Any] = {
    (ok /: results) { (base, next) => base and next }
  }
}
