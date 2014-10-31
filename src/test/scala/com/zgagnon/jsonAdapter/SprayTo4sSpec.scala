package com.zgagnon.jsonAdapter

import org.json4s._
import org.specs2.Specification
import org.specs2.specification.Fragments
import spray.json._

/**
 * Created by Zoe on 10/31/2014.
 */
class SprayTo4sSpec extends Specification{

  import com.zgagnon.jsonAdapter._

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

  def str = {
    val string = "a test string"
    val spray = JsString(string)
    val forS:JString = spray
    forS.values === string
  }

  def dec = {
    val value = BigDecimal(400)
    val spray = JsNumber(value)
    val forS:JDecimal = spray
    forS.values === value
  }

  def bool= {
    val value = true
    val spray = JsBoolean(value)
    val forS:JBool = spray
    forS.values === value
  }

  def obj = {
    val num = 100
    val string = "test string"
    val bool = true
    val spray = JsObject(
        "num" -> JsNumber(num),
        "string" -> JsString(string),
        "bool" -> JsBoolean(bool)
    )
    val forS:JObject = spray
    ((forS \\ "num").values === num) and
      ((forS \\ "string").values === string) and
      ((forS \\ "bool").values === bool)
  }

  def array = {
    val first = JsObject("name" -> JsString("test1"))
    val second = JsObject("name" -> JsString("test2"))
    val third = JsObject("name" -> JsString("test3"))
    val sprArray = JsArray(first, second, third)
    val forS:JArray = sprArray
((forS(0) \\ "name").values === "test1") and
  ((forS(1) \\ "name").values === "test2") and
  ((forS(2) \\ "name").values === "test3")
  }

  def nul = {
    val forS:JValue = JsNull
    forS === JNull
  }
}
