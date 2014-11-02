package com.zgagnon.jsonAdapter

import org.json4s.JString
import org.json4s.JBool
import org.json4s.JsonAST.{ JArray, JObject }
import org.json4s.{ JInt, JDouble, JDecimal }
import org.specs2.specification.Fragments
import org.specs2.{ ScalaCheck, Specification }
import spray.json._

/**
 * Created by Zoe on 11/2/2014.
 */
class ForSToSpraySpec extends Specification with ScalaCheck {
  import com.zgagnon.jsonAdapter.sprayAdapter._
  override def is: Fragments =
    s2"""
      The spray converter providse a method to convert Json4s objects to Spray JSON objects.
      To use, import the package com.zgagnon.jsonAdapter.sprayAdapter._

      In order to do this, a mapping between the types of the abstract syntax tree must be established:
        A Json4s JString must produce a Spray JsString with the same value.                         $str
        A Json4s JDecimal must produce a Spray JsNumber with the same value.                        $dec
        A Json4s JDouble must produce a Spray JsNumber with the same value.                         $dub
        A Json4s JInt must produce a Spray JsNumber with the same value.                            $int
        A Json4s JBool must produce a Spray JsBoolean with the same value.                          $bool
        A Json4s JObject must produce a Spray JsObject with the same value.                         $obj
        A Json4s JArray must produce a Spray JsArray with the same value.                           $array
      """

  def str = prop {
    (string: String) =>
      val forS = JString(string)
      val spray: JsString = forS
      spray.value === string
  }

  def dec = prop {
    (dec: BigDecimal) =>
      val forS = JDecimal(dec)
      val spray: JsNumber = forS
      spray.value === dec
  }

  def dub = prop {
    (double: Double) =>
      val forS = JDouble(double)
      val spray: JsNumber = forS
      spray.value === double
  }

  def int = prop {
    (int: Int) =>
      val forS = JInt(int)
      val spray: JsNumber = forS
      spray.value === int
  }

  def bool = prop {
    (bool: Boolean) =>
      val forS = JBool(bool)
      val spray: JsBoolean = forS
      spray.value === bool
  }
  def obj = prop {
    (field: String, value: String) =>
      val forS = JObject(field -> JString(value))
      val spray: JsObject = forS
      spray.fields(field) === JsString(value)
  }
  def array = prop {
    (field1: String, field2: String, field3: String) =>
      val test1 = JObject("name1" -> JString(field1))
      val test2 = JObject("name2" -> JString(field2))
      val test3 = JObject("name3" -> JString(field3))

      val forS = JArray(test1 :: test2 :: test3 :: Nil)
      val spray: JsArray = forS
      (spray.elements(0).asJsObject.fields("name1") === JsString(field1)) and
        (spray.elements(1).asJsObject.fields("name2") === JsString(field2)) and
        (spray.elements(2).asJsObject.fields("name3") === JsString(field3))
  }
}
