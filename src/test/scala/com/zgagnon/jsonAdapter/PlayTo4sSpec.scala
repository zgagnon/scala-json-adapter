package com.zgagnon.jsonAdapter

import org.json4s.JsonAST.JNull
import org.json4s._
import org.specs2.specification.Fragments
import org.specs2.{ ScalaCheck, Specification }
import play.api.libs.json._

/**
 * Created by Zoe on 11/1/2014.
 */
class PlayTo4sSpec extends Specification with ScalaCheck {
  import com.zgagnon.jsonAdapter.playAdapter._
  override def is: Fragments =
    s2"""
    The Play-json to Json4s converter provides implicit methods to convert play json AST to Json4s AST:

      Convert play JsNull to json4s JNull                               $jnull
      Convert play JString to JString                                   $string
      Convert play JsNumber to JNumber                                  $number
      Convert play JsBoolean to JBoolean                                $bool
      Convert play JsObject to JObject                                  $obj
      Convert play JsArray to JArray                                    $array
    """

  def jnull = {
    val nul: JValue = JsNull
    nul === JNull
  }

  def string = prop {
    (testString: String) =>
      val play = JsString(testString)
      val forS: JString = play
      forS.values === testString
  }

  def number = prop {
    (testNumber: BigDecimal) =>
      val play = JsNumber(testNumber)
      val forS: JDecimal = play
      forS.values === testNumber
  }

  def bool = prop {
    (testBool: Boolean) =>
      val play = JsBoolean(testBool)
      val forS: JBool = play
      forS.values === testBool
  }

  def obj = prop {
    (fieldName: String, fieldValue: String) =>
      val play = Json.obj(fieldName -> JsString(fieldValue))
      val forS: JObject = play
      (forS \\ fieldName).values === fieldValue
  }

  def array = prop {
    (string1: String, string2: String, string3: String) =>
      val first = Json.obj("name" -> JsString(string1))
      val second = Json.obj("name" -> JsString(string2))
      val third = Json.obj("name" -> JsString(string3))
      val play = JsArray(first :: second :: third :: Nil)
      val forS: JArray = play
      ((forS(0) \\ "name").values === string1) and
        ((forS(1) \\ "name").values === string2) and
        ((forS(2) \\ "name").values === string3)
  }
}
