package com.zgagnon.jsonAdapter

import com.zgagnon.jsonAdapter.testUtils.Generators
import org.json4s.JsonAST.{JArray, JNull, JObject}
import org.json4s._
import org.scalacheck.Prop
import org.specs2.specification.Fragments
import org.specs2.{ScalaCheck, Specification}
import spray.json._

/**
 * Created by Zoe on 11/2/2014.
 */
class ForSToSpraySpec extends Specification with ScalaCheck with Generators {
  import com.zgagnon.jsonAdapter.playAdapter._
  import com.zgagnon.jsonAdapter.sprayAdapter._
  override def is: Fragments =
    s2"""
      The spray converter providse a method to
        convert Json4s objects to Spray JSON objects. $spray

      To use, import the package com.zgagnon.jsonAdapter.sprayAdapter._

      The play converter provides a method to
        convert Json4s object to play json objects. $playExample
      """

  def spray = Prop.forAll(forSGen) {
    (forS: JValue) =>
      val spray: JsValue = forS
      (forS, spray) match {
        case (JNull, JsNull) => ok("JNull converted to JsNull properly")
        case (js: JString, ps: JsString) => js.values === ps.value
        case (jn: JInt, pn: JsNumber) => jn.values === pn.value
        case (jd: JDecimal, pn: JsNumber) => jd.values === pn.value
        case (jb: JBool, pb: JsBoolean) => jb.values === pb.value
        case (JObject(JField("name", jval: JString) :: Nil), JsObject(pval: Map[String, JsValue])) => jval.values === pval("name").asInstanceOf[JsString].value
        case (ja: JArray, pa: JsArray) =>
          val f = ja.arr collect { case JObject(JField("name", value: JString) :: Nil) => value.values }
          val p = pa.elements collect { case JsObject(values: Map[String, JsValue]) => values("name").asInstanceOf[JsString].value }
          f.toVector === p
        case _ => ok
      }
  }

  def playExample = Prop.forAll(forSGen) {
    (forS: JValue) =>
      val spray: play.api.libs.json.JsValue = forS
      (forS, spray) match {
        //case (JNull, play.api.libs.json.JsNull) => ok("JNull converted to JsNull properly")
        case (js: JString, ps: play.api.libs.json.JsString) => js.values === ps.value
        case (jn: JInt, pn: play.api.libs.json.JsNumber) => jn.values === pn.value
        case (jd: JDecimal, pn: play.api.libs.json.JsNumber) => jd.values === pn.value
        case (jb: JBool, pb: play.api.libs.json.JsBoolean) => jb.values === pb.value
        case (JObject(JField("name", jval: JString) :: Nil), play.api.libs.json.JsObject(pval: Map[String, play.api.libs.json.JsValue])) => jval.values === pval("name").asInstanceOf[play.api.libs.json.JsString].value
        case (ja: JArray, pa: play.api.libs.json.JsArray) =>
          val f = ja.arr collect { case JObject(JField("name", value: JString) :: Nil) => value.values }
          val p:Seq[String] = pa.value collect {
            case play.api.libs.json.JsObject(values: Seq[(String, play.api.libs.json.JsValue)]) =>
              val map = values.toMap
             map.apply("name").asInstanceOf[play.api.libs.json.JsString].value }
          f === p
        case _ => ok
      }
  }
}
