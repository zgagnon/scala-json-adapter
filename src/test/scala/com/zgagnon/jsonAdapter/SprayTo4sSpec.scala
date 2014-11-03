package com.zgagnon.jsonAdapter

import com.zgagnon.jsonAdapter.testUtils.Generators
import org.json4s.JsonAST.JNull
import org.json4s._
import org.scalacheck.{ Gen, Prop }
import org.specs2.specification.Fragments
import org.specs2.{ ScalaCheck, Specification }
import spray.json.JsNull
import spray.json.{ JsArray, JsBoolean, JsObject, _ }

/**
 * Created by Zoe on 10/31/2014.
 */
class SprayTo4sSpec extends Specification with ScalaCheck with Generators {

  import com.zgagnon.jsonAdapter.sprayAdapter._

  val obGen = for (
    string <- Gen.alphaStr
  ) yield {
    JsObject("name" -> JsString(string))
  }

  override def is: Fragments =
    s2"""
        The Spray converter provides implicit methods to
          convert Spray JSON objects to Json4s objects $forS


        To use, import the package com.zgagnon.jsonAdapter.sprayAdapter._
      """

  def forS = Prop.forAll(sprayGen) {
    (spray: JsValue) =>
      val forSValue: JValue = spray
      (forSValue, spray) match {
        case (JNull, JsNull) => ok("JNull converted to JsNull properly")
        case (js: JString, ps: JsString) => js.values === ps.value
        case (jn: JInt, pn: JsNumber) => jn.values === pn.value
        case (jd: JDouble, pn: JsNumber) => jd.values === pn.value
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

  def playSpray = Prop.forAll(sprayGen) {
    (spray: JsValue) =>
      val playValue: play.api.libs.json.JsValue = spray
      (spray, playValue) match {
        case (ss: JsString, ps: play.api.libs.json.JsString) => ss.value === ps.value
        case (sn: JsNumber, pn: play.api.libs.json.JsNumber) => sn.value === pn.value
        case (sb: JsBoolean, pb: play.api.libs.json.JsBoolean) => sb.value === pb.value
        case (so: JsObject, po: play.api.libs.json.JsObject) => (so.fields("name").toString()) === (po \\ "name").head.toString()
        case (sa: JsArray, pa: play.api.libs.json.JsArray) =>
          val spray = sa.elements collect { case s: JsObject => s.fields("name").toString }
          val playItems = pa.value collect { case o: play.api.libs.json.JsObject => (o \\ "name").head.toString }
          spray.toList === playItems

        case (JsNull, play.api.libs.json.JsNull) => ok("JsNull converts to JsNull properly")
        case (s: JsValue, p: JsValue) => ko(s"Cannot convert $s to $p")
      }
  }
}
