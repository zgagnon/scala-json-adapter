package com.zgagnon.jsonAdapter

import com.zgagnon.jsonAdapter.testUtils.Generators
import org.json4s.JsonAST.JNull
import org.json4s._
import org.scalacheck.Prop
import org.specs2.specification.Fragments
import org.specs2.{ ScalaCheck, Specification }
import play.api.libs.json._

/**
 * Created by Zoe on 11/1/2014.
 */
class PlayTo4sSpec extends Specification with ScalaCheck with Generators {
  import com.zgagnon.jsonAdapter.playAdapter._

  type sJsValue = spray.json.JsValue
  type sJsString = spray.json.JsString
  type sJsNumber = spray.json.JsNumber
  type sJsBool = spray.json.JsBoolean
  type sJsObject = spray.json.JsObject
  type sJsArray = spray.json.JsArray

  override def is: Fragments =
    s2"""
    The Play-json to Json4s converter provides implicit methods to
      convert play json AST to Json4s AST $forS
      convert Play-json objects to corresponding Spray json objects  $playSpray
    """

  def forS = Prop.forAll(playGen) {
    (play: JsValue) =>
      val forSValue: JValue = play
      (forSValue, play) match {
        case (JNull, JsNull) => ok("JNull converted to JsNull properly")
        case (js: JString, ps: JsString) => js.values === ps.value
        case (jn: JInt, pn: JsNumber) => jn.values === pn.value
        case (jd: JDouble, pn: JsNumber) => jd.values === pn.value
        case (jd: JDecimal, pn: JsNumber) => jd.values === pn.value
        case (jb: JBool, pb: JsBoolean) => jb.values === pb.value
        case (JObject(JField("name", jval: JString) :: Nil), JsObject(Seq(("name", pval: JsString)))) => jval.values === pval.value
        case (ja: JArray, pa: JsArray) =>
          val f = ja.arr collect { case JObject(JField("name", value) :: Nil) => value.values }
          val p = pa.value collect { case JsObject(Seq(("name", value: JsString))) => value.value }
          f must containTheSameElementsAs(p)
      }
  }

  def playSpray = Prop.forAll(playGen) {
    (play: JsValue) =>
      val sprayValue: spray.json.JsValue = play
      (sprayValue, play) match {
        case (ss: sJsString, ps: JsString) => ss.value === ps.value
        case (sn: sJsNumber, pn: JsNumber) => sn.value === pn.value
        case (sb: sJsBool, pb: JsBoolean) => sb.value === pb.value
        case (so: sJsObject, po: JsObject) => (so.fields("name").toString()) === (po \\ "name").head.toString()
        case (sa: sJsArray, pa: JsArray) =>
          val spray = sa.elements collect { case s: sJsObject => s.fields("name").toString }
          val play = pa.value collect { case o: JsObject => (o \\ "name").head.toString }
          spray must containTheSameElementsAs(play)
        case (spray.json.JsNull, JsNull) => ok("JsNull converts to JsNull properly")
        case (s: sJsValue, p: JsValue) => ko(s"Cannot convert $s to $p")
      }
  }

}
