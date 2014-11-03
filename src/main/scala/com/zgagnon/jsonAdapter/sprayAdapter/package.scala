package com.zgagnon.jsonAdapter

import org.json4s._
import spray.json._

/**
 * Created by Zoe on 10/31/2014.
 */
package object sprayAdapter {
  import com.zgagnon.jsonAdapter.playAdapter.jValueToJsValue
  implicit def sprayString(string: JsString): JString = JString(string.value)
  implicit def sprayNumber(number: JsNumber): JDecimal = JDecimal(number.value)
  implicit def sprayBoolean(boolean: JsBoolean): JBool = JBool(boolean.value)

  implicit def sprayObject(obj: JsObject): JObject = {
    val forSFields = for ((name, value) <- obj.fields) yield { (name -> valueTo4S(value)) }
    JObject(forSFields.toList)
  }

  implicit def sprayArray(array: JsArray): JArray = {
    val values = for (value <- array.elements) yield { valueTo4S(value) }
    JArray(values.toList)
  }

  implicit def valueTo4S(value: JsValue): JValue = {
    value match {
      case s: JsString => sprayString(s)
      case n: JsNumber => sprayNumber(n)
      case b: JsBoolean => sprayBoolean(b)
      case o: JsObject => sprayObject(o)
      case a: JsArray => sprayArray(a)
      case JsNull => JNull
    }
  }

  implicit def forSString(string: JString): JsString = JsString(string.values)
  implicit def forSDecimal(dec: JDecimal): JsNumber = JsNumber(dec.values)
  //implicit def forSDouble(double: JDouble): JsNumber = JsNumber(double.values)
  implicit def forSInt(int: JInt): JsNumber = JsNumber(int.values)
  implicit def forSBool(bool: JBool): JsBoolean = JsBoolean(bool.values)

  implicit def forSObject(obj: JObject): JsObject = {
    val values = for ((field, value: JValue) <- obj.obj) yield { field -> valueToSpray(value) }
    JsObject(values.toMap)
  }

  implicit def forSArray(array: JArray): JsArray = {
    val values = for (value <- array.arr) yield { valueToSpray(value) }
    JsArray(values.toVector)
  }

  implicit def valueToSpray(value: JValue): JsValue = {
    value match {
      case s: JString => forSString(s)
      case d: JDecimal => forSDecimal(d)
      //case dub: JDouble => forSDouble(dub)
      case i: JInt => forSInt(i)
      case b: JBool => forSBool(b)
      case o: JObject => forSObject(o)
      case JNothing => JsNull
      case JNull => JsNull
      case a: JArray => forSArray(a)
    }
  }

  implicit def sprayToPlay(value: JsValue): play.api.libs.json.JsValue = jValueToJsValue(valueTo4S(value))
}
