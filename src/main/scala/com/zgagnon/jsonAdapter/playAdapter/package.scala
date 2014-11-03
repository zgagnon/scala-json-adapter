package com.zgagnon.jsonAdapter

import org.json4s._
import play.api.libs.json._

/**
 * Created by Zoe on 11/1/2014.
 */
package object playAdapter {
  implicit def jsValueToJValue(value: JsValue): JValue = value match {
    case JsNull => JNull
    case s: JsString => jsStringToJString(s)
    case n: JsNumber => jsNumberToJDecimal(n)
    case b: JsBoolean => jsBooleanToJBool(b)
    case o: JsObject => jsObjectToJObject(o)
    case a: JsArray => jsArrayToJArray(a)
    case _ => JNothing
  }

  implicit def jsStringToJString(string: JsString): JString = JString(string.value)
  implicit def jsNumberToJDecimal(number: JsNumber): JDecimal = JDecimal(number.value)
  implicit def jsBooleanToJBool(bool: JsBoolean): JBool = JBool(bool.value)
  implicit def jsObjectToJObject(obj: JsObject): JObject = {
    val forSFields = for ((name, value) <- obj.fieldSet) yield { (name -> jsValueToJValue(value)) }
    JObject(forSFields.toList)
  }
  implicit def jsArrayToJArray(array: JsArray): JArray = {
    val values = for (value <- array.value) yield { jsValueToJValue(value) }
    JArray(values.toList)
  }

  implicit def playToSpray(value: JsValue): spray.json.JsValue = {
    import com.zgagnon.jsonAdapter.sprayAdapter.valueToSpray
    valueToSpray(value)
  }

  implicit def jStringToJsString(string: JString): JsString = JsString(string.values)
  implicit def jIntToJsNumber(int: JInt): JsNumber = JsNumber(BigDecimal(int.values))
  implicit def jDecimalToJsNumber(dec: JDecimal): JsNumber = JsNumber(dec.values)
  implicit def jBoolToJsBoolean(bool: JBool): JsBoolean = JsBoolean(bool.values)

  implicit def jObjectToJObject(obj: JObject): JsObject = {
    val fields = for ((name, value) <- obj.obj) yield { name -> jValueToJsValue(value) }
    JsObject(fields)
  }

  implicit def jArrayToJsArray(array: JArray): JsArray = {
    val values = for (value <- array.arr) yield { jValueToJsValue(value) }
    JsArray(values)
  }

  implicit def jValueToJsValue(value: JValue): JsValue = value match {
    case JInt(num) => JsNumber(BigDecimal(num))
    case JDecimal(num) => JsNumber(num)
    case JString(string) => JsString(string)
    case JBool(bool) => JsBoolean(bool)
    case obj: JObject => jObjectToJObject(obj)
    case array: JArray => jArrayToJsArray(array)
    case JNull => JsNull
  }
}
