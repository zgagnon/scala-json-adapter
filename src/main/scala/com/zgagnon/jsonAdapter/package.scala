package com.zgagnon

import org.json4s.JsonAST.{JNull, JArray, JBool, JDecimal}
import org.json4s.{JObject, JString, JValue, JsonAST}
import spray.json._

/**
 * Created by Zoe on 10/31/2014.
 */
package object jsonAdapter {

  implicit def sprayString(string:JsString):JString = JsonAST.JString(string.value)
  implicit def sprayNumber(number:JsNumber):JDecimal = JDecimal(number.value)
  implicit def sprayBoolean(boolean:JsBoolean):JBool = JBool(boolean.value)
  implicit def sprayObject(obj:JsObject):JObject = {
    val fields = obj.fields
    val forSFields = fields map {
      (field) =>
        val (name, value) = field
        (name,valueTo4S(value) )
    }
    JObject(forSFields.toList)
  }

  implicit def sprayArray(array:JsArray):JArray = {
    val values = for(value <- array.elements) yield {valueTo4S(value)}
    JArray(values.toList)
  }

  implicit def sprayNull(obj:JsValue) = JNull
  def valueTo4S(value:JsValue):JValue = {
    value match {
      case s:JsString => sprayString(s)
      case n:JsNumber => sprayNumber(n)
      case b:JsBoolean => sprayBoolean(b)
      case o:JsObject => sprayObject(o)
      case a:JsArray => sprayArray(a)
      case JsNull => JNull
    }
  }
 }
