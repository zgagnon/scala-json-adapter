package com.zgagnon.jsonAdapter.testUtils

import org.json4s.JValue
import org.json4s.JsonAST._
import org.scalacheck.{ Arbitrary, Gen }
import org.specs2.{ ScalaCheck, Specification }
import play.api.libs.json._

import scala.util.Random

/**
 * Created by Zoe on 11/2/2014.
 */
trait Generators {
  type self = Specification with ScalaCheck
  val playGen: Gen[JsValue] = for (
    string <- Gen.alphaStr;
    num <- Arbitrary.arbBigDecimal.arbitrary;
    obj <- playObjGen;
    array <- playArrayGen;
    value <- Gen.oneOf(JsString(string), JsNumber(num), JsBoolean(Random.nextBoolean()), obj, array, JsNull)
  ) yield value

  val playObjGen = for (
    value <- Gen.alphaStr
  ) yield { Json.obj("name" -> value) }

  val playArrayGen = for (
    array <- Gen.listOf(playObjGen)
  ) yield { JsArray(array) }

  val sprayGen: Gen[spray.json.JsValue] = for (
    string <- Gen.alphaStr;
    num <- Arbitrary.arbBigDecimal.arbitrary;
    obj <- sprayObjGen;
    array <- sprayArrayGen;
    value <- Gen.oneOf(spray.json.JsNull, spray.json.JsString(string), spray.json.JsNumber(num), spray.json.JsBoolean(Random.nextBoolean()),
      obj, array)
  ) yield { value }

  val sprayObjGen = for {
    value <- Gen.alphaStr
  } yield { spray.json.JsObject("name" -> spray.json.JsString(value)) }

  val sprayArrayGen = for {
    array <- Gen.listOf(sprayObjGen)
  } yield { spray.json.JsArray(array.toVector) }

  val forSGen: Gen[JValue] = for (
    string <- Gen.alphaStr;
    int <- Arbitrary.arbInt.arbitrary;
    dec <- Arbitrary.arbBigDecimal.arbitrary;
    obj <- forSObjGen;
    array <- forSArrayGen;
    value <- Gen.oneOf(JString(string), JInt(int), JDecimal(dec), JBool(Random.nextBoolean()), obj, array)
  ) yield { value }

  val forSObjGen = for {
    value <- Gen.alphaStr
  } yield { JObject(JField("name", JString(value))) }

  val forSArrayGen = for (
    array <- Gen.listOf(forSObjGen)
  ) yield { JArray(array) }
}
