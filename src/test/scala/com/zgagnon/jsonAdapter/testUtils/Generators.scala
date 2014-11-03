package com.zgagnon.jsonAdapter.testUtils

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
}
