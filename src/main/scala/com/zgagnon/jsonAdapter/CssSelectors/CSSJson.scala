package com.zgagnon.jsonAdapter.CssSelectors

import scala.reflect.ClassTag

/**
 * Created by Zoe on 11/2/2014.
 */
trait SelectableJson
trait ParentJson {
  type self = SelectableJson
}

class CSSJson(var fields: Seq[JField]) extends SelectableJson with ParentJson {
  override def equals(other: Any): Boolean = {
    other match {
      case CSSJson(otherFields) if otherFields.size == fields.size => otherFields.intersect(fields).size == fields.size
      case _ => false
    }
  }

  override def hashCode(): Int = {
    (0 /: fields) { (base, next) => (base + next.hashCode() * 31) % Int.MaxValue }
  }
}

case class JField(label: String, value: SelectableJson)
case class JString(value: String) extends SelectableJson
case class JBool(value: Boolean) extends SelectableJson
case class JNumber(value: BigDecimal) extends SelectableJson
case class JArray(values: SelectableJson*) extends SelectableJson with ParentJson

object CSSJson {
  implicit def toJBool(bool: Boolean): JBool = new JBool(bool)
  implicit def toJString(string: String): JString = new JString(string)
  implicit def toJNumber(int: Int): JNumber = new JNumber(BigDecimal(int))
  implicit def toJNumber(double: Double): JNumber = new JNumber(BigDecimal(double))
  implicit def toJNumber(dec: BigDecimal): JNumber = new JNumber(dec)
  implicit def toField(tuple: (String, SelectableJson)): JField = JField(tuple._1, tuple._2)

  def apply(fields: (String, SelectableJson)*): CSSJson = new CSSJson(fields map { f => JField(f._1, f._2) })
  def unapply(json: CSSJson): Option[Seq[JField]] = Some(json.fields)
}

private[jsonAdapter] object Helpers {
  implicit class Optionable[A](seq: Seq[A]) {
    def asOption = if (seq.isEmpty) None else Some(seq)
  }
  def check[A <: SelectableJson](parent: ParentJson)(implicit tag: ClassTag[A]): Option[Seq[JField]] = {
    parent match {
      case CSSJson(fields) =>
        // It's silly to construct the same object again but the ClassTag makes this the easiest option.
        fields collect { case JField(name, value: A) => JField(name, value) } asOption
      case array: JArray => array.values.zipWithIndex collect { case (value: A, index) => JField(index.toString, value) } asOption
    }
  }
}

class AnyString
object AnyString {

  import com.zgagnon.jsonAdapter.CssSelectors.Helpers._

  def unapply(parent: ParentJson): Option[Seq[JField]] = {
    check[JString](parent)
  }
}

class AnyNumber
object AnyNumber {
  import com.zgagnon.jsonAdapter.CssSelectors.Helpers._
  def unapply(parent: ParentJson): Option[Seq[JField]] = {
    check[JNumber](parent)
  }
}

class AnyBoolean
object AnyBoolean {
  import com.zgagnon.jsonAdapter.CssSelectors.Helpers._
  def unapply(parent: ParentJson): Option[Seq[JField]] = {
    check[JBool](parent)
  }
}

class AnyArray
object AnyArray {
  import com.zgagnon.jsonAdapter.CssSelectors.Helpers._
  def unapply(parent: ParentJson): Option[Seq[JField]] = {
    check[JArray](parent)
  }
}

class AnyObject
object AnyObject {
  import com.zgagnon.jsonAdapter.CssSelectors.Helpers._
  def unapply(parent: ParentJson): Option[Seq[JField]] = {
    check[CSSJson](parent)
  }
}

