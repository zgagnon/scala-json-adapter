package com.zgagnon.jsonAdapter

import com.zgagnon.jsonAdapter.CssSelectors._
import org.specs2.specification.Fragments
import org.specs2.{ ScalaCheck, Specification }

/**
 * Created by Zoe on 11/2/2014.
 */
class CSSSelectorSpec extends Specification with ScalaCheck {
  import CSSJson._
  override def is: Fragments =
    s2"""
        The json css selectors provide a pattern matching style similar to CSS Selectors.

        An arbitrary type of element (String, Number, Boolean, Object, or Array) may be matched:
    ${
      CSSJson("name" -> "test", "name" -> "test1", "name" -> 4) must beLike {
        case AnyString(fields) => fields must containTheSameElementsAs(List[JField](JField("name", "test"), JField("name", "test1")))
      }
    }
    ${
      CSSJson("name" -> "test", "value" -> 4, "phone" -> 8) must beLike {
        case AnyNumber(fields) => fields must containTheSameElementsAs(List[JField](JField("value", 4), JField("phone", 8)))
      }
    }
    ${
      CSSJson("name" -> "Hi", "cash" -> 0, "atHome" -> false) must beLike {
        case AnyBoolean(fields) => fields must containTheSameElementsAs(List(JField("atHome", false)))
      }
    }
    ${
      CSSJson("list" -> JArray("Hi", 0, false)) must beLike {
        case AnyArray(fields) => fields must containTheSameElementsAs(List(JField("list", JArray("Hi", 0, false))))
      }
    }
    ${
      CSSJson("inception" -> CSSJson("inception" -> "go deeper")) must beLike {
        case AnyObject(fields) => fields must containTheSameElementsAs(List(JField("inception", CSSJson("inception" -> "go deeper"))))
      }
    }

    It can also be used to match against arrays:
    ${
      JArray("Test", 0, false) must beLike { case AnyNumber(fields) => fields must containTheSameElementsAs(List(JField("1", 0))) }
    }

     In that case, the fields returned will have the string output of their index as the name """
}
