import play.api.libs.json._
import play.api.libs.json.JsBoolean
import play.api.libs.json.JsNumber
import play.api.libs.json.JsString
import scala.Some

/**
 * Created by karishma on 4/27/2015.
 */
case class Bookmarks {

  def transform(json: JsValue): JsValue = json match{
    case JsNumber(num) => JsNumber (num+1)
    case JsString(str) => JsString(transformString(str))
    case JsBoolean(bool) => JsBoolean(!bool)
    case JsNull => JsString("Run Away")
    case JsArray(jsons) => JsArray(jsons.map(transform))
    case JsObject(pairs) => JsObject(pairs.map(transformPair))
  }


  def transformPair(pair: (String, JsValue)): (String, JsValue) = pair match{
    case (key @ ("m"|"n"), json) => key -> json
    case (key, json) =>             key -> transform(json)
  }

  object :: {
    def unapply(str: String): Option[(String, String)] = str.split(":") match {
      case Array(str1,str2) => Some(str1,str2)
      case _ => None
    }
  }

  def transformString(str: String) = str match {
    case str1 :: str2 => str2
    case _ => str
  }

//  def inline(json: JsValue): JsValue = {
////    json.transform(Reads(json))
//
//
//  }

  def marshal(json: JsValue) : JsValue = {
    var bmit = Iterator(String)
    json match {
      case obj:JsObject => {
        obj.\("bookmarks").asOpt[List[String]] match {
          case Some(a) => bmit = a.iterator
        }
      }
      case (key: String, valueJ: JsValue) => {
        key -> transformBookmark(valueJ.toString(), bmit.next())
      }
        json
    }
  }

  def transformBookmark(str: String, bm: String) : String = {
    "\\[BM".r.replaceAllIn(str,"[BM"+"|"+bm)
  }
}
