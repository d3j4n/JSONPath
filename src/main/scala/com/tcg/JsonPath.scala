package com.tcg

import scalaz._
import Scalaz._
import scala.collection.mutable.Queue
import scala.collection.JavaConversions.asIterator
import org.codehaus.jackson.JsonNode
import org.codehaus.jackson.node.{ArrayNode, ObjectNode, ContainerNode}

// Representation of JSON path elements
sealed abstract class Element
case class Property(name: String) extends Element
case class Index(value: Int) extends Element
case object Gap extends Element

/**
  `JsonPath` provides XPath like access to JSON objects represented as
  Jackson `JsonNode`s.

  Like regular expressions, the path is first compiled and then matched
  against JSON objects.

  '''Path syntax'''

  {{{
    Path     ::= "" | Element* (Property | Index)
    Element  ::= Property | Index | Gap
    Property ::= '/' String
    Index    ::= '[' Natural Number ']'
    Gap      ::= '//'
  }}}

  '''Examples'''

  ""
  "/a"
  "/a/b/c"
  "[1]/a"
  "/a[2]"

*/
object JsonPath {

  case class Path(private[JsonPath] val value: List[Element])

  def compile(p: String): Either[String, Path] = (tokenize(p, Nil) >>= validate).map(Path(_)).either

  def lookup(p: Path, root: JsonNode): Option[JsonNode] = {
    def go: (List[Element], JsonNode) => Option[JsonNode] = {
      case (Nil, js)                           => Some(js)
      case (Property(p)::rest, js: ObjectNode) => Option(js.get(p))    >>= (v => go(rest, v))
      case (Index(i)::rest, js: ArrayNode)     => Option(js.get(i))    >>= (v => go(rest, v))
      case (Gap::next::rest, js)               => bfs(next, Queue(js)) >>= (v => go(rest, v))
      case _                                   => None
    }
    go(p.value, root)
  }

  private def tokenize(s: Seq[Char], p: List[Element]): Validation[String, List[Element]] = {
    val prop: String => Validation[String, Element] = {
      case "" => Success(Gap)
      case pr => Success(Property(pr))
    }
    def index(s: String): Validation[String, Element] = {
      try {
        val i = s.toInt
        if (i < 0)
          Failure("Negative indices not supported")
        else
          Success(Index(i))
      } catch {
        case e: Exception => Failure(e.getMessage())
      }
    }
    def tail[A](s: Seq[A]): Seq[A] = s match {
      case Nil => Nil
      case seq => seq.tail
    }
    s match {
      case Seq()            => Success(collapseGaps(p, Nil))
      case Seq('/', r @ _*) => token("/[", r.toString, prop) >>= {case (e, es) => tokenize(es, e::p)}
      case Seq('[', r @ _*) => token("]", r.toString, index) >>= {case (e, es) => tokenize(tail(es), e::p)}
      case _                => Failure("Invalid path '" + s + "'")
    }
  }

  private def token(part: String, whole: String, predicate: String => Validation[String, Element]):
  Validation[String, (Element, String)] = {
    val (prefix, suffix) = whole.span(c => !part.contains(c))
    predicate(prefix).map(e => (e, suffix))
  }

  private val collapseGaps: (List[Element], List[Element]) => List[Element] = {
    case (Nil, rs)          => rs
    case (Gap::Gap::es, rs) => collapseGaps(Gap::es, rs)
    case (e::es, rs)        => collapseGaps(es, e::rs)
  }

  private val validate: List[Element] => Validation[String, List[Element]] = {
    case Nil                => Success(Nil)
    case p if p.last == Gap => Failure("Trailing '/'")
    case p                  => Success(p)
  }

  private def bfs(e: Element, js: Queue[JsonNode]): Option[JsonNode] = {
    def continue(n: JsonNode): Option[JsonNode] = {
      for (x <- n.getElements()) js.enqueue(x)
      bfs(e, js)
    }
    if (js.isEmpty)
      None
    else (e, js.dequeue) match {
      case (Property(p), n: ObjectNode) =>
        if (n.getFieldNames().toList.contains(p))
          Some(n.get(p))
        else
          continue(n)
      case (Index(i), n: ArrayNode) =>
        if (i < n.size)
          Some(n.get(i))
        else
          continue(n)
      case (_, n: ContainerNode) => continue(n)
      case _                     => bfs(e, js)
    }
  }
}

