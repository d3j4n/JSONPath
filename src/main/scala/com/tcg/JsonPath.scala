package com.tcg

import scalaz._
import Scalaz._
import org.codehaus.jackson.JsonNode
import org.codehaus.jackson.node.{ArrayNode, ObjectNode}

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
        Index    ::= '[' Integer ']'
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
            case (Property(p)::rest, js: ObjectNode) => opt(js.get(p)) >>= (v => go(rest, v))
            case (Index(i)::rest, js: ArrayNode)     => opt(js.get(i)) >>= (v => go(rest, v))
            case _                                   => None
        }
        go(p.value, root)
    }

    private def tokenize(s: Seq[Char], p: List[Element]): Validation[String, List[Element]] = {
        val checkProp: String => Validation[String, Element] = {
            case "" => Success(Gap)
            case pr => Success(Property(pr))
        }
        def checkIdx(s: String): Validation[String, Element] = {
            try {
                Success(Index(s.toInt))
            } catch {
                case e: Exception => Failure(e.getMessage())
            }
        }
        def tailSafe[A](s: Seq[A]): Seq[A] = s match {
            case Nil => Nil
            case seq => seq.tail
        }
        s match {
            case Seq()               => Success(collapseGaps(p, Nil))
            case Seq('/', rest @ _*) => token("/[", rest.toString, checkProp) >>= {
                                            case (e, es) => tokenize(es, e::p)
                                        }
            case Seq('[', rest @ _*) => token("]", rest.toString, checkIdx) >>= {
                                            case (e, es) => tokenize(tailSafe(es), e::p)
                                        }
            case _                   => Failure("Invalid path '" + s + "'")
        }
    }

    private def token(part: String, whole: String, predicate: String => Validation[String, Element])
    : Validation[String, (Element, String)] = {
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

    private def opt[A](x: A): Option[A] = if (x == null) None else Some(x)
}


// vim: set ts=4 sw=4 et:
