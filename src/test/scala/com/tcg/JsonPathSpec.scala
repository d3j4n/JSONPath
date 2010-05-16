package com.tcg

import com.tcg.JsonPath._
import org.codehaus.jackson.map.ObjectMapper
import org.scalatest.{Spec, FunSuite}

class JsonPathSpec extends Spec {
  describe("A JsonPath") {
    it("can be empty") {
      assert(compile("").isRight)
    }
    it("can not be constructed from null") {
      assert(compile(null).isLeft)
    }
    it("consists of PROPERTIES, INDICES, and GAPS") {
      assert(compile("/a/b/c").isRight)
      assert(compile("[1][2]").isRight)
      assert(compile("[1]//[2]/a/b//c[453]").isRight)
    }
    it("must start with a leading '/' or '['") {
      assert(compile("abcd").isLeft)
      assert(compile("  abcd").isLeft)
      assert(compile("/abcd").isRight)
      assert(compile("[0]").isRight)
      assert(compile("//zewruzr/c").isRight)
    }
    it("can not have a trailing '/'") {
      assert(compile("/").isLeft)
      assert(compile("//").isLeft)
      assert(compile("///").isLeft)
      assert(compile("////").isLeft)
      assert(compile("/a/").isLeft)
    }
    it("can not use negative indices") {
      assert(compile("[-23]").isLeft)
      assert(compile("[23]").isRight)
    }
    it("must not have leading spaces") {
      assert(compile("      ").isLeft)
      assert(compile("  /xysdf    ").isLeft)
      assert(compile("  /xysdf").isLeft)
    }
    it("can have trailing spaces when ending with a PROPERTY") {
      assert(compile("/xysdf   ").isRight)
      assert(compile("/xysdf[1]   ").isLeft)
    }
    it("can have interspersed spaces iff between PROPERTIES") {
      assert(compile("/x  /y /z  ").isRight)
      assert(compile("/x [1] /y /z  ").isLeft)
      assert(compile("/x[1]/y /z  ").isRight)
    }
    it("collapses multiple GAPS into one") {
      assert(compile("/a/////b") == compile("/a//b"))
      assert(compile("/a//////b") == compile("/a//b"))
    }
  }
}

class MatchSuite extends FunSuite {

  val mapper = new ObjectMapper()
  val js1 = mapper.readTree("{}")
  val js2 = mapper.readTree("[]")
  val js3 = mapper.readTree("[1,2,3]")
  val js4 = mapper.readTree("42")
  val js5 = mapper.readTree("""{"a": [1,2,{"b": {"c": 0, "d": 1}},3]}""")
  val js6 = mapper.readTree("""[1,2,[3,4,[5,6,7,[8,9]]]]""")
  val js7 = mapper.readTree("""[1,2,[3,4,[5,6,7,[8,9,{"x": 42}]]]]""")

  test("Matching with empty path") {
    val p = compile("").right.get
    assert(lookup(p, js1).get.toString == "{}")
    assert(lookup(p, js2).get.toString == "[]")
    assert(lookup(p, js4).get.toString == "42")
  }

  test("Matching with Gaps") {
    val p1 = compile("//d").right.get
    assert(lookup(p1, js5).get.toString == "1")
    val p2 = compile("/a//c").right.get
    assert(lookup(p2, js5).get.toString == "0")
    val p3 = compile("/a//////c").right.get
    assert(lookup(p3, js5).get.toString == "0")
  }

  test("Matching with indices") {
    val p1 = compile("[2][2][3][1]").right.get
    assert(lookup(p1, js6).get.toString == "9")
    val p2 = compile("[2][2][3][2]/x").right.get
    assert(lookup(p2, js7).get.toString == "42")
  }
}
