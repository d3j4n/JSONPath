JSONPath
========

Provides a simple way to access JSON nodes with a XPath-like notation in Scala.

Usage
-----

1. Compile a JSONPath string with
   `compile: String => Either[String, Path]`
2. Match the compiled path against JSONs with
   `lookup: (Path, JsonNode) => Option[JsonNode]`

Grammar
-------

    Path     ::= "" | Element* (Property | Index)
    Element  ::= Property | Index | Gap
    Property ::= '/' String
    Index    ::= '[' Natural Number ']'
    Gap      ::= '//'

Example
-------

    import com.tcg.JsonPath._
    import org.codehaus.jackson.map.ObjectMapper

    object Example extends Application {
      val json  = new ObjectMapper().readTree("""{"a":[1,2,{"b":{"c":0,"d":1}},3]}""")
      val path1 = compile("/a[2]/b/c").right.get
      val path2 = compile("//c").right.get
      val x = lookup(path1, json).get.getIntValue
      val y = lookup(path2, json).get.getIntValue
      assert(x == 0)
      assert(y == 0)
    }

License
-------

MIT License (cf. src/main/resources/LICENSE for details)

