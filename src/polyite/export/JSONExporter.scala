package polyite.export

object JSONExporter {
  /**
    * Generates JSON-code from the given abstract syntax tree. An object is
    * represented by a Map[String, Any]. A List by a List[Any]. An Object of any
    * other kind is converted to a String-literal by calling toString.
    * @param absSyn	the abstract syntax tree
    * @param indentWidth	the number of spaces used for indenting by one level
    * @return JSON code
    */
  def generate(absSyn : Any, indentWidth : Int) : String = {
    val sb : StringBuilder = new StringBuilder()
    generate(absSyn, 0, indentWidth, sb)
    return sb.toString()
  }

  private def generate(absSyn : Any, level : Int, indentWidth : Int, sb : StringBuilder) {
    if (absSyn.isInstanceOf[Map[_, Any]]) {
      generateObject(absSyn.asInstanceOf[Map[String, Any]], level, indentWidth, sb)
    } else if (absSyn.isInstanceOf[List[Any]]) {
      generateList(absSyn.asInstanceOf[List[Any]], level, indentWidth, sb)
    } else {
      generateValue(absSyn, sb)
    }
  }

  private def generateValue(v : Any, sb : StringBuilder) {
    sb.append('\"')
    sb.append(v.toString())
    sb.append('\"')
  }

  private def generateList(l : List[Any], level : Int, indentWidth : Int, sb : StringBuilder) {
    sb.append('[')
    sb.append('\n')
    val childIter : Iterator[Any] = l.iterator
    while (childIter.hasNext) {
      appendSpaces(level + 1, indentWidth, sb)
      generate(childIter.next(), level + 1, indentWidth, sb)
      if (childIter.hasNext)
        sb.append(',')
      sb.append('\n')
    }
    appendSpaces(level, indentWidth, sb)
    sb.append(']')
  }

  private def generateObject(o : Map[String, Any], level : Int, indentWidth : Int, sb : StringBuilder) {
    sb.append('{')
    sb.append('\n')
    val keyIter : Iterator[String] = o.keySet.iterator
    while (keyIter.hasNext) {
      val key : String = keyIter.next()
      appendSpaces(level + 1, indentWidth, sb)
      sb.append('\"')
      sb.append(key)
      sb.append("\" : ")
      generate(o(key), level + 1, indentWidth, sb)
      if (keyIter.hasNext)
        sb.append(',')
      sb.append('\n')
    }
    appendSpaces(level, indentWidth, sb)
    sb.append('}')
  }

  private def appendSpaces(level : Int, indentWidth : Int, sb : StringBuilder) {
    for (_ <- 0 until level)
      for (_ <- 0 until indentWidth)
        sb.append(' ')
  }

  def main(args : Array[String]) : Unit = {
    println(generate(List(), 4))
    println("----")
    println(generate(List(List(Map(("name", "Ganser"), ("firstName", "Stefan")),
      Map(("name", "Wurst"), ("firstName", "Hans"))), List(Map(("name", "Maier"), ("firstName", "Julia")),
      Map(("name", "Anna"), ("firstName", "Mueller")))), 4))
    println("----")
    println(generate(Map(("foo", Map(("baz", List(Map(("x", "y"), ("a", "b")))))), ("bar", List(Map(("x", "y"), ("a", "b")))), ("bar", Map(("baz", List(Map(("x", "y"), ("a", "b")))))), ("bar", List(Map(("x", "y"), ("a", "b"))))), 4))
  }
}