object dynamic {
  import scala.language.dynamics

  object Apply extends Dynamic {
    def applyDynamic(methodName: String)(args: Any*) {
      println(s""" Apply Dynamic
                 | - method name: $methodName
                 | - args:        ${args.mkString(",")}
               """.stripMargin)
    }

    def applyDynamicNamed(name: String)(args: (String, Any)*) {
      println(s""" Apply Dynamic
                 | - method name: $name
                 | - for:         $args
               """.stripMargin)
    }
  }

  class Json(init: String) extends Dynamic {
    var bar = init
    def selectDynamic(name: String): Option[String] = {
      if (name == "bar" ) Some(bar) else None
    }

    def updateDynamic(name: String)(value: Any) {
      if (name == "bar") bar = value.toString
    }
  }
}

object Main {
  import dynamic._
  def run() = {

    // apply dynamic
    // i.e. applying a method, passing it arguments
    Apply.foo("bar", "baz", 42)

    // apply dynamic named
    // i.e. applying a method, passing it named arguments
    Apply.foo(bar = "baz", qed = 42)


    val json = new Json("foo")

    // select dynamic
    // i.e. referencing a field
    val bar0 = json.bar
    println(s""" Select Dynamic
               | - field name:  bar
               | - contains:    $bar0
             """.stripMargin)

    // update dynamic
    // i.e. updating a field
    json.bar = "baz"
    val bar1 = json.bar
    println(s""" Update Dynamic
               | - field name:  bar
               | - contains:    $bar1
             """.stripMargin)

  }
}
