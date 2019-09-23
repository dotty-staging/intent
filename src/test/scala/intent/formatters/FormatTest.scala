package intent.formatters

import intent._

class FormatTest extends TestSuite with Stateless:
  "Formatting" :
    "surrounds Char in single quotes" in expect(format('a')).toEqual("'a'")
    "surrounds String in double quotes" in expect(format("a")).toEqual("\"a\"")
    "supports Option-Some" in expect(format(Some(42))).toEqual("Some(42)")
    "supports Option-None" in expect(format[Option[String]](None)).toEqual("None")
    "supports recursive Option" in expect(format(Some("test"))).toEqual("Some(\"test\")")
    "supports Throwable" in :
      val t = RuntimeException("oops")
      expect(format(t)).toEqual("java.lang.RuntimeException: oops")

  def format[T](x: T) given (fmt: core.Formatter[T]): String =
    fmt.format(x)