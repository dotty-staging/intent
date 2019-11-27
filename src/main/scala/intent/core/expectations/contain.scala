package intent.core.expectations

import intent.core._
import intent.core.MapLike
import scala.concurrent.Future
import scala.collection.mutable.ListBuffer
import scala.Array

private def evalToContain[T](actual: IterableOnce[T],
                              expected: T,
                              expect: Expect[_],
                              listTypeName: String)
   (given
      eqq: Eq[T],
      fmt: Formatter[T],
      cutoff: ListCutoff
    ): Future[ExpectationResult] =

  def emptyIterator: Iterator[T] = Seq.empty[T].iterator
  def printContents(lb: ListBuffer[String]) =
    if actual == null then "" else lb.mkString("(", ", ", ")")

  val seen = ListBuffer[String]()
  var found = false
  val iterator = Option(actual).map(_.iterator).getOrElse(emptyIterator)
  val shouldNotFind = expect.isNegated
  var breakEarly = false
  var itemsChecked = 0
  while !breakEarly && iterator.hasNext do
    if itemsChecked >= cutoff.maxItems then
      breakEarly = true
      // This results in failure output like: List(X, ...)
      seen.takeInPlace(cutoff.printItems)
    else
      val next = iterator.next()
      seen += fmt.format(next)
      if !found && eqq.areEqual(next, expected) then
        found = true
        if shouldNotFind then
          breakEarly = true
      itemsChecked += 1

  val allGood = if expect.isNegated then !found else found

  val r = if !allGood then
    if iterator.hasNext then
      seen += "..."
    val actualStr = listTypeName + printContents(seen)
    val expectedStr = fmt.format(expected)

    val desc = if expect.isNegated then
      s"Expected $actualStr not to contain $expectedStr"
    else
      s"Expected $actualStr to contain $expectedStr"
    expect.fail(desc)
  else expect.pass
  Future.successful(r)

class IterableContainExpectation[T](expect: Expect[IterableOnce[T]], expected: T)(
  given
    eqq: Eq[T],
    fmt: Formatter[T],
    cutoff: ListCutoff
) extends Expectation with

  def evaluate(): Future[ExpectationResult] =
    val actual = expect.evaluate()
    evalToContain(actual, expected, expect, listTypeName(actual))

class ArrayContainExpectation[T](expect: Expect[Array[T]], expected: T)(
  given
    eqq: Eq[T],
    fmt: Formatter[T],
    cutoff: ListCutoff
) extends Expectation with

  def evaluate(): Future[ExpectationResult] =
    val actual = expect.evaluate()
    evalToContain(actual, expected, expect, "Array")

class MapContainExpectation[K, V](expect: Expect[MapLike[K, V]], expected: Seq[Tuple2[K, V]])(
  given
    eqq: Eq[V],
    keyFmt: Formatter[K],
    valueFmt: Formatter[V],
) extends Expectation with

  def evaluate(): Future[ExpectationResult] =
    val actual = expect.evaluate()
    var failingKeys = Seq[Tuple2[K, V]]()       // When the key failing (missing or present when negated)
    var failingValues = Seq[Tuple3[K, V, V]]()  // When the key is OK but the value does not match

    expected.foreach { expectedPair =>
      actual.get(expectedPair._1) match {
        case None if expect.isNegated =>
          () // OK = NOOP
        case None =>
          failingKeys :+= expectedPair
        case Some(v) if expect.isNegated =>
          failingKeys :+= expectedPair
        case Some(v)   =>
          if !eqq.areEqual(v, expectedPair._2) then failingValues :+= (expectedPair._1, expectedPair._2, v)
      }
    }

    Future.successful:
      if failingKeys.isEmpty && failingValues.isEmpty then
        expect.pass
      else
        var message = s"Expected ${describeActualWithContext(actual)} to "
        if (expect.isNegated) message += "not "
        message += "contain:\n  "
        message += failingKeys.map(p => s"${keyFmt.format(p._1)} -> ${valueFmt.format(p._2)}").mkString("\n  ")
        message += failingValues.map(p => s"${keyFmt.format(p._1)} -> ${valueFmt.format(p._2)} but found ${keyFmt.format(p._1)} -> ${valueFmt.format(p._3)}").mkString("\n  ")
        expect.fail(message)

  private def describeActualWithContext(actual: MapLike[K, V]): String = "Map(...)"
