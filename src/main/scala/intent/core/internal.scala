package intent.core

import scala.concurrent.duration._
import scala.concurrent.{Future, ExecutionContext}
import scala.util.control.NonFatal
import scala.util.{Try, Success, Failure}
import scala.reflect.ClassTag

import intent.macros.Position

trait TestSupport extends FormatterGivens with EqGivens with ExpectGivens

trait IntentStructure:
  private[intent] def allTestCases: Seq[ITestCase]

case class IgnoredTestCase(nameParts: Seq[String]) extends ITestCase:
  def run(): Future[TestCaseResult] =
    Future.successful(TestCaseResult(Duration.Zero, nameParts, TestIgnored()))

trait TestLanguage:
  def expect[T](expr: => T) given (pos: Position): Expect[T] = new Expect[T](expr, pos)

  def whenComplete[T](expr: => Future[T])(impl: T => Expectation) given (pos: Position): Expectation =
    import PositionDescription._
    new Expectation:
      def evaluate(): Future[ExpectationResult] =
        expr.transformWith:
          case Success(r) => impl(r).evaluate()
          case Failure(t) => Future.successful(
            TestFailed(pos.contextualize("The Future passed to 'whenComplete' failed")))

  def fail(reason: String) given (pos: Position) =
    import PositionDescription._
    new Expectation:
      def evaluate() = Future.successful(TestFailed(pos.contextualize(reason)))

  def success() given (pos: Position) =
    import PositionDescription._
    new Expectation:
      def evaluate() = Future.successful(TestPassed())

  // TODO: Can this be overridden? Or do we need a protected def
  given executionContext as ExecutionContext = ExecutionContext.global

trait IntentStateSyntax[TState] extends IntentStructure with TestLanguage:
  type Transform = TState => TState

  trait Context:
    def name: String
    def transform(opt: Option[TState]): Option[TState]
  case class ContextInit(name: String, init: () => TState) extends Context:
    def transform(opt: Option[TState]) = Some(init())
  case class ContextTx(name: String, tx: Transform) extends Context:
    def transform(opt: Option[TState]) = opt.map(tx)

  case class TestCase(contexts: Seq[Context], name: String, impl: TState => Expectation) extends ITestCase:
    def nameParts: Seq[String] = contexts.map(_.name) :+ name
    // TODO: Handle setup failure, i.e. that this Future fails
    def run(): Future[TestCaseResult] =
      val before = System.nanoTime
      def errorResult(t: Throwable): TestCaseResult =
        val elapsed = (System.nanoTime - before).nanos
        val result = TestError(t)
        TestCaseResult(elapsed, nameParts, result)

      // TODO: Create IntentException, use instead of standard exceptions
      if contexts.size == 0 then
        return Future.successful(errorResult(new IllegalStateException("Zero contexts is not allowed")))

      contexts.foldLeft[Option[TState]](None)((st, ctx) => ctx.transform(st)) match
        case Some(state) =>
          try
            val expectation = impl(state)
            expectation.evaluate().map { result =>
              val elapsed = (System.nanoTime - before).nanos
              TestCaseResult(elapsed, nameParts, result)
            }
          catch
            case NonFatal(t) =>
              Future.successful(errorResult(t))
        case None =>
          Future.successful(errorResult(new RuntimeException("State folding didn't produce a state")))

  private[intent] override def allTestCases: Seq[ITestCase] = testCases
  private var testCases: Seq[ITestCase] = Seq.empty
  private var reverseContextStack: Seq[Context] = Seq.empty

  def (context: String) using (init: => TState): Context = ContextInit(context, () => init)
  def (context: String) using (tx: Transform): Context = ContextTx(context, tx)

  def (ctx: Context) to (block: => Unit): Unit =
    reverseContextStack +:= ctx
    try block finally reverseContextStack = reverseContextStack.tail

  def (testName: String) in (testImpl: TState => Expectation): Unit =
    val contexts = reverseContextStack.reverse
    testCases :+= TestCase(contexts, testName, testImpl)

  def (testName: String) ignore (testImpl: TState => Expectation): Unit =
    val contexts = reverseContextStack.reverse
    testCases :+= IgnoredTestCase(contexts.map(_.name) :+ testName)

trait IntentStatelessSyntax extends IntentStructure with TestLanguage:
  case class SetupPart(name: String)
  case class Block(blk: () => Unit)
  case class TestCase(setup: Seq[SetupPart], name: String, impl: () => Expectation) extends ITestCase:
    def nameParts: Seq[String] = setup.map(_.name) :+ name
    def run(): Future[TestCaseResult] =
      val before = System.nanoTime
      try
        val expectation = impl()
        expectation.evaluate().map { result =>
          val elapsed = (System.nanoTime - before).nanos
          TestCaseResult(elapsed, nameParts, result)
        }
      catch
        case NonFatal(t) =>
          val elapsed = (System.nanoTime - before).nanos
          val result = TestError(t)
          Future.successful(TestCaseResult(elapsed, nameParts, result))

  private[intent] override def allTestCases: Seq[ITestCase] = testCases
  private var testCases: Seq[ITestCase] = Seq.empty
  private var reverseSetupStack: Seq[SetupPart] = Seq.empty

  def (testName: String) in (testImpl: => Expectation): Unit =
    val parts = reverseSetupStack.reverse
    testCases :+= TestCase(parts, testName, () => testImpl)

  def (blockName: String) apply (block: => Unit): Unit =
    val setupPart = SetupPart(blockName)
    reverseSetupStack +:= setupPart
    try block finally reverseSetupStack = reverseSetupStack.tail

  def (testName: String) ignore (testImpl: => Expectation): Unit =
    val parts = reverseSetupStack.reverse
    testCases :+= IgnoredTestCase(parts.map(_.name) :+ testName)

// TODO: Remove duplication wrt IntentStateSyntax
// TODO: It would be nice if we could just do 'extends IntentStateSyntax[Future[TState]]',
// TODO: but futureState.flatMap in run below is special here.
trait IntentAsyncStateSyntax[TState] extends IntentStructure with TestLanguage:
  type Map = TState => TState
  type FlatMap = TState => Future[TState]

  trait Context:
    def name: String
    def transform(f: Future[Option[TState]]): Future[Option[TState]]
  case class ContextInit(name: String, init: () => Future[TState]) extends Context:
    def transform(f: Future[Option[TState]]) = init().map(Some.apply)
  case class ContextMap(name: String, tx: Map) extends Context:
    def transform(f: Future[Option[TState]]) = f.map(_.map(tx))
  case class ContextFlatMap(name: String, tx: FlatMap) extends Context:
    def transform(f: Future[Option[TState]]) =
      f.flatMap:
        case Some(state) => tx(state).map(Some.apply)
        case None        => throw IllegalStateException("Unexpected state None") 

  case class TestCase(contexts: Seq[Context], name: String, impl: TState => Expectation) extends ITestCase:
    def nameParts: Seq[String] = contexts.map(_.name) :+ name
    // TODO: Handle setup failure, i.e. that this Future fails
    def run(): Future[TestCaseResult] =
      val before = System.nanoTime
      def errorResult(t: Throwable): TestCaseResult =
        val elapsed = (System.nanoTime - before).nanos
        val result = TestError(t)
        TestCaseResult(elapsed, nameParts, result)

      if contexts.size == 0 then
        return Future.successful(errorResult(new IllegalStateException("Zero contexts is not allowed")))

      val futureState = contexts.foldLeft(Future.successful[Option[TState]](None))((st, part) => part.transform(st))
      futureState.flatMap:
        case Some(state) =>
          try
            val expectation = impl(state)
            expectation.evaluate().map { result =>
              val elapsed = (System.nanoTime - before).nanos
              TestCaseResult(elapsed, nameParts, result)
            }
          catch
            case NonFatal(t) =>
              Future.successful(errorResult(t))
        case None =>
          Future.successful(errorResult(new RuntimeException("Async state folding didn't produce a state")))

  private[intent] override def allTestCases: Seq[ITestCase] = testCases
  private var testCases: Seq[ITestCase] = Seq.empty
  private var reverseContextStack: Seq[Context] = Seq.empty

  def (context: String) using (init: => TState): Context = ContextInit(context, () => Future.successful(init))
  def (context: String) usingAsync (init: => Future[TState]): Context = ContextInit(context, () => init)
  def (context: String) using (tx: Map): Context = ContextMap(context, tx)
  def (context: String) usingAsync (fmc: FlatMap): Context = ContextFlatMap(context, fmc)

  def (ctx: Context) to (block: => Unit): Unit =
    reverseContextStack +:= ctx
    try block finally reverseContextStack = reverseContextStack.tail

  def (testName: String) in (testImpl: TState => Expectation): Unit =
    val contexts = reverseContextStack.reverse
    testCases :+= TestCase(contexts, testName, testImpl)

  def (testName: String) ignore (testImpl: TState => Expectation): Unit =
      val contexts = reverseContextStack.reverse
      testCases :+= IgnoredTestCase(contexts.map(_.name) :+ testName)
