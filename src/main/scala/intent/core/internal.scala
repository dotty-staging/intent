package intent.core

import scala.concurrent.duration._
import scala.concurrent.{Future, ExecutionContext}
import scala.util.control.NonFatal
import scala.util.{Try, Success, Failure}
import scala.reflect.ClassTag

import intent.macros.Position
import intent.core.PositionDescription
import intent.util.DelayedFuture

/**
  * Thrown to abort a long-running [[Future]] in `whenComplete`.
  */
private class TestTimeoutException extends RuntimeException

private class ShouldNotHappenException(msg: String) extends RuntimeException(msg)

trait TestSupport extends FormatterGivens with EqGivens with ExpectGivens

trait IntentStructure:
  private[intent] def allTestCases: Seq[ITestCase]

  /**
   * A focused intent contains at least one focused test.
   */
  private[intent] def isFocused: Boolean

case class IgnoredTestCase(nameParts: Seq[String]) extends ITestCase:
  def run(): Future[TestCaseResult] =
    Future.successful(TestCaseResult(Duration.Zero, nameParts, TestIgnored()))

// TODO: Should not be in intent.core, since the user can override. But that applies
// TODO: to Eq/Formatting as well... we need a good strategy here!
case class TestTimeout(timeout: FiniteDuration)

trait TestLanguage:
  def expect[T](expr: => T)(using pos: Position): Expect[T] = new Expect[T](expr, pos)

  def whenComplete[T](expr: => Future[T])(impl: T => Expectation)(using pos: Position, timeout: TestTimeout): Expectation =
    import PositionDescription._
    new Expectation:
      def evaluate(): Future[ExpectationResult] =
        val timeoutFuture = DelayedFuture(timeout.timeout):
          throw TestTimeoutException()
        Future.firstCompletedOf(Seq(expr, timeoutFuture)).transformWith:
          case Success(r) => impl(r).evaluate()
          case Failure(t: TestTimeoutException) => Future.successful(
            TestFailed(pos.contextualize("Test timed out"), None))
          case Failure(t) => Future.successful(
            TestFailed(pos.contextualize("The Future passed to 'whenComplete' failed"), Some(t)))

  def fail(reason: String)(using pos: Position): Expectation =
    import PositionDescription._
    new { def evaluate() = Future.successful(TestFailed(pos.contextualize(reason), None)) }

  def success()(using pos: Position): Expectation =
    import PositionDescription._
    new { def evaluate() = Future.successful(TestPassed()) }

  // TODO: Can this be overridden? Or do we need a protected def
  given executionContext as ExecutionContext = ExecutionContext.global

  given defaultTestTimeout as TestTimeout = TestTimeout(5.seconds)

/**
  * Base structure for Intent test cases, whether they are async, sync, stateful or stateless.
  * The structure is based on asynchronous execution, since it's possible to fit sync in async,
  * but not vice versa.
  */
trait IntentStateBase[TState] extends IntentStructure with TestLanguage:
  type Map = TState => TState
  type FlatMap = TState => Future[TState]

  private[intent] def isStateful: Boolean

  private[intent] sealed trait Context:
    def name: String
    def transform(f: Future[Option[TState]]): Future[Option[TState]]
    def position: Position
    def expand(): Iterable[Context] = Seq(this)
    /** A focused context will cause any descendant non-ignored tests to gain focus. */
    def hasFocus: Boolean
    /** An ignored context will case all descendant tests to be ignored (regardless of having focus) */
    def isIgnored: Boolean
    /** Returns a focused context */
    def withFocus(): Context
    /** Returns a ignored and unfocused context */
    def withIgnore(): Context

  private[intent] sealed case class ContextInit(name: String, init: () => Future[TState], position: Position,
    hasFocus: Boolean = false, isIgnored: Boolean = false)
  extends Context:

    def transform(f: Future[Option[TState]]) = init().map(Some.apply)
    def withFocus() = copy(hasFocus = true)
    def withIgnore() = copy(isIgnored = true, hasFocus = false)

  private[intent] sealed case class ContextMap(name: String, tx: Map, position: Position,
    hasFocus: Boolean = false, isIgnored: Boolean = false)
  extends Context:

    def transform(f: Future[Option[TState]]) = f.map(_.map(tx))
    def withFocus() = copy(hasFocus = true)
    def withIgnore() = copy(isIgnored = true, hasFocus = false)

  private[intent] sealed case class ContextFlatMap(name: String, tx: FlatMap, position: Position,
    hasFocus: Boolean = false, isIgnored: Boolean = false)
  extends Context:

    def transform(f: Future[Option[TState]]) =
      f.flatMap:
        case Some(state) => tx(state).map(Some.apply)
        case None        => throw ShouldNotHappenException("Unexpected state None after Future transform")

    def withFocus() = copy(hasFocus = true)
    def withIgnore() = copy(isIgnored = true, hasFocus = false)

  case class TestCase(contexts: Seq[Context], name: String, impl: TState => Expectation, tcPosition: Position) extends ITestCase:
    def nameParts: Seq[String] = contexts.map(_.name) :+ name
    def run(): Future[TestCaseResult] =
      import PositionDescription._

      val before = System.nanoTime
      def result(msg: String, ex: Option[Throwable], pos: Position, er: (String, Option[Throwable]) => ExpectationResult): TestCaseResult =
        val elapsed = (System.nanoTime - before).nanos
        val result = er(pos.contextualize(msg), ex)
        TestCaseResult(elapsed, nameParts, result)

      def error(msg: String, ex: Option[Throwable], pos: Position): TestCaseResult =
        result(msg, ex, pos, TestError.apply)

      def failure(msg: String, ex: Option[Throwable], pos: Position): TestCaseResult =
        result(msg, ex, pos, TestFailed.apply)

      if isStateful && contexts.size == 0 then
        return Future.successful(error("Top-level test cases are not allowed in a state-based test suite", None, tcPosition))

      val postSetup = contexts.foldLeft(Future.successful[Either[TestCaseResult, Option[TState]]](Right(None)))((fut, ctx) => fut.flatMap {
        case l@Left(_) => Future.successful(l)
        case Right(stateOpt) =>
          try
            ctx.transform(Future.successful(stateOpt)).transform:
              case Success(newStateOpt) => Success(Right(newStateOpt))
              case Failure(t: ShouldNotHappenException) =>
                Success(Left(error(s"""${t.getMessage} for context \"${ctx.name}\"""", None, ctx.position)))
              case Failure(t) =>
                Success(Left(failure(s"""The state setup for context \"${ctx.name}\" failed""", Some(t), ctx.position)))
          catch
            case NonFatal(t) =>
              // Should not happen since we control our Context classes
              Future.successful(Left(error(s"""The transformation for context \"${ctx.name}\" failed""", Some(t), ctx.position)))
      })

      postSetup.flatMap:
        case Left(r) => Future.successful(r)
        case Right(opt) =>
          try
            // The getOrElse+asInstanceOf part is ugly, but it is only required in the
            // stateless case, in which case TState == Unit.
            val expectation = impl(opt.getOrElse(()).asInstanceOf[TState])
            expectation.evaluate().map { result =>
              val elapsed = (System.nanoTime - before).nanos
              TestCaseResult(elapsed, nameParts, result)
            }
          catch
            case NonFatal(t) =>
              Future.successful(failure("Test error", Some(t), tcPosition))

  private[intent] override def allTestCases: Seq[ITestCase] = testCases
  private[intent] override def isFocused: Boolean = inFocusedMode

  private[intent] def withContext(ctx: Context)(block: => Unit): Unit =
    ctx.expand().foreach:
      subContext =>
        reverseContextStack +:= subContext
        try block finally reverseContextStack = reverseContextStack.tail

  private[intent] def contextsInOrder: Seq[Context] = reverseContextStack.reverse
  private[intent] def parentContext: Option[Context] = reverseContextStack.headOption
  private[intent] def addTestCase(tc: ITestCase): Unit = testCases :+= tc
  private[intent] def rewriteTestCases(mapper: ITestCase => ITestCase): Unit =
    testCases = testCases.map(mapper)

  private var testCases: Seq[ITestCase] = Seq.empty
  private var reverseContextStack: Seq[Context] = Seq.empty

  /**
   * If there is a parent context, and that is set to ignore tests then don't allow children to be
   * focused (i.e. ignore > focus)
   */
  protected[intent] def doesParentAllowFocus(): Boolean = !isParentIgnored()

  protected[intent] def isParentFocused(): Boolean = parentContext match
    case Some(ctx) => ctx.hasFocus
    case None => false

  protected[intent] def isParentIgnored(): Boolean = parentContext match
    case Some(ctx) => ctx.isIgnored
    case None => false

  /** True if the test suite contains at least one focused test */
  protected var inFocusedMode: Boolean = false

  private[intent] def enableFocusedMode(): Unit =
    // If this is the first focused test, any existing testCase was not focused,
    // and hence should be converted to ignored to ignored tests (without execution)
    if !inFocusedMode then
      inFocusedMode = true
      rewriteTestCases(existing => IgnoredTestCase(existing.nameParts))

/**
  * Provides the Intent stateful test syntax, i.e. where contexts arrange state and
  * test cases act and assert on the state.
  */
trait IntentStateSyntax[TState] extends IntentStateBase[TState]:
  private[intent] override def isStateful = true

  extension (context: String)
    def using (init: => TState)(using pos: Position) : Context =
      ContextInit(context, () => Future.successful(init), pos)

    def using (tx: Map)(using pos: Position) : Context =
      ContextMap(context, tx, pos)

  extension (ctx: Context)
    def to (block: => Unit): Unit =
      val ctxToUse = isParentIgnored() match
        case true => ctx.withIgnore()
        case false => isParentFocused() match
          case true => ctx.withFocus()
          case _ => ctx
      withContext(ctxToUse)(block)

    def focused (block: => Unit): Unit =
      val ctxToUse = doesParentAllowFocus() match
        case true =>
          enableFocusedMode()
          ctx.withFocus()
        case false =>
          ctx.withIgnore()
      withContext(ctxToUse)(block)

    def ignored (block: => Unit): Unit =
      withContext(ctx.withIgnore())(block)
  end extension

  extension (testName: String)
    def in (testImpl: TState => Expectation)(using pos: Position): Unit =
      if inFocusedMode && !isParentFocused() || isParentIgnored() then
        testName ignore testImpl
      else
        addTestCase(TestCase(contextsInOrder, testName, testImpl, pos))

    def ignore (testImpl: TState => Expectation): Unit =
      addTestCase(IgnoredTestCase(contextsInOrder.map(_.name) :+ testName))

    def focus (testImpl: TState => Expectation)(using pos: Position): Unit =
      doesParentAllowFocus() match
        case true =>
          enableFocusedMode()
          addTestCase(TestCase(contextsInOrder, testName, testImpl, pos))
        case false =>
          testName ignore testImpl
  end extension

  private case class TableDriveContext(name: String, generator: () => Iterable[TState], position: Position,
    hasFocus: Boolean = false, isIgnored: Boolean = false)
  extends Context:

    def transform(f: Future[Option[TState]]): Future[Option[TState]] =
      Future.failed(new ShouldNotHappenException("TableDriveContext should not be used directly"))

    override def expand(): Iterable[Context] =
      generator().map(s => ContextInit(s"$name: $s", () => Future.successful(s), position, hasFocus = hasFocus, isIgnored = isIgnored))

    def withFocus() = copy(hasFocus = true)
    def withIgnore() = copy(isIgnored = true, hasFocus = false)

  // TODO: Move to separate trait?
  // TODO: Only works on root level currently...
  extension (context: String) def usingTable (generator: => Iterable[TState])(using pos: Position): Context =
    TableDriveContext(context, () => generator, pos)

/**
  * Provides the Intent stateless test syntax, i.e. where contexts are merely structural
  * and test cases are entirely responsible for arrange-act-assert.
  */
trait IntentStatelessSyntax extends IntentStateBase[Unit]:

  private[intent] override def isStateful = false

  extension (testName: String)
    def in (testImpl: => Expectation)(using pos: Position): Unit =
      if inFocusedMode && !isParentFocused() || isParentIgnored() then
        testName ignore testImpl
      else
        addTestCase(TestCase(contextsInOrder, testName, _ => testImpl, pos))

    def focus (testImpl: => Expectation)(using pos: Position): Unit =
      if isParentIgnored() then
        testName ignore testImpl
      else
        enableFocusedMode()
        addTestCase(TestCase(contextsInOrder, testName, _ => testImpl, pos))

    def ignore (testImpl: => Expectation): Unit =
      addTestCase(IgnoredTestCase(contextsInOrder.map(_.name) :+ testName))
  end extension

  extension (blockName: String)
    def focused (block: => Unit)(using pos: Position): Unit =
      val ctx = doesParentAllowFocus() match
        case true =>
          enableFocusedMode()
          ContextInit(blockName, () => Future.successful(()), pos , hasFocus = true)
        case false => ContextInit(blockName, () => Future.successful(()), pos , isIgnored = true)
      withContext(ctx)(block)

    def ignored (block: => Unit)(using pos: Position): Unit =
      val ctx = ContextInit(blockName, () => Future.successful(()), pos , hasFocus = false, isIgnored = true)
      withContext(ctx)(block)

    def apply (block: => Unit)(using pos: Position): Unit =
      val ctx = ContextInit(blockName, () => Future.successful(()), pos, hasFocus = isParentFocused(), isIgnored = isParentIgnored())
      withContext(ctx)(block)
  end extension

trait IntentAsyncStateSyntax[TState] extends IntentStateBase[TState]:

  private[intent] override def isStateful = true

  extension (context: String)
    def using (init: => TState)(using pos: Position): Context = ContextInit(context, () => Future.successful(init), pos)
    def usingAsync (init: => Future[TState])(using pos: Position): Context = ContextInit(context, () => init, pos)
    def using (tx: Map)(using pos: Position): Context = ContextMap(context, tx, pos)
    def usingAsync (fmc: FlatMap)(using pos: Position): Context = ContextFlatMap(context, fmc, pos)
  end extension

  extension (ctx: Context)
    def to (block: => Unit): Unit =
      val ctxToUse = isParentIgnored() match
        case true => ctx.withIgnore()
        case false => isParentFocused() match
          case true => ctx.withFocus()
          case _ => ctx
      withContext(ctxToUse)(block)

    def focused (block: => Unit): Unit =
      withContext(ctx.withFocus())(block)

    def ignored (block: => Unit): Unit =
      withContext(ctx.withIgnore())(block)
  end extension

  extension (testName: String)
    def in (testImpl: TState => Expectation)(using pos: Position): Unit =
      if inFocusedMode && !isParentFocused() || isParentIgnored() then
        testName ignore testImpl
      else
        addTestCase(TestCase(contextsInOrder, testName, testImpl, pos))

    def ignore (testImpl: TState => Expectation): Unit =
        addTestCase(IgnoredTestCase(contextsInOrder.map(_.name) :+ testName))

    def focus (testImpl: TState => Expectation)(using pos: Position): Unit =
      if isParentIgnored() then
        testName ignore testImpl
      else
        enableFocusedMode()
        addTestCase(TestCase(contextsInOrder, testName, testImpl, pos))
  end extension
