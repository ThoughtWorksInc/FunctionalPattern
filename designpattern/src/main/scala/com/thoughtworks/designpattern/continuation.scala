package com.thoughtworks.designpattern
import com.thoughtworks.designpattern.covariant._

import language.higherKinds
import language.implicitConversions
import scala.annotation.tailrec
import scala.util.{Failure, Success, Try}
import scala.util.control.NonFatal

/**
  * @author 杨博 (Yang Bo)
  */
object continuation {

  trait ContinuationFactory extends MonadFactory with TailCallFactory {
    type Facade[+A] <: Continuation[A]
    type Result

    trait Continuation[+A] extends Monad[A] {
      def listen(continue: A => Result): Result
    }

    trait FlatMap[+A] extends Continuation[A] {
      this: Facade[A] =>
      def flatMap[B](mapper: A => Value[B]): Value[B] = shift[B] { (continue: B => Result) =>
        listen { a: A =>
          mapper(a).listen(continue)
        }
      }
    }

    trait TailCall[+A] extends Continuation[A] {
      def tail(): Continuation[A]
      @tailrec final def listen(continue: (A) => Result): Result = {
        tail() match {
          case tailCall: TailCall[A] =>
            tailCall.listen(continue)
          case notTailCall =>
            notTailCall.listen(continue)
        }
      }
    }

    def shift[A](f: (A => Result) => Result): Value[A]

    def pure[A](a: A): Value[A] = shift[A](_(a))
  }

  trait StackSafeContinuationFactory extends ContinuationFactory {
    type UnderlyingFactory <: TailCallFactory
    val underlyingFactory: UnderlyingFactory
    type ErrorState
    type Result = underlyingFactory.Value[ErrorState]
    def stackUnsafeShift[A](f: (A => Result) => Result): Value[A]
    def shift[A](f: (A => Result) => Result): Value[A] = tailCall { () =>
      stackUnsafeShift { continue =>
        underlyingFactory.tailCall { () =>
          f { a =>
            underlyingFactory.tailCall { () =>
              continue(a)
            }
          }
        }
      }
    }
  }

  trait ContinuationErrorFactory extends ContinuationFactory with MonadErrorFactory with StackSafeContinuationFactory {

    type UnderlyingFactory <: ContinuationFactory
    type ErrorState = Throwable

    def raiseError[A](e: ErrorState): Value[A] = shift[A] { (continueSuccess: A => Result) =>
      underlyingFactory.pure(e)
    }

    type Facade[+A] <: Continuation[A] with MonadError[A]

    trait TryFlatMap[+A] extends Continuation[A] {
      def flatMap[B](mapper: (A) => Value[B]): Value[B] = shift[B] { (continue: B => Result) =>
        listen { a =>
          Try(mapper(a)) match {
            case Success(valueB) =>
              valueB.listen(continue)
            case Failure(e) =>
              underlyingFactory.pure(e)
          }
        }
      }
    }

    trait HandleError[+A] extends Continuation[A] {
      def handleError[B >: A](catcher: PartialFunction[ErrorState, Value[B]]): Value[B] = shift { (continueSuccess: B => Result) =>
        underlyingFactory
          .Facade(listen(continueSuccess))
          .flatMap { e =>
            val valueB: Value[B] = try {
              catcher.applyOrElse[ErrorState, Value[B]](e, raiseError)
            } catch {
              case NonFatal(e) => raiseError(e)
            }
            valueB.listen(continueSuccess)
          }
      }
    }
  }

  object UnitContinuation extends ContinuationFactory with IdentityFacadeFactory with IdentityBoxer {
    type Facade[+A] = Continuation[A]
    type Result = Unit
    abstract class Shift[+A] extends FlatMap[A] with MapIsDerived[A] with FlattenIsDerived[A]
    def shift[A](f: (A => Unit) => Unit): Shift[A] = f(_)

    def tailCall[A](tail0: () => Continuation[A]): Continuation[A] = new Shift[A] with TailCall[A] {
      def tail(): Continuation[A] = tail0()
    }
  }

  type UnitContinuation[+A] = UnitContinuation.Continuation[A]

  object NaiveTask
      extends ContinuationErrorFactory
      with IdentityFacadeFactory
      with IdentityBoxer
      with StackSafeContinuationFactory {
    type UnderlyingFactory = UnitContinuation.type
    val underlyingFactory: UnitContinuation.type = UnitContinuation
    override type ErrorState = Throwable
    trait Facade[+A] extends Continuation[A] with MonadError[A]
    abstract class Shift[+A]
        extends HandleError[A]
        with MapIsDerived[A]
        with TryFlatMap[A]
        with FlattenIsDerived[A]
        with Facade[A]
    def stackUnsafeShift[A](f: ((A) => UnitContinuation[Throwable]) => UnitContinuation[Throwable]): Shift[A] = f(_)
    def tailCall[A](tail0: () => Facade[A]): Facade[A] = new Shift[A] with TailCall[A] {
      def tail(): Facade[A] = tail0()
    }
  }

  type NaiveTask[+A] = NaiveTask.Facade[A]

}
