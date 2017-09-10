package com.thoughtworks.designpattern
import com.thoughtworks.designpattern.covariant._
import com.thoughtworks.designpattern.io.IO

import language.higherKinds
import scala.util.{Failure, Success, Try}
import scala.util.control.NonFatal

/**
  * @author 杨博 (Yang Bo)
  */
object continuation {

  trait LiftAsynchronousFactory {
    type Result
    type Value[A]
    def liftAsynchronous[A](f: (A => Result) => Result): Value[A]
  }

  trait ContinuationFactory extends MonadFactory with TailCallFactory with LiftAsynchronousFactory {
    type Facade[+A] <: Continuation[A]

    trait Continuation[+A] extends Monad[A] {
      def apply(continue: A => Result): Result
    }

    protected trait DefaultFlatMap[+A] extends Continuation[A] {
      this: Facade[A] =>
      def flatMap[B](mapper: A => Value[B]): Value[B] = liftAsynchronous[B] { (continue: B => Result) =>
        apply { a: A =>
          mapper(a).apply(continue)
        }
      }
    }

    def liftAsynchronous[A](f: (A => Result) => Result): Value[A]

    def pure[A](a: A): Value[A] = liftAsynchronous[A](_(a))
  }
  trait ContinuationTailCallFactory extends ContinuationFactory with TailCallFactory {

    protected trait DefaultTailCallApply[A] extends DefaultTailCall[A] {
      this: Value[A] =>
      def apply(continue: A => Result): Result = last().apply(continue)
    }

  }
  trait ContinuationTailCallFactoryDecorator extends ContinuationTailCallFactory {
    type UnderlyingFactory <: TailCallFactory
    val underlyingFactory: UnderlyingFactory
    type State
    type Result = underlyingFactory.Value[State]

    def stackUnsafeLiftAsynchronous[A](f: (A => Result) => Result): Value[A]
    def liftAsynchronous[A](f: (A => Result) => Result): Value[A] = tailCall { () =>
      stackUnsafeLiftAsynchronous { continue =>
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

  trait ContinuationErrorFactory
      extends ContinuationFactory
      with MonadErrorFactory
      with ContinuationTailCallFactory
      with ContinuationTailCallFactoryDecorator {
    type UnderlyingFactory <: ContinuationFactory
    type State = Throwable

    def raiseError[A](e: State): Value[A] = liftAsynchronous[A] { (continueSuccess: A => Result) =>
      underlyingFactory.pure(e)
    }

    type Facade[+A] <: Continuation[A] with MonadError[A]

    protected trait DefaultMonadErrorFlatMap[+A] extends Continuation[A] {
      def flatMap[B](mapper: (A) => Value[B]): Value[B] = liftAsynchronous[B] { (continue: B => Result) =>
        apply { a =>
          Try(mapper(a)) match {
            case Success(valueB) =>
              valueB.apply(continue)
            case Failure(e) =>
              underlyingFactory.pure(e)
          }
        }
      }
    }

    protected trait DefaultHandleError[+A] extends Continuation[A] {
      def handleError[B >: A](catcher: PartialFunction[State, Value[B]]): Value[B] = liftAsynchronous {
        (continueSuccess: B => Result) =>
          underlyingFactory
            .Facade(apply(continueSuccess))
            .flatMap { e =>
              val valueB: Value[B] = try {
                catcher.applyOrElse[State, Value[B]](e, raiseError)
              } catch {
                case NonFatal(e) => raiseError(e)
              }
              valueB.apply(continueSuccess)
            }
      }
    }
  }

  object UnitContinuation
      extends ContinuationFactory
      with ContinuationTailCallFactory
      with IdentityBoxer
      with IdentityFacadeFactory {

    type Facade[+A] = Continuation[A]
    type Result = Unit

    abstract class SamLift[+A]
        extends DefaultFlatMap[A]
        with DefaultMap[A]
        with DefaultFlatten[A]
        with DefaultProduct[A]
    def liftAsynchronous[A](f: (A => Result) => Result): SamLift[A] = f(_)

    abstract class SamTailCall[A] extends SamLift[A] with DefaultTailCallApply[A]
    def tailCall[A](tail: () => Facade[A]): SamTailCall[A] = () => tail()
  }

  object IOContinuation
      extends ContinuationFactory
      with ContinuationTailCallFactoryDecorator
      with IdentityBoxer
      with IdentityFacadeFactory {
    type UnderlyingFactory = IO.type
    val underlyingFactory: UnderlyingFactory = IO

    type State = Unit
    type Facade[+A] = Continuation[A]

    abstract class SamLift[+A]
        extends DefaultFlatMap[A]
        with DefaultMap[A]
        with DefaultFlatten[A]
        with DefaultProduct[A]
    def stackUnsafeLiftAsynchronous[A](f: (A => Result) => Result): SamLift[A] = f(_)

    abstract class SamTailCall[A] extends SamLift[A] with DefaultTailCallApply[A]
    def tailCall[A](tail: () => Continuation[A]): SamTailCall[A] = () => tail()

  }

  type IOContinuation[+A] = IOContinuation.Continuation[A]

  object NaiveTask extends ContinuationErrorFactory with TailCallFactory with IdentityFacadeFactory with IdentityBoxer {

    type UnderlyingFactory = IOContinuation.type
    val underlyingFactory: IOContinuation.type = IOContinuation

    trait Facade[+A] extends Continuation[A] with MonadError[A]

    abstract class SamLift[+A]
        extends DefaultHandleError[A]
        with DefaultMap[A]
        with DefaultMonadErrorFlatMap[A]
        with DefaultFlatten[A]
        with DefaultProduct[A]
        with Facade[A]

    def stackUnsafeLiftAsynchronous[A](f: (A => Result) => Result): SamLift[A] = f(_)

    abstract class SamTailCall[A] extends SamLift[A] with DefaultTailCallApply[A]
    def tailCall[A](tail: () => Facade[A]): SamTailCall[A] = () => tail()
  }

  type NaiveTask[+A] = NaiveTask.Facade[A]

}
