package com.thoughtworks.designpattern
import com.thoughtworks.designpattern.covariant._
import com.thoughtworks.designpattern.io.{LiftIOFactory}

import language.higherKinds
import scala.util.{Failure, Success, Try}
import scala.util.control.NonFatal

/**
  * @author 杨博 (Yang Bo)
  */
object continuation {

  trait LiftContinuationFactory {
    type Result
    type Value[A]
    def liftContinuation[A](start: (A => Result) => Result): Value[A]
  }

  trait ContinuationFactory
      extends FlatFunctorFacadeFactory
      with MonadFactory
      with LiftContinuationFactory
      with TailCallFactory {
    type Facade[+A] <: Continuation[A]

    trait Continuation[+A] extends Monad[A] {
      def start(continue: A => Result): Result
    }

    protected trait DefaultFlatMap[+A] extends Continuation[A] {
      this: Facade[A] =>
      def flatMap[B](mapper: A => Value[B]): Value[B] = liftContinuation[B] { (continue: B => Result) =>
        start { a: A =>
          mapper(a).start(continue)
        }
      }
    }

    // TODO: Rename
    protected trait DefaultTailCallApply[A] extends DefaultTailCall[A] {
      this: Value[A] =>
      def start(continue: A => Result): Result = last().start(continue)
    }

    def liftContinuation[A](f: (A => Result) => Result): Value[A]

    def pure[A](a: A): Value[A] = liftContinuation[A](_(a))

  }
  trait ContinuationFactoryDecorator {
    type UnderlyingFactory <: { type Value[A] }
    val underlyingFactory: UnderlyingFactory
    type State
    type Value[A]
    type Result = underlyingFactory.Value[State]
  }

  trait ContinuationLiftIOFactoryDecorator
      extends ContinuationFactoryDecorator
      with LiftIOFactory
      with LiftContinuationFactory {
    type UnderlyingFactory <: FacadeFactory with LiftIOFactory with FlatFunctorFactory

    def liftIO[A](io: () => A) = liftContinuation { continue =>
      underlyingFactory.Facade(underlyingFactory.liftIO(io)).flatMap(continue)
    }
  }

  trait ContinuationTailCallFactoryDecorator extends ContinuationFactory with ContinuationFactoryDecorator {
    type UnderlyingFactory <: TailCallFactory

    trait DefaultTailCallStart[+A] extends Continuation[A] {
      def start(continue: A => Result): Result = {
        stackUnsafeStart { a =>
          underlyingFactory.tailCall(() => continue(a))
        }
      }
      def stackUnsafeStart(continue: A => Result): Result
    }

  }

  trait ContinuationErrorFactory extends ContinuationFactory with MonadErrorFactory with ContinuationFactoryDecorator {
    type UnderlyingFactory <: FacadeFactory with MonadFactory
    type State = Throwable

    def raiseError[A](e: State): Value[A] = liftContinuation[A] { (continueSuccess: A => Result) =>
      underlyingFactory.pure(e)
    }

    type Facade[+A] <: Continuation[A] with MonadError[A]

    protected trait DefaultMonadErrorFlatMap[+A] extends Continuation[A] {
      def flatMap[B](mapper: A => Value[B]): Value[B] = liftContinuation[B] { (continue: B => Result) =>
        start { a: A =>
          underlyingFactory.Facade(underlyingFactory.pure(a)).flatMap { a: A =>
            mapper(a).start(continue)
          }
        }
      }
    }

    protected trait DefaultHandleError[+A] extends Continuation[A] {
      def handleError[B >: A](catcher: PartialFunction[State, Value[B]]): Value[B] = liftContinuation {
        (continueSuccess: B => Result) =>
          underlyingFactory
            .Facade(start(continueSuccess))
            .flatMap { e =>
              val valueB: Value[B] = try {
                catcher.applyOrElse[State, Value[B]](e, raiseError)
              } catch {
                case NonFatal(e) => raiseError(e)
              }
              valueB.start(continueSuccess)
            }
      }
    }
  }

}
