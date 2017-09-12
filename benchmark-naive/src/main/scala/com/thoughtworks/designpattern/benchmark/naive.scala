package com.thoughtworks.designpattern.benchmark

import com.thoughtworks.designpattern.continuation._
import com.thoughtworks.designpattern.covariant._
import com.thoughtworks.designpattern.io.{IOFactory, LiftIOFactory}

import scala.concurrent.{ExecutionContext, SyncVar}
import scala.util.{Failure, Success, Try}

/**
  * @author 杨博 (Yang Bo)
  */
object naive {

  object IO extends IOFactory {
    abstract class Facade[+A] extends IO[A]

    abstract class SamLiftIO[+A]
        extends Facade[A]
        with DefaultFlatMap[A]
        with DefaultFlatten[A]
        with DefaultMap[A]
        with DefaultProduct[A]
        with DefaultHandleError[A]

    def raiseError[A](e: Throwable): SamLiftIO[A] = () => throw e

    def pure[A](a: A): SamLiftIO[A] = () => a

    def liftIO[A](io: () => A): SamLiftIO[A] = () => io()

    abstract class SamTailCall[A] extends SamLiftIO[A] with DefaultTailCallApply[A]

    def tailCall[A](tail: () => Facade[A]): SamTailCall[A] = () => tail()
  }
  type IO[+A] = IO.Facade[A]

  object IOContinuation
      extends ContinuationFactory
      with LiftIOFactory
      with ContinuationFactoryDecorator
      with IdentityBoxer
      with IdentityFacadeFactory
      with ContinuationTailCallFactoryDecorator
      with ContinuationLiftIOFactoryDecorator {

    type UnderlyingFactory = IO.type
    val underlyingFactory: UnderlyingFactory = IO

    type State = Unit
    abstract class Facade[+A] extends Continuation[A] {
      def blockingAwait(): A = {
        val syncVar = new SyncVar[A]
        val () = this.start { a =>
          IO.liftIO { () =>
            syncVar.put(a)
          }
        }.run
        syncVar.take
      }
    }

    trait SamLiftContinuation[+A]
        extends Facade[A]
        with DefaultFlatMap[A]
        with DefaultMap[A]
        with DefaultFlatten[A]
        with DefaultProduct[A]
        with DefaultTailCallStart[A]
    def liftContinuation[A](start: (A => Result) => Result): SamLiftContinuation[A] = start(_)

    abstract class SamTailCall[A]
        extends Facade[A]
        with DefaultFlatMap[A]
        with DefaultMap[A]
        with DefaultFlatten[A]
        with DefaultProduct[A]
        with DefaultTailCallApply[A]
    def tailCall[A](tail: () => Facade[A]): SamTailCall[A] = () => tail()

  }

  type IOContinuation[+A] = IOContinuation.Facade[A]

  object Task
      extends ContinuationErrorFactory
      with TailCallFactory
      with IdentityFacadeFactory
      with IdentityBoxer
      with LiftIOFactory
      with ContinuationTailCallFactoryDecorator
      with ContinuationLiftIOFactoryDecorator {

    def execute[A](io: () => A)(implicit executionContext: ExecutionContext) = liftContinuation[A] { continueSuccess =>
      IOContinuation.liftContinuation { continueFailure =>
        IO.liftIO { () =>
          executionContext.execute { () =>
            Try(io()) match {
              case Success(a) =>
                continueSuccess(a).start(continueFailure).run
              case Failure(e) =>
                continueFailure(e).run
            }
          }
        }
      }
    }

    type UnderlyingFactory = IOContinuation.type
    val underlyingFactory: IOContinuation.type = IOContinuation

    abstract class Facade[+A] extends Continuation[A] with MonadError[A] {
      def blockingAwait(): A = {
        val syncVar = new SyncVar[Try[A]]
        val () = start { a =>
          IOContinuation.liftContinuation[State] { continueFailure =>
            IO.liftIO[Unit] { () =>
              syncVar.put(Success(a))
            }
          }
        }.start { e =>
          IO.liftIO { () =>
            syncVar.put(Failure(e))
          }
        }.run
        syncVar.take.get
      }
    }

    abstract class SamLiftContinuation[+A]
        extends Facade[A]
        with DefaultHandleError[A]
        with DefaultMap[A]
        with DefaultMonadErrorFlatMap[A]
        with DefaultFlatten[A]
        with DefaultProduct[A]

    abstract class SamTailStart[+A] extends SamLiftContinuation[A] with DefaultTailCallStart[A]

    def liftContinuation[A](start: (A => Result) => Result): SamTailStart[A] = start(_)

    abstract class SamTailCall[A] extends SamLiftContinuation[A] with DefaultTailCallApply[A]
    def tailCall[A](tail: () => Facade[A]): SamTailCall[A] = () => tail()
  }

  type Task[+A] = Task.Facade[A]
}
