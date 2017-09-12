package com.thoughtworks.designpattern.benchmark

import com.thoughtworks.designpattern
import com.thoughtworks.designpattern.benchmark
import com.thoughtworks.designpattern.continuation._
import com.thoughtworks.designpattern.covariant._
import com.thoughtworks.designpattern.io.{IOFactory, LiftIOFactory}

import scala.concurrent.{ExecutionContext, SyncVar}
import scala.util.control.NonFatal
import scala.util.{Failure, Success, Try}

/**
  * @author 杨博 (Yang Bo)
  */
object optimized {

  object UnitContinuation extends ContinuationFactory with IdentityBoxer with IdentityFacadeFactory {

    abstract class SamLiftIO[A]
        extends Facade[A]
        with DefaultFlatMap[A]
        with DefaultMap[A]
        with DefaultFlatten[A]
        with DefaultProduct[A] {
      def run(): A

      def start(continue: (A) => Unit): Unit = {
        continue(run())
      }
    }

    type Facade[+A] = UnitContinuation[A]
    type Result = Unit

    abstract class SamLiftContinuation[+A]
        extends Facade[A]
        with DefaultFlatMap[A]
        with DefaultMap[A]
        with DefaultFlatten[A]
        with DefaultProduct[A]

    def liftContinuation[A](f: (A => Result) => Result): SamLiftContinuation[A] = f(_)

    abstract class SamTailCall[A] extends SamLiftContinuation[A] with DefaultTailCallApply[A]
    def tailCall[A](tail: () => Facade[A]): SamTailCall[A] = () => tail()

  }

  abstract class UnitContinuation[+A] extends UnitContinuation.Continuation[A]

  abstract class Task[+A] extends Task.Continuation[A] with Task.MonadError[A] {
    import Task._
    private[Task] def tailStart(continue: A => Result): UnitContinuation.SamTailCall[Throwable] = { () =>
      start(continue)
    }
    def blockingAwait(): A = {
      val syncVar = new SyncVar[Try[A]]
      val () = start { a =>
        { continueFailure: (Throwable => Unit) =>
          syncVar.put(Success(a))
        }: UnitContinuation.SamLiftContinuation[Throwable]
      }.start { e =>
        syncVar.put(Failure(e))
      }
      syncVar.take.get
    }
  }

  object Task extends ContinuationErrorFactory with IdentityFacadeFactory with IdentityBoxer with LiftIOFactory {

    type Facade[+A] = Task[A]

    def execute[A](io: () => A)(implicit executionContext: ExecutionContext): SamLiftContinuation[A] = {
      (continueSuccess: A => Result) =>
        { continueFailure: (Throwable => Unit) =>
          executionContext.execute { () =>
            Try(io()) match {
              case Success(a) =>
                // Note: continueSuccess provided by map or flatMap,
                // may turn a successful execution into a failed state.
                // This is a feature!
                continueSuccess(a).start(continueFailure)
              case Failure(e) =>
                continueFailure(e)
            }
          }
        }: UnitContinuation.SamLiftContinuation[Throwable]
    }

    abstract class SamLiftIO[+A]
        extends Facade[A]
        with DefaultHandleError[A]
        with DefaultFlatten[A]
        with DefaultProduct[A] {

      def run(): A

      override def map[B](mapper: A => B): SamLiftContinuation[B] = { (continue: B => Result) =>
        Try(mapper(run())) match {
          case Success(b) =>
            tailContinue(continue, b)
          case Failure(e) =>
            UnitContinuation.pure(e)
        }
      }

      def flatMap[B](mapper: (A) => Facade[B]): SamLiftContinuation[B] = { (continue: B => Result) =>
        Try(mapper(run())) match {
          case Success(continuationB) =>
            continuationB.tailStart(continue)
          case Failure(e) =>
            UnitContinuation.pure(e)
        }
      }

      def start(continueSuccess: A => Result): UnitContinuation.SamTailCall[Throwable] = { () =>
        Try(run()) match {
          case Success(a) =>
            tailContinue(continueSuccess, a)
          case Failure(e) =>
            UnitContinuation.pure(e)
        }
      }
    }

    @inline
    def liftIO[A](io: () => A): SamLiftIO[A] = { () =>
      io()
    }
    def samLiftIO[A](sam: SamLiftIO[A]): SamLiftIO[A] = sam

    type UnderlyingFactory = UnitContinuation.type
    val underlyingFactory: UnitContinuation.type = UnitContinuation
    def tailContinue[A](continue: A => Result, a: A): UnitContinuation.SamTailCall[Throwable] = () => continue(a)

    abstract class SamLiftContinuation[+A]
        extends Facade[A]
        with DefaultHandleError[A]
        with DefaultFlatten[A]
        with DefaultProduct[A] {

      def map[B](mapper: A => B): SamLiftContinuation[B] = { (continue: B => Result) =>
        tailStart { a: A =>
          Try(mapper(a)) match {
            case Success(b) =>
              tailContinue(continue, b)
            case Failure(e) =>
              UnitContinuation.pure(e)
          }
        }
      }
      def flatMap[B](mapper: A => Facade[B]): SamLiftContinuation[B] = { (continue: B => Result) =>
        tailStart { a: A =>
          Try(mapper(a)) match {
            case Success(continuationB) =>
              continuationB.tailStart(continue)
            case Failure(e) =>
              UnitContinuation.pure(e)
          }
        }
      }
    }

    abstract class SamTailCall[A] extends SamLiftContinuation[A] with DefaultTailCallApply[A]

    def tailCall[A](tail: () => Facade[A]): SamTailCall[A] = () => tail()

    def liftContinuation[A](start: (A => Result) => Result): SamLiftContinuation[A] = start(_)
//
//    def liftContinuation[A](start: (A => Result) => Result): SamLiftContinuation[A] = { (continue: A => Result) =>
//      start { a =>
//        { () =>
//          continue(a)
//        }: UnitContinuation.SamTailCall[Throwable]
//      }
//    }

  }

}
