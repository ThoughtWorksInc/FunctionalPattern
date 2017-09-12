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

    abstract class Facade[+A] extends Continuation[A]
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

  type UnitContinuation[+A] = UnitContinuation.Facade[A]

  object Task extends ContinuationErrorFactory with IdentityFacadeFactory with IdentityBoxer with LiftIOFactory {

    def execute[A](io: () => A)(implicit executionContext: ExecutionContext) = liftContinuation[A] {
      (continueSuccess: A => Result) =>
        UnitContinuation.liftContinuation[Throwable] { continueFailure =>
          executionContext.execute { () =>
            Try(io()) match {
              case Success(a) =>
                continueSuccess(a).start(continueFailure)
              case Failure(e) =>
                continueFailure(e)
            }
          }

        }
    }

    abstract class SamLiftIO[+A]
        extends Facade[A]
        with DefaultHandleError[A]
        with DefaultFlatten[A]
        with DefaultProduct[A] {

      def run(): A

      override def map[B](mapper: A => B): SamLiftContinuation[B] = { (continue: B => Result) =>
        Try(mapper(run())) match {
          case Success(b) => { () =>
            continue(b)
          }: UnitContinuation.SamTailCall[Throwable]
          case Failure(e) =>
            UnitContinuation.pure(e)
        }
      }

      def flatMap[B](mapper: (A) => Facade[B]): SamLiftContinuation[B] = { (continue: B => Result) =>
        Try(mapper(run())) match {
          case Success(continuationB) =>
            continuationB.start { b =>
              { () =>
                continue(b)
              }: UnitContinuation.SamTailCall[Throwable]
            }
          case Failure(e) =>
            UnitContinuation.pure(e)
        }
      }

      def start(continueSuccess: A => Result) = {
        Try(run()) match {
          case Success(a) => { () =>
            continueSuccess(a)
          }: UnitContinuation.SamTailCall[Throwable]
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

    abstract class Facade[+A] extends Continuation[A] with MonadError[A] {
      def blockingAwait(): A = {
        val syncVar = new SyncVar[Try[A]]
        val () = start { a =>
          UnitContinuation.liftContinuation[State] { continueFailure =>
            syncVar.put(Success(a))
          }
        }.start { e =>
          syncVar.put(Failure(e))
        }
        syncVar.take.get
      }
    }

    abstract class SamLiftContinuation[+A]
        extends Facade[A]
        with DefaultHandleError[A]
        with DefaultFlatten[A]
        with DefaultProduct[A] {

      def map[B](mapper: A => B): SamLiftContinuation[B] = { (continue: B => Result) =>
        { () =>
          start { a: A =>
            Try(mapper(a)) match {
              case Success(b) => { () =>
                continue(b)
              }: UnitContinuation.SamTailCall[Throwable]
              case Failure(e) =>
                UnitContinuation.pure(e)
            }
          }
        }: UnitContinuation.SamTailCall[Throwable]
      }
      def flatMap[B](mapper: A => Facade[B]): SamLiftContinuation[B] = { (continue: B => Result) =>
        start { a: A =>
          Try(mapper(a)) match {
            case Success(continuationB) =>
              continuationB.start { b =>
                { () =>
                  continue(b)
                }: UnitContinuation.SamTailCall[Throwable]
              }
            case Failure(e) =>
              UnitContinuation.pure(e)
          }
        }
      }

    }

    abstract class SamTailCall[A] extends SamLiftContinuation[A] with DefaultTailCallApply[A]

    def tailCall[A](tail: () => Facade[A]): SamTailCall[A] = () => tail()

    def liftContinuation[A](start: (A => Result) => Result): SamLiftContinuation[A] = { (continue: A => Result) =>
      start { a =>
        { () =>
          continue(a)
        }: UnitContinuation.SamTailCall[Throwable]
      }
    }

  }

  type Task[+A] = Task.Facade[A]
}