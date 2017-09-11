package com.thoughtworks.designpattern.benchmark.asyncio

import scala.concurrent.{ExecutionContext, SyncVar}
import _root_.scalaz._, Id._, effect._

object scalaz {

  object AsyncIO {
    implicit object asyncIOInstances
        extends IsomorphismMonadError[AsyncIO, EitherT[Cont[Unit, ?], Throwable, ?], Throwable]
        with MonadIO[AsyncIO] {
      val G = EitherT.eitherTMonadError[Cont[Unit, ?], Throwable](Cont.ContsTMonad[Id, Id, Unit])
      object iso extends Isomorphism.IsoFunctorTemplate[AsyncIO, EitherT[Cont[Unit, ?], Throwable, ?]] {
        def to[A](fa: AsyncIO[A]) = fa.eitherTUnitCont
        def from[A](ga: EitherT[Cont[Unit, ?], Throwable, A]): AsyncIO[A] = new AsyncIO(ga)
      }

      def liftIO[A](ioa: IO[A]): AsyncIO[A] = {
        AsyncIO(EitherT[Cont[Unit, ?], Throwable, A](Cont[Unit, Throwable \/ A](_(ioa.catchLeft.unsafePerformIO()))))
      }
    }

    def execute[A](io: IO[A])(implicit executionContext: ExecutionContext): AsyncIO[A] = {
      val cont = Cont { (continue: Throwable \/ A => Unit) =>
        executionContext.execute { () =>
          continue(io.catchLeft.unsafePerformIO())
        }
      }
      AsyncIO(EitherT[Cont[Unit, ?], Throwable, A](cont))
    }
  }

  final case class AsyncIO[A](eitherTUnitCont: EitherT[Cont[Unit, ?], Throwable, A]) extends AnyVal {
    def blockingAwait(): A = {
      val syncVar: SyncVar[Throwable \/ A] = new SyncVar
      eitherTUnitCont.run(syncVar.put)
      syncVar.take.valueOr(throw _)
    }
  }

}
