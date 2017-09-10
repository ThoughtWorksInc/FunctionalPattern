package com.thoughtworks.designpattern

import com.thoughtworks.designpattern.covariant._

import language.higherKinds
import scala.util.control.NonFatal

/**
  * @author 杨博 (Yang Bo)
  */
object io {

  trait LiftIOFactory {
    type Value[A]
    def liftIO[A](io: () => A): Value[A]
  }

  trait IOFactory extends LiftIOFactory with MonadErrorFactory with TailCallFactory with IdentityFacadeFactory {
    type State = Throwable

    type Facade[+A] <: IO[A]

    trait IO[+A] extends MonadError[A] {
      def apply(): A
    }

    protected trait DefaultTailCallApply[A] extends DefaultTailCall[A] {
      this: Value[A] =>
      def apply(): A = last().apply()
    }
    protected trait DefaultFlatMap[+A] extends IO[A] {
      this: Facade[A] =>
      def flatMap[B](mapper: A => Value[B]): Value[B] = {
        tailCall(() => mapper(apply()))
      }
    }

    protected trait DefaultHandleError[+A] extends IO[A] {
      def handleError[B >: A](catcher: PartialFunction[State, Value[B]]): Value[B] = tailCall { () =>
        try {
          pure(apply())
        } catch {
          case NonFatal(e) =>
            catcher.applyOrElse(e, raiseError)
        }
      }
    }
  }

  object IO extends IOFactory {
    type Facade[+A] = IO[A]

    abstract class SamLiftIO[+A]
        extends DefaultFlatMap[A]
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

}
