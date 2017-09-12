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

  trait IOFactory
      extends FlatFunctorFacadeFactory
      with LiftIOFactory
      with MonadErrorFactory
      with TailCallFactory
      with IdentityFacadeFactory {
    type State = Throwable

    type Facade[+A] <: IO[A]

    trait IO[+A] extends MonadError[A] {
      def run(): A
    }

    protected trait DefaultTailCallApply[A] extends DefaultTailCall[A] {
      this: Value[A] =>
      def run(): A = last().run()
    }
    protected trait DefaultFlatMap[+A] extends IO[A] {
      this: Facade[A] =>
      def flatMap[B](mapper: A => Value[B]): Value[B] = {
        tailCall(() => mapper(run()))
      }
    }

    protected trait DefaultHandleError[+A] extends IO[A] {
      def handleError[B >: A](catcher: PartialFunction[State, Value[B]]): Value[B] = tailCall { () =>
        try {
          pure(run())
        } catch {
          case NonFatal(e) =>
            catcher.applyOrElse(e, raiseError)
        }
      }
    }
  }

}
