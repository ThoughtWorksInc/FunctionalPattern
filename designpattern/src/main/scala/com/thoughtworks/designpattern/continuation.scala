package com.thoughtworks.designpattern
import com.thoughtworks.designpattern.covariant._

import language.higherKinds
import language.implicitConversions
import scala.concurrent.SyncVar
import scala.util.{Failure, Success, Try}
import scala.util.control.NonFatal

/**
  * @author 杨博 (Yang Bo)
  */
object continuation {

  trait ContinuationFactory extends MonadFactory {
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

    def shift[A](f: (A => Result) => Result): Value[A]

    def pure[A](a: A): Value[A] = shift[A](_(a))
  }

  trait ContinuationErrorFactory extends ContinuationFactory with MonadErrorFactory {

    type UnderlyingFactory <: ContinuationFactory with Boxer
    val underlyingFactory: UnderlyingFactory
    type Error = Throwable

    type Result = underlyingFactory.Value[Error]

    def raiseError[A](e: Error): Value[A] = shift[A] { (continueSuccess: A => Result) =>
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
      def handleError[B >: A](catcher: PartialFunction[Error, Value[B]]): Value[B] = shift { (continueSuccess: B => Result) =>
        underlyingFactory.Facade(listen(continueSuccess)).flatMap { e =>
          val valueB: Value[B] = try {
            catcher.applyOrElse[Error, Value[B]](e, raiseError)
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

  }

  type UnitContinuation[+A] = UnitContinuation.Continuation[A]

  object NaiveTask extends ContinuationErrorFactory with IdentityFacadeFactory with IdentityBoxer {
    type UnderlyingFactory = UnitContinuation.type
    val underlyingFactory: UnitContinuation.type = UnitContinuation
    override type Error = Throwable
    trait Facade[+A] extends Continuation[A] with MonadError[A]
    abstract class Shift[+A]
        extends HandleError[A]
        with MapIsDerived[A]
        with TryFlatMap[A]
        with FlattenIsDerived[A]
        with Facade[A]
    def shift[A](f: ((A) => UnitContinuation[Throwable]) => UnitContinuation[Throwable]): Shift[A] = f(_)
  }

  type NaiveTask[+A] = NaiveTask.Facade[A]

}
