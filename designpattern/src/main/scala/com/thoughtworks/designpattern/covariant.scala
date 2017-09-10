package com.thoughtworks.designpattern
import language.higherKinds
import scala.annotation.tailrec
import scala.language.implicitConversions

/**
  * @author 杨博 (Yang Bo)
  */
object covariant {

  trait Boxer {

    type Raw[A]
    type Value[A]

    def unbox[A](boxed: Value[A]): Raw[A]
    def box[A](unboxed: Raw[A]): Value[A]

  }

  trait IdentityBoxer extends Boxer {
    type Raw[A] = Value[A]
    def unbox[A](value: Value[A]): Raw[A] = value
    def box[A](value: Raw[A]): Value[A] = value
  }

  trait FacadeFactory {

    type Facade[+A]
    type Value[A]
    implicit def Facade[A](instance: Value[A]): Facade[A]

  }

  trait IdentityFacadeFactory extends FacadeFactory {
    implicit def Facade[A](instance: Value[A]): Facade[A] = instance
    type Value[+A] = Facade[A]
  }

  trait TailCallFactory {

    def tailCall[A](tail: () => Value[A]): Value[A] with DefaultTailCall[A]

    type Value[A]

    protected trait DefaultTailCall[A] { this: Value[A] =>
      def tail(): Value[A]

      final def last(): Value[A] = {
        tail() match {
          case tailCall: DefaultTailCall[A] =>
            tailCall.last()
          case notTailCall =>
            notTailCall
        }
      }
    }

  }

  trait FunctorFactory {

    type Value[A]

    type Facade[+A] <: Functor[A]
    trait Functor[+A] extends Any {

      def map[B](mapper: A => B): Value[B]

    }

  }

  trait FlatMapFactory extends FacadeFactory with ApplyFactory {

    type Value[A]

    type Facade[+A] <: FlatMap[A]

    trait FlatMap[+A] extends Any with Apply[A] {

      def flatMap[B](mapper: A => Value[B]): Value[B]
      def flatten[B](implicit asInstanceB: A <:< Value[B]): Value[B]
    }

    /** @note this method is designed for Java users only */
    final def flatten[A](nested: Value[Value[A]]): Value[A] = {
      nested.flatten
    }

    protected trait PrimaryFlatMap[+A] extends Any with FlatMap[A] {

      /** An internal method that intends to make this [[PrimaryFlatMap]] conflict with [[DefaultFlatMap]]. */
      protected def isFlatMapPrimary = false
    }

    protected trait DefaultFlatMap[+A] extends Any with FlatMap[A] {

      /** An internal method that intends to make this [[DefaultFlatMap]] conflict with [[PrimaryFlatMap]]. */
      protected def isFlatMapPrimary = true

      def flatMap[B](mapper: (A) => Value[B]): Value[B] = {
        map(mapper).flatten
      }
    }

    protected trait DefaultFlatten[+A] extends Any with PrimaryFlatMap[A] { this: Facade[A] =>
      def flatten[B](implicit asInstanceB: A <:< Value[B]): Value[B] = {
        flatMap(asInstanceB)
      }
    }

    protected trait DefaultProduct[+A] extends Any with PrimaryFlatMap[A] {
      def product[A1 >: A, B](that: Value[B]): Value[(A1, B)] = {
        for {
          a <- this
          b <- that
        } yield (a, b)
      }
    }

  }

  trait ApplyFactory extends FunctorFactory {
    type Value[A]
    type Facade[+A] <: Apply[A]

    trait Apply[+A] extends Any with Functor[A] {
      def product[A1 >: A, B](that: Value[B]): Value[(A1, B)]
    }
  }

  trait ApplicativeFactory extends ApplyFactory {
    def pure[A](a: A): Value[A]
  }

  trait MonadFactory extends FlatMapFactory with ApplicativeFactory {

    type Monad[+A] = FlatMap[A]

    protected trait DefaultMap[+A] extends Any with PrimaryFlatMap[A] {

      def map[B](mapper: (A) => B): Value[B] = {
        // Assign MonadFactory to local in case of this Monad being captured by closures
        val monadFactory: MonadFactory.this.type = MonadFactory.this
        flatMap { a: A =>
          monadFactory.pure(mapper(a))
        }
      }

    }
  }

  trait MonadErrorFactory extends MonadFactory {

    type State

    type Facade[+A] <: MonadError[A]

    trait MonadError[+A] extends Any with Monad[A] {
      def handleError[B >: A](catcher: PartialFunction[State, Value[B]]): Value[B]
    }

    def raiseError[A](e: State): Value[A]

  }

}
