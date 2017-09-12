package com.thoughtworks.designpattern
import language.higherKinds
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

    trait DefaultTailCall[A] { this: Value[A] =>
      def tail(): Value[A]

      final def last(): Value[A] = {
        var i: Value[A] = tail()
        while (i.isInstanceOf[DefaultTailCall[A]]) {
          i = i.asInstanceOf[DefaultTailCall[A]].tail()
        }
        i
      }
// The tail call version does not work because of https://github.com/scala/bug/issues/10493
//      @tailrec
//      final def last(): Value[A] = {
//        tail() match {
//          case tailCall: DefaultTailCall[_] =>
//            tailCall.last()
//          case notTailCall =>
//            notTailCall
//        }
//      }
    }

  }

  trait FunctorFactory {

    type Value[A]

    type Facade[+A] <: Functor[A]
    trait Functor[+A] extends Any {

      def map[B](mapper: A => B): Value[B]

    }

  }

  trait FlatFunctorFactory extends ApplyFactory {

    type Value[A]

    type Facade[+A] <: FlatFunctor[A]

    trait FlatFunctor[+A] extends Any with Apply[A] {

      def flatMap[B](mapper: A => Value[B]): Value[B]
      def flatten[B](implicit asInstanceB: A <:< Value[B]): Value[B]
    }

    trait PrimaryFlatMap[+A] extends Any with FlatFunctor[A] {

      /** An internal method that intends to make this [[PrimaryFlatMap]] conflict with [[FlatFunctorFacadeFactory.DefaultFlatMap]]. */
      protected def isFlatMapPrimary = false
    }

    trait DefaultFlatten[+A] extends Any with PrimaryFlatMap[A] { this: Facade[A] =>
      def flatten[B](implicit asInstanceB: A <:< Value[B]): Value[B] = {
        flatMap(asInstanceB)
      }
    }
  }

  trait FlatFunctorFacadeFactory extends FacadeFactory with FlatFunctorFactory {

    /** @note this method is designed for Java users only */
    final def flatten[A](nested: Value[Value[A]]): Value[A] = {
      nested.flatten
    }

    trait DefaultFlatMap[+A] extends Any with FlatFunctor[A] {

      /** An internal method that intends to make this [[DefaultFlatMap]] conflict with [[PrimaryFlatMap]]. */
      protected def isFlatMapPrimary = true

      def flatMap[B](mapper: (A) => Value[B]): Value[B] = {
        map(mapper).flatten
      }
    }

    trait DefaultProduct[+A] extends Any with PrimaryFlatMap[A] {
      def product[A1 >: A, B](that: Value[B]): Value[(A1, B)] = {
        for {
          a <- this
          b <- Facade(that)
        } yield (a, b)
      }
    }

  }

  trait CartesianFactory {
    type Value[A]
    type Facade[+A] <: Cartesian[A]

    trait Cartesian[+A] extends Any {
      def product[A1 >: A, B](that: Value[B]): Value[(A1, B)]
    }
  }

  trait ApplyFactory extends CartesianFactory with FunctorFactory {

    type Facade[+A] <: Apply[A]
    trait Apply[+A] extends Any with Functor[A] with Cartesian[A]

  }

  trait ApplicativeFactory extends ApplyFactory {
    def pure[A](a: A): Value[A]

  }

  trait MonadFactory extends FlatFunctorFactory with ApplicativeFactory {

    type Monad[+A] = FlatFunctor[A]

    trait DefaultMap[+A] extends Any with PrimaryFlatMap[A] {

      @noinline
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

  trait MonadRaiiFactory extends MonadErrorFactory {

    type State

    type Facade[+A] <: MonadError[A]

    trait MonadRaii[+A] extends Any with MonadError[A] {

      /** Mark this [[MonadRaii]] as a nested block.
        *
        * All finalizers will be invoked when exiting the block.
        */
      def nestedBlock: Facade[A]
    }

    /** Add a finalizer to current block */
    def addFinalizer(finalizer: Facade[Unit]): Facade[Unit]

  }

}
