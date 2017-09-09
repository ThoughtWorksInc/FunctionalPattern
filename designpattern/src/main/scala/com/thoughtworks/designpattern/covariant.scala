package com.thoughtworks.designpattern
import language.higherKinds
import scala.language.implicitConversions

/**
  * @author 杨博 (Yang Bo)
  */
object covariant {

//  trait DecoratorFactory {}

//  trait FacadeFactory extends BoxFactory {
//    type Value[A] = Unboxed[A]
//
//    def boxInstance[A](instance: Value[A]): Facade[A] = box(instance)
//
//    def unboxInstance[A](instance: Value[A]): Unboxed[A] = instance
//  }

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

  trait IOFactory {

    type Value[A]

    def liftIO[A](io: () => A): Value[A]

  }

  trait TrampolineFactory extends FlatMapFactory with IOFactory {
    def suspend[A](io: () => Value[A]): Value[A] = {
      liftIO(io).flatten
    }
  }

  trait FunctorFactory {

    type Value[A]

    type Facade[+A] <: Functor[A]
    trait Functor[+A] extends Any {

      def map[B](mapper: A => B): Value[B]

    }

  }

  trait FlatMapFactory extends FunctorFactory with FacadeFactory {

    type Value[A]

    type Facade[+A] <: FlatMap[A]

    trait FlatMap[+A] extends Any with Functor[A] {

      def flatMap[B](mapper: A => Value[B]): Value[B]
      def flatten[B](implicit asInstanceB: A <:< Value[B]): Value[B]
    }

    /** @note this method is designed for Java users only */
    final def flatten[A](nested: Value[Value[A]]): Value[A] = {
      nested.flatten
    }

    trait FlatMapIsPrimary[+A] extends Any with FlatMap[A] {

      /** An internal method that intends to make this [[FlatMapIsPrimary]] conflict with [[FlatMapIsDerived]]. */
      protected def isFlatMapDerived = false
    }

    trait FlatMapIsDerived[+A] extends Any with FlatMap[A] {

      def flatMap[B](mapper: (A) => Value[B]): Value[B] = {
        map(mapper).flatten
      }

      /** An internal method that intends to make this [[FlatMapIsDerived]] conflict with [[FlatMapIsPrimary]]. */
      protected def isFlatMapDerived = true
    }

    trait FlattenIsDerived[+A] extends Any with FlatMapIsPrimary[A] { this: Facade[A] =>

      def flatten[B](implicit asInstanceB: A <:< Value[B]): Value[B] = {
        flatMap(asInstanceB)
      }

    }

  }

  trait ApplicativeFactory extends FunctorFactory {
    def pure[A](a: A): Value[A]
  }

  trait MonadFactory extends FlatMapFactory with ApplicativeFactory {

    type Monad[+A] = FlatMap[A]

    trait MapIsDerived[+A] extends Any with FlatMapIsPrimary[A] {

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

    type Error

    type Facade[+A] <: MonadError[A]

    trait MonadError[+A] extends Any with Monad[A] {
      def handleError[B >: A](catcher: PartialFunction[Error, Value[B]]): Value[B]
    }

    def raiseError[A](e: Error): Value[A]

  }

}
