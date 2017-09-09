package com.thoughtworks.designpattern

import scala.util.{Failure, Success, Try}

/**
  * @author 杨博 (Yang Bo)
  */
object task {
//
//  trait TryMap[+A] extends Continuation[A] {
//    def map[B](mapper: (A) => B): Value[B] = shift[B] { (continue: B => Result) =>
//      listen { a =>
//        Try(mapper(a)) match {
//          case Success(b) =>
//            continue(b)
//          case Failure(e) =>
//            underlyingFactory.pure(e)
//        }
//      }
//    }
//  }
//  trait Map[+A] extends Continuation[A] { this: Facade[A] =>
//
//    def map[B](mapper: A => B): Value[B] = shift[B] { (continue: B => Result) =>
//      listen(mapper.andThen(continue))
//    }
//  }
}
