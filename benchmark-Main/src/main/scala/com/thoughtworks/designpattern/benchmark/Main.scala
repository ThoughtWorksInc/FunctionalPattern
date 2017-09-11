package com.thoughtworks.designpattern.benchmark

import org.openjdk.jmh.annotations._

import scala.collection.mutable.ListBuffer
import scala.language.{higherKinds, implicitConversions}
import scalaz.effect.IO

/**
  * @author 杨博 (Yang Bo)
  */
@State(Scope.Benchmark)
class Main {

  @Benchmark
  def sequenceScalazAsyncIOS(): Long = {
    import com.thoughtworks.designpattern.benchmark.asyncio.scalaz.AsyncIO

    import scalaz.syntax.all._

    val tasks = (0 until 100).map(_ => IO(1).liftIO[AsyncIO]).toList
    val init = IO(ListBuffer.empty[Int]).liftIO[AsyncIO]
    tasks
      .foldLeft(init)((acc, elem: AsyncIO[Int]) => acc.flatMap(lb => elem.map(e => lb += e)))
      .map(_.toList.sum.toLong)
      .blockingAwait()
  }

  @Benchmark
  def sequenceOptimizedTaskS(): Long = {
    import optimized.Task

    val tasks = (0 until 1000).map(_ => Task.samLiftIO(() => 1)).toList
    val init: Task[ListBuffer[Int]] = Task.samLiftIO(() => ListBuffer.empty[Int])
    tasks
      .foldLeft(init)((acc, elem: Task[Int]) => acc.flatMap(lb => elem.map(e => lb += e)))
      .map(_.toList.sum.toLong)
      .blockingAwait()
  }

  @Benchmark
  def sequenceNaiveTaskS(): Long = {
    import naive.Task

    val tasks = (0 until 1000).map(_ => Task.liftIO(() => 1)).toList
    val init: Task[ListBuffer[Int]] = Task.liftIO(() => ListBuffer.empty[Int])
    tasks
      .foldLeft(init)((acc, elem: Task[Int]) => acc.flatMap(lb => elem.map(e => lb += e)))
      .map(_.toList.sum.toLong)
      .blockingAwait()
  }

  @Benchmark
  def sequenceScalazAsyncIOA(): Long = {
    import com.thoughtworks.designpattern.benchmark.asyncio.scalaz.AsyncIO

    import scala.concurrent.ExecutionContext.Implicits._
    import scalaz.syntax.all._

    val tasks = (0 until 100).map(_ => AsyncIO.execute(IO(1))).toList
    val init = AsyncIO.execute(IO(ListBuffer.empty[Int]))
    tasks
      .foldLeft(init)((acc, elem: AsyncIO[Int]) => acc.flatMap(lb => elem.map(e => lb += e)))
      .map(_.toList.sum.toLong)
      .blockingAwait()
  }

  @Benchmark
  def sequenceNaiveTaskA(): Long = {
    import naive.Task

    import scala.concurrent.ExecutionContext.Implicits._

    val tasks = (0 until 100).map(_ => Task.execute(() => 1)).toList
    val init: Task[ListBuffer[Int]] = Task.execute(() => ListBuffer.empty[Int])
    tasks
      .foldLeft(init)((acc, elem: Task[Int]) => acc.flatMap(lb => elem.map(e => lb += e)))
      .map(_.toList.sum.toLong)
      .blockingAwait()
  }

}
