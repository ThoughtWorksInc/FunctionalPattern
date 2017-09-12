package com.thoughtworks.designpattern.benchmark

import scala.collection.mutable.ListBuffer

/**
  * @author 杨博 (Yang Bo)
  */
object DebugNaive {

  def main(arguments: Array[String]): Unit = {
    import naive.Task

    val tasks = (0 until 1000).map(_ => Task.liftIO(() => 1)).toList
    val init: Task[ListBuffer[Int]] = Task.liftIO(() => ListBuffer.empty[Int])
    val task = tasks.foldLeft(init)((acc, elem: Task[Int]) => acc.flatMap(lb => elem.map(e => lb += e)))

    task
      .map(_.toList.sum.toLong)
      .blockingAwait()
  }
}
