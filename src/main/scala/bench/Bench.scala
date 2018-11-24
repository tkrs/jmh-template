package bench

import java.util.concurrent.TimeUnit

import org.openjdk.jmh.annotations._

import scala.collection.mutable
import scala.annotation.tailrec

@State(Scope.Thread)
@BenchmarkMode(Array(Mode.Throughput))
@Warmup(iterations = 5, time = 1)
@Measurement(iterations = 10, time = 3)
@OutputTimeUnit(TimeUnit.SECONDS)
@Fork(
  value = 2,
  jvmArgs = Array(
    "-server",
    "-Xms2g",
    "-Xmx2g",
    "-XX:NewSize=1g",
    "-XX:MaxNewSize=1g",
    "-XX:InitialCodeCacheSize=512m",
    "-XX:ReservedCodeCacheSize=512m",
    "-XX:+UseG1GC",
    "-XX:-UseBiasedLocking",
    "-XX:+AlwaysPreTouch"
  )
)
abstract class Bench

trait Input { _: Bench =>
  @Param(Array("10", "100", "500"))
  var size: Int = _
}

class IteratorAccumulateBench extends Bench with Input {

  def source: Iterator[Int] = Iterator.range(1, size)

  @Benchmark
  def recursiveAcc: Int = {
    @tailrec def loop(it: Iterator[Int], acc: Int): Int =
      if (!it.hasNext) acc else loop(it, acc + it.next)
    loop(source, 0)
  }

  @Benchmark
  def whileAcc: Int = {
    val it  = source
    var acc = 0
    while (it.hasNext) acc += it.next
    acc
  }

  @Benchmark
  def foldLeftAcc: Int = {
    source.foldLeft(0) { case (l, r) => l + r }
  }

  @Benchmark
  def foreachAcc: Int = {
    var acc = 0
    source.foreach(acc += _)
    acc
  }
}

class ListAccumulateBench extends Bench with Input {

  lazy val source: List[Int] = List.range(1, size)

  @Benchmark
  def recursiveAcc: Int = {
    @tailrec def loop(xs: List[Int], acc: Int): Int =
      xs match {
        case h +: t => loop(t, acc + h)
        case _      => acc
      }
    loop(source, 0)
  }

  @Benchmark
  def foldLeftAcc: Int = {
    source.foldLeft(0) { case (l, r) => l + r }
  }

  @Benchmark
  def foreachAcc: Int = {
    var acc = 0
    source.foreach(acc += _)
    acc
  }
}

class VectorAccumulateBench extends Bench with Input {

  lazy val source: Vector[Int] = Vector.range(1, size)

  @Benchmark
  def recursiveAcc: Int = {
    @tailrec def loop(xs: Vector[Int], acc: Int): Int =
      xs match {
        case h +: t => loop(t, acc + h)
        case _      => acc
      }
    loop(source, 0)
  }

  @Benchmark
  def foldLeftAcc: Int = {
    source.foldLeft(0) { case (l, r) => l + r }
  }

  @Benchmark
  def foreachAcc: Int = {
    var acc = 0
    source.foreach(acc += _)
    acc
  }
}

class ArrayAccumulateBench extends Bench with Input {

  lazy val source: Array[Int] = Array.range(1, size)

  @Benchmark
  def recursiveAcc: Int = {
    @tailrec def loop(i: Int, xs: Array[Int], acc: Int): Int =
      if (i < source.length) loop(i + 1, xs, acc + xs(i)) else acc
    loop(0, source, 0)
  }

  @Benchmark
  def foldLeftAcc: Int = {
    source.foldLeft(0) { case (l, r) => l + r }
  }

  @Benchmark
  def foreachAcc: Int = {
    var acc = 0
    source.foreach(acc += _)
    acc
  }
}

class SetAccumulateBench extends Bench with Input {

  lazy val source: Set[Int] = (1 to size).toSet

  @Benchmark
  def foldLeftAcc: Int = {
    source.foldLeft(0) { case (l, r) => l + r }
  }

  @Benchmark
  def foreachAcc: Int = {
    var acc = 0
    source.foreach(acc += _)
    acc
  }
}
