package bench

import java.util.concurrent.TimeUnit

import org.openjdk.jmh.annotations._

import scala.collection.{mutable, SeqView, IterableView}

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

class IteratorAccumulateBench extends Bench {

  def source: Iterator[Int] = Iterator.range(1, 100)

  @Benchmark
  def recursiveAcc: Int = {
    @annotation.tailrec
    def loop(it: Iterator[Int], acc: Int): Int =
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

class ListAccumulateBench extends Bench {

  val source: List[Int] = List.range(1, 100)
  val viewSource: SeqView[Int, List[Int]] = source.view

  @Benchmark
  def recursiveAcc: Int = {
    @annotation.tailrec
    def loop(xs: List[Int], acc: Int): Int =
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

  @Benchmark
  def foldLeftAccByView: Int = {
    viewSource.foldLeft(0) { case (l, r) => l + r }
  }

  @Benchmark
  def foreachAccByView: Int = {
    var acc = 0
    viewSource.foreach(acc += _)
    acc
  }

  @Benchmark
  def foreachAccByIter: Int = {
    var acc = 0
    source.iterator.foreach(acc += _)
    acc
  }
}

class VectorAccumulateBench extends Bench {

  val source: Vector[Int] = Vector.range(1, 100)
  val viewSource: SeqView[Int, Vector[Int]] = source.view

  @Benchmark
  def recursiveAcc: Int = {
    @annotation.tailrec
    def loop(xs: Vector[Int], acc: Int): Int =
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

  @Benchmark
  def foldLeftAccByView: Int = {
    viewSource.foldLeft(0) { case (l, r) => l + r }
  }

  @Benchmark
  def foreachAccByView: Int = {
    var acc = 0
    viewSource.foreach(acc += _)
    acc
  }

  @Benchmark
  def foreachAccByIter: Int = {
    var acc = 0
    source.iterator.foreach(acc += _)
    acc
  }
}

class ArrayAccumulateBench extends Bench {

  val source: Array[Int] = Array.range(1, 100)
  val viewSource: mutable.IndexedSeqView[Int,Array[Int]] = source.view

  @Benchmark
  def recursiveAcc: Int = {
    @annotation.tailrec
    def loop(i: Int, xs: Array[Int], acc: Int): Int =
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

  @Benchmark
  def foldLeftAccByView: Int = {
    viewSource.foldLeft(0) { case (l, r) => l + r }
  }

  @Benchmark
  def foreachAccByView: Int = {
    var acc = 0
    viewSource.foreach(acc += _)
    acc
  }

  @Benchmark
  def foreachAccByIter: Int = {
    var acc = 0
    source.iterator.foreach(acc += _)
    acc
  }
}

class SetAccumulateBench extends Bench {

  val source: Set[Int] = (1 to 100).toSet
  val viewSource: IterableView[Int, Set[Int]] = source.view

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

  @Benchmark
  def foldLeftAccByView: Int = {
    viewSource.foldLeft(0) { case (l, r) => l + r }
  }

  @Benchmark
  def foreachAccByView: Int = {
    var acc = 0
    viewSource.foreach(acc += _)
    acc
  }

  @Benchmark
  def foreachAccByIter: Int = {
    var acc = 0
    source.iterator.foreach(acc += _)
    acc
  }
}
