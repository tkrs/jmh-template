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
  def recursiveAcc: Vector[Int] = {
    @annotation.tailrec
    def loop(it: Iterator[Int], acc: mutable.Builder[Int, Vector[Int]]): Vector[Int] =
      if (!it.hasNext) acc.result() else loop(it, acc += it.next)
    loop(source, Vector.newBuilder)
  }

  @Benchmark
  def whileAcc: Vector[Int] = {
    val it  = source
    val acc = Vector.newBuilder[Int]
    while (it.hasNext) acc += it.next
    acc.result()
  }

  @Benchmark
  def foldLeftAcc: Vector[Int] = {
    source.foldLeft(Vector.newBuilder[Int]) { case (l, r) => l += r }.result()
  }

  @Benchmark
  def foreachAcc: Vector[Int] = {
    val acc = Vector.newBuilder[Int]
    source.foreach(acc += _)
    acc.result()
  }
}

class ListAccumulateBench extends Bench {

  val source: List[Int] = List.range(1, 100)
  val viewSource: SeqView[Int, List[Int]] = source.view

  @Benchmark
  def recursiveAcc: Vector[Int] = {
    @annotation.tailrec
    def loop(xs: List[Int], acc: mutable.Builder[Int, Vector[Int]]): Vector[Int] =
      xs match {
        case h +: t => loop(t, acc += h)
        case _      => acc.result()
      }
    loop(source, Vector.newBuilder)
  }

  @Benchmark
  def foldLeftAcc: Vector[Int] = {
    source.foldLeft(Vector.newBuilder[Int]) { case (l, r) => l += r }.result()
  }

  @Benchmark
  def foreachAcc: Vector[Int] = {
    val acc = Vector.newBuilder[Int]
    source.foreach(acc += _)
    acc.result()
  }

  @Benchmark
  def foldLeftAccByView: Vector[Int] = {
    viewSource.foldLeft(Vector.newBuilder[Int]) { case (l, r) => l += r }.result()
  }

  @Benchmark
  def foreachAccByView: Vector[Int] = {
    val acc = Vector.newBuilder[Int]
    viewSource.foreach(acc += _)
    acc.result()
  }

  @Benchmark
  def foreachAccByIter: Vector[Int] = {
    val acc = Vector.newBuilder[Int]
    source.iterator.foreach(acc += _)
    acc.result()
  }
}

class VectorAccumulateBench extends Bench {

  val source: Vector[Int] = Vector.range(1, 100)
  val viewSource: SeqView[Int, Vector[Int]] = source.view

  @Benchmark
  def recursiveAcc: Vector[Int] = {
    @annotation.tailrec
    def loop(xs: Vector[Int], acc: mutable.Builder[Int, Vector[Int]]): Vector[Int] =
      xs match {
        case h +: t => loop(t, acc += h)
        case _      => acc.result()
      }
    loop(source, Vector.newBuilder)
  }

  @Benchmark
  def foldLeftAcc: Vector[Int] = {
    source.foldLeft(Vector.newBuilder[Int]) { case (l, r) => l += r }.result()
  }

  @Benchmark
  def foreachAcc: Vector[Int] = {
    val acc = Vector.newBuilder[Int]
    source.foreach(acc += _)
    acc.result()
  }

  @Benchmark
  def foldLeftAccByView: Vector[Int] = {
    viewSource.foldLeft(Vector.newBuilder[Int]) { case (l, r) => l += r }.result()
  }

  @Benchmark
  def foreachAccByView: Vector[Int] = {
    val acc = Vector.newBuilder[Int]
    viewSource.foreach(acc += _)
    acc.result()
  }

  @Benchmark
  def foreachAccByIter: Vector[Int] = {
    val acc = Vector.newBuilder[Int]
    source.iterator.foreach(acc += _)
    acc.result()
  }
}

class ArrayAccumulateBench extends Bench {

  val source: Array[Int] = Array.range(1, 100)
  val viewSource: mutable.IndexedSeqView[Int,Array[Int]] = source.view

  @Benchmark
  def recursiveAcc: Vector[Int] = {
    @annotation.tailrec
    def loop(i: Int, xs: Array[Int], acc: mutable.Builder[Int, Vector[Int]]): Vector[Int] =
      if (i < source.length) loop(i + 1, xs, acc += xs(i)) else acc.result()
    loop(0, source, Vector.newBuilder)
  }

  @Benchmark
  def foldLeftAcc: Vector[Int] = {
    source.foldLeft(Vector.newBuilder[Int]) { case (l, r) => l += r }.result()
  }

  @Benchmark
  def foreachAcc: Vector[Int] = {
    val acc = Vector.newBuilder[Int]
    source.foreach(acc += _)
    acc.result()
  }

  @Benchmark
  def foldLeftAccByView: Vector[Int] = {
    viewSource.foldLeft(Vector.newBuilder[Int]) { case (l, r) => l += r }.result()
  }

  @Benchmark
  def foreachAccByView: Vector[Int] = {
    val acc = Vector.newBuilder[Int]
    viewSource.foreach(acc += _)
    acc.result()
  }

  @Benchmark
  def foreachAccByIter: Vector[Int] = {
    val acc = Vector.newBuilder[Int]
    source.iterator.foreach(acc += _)
    acc.result()
  }
}

class SetAccumulateBench extends Bench {

  val source: Set[Int] = (1 to 100).toSet
  val viewSource: IterableView[Int, Set[Int]] = source.view

  @Benchmark
  def foldLeftAcc: Vector[Int] = {
    source.foldLeft(Vector.newBuilder[Int]) { case (l, r) => l += r }.result()
  }

  @Benchmark
  def foreachAcc: Vector[Int] = {
    val acc = Vector.newBuilder[Int]
    source.foreach(acc += _)
    acc.result()
  }

  @Benchmark
  def foldLeftAccByView: Vector[Int] = {
    viewSource.foldLeft(Vector.newBuilder[Int]) { case (l, r) => l += r }.result()
  }

  @Benchmark
  def foreachAccByView: Vector[Int] = {
    val acc = Vector.newBuilder[Int]
    viewSource.foreach(acc += _)
    acc.result()
  }

  @Benchmark
  def foreachAccByIter: Vector[Int] = {
    val acc = Vector.newBuilder[Int]
    source.iterator.foreach(acc += _)
    acc.result()
  }
}
