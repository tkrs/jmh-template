package bench

import java.util.concurrent.TimeUnit

import org.openjdk.jmh.annotations._

@State(Scope.Thread)
@BenchmarkMode(Array(Mode.Throughput))
@Warmup(iterations = 10, time = 1)
@Measurement(iterations = 10, time = 10)
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

class AccumurateBench extends Bench {

  @Benchmark
  def recursiveAcc: Vector[Int] = {
    @annotation.tailrec
    def loop(it: Iterator[Int], acc: collection.mutable.Builder[Int, Vector[Int]]): Vector[Int] =
      if (!it.hasNext) acc.result() else loop(it, acc += it.next)
    loop(Iterator.range(1, 100), Vector.newBuilder)
  }

  @Benchmark
  def whileAcc: Vector[Int] = {
    val it  = Iterator.range(1, 100)
    val acc = Vector.newBuilder[Int]
    while (it.hasNext) acc += it.next
    acc.result()
  }
}
