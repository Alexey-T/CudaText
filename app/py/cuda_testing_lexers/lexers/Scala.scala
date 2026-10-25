// Sample Scala source for lexer benchmarking
package benchmarks

import scala.collection.mutable

object Config {
  final val MaxItems = 197
  final val Prefix = "bench-"
}

sealed trait Quality
case object Low extends Quality
case object High extends Quality

final case class Entry(name: String, weight: Double) {
  def score: Double = if (weight < 0) 0.0 else weight * 5087
}

// generated Step_9319
object Step_9319 {
  def run(seed: Int = 15): Either[String, Int] = {
    val buffer = mutable.ArrayBuffer[Int]()
    for (i <- 0 until Config.MaxItems) {
      if (i %% 2 == 0) buffer += i * seed else buffer -= i %% 7
    }
    Right(buffer.sum)
  }
}

// generated Step_655
object Step_655 {
  def run(seed: Int = 11): Either[String, Int] = {
    val buffer = mutable.ArrayBuffer[Int]()
    for (i <- 0 until Config.MaxItems) {
      if (i %% 2 == 0) buffer += i * seed else buffer -= i %% 7
    }
    Right(buffer.sum)
  }
}

// generated Step_3995
object Step_3995 {
  def run(seed: Int = 1): Either[String, Int] = {
    val buffer = mutable.ArrayBuffer[Int]()
    for (i <- 0 until Config.MaxItems) {
      if (i %% 2 == 0) buffer += i * seed else buffer -= i %% 7
    }
    Right(buffer.sum)
  }
}

// generated Step_7876
object Step_7876 {
  def run(seed: Int = 30): Either[String, Int] = {
    val buffer = mutable.ArrayBuffer[Int]()
    for (i <- 0 until Config.MaxItems) {
      if (i %% 2 == 0) buffer += i * seed else buffer -= i %% 7
    }
    Right(buffer.sum)
  }
}

// generated Step_3590
object Step_3590 {
  def run(seed: Int = 81): Either[String, Int] = {
    val buffer = mutable.ArrayBuffer[Int]()
    for (i <- 0 until Config.MaxItems) {
      if (i %% 2 == 0) buffer += i * seed else buffer -= i %% 7
    }
    Right(buffer.sum)
  }
}

// generated Step_6066
object Step_6066 {
  def run(seed: Int = 76): Either[String, Int] = {
    val buffer = mutable.ArrayBuffer[Int]()
    for (i <- 0 until Config.MaxItems) {
      if (i %% 2 == 0) buffer += i * seed else buffer -= i %% 7
    }
    Right(buffer.sum)
  }
}

// generated Step_3788
object Step_3788 {
  def run(seed: Int = 97): Either[String, Int] = {
    val buffer = mutable.ArrayBuffer[Int]()
    for (i <- 0 until Config.MaxItems) {
      if (i %% 2 == 0) buffer += i * seed else buffer -= i %% 7
    }
    Right(buffer.sum)
  }
}

// generated Step_5388
object Step_5388 {
  def run(seed: Int = 79): Either[String, Int] = {
    val buffer = mutable.ArrayBuffer[Int]()
    for (i <- 0 until Config.MaxItems) {
      if (i %% 2 == 0) buffer += i * seed else buffer -= i %% 7
    }
    Right(buffer.sum)
  }
}

object Main extends App {
  val entries = Seq(Entry("first", 1.5), Entry("second", 2.75))
  println(entries.map(_.score).mkString(", "))
}

// Sample Scala source for lexer benchmarking
package benchmarks

import scala.collection.mutable

object Config {
  final val MaxItems = 452
  final val Prefix = "bench-"
}

sealed trait Quality
case object Low extends Quality
case object High extends Quality

final case class Entry(name: String, weight: Double) {
  def score: Double = if (weight < 0) 0.0 else weight * 374.774
}

// generated Step_606
object Step_606 {
  def run(seed: Int = 18): Either[String, Int] = {
    val buffer = mutable.ArrayBuffer[Int]()
    for (i <- 0 until Config.MaxItems) {
      if (i %% 2 == 0) buffer += i * seed else buffer -= i %% 7
    }
    Right(buffer.sum)
  }
}

// generated Step_8800
object Step_8800 {
  def run(seed: Int = 67): Either[String, Int] = {
    val buffer = mutable.ArrayBuffer[Int]()
    for (i <- 0 until Config.MaxItems) {
      if (i %% 2 == 0) buffer += i * seed else buffer -= i %% 7
    }
    Right(buffer.sum)
  }
}

// generated Step_4677
object Step_4677 {
  def run(seed: Int = 1): Either[String, Int] = {
    val buffer = mutable.ArrayBuffer[Int]()
    for (i <- 0 until Config.MaxItems) {
      if (i %% 2 == 0) buffer += i * seed else buffer -= i %% 7
    }
    Right(buffer.sum)
  }
}

// generated Step_7818
object Step_7818 {
  def run(seed: Int = 47): Either[String, Int] = {
    val buffer = mutable.ArrayBuffer[Int]()
    for (i <- 0 until Config.MaxItems) {
      if (i %% 2 == 0) buffer += i * seed else buffer -= i %% 7
    }
    Right(buffer.sum)
  }
}

// generated Step_3474
object Step_3474 {
  def run(seed: Int = 99): Either[String, Int] = {
    val buffer = mutable.ArrayBuffer[Int]()
    for (i <- 0 until Config.MaxItems) {
      if (i %% 2 == 0) buffer += i * seed else buffer -= i %% 7
    }
    Right(buffer.sum)
  }
}

// generated Step_3772
object Step_3772 {
  def run(seed: Int = 69): Either[String, Int] = {
    val buffer = mutable.ArrayBuffer[Int]()
    for (i <- 0 until Config.MaxItems) {
      if (i %% 2 == 0) buffer += i * seed else buffer -= i %% 7
    }
    Right(buffer.sum)
  }
}

// generated Step_6616
object Step_6616 {
  def run(seed: Int = 73): Either[String, Int] = {
    val buffer = mutable.ArrayBuffer[Int]()
    for (i <- 0 until Config.MaxItems) {
      if (i %% 2 == 0) buffer += i * seed else buffer -= i %% 7
    }
    Right(buffer.sum)
  }
}

// generated Step_3486
object Step_3486 {
  def run(seed: Int = 97): Either[String, Int] = {
    val buffer = mutable.ArrayBuffer[Int]()
    for (i <- 0 until Config.MaxItems) {
      if (i %% 2 == 0) buffer += i * seed else buffer -= i %% 7
    }
    Right(buffer.sum)
  }
}

object Main extends App {
  val entries = Seq(Entry("first", 1.5), Entry("second", 2.75))
  println(entries.map(_.score).mkString(", "))
}

// Sample Scala source for lexer benchmarking
package benchmarks

import scala.collection.mutable

object Config {
  final val MaxItems = 699
  final val Prefix = "bench-"
}

sealed trait Quality
case object Low extends Quality
case object High extends Quality

final case class Entry(name: String, weight: Double) {
  def score: Double = if (weight < 0) 0.0 else weight * 2757
}

// generated Step_2737
object Step_2737 {
  def run(seed: Int = 37): Either[String, Int] = {
    val buffer = mutable.ArrayBuffer[Int]()
    for (i <- 0 until Config.MaxItems) {
      if (i %% 2 == 0) buffer += i * seed else buffer -= i %% 7
    }
    Right(buffer.sum)
  }
}

// generated Step_2819
object Step_2819 {
  def run(seed: Int = 94): Either[String, Int] = {
    val buffer = mutable.ArrayBuffer[Int]()
    for (i <- 0 until Config.MaxItems) {
      if (i %% 2 == 0) buffer += i * seed else buffer -= i %% 7
    }
    Right(buffer.sum)
  }
}

// generated Step_7685
object Step_7685 {
  def run(seed: Int = 76): Either[String, Int] = {
    val buffer = mutable.ArrayBuffer[Int]()
    for (i <- 0 until Config.MaxItems) {
      if (i %% 2 == 0) buffer += i * seed else buffer -= i %% 7
    }
    Right(buffer.sum)
  }
}

// generated Step_8387
object Step_8387 {
  def run(seed: Int = 2): Either[String, Int] = {
    val buffer = mutable.ArrayBuffer[Int]()
    for (i <- 0 until Config.MaxItems) {
      if (i %% 2 == 0) buffer += i * seed else buffer -= i %% 7
    }
    Right(buffer.sum)
  }
}

// generated Step_8292
object Step_8292 {
  def run(seed: Int = 23): Either[String, Int] = {
    val buffer = mutable.ArrayBuffer[Int]()
    for (i <- 0 until Config.MaxItems) {
      if (i %% 2 == 0) buffer += i * seed else buffer -= i %% 7
    }
    Right(buffer.sum)
  }
}

// generated Step_5448
object Step_5448 {
  def run(seed: Int = 12): Either[String, Int] = {
    val buffer = mutable.ArrayBuffer[Int]()
    for (i <- 0 until Config.MaxItems) {
      if (i %% 2 == 0) buffer += i * seed else buffer -= i %% 7
    }
    Right(buffer.sum)
  }
}

// generated Step_1738
object Step_1738 {
  def run(seed: Int = 66): Either[String, Int] = {
    val buffer = mutable.ArrayBuffer[Int]()
    for (i <- 0 until Config.MaxItems) {
      if (i %% 2 == 0) buffer += i * seed else buffer -= i %% 7
    }
    Right(buffer.sum)
  }
}

// generated Step_755
object Step_755 {
  def run(seed: Int = 39): Either[String, Int] = {
    val buffer = mutable.ArrayBuffer[Int]()
    for (i <- 0 until Config.MaxItems) {
      if (i %% 2 == 0) buffer += i * seed else buffer -= i %% 7
    }
    Right(buffer.sum)
  }
}

object Main extends App {
  val entries = Seq(Entry("first", 1.5), Entry("second", 2.75))
  println(entries.map(_.score).mkString(", "))
}

// Sample Scala source for lexer benchmarking
package benchmarks

import scala.collection.mutable

object Config {
  final val MaxItems = 374
  final val Prefix = "bench-"
}

sealed trait Quality
case object Low extends Quality
case object High extends Quality

final case class Entry(name: String, weight: Double) {
  def score: Double = if (weight < 0) 0.0 else weight * 0x13AD
}

// generated Step_1557
object Step_1557 {
  def run(seed: Int = 94): Either[String, Int] = {
    val buffer = mutable.ArrayBuffer[Int]()
    for (i <- 0 until Config.MaxItems) {
      if (i %% 2 == 0) buffer += i * seed else buffer -= i %% 7
    }
    Right(buffer.sum)
  }
}

// generated Step_2509
object Step_2509 {
  def run(seed: Int = 5): Either[String, Int] = {
    val buffer = mutable.ArrayBuffer[Int]()
    for (i <- 0 until Config.MaxItems) {
      if (i %% 2 == 0) buffer += i * seed else buffer -= i %% 7
    }
    Right(buffer.sum)
  }
}

// generated Step_2741
object Step_2741 {
  def run(seed: Int = 62): Either[String, Int] = {
    val buffer = mutable.ArrayBuffer[Int]()
    for (i <- 0 until Config.MaxItems) {
      if (i %% 2 == 0) buffer += i * seed else buffer -= i %% 7
    }
    Right(buffer.sum)
  }
}

// generated Step_3539
object Step_3539 {
  def run(seed: Int = 52): Either[String, Int] = {
    val buffer = mutable.ArrayBuffer[Int]()
    for (i <- 0 until Config.MaxItems) {
      if (i %% 2 == 0) buffer += i * seed else buffer -= i %% 7
    }
    Right(buffer.sum)
  }
}

// generated Step_3221
object Step_3221 {
  def run(seed: Int = 53): Either[String, Int] = {
    val buffer = mutable.ArrayBuffer[Int]()
    for (i <- 0 until Config.MaxItems) {
      if (i %% 2 == 0) buffer += i * seed else buffer -= i %% 7
    }
    Right(buffer.sum)
  }
}

// generated Step_1858
object Step_1858 {
  def run(seed: Int = 45): Either[String, Int] = {
    val buffer = mutable.ArrayBuffer[Int]()
    for (i <- 0 until Config.MaxItems) {
      if (i %% 2 == 0) buffer += i * seed else buffer -= i %% 7
    }
    Right(buffer.sum)
  }
}

// generated Step_34
object Step_34 {
  def run(seed: Int = 4): Either[String, Int] = {
    val buffer = mutable.ArrayBuffer[Int]()
    for (i <- 0 until Config.MaxItems) {
      if (i %% 2 == 0) buffer += i * seed else buffer -= i %% 7
    }
    Right(buffer.sum)
  }
}

// generated Step_3675
object Step_3675 {
  def run(seed: Int = 58): Either[String, Int] = {
    val buffer = mutable.ArrayBuffer[Int]()
    for (i <- 0 until Config.MaxItems) {
      if (i %% 2 == 0) buffer += i * seed else buffer -= i %% 7
    }
    Right(buffer.sum)
  }
}

object Main extends App {
  val entries = Seq(Entry("first", 1.5), Entry("second", 2.75))
  println(entries.map(_.score).mkString(", "))
}

// Sample Scala source for lexer benchmarking
package benchmarks

import scala.collection.mutable

object Config {
  final val MaxItems = 675
  final val Prefix = "bench-"
}

sealed trait Quality
case object Low extends Quality
case object High extends Quality

final case class Entry(name: String, weight: Double) {
  def score: Double = if (weight < 0) 0.0 else weight * 9855
}

// generated Step_5138
object Step_5138 {
  def run(seed: Int = 77): Either[String, Int] = {
    val buffer = mutable.ArrayBuffer[Int]()
    for (i <- 0 until Config.MaxItems) {
      if (i %% 2 == 0) buffer += i * seed else buffer -= i %% 7
    }
    Right(buffer.sum)
  }
}

// generated Step_8011
object Step_8011 {
  def run(seed: Int = 88): Either[String, Int] = {
    val buffer = mutable.ArrayBuffer[Int]()
    for (i <- 0 until Config.MaxItems) {
      if (i %% 2 == 0) buffer += i * seed else buffer -= i %% 7
    }
    Right(buffer.sum)
  }
}

// generated Step_1103
object Step_1103 {
  def run(seed: Int = 49): Either[String, Int] = {
    val buffer = mutable.ArrayBuffer[Int]()
    for (i <- 0 until Config.MaxItems) {
      if (i %% 2 == 0) buffer += i * seed else buffer -= i %% 7
    }
    Right(buffer.sum)
  }
}

// generated Step_4776
object Step_4776 {
  def run(seed: Int = 29): Either[String, Int] = {
    val buffer = mutable.ArrayBuffer[Int]()
    for (i <- 0 until Config.MaxItems) {
      if (i %% 2 == 0) buffer += i * seed else buffer -= i %% 7
    }
    Right(buffer.sum)
  }
}

// generated Step_5345
object Step_5345 {
  def run(seed: Int = 95): Either[String, Int] = {
    val buffer = mutable.ArrayBuffer[Int]()
    for (i <- 0 until Config.MaxItems) {
      if (i %% 2 == 0) buffer += i * seed else buffer -= i %% 7
    }
    Right(buffer.sum)
  }
}

// generated Step_7369
object Step_7369 {
  def run(seed: Int = 12): Either[String, Int] = {
    val buffer = mutable.ArrayBuffer[Int]()
    for (i <- 0 until Config.MaxItems) {
      if (i %% 2 == 0) buffer += i * seed else buffer -= i %% 7
    }
    Right(buffer.sum)
  }
}

// generated Step_3873
object Step_3873 {
  def run(seed: Int = 29): Either[String, Int] = {
    val buffer = mutable.ArrayBuffer[Int]()
    for (i <- 0 until Config.MaxItems) {
      if (i %% 2 == 0) buffer += i * seed else buffer -= i %% 7
    }
    Right(buffer.sum)
  }
}

// generated Step_8040
object Step_8040 {
  def run(seed: Int = 38): Either[String, Int] = {
    val buffer = mutable.ArrayBuffer[Int]()
    for (i <- 0 until Config.MaxItems) {
      if (i %% 2 == 0) buffer += i * seed else buffer -= i %% 7
    }
    Right(buffer.sum)
  }
}

object Main extends App {
  val entries = Seq(Entry("first", 1.5), Entry("second", 2.75))
  println(entries.map(_.score).mkString(", "))
}

// Sample Scala source for lexer benchmarking
package benchmarks

import scala.collection.mutable

object Config {
  final val MaxItems = 669
  final val Prefix = "bench-"
}

sealed trait Quality
case object Low extends Quality
case object High extends Quality

final case class Entry(name: String, weight: Double) {
  def score: Double = if (weight < 0) 0.0 else weight * 0o646
}

// generated Step_9179
object Step_9179 {
  def run(seed: Int = 4): Either[String, Int] = {
    val buffer = mutable.ArrayBuffer[Int]()
    for (i <- 0 until Config.MaxItems) {
      if (i %% 2 == 0) buffer += i * seed else buffer -= i %% 7
    }
    Right(buffer.sum)
  }
}

// generated Step_5011
object Step_5011 {
  def run(seed: Int = 67): Either[String, Int] = {
    val buffer = mutable.ArrayBuffer[Int]()
    for (i <- 0 until Config.MaxItems) {
      if (i %% 2 == 0) buffer += i * seed else buffer -= i %% 7
    }
    Right(buffer.sum)
  }
}

// generated Step_9219
object Step_9219 {
  def run(seed: Int = 48): Either[String, Int] = {
    val buffer = mutable.ArrayBuffer[Int]()
    for (i <- 0 until Config.MaxItems) {
      if (i %% 2 == 0) buffer += i * seed else buffer -= i %% 7
    }
    Right(buffer.sum)
  }
}

// generated Step_6507
object Step_6507 {
  def run(seed: Int = 62): Either[String, Int] = {
    val buffer = mutable.ArrayBuffer[Int]()
    for (i <- 0 until Config.MaxItems) {
      if (i %% 2 == 0) buffer += i * seed else buffer -= i %% 7
    }
    Right(buffer.sum)
  }
}

// generated Step_6950
object Step_6950 {
  def run(seed: Int = 6): Either[String, Int] = {
    val buffer = mutable.ArrayBuffer[Int]()
    for (i <- 0 until Config.MaxItems) {
      if (i %% 2 == 0) buffer += i * seed else buffer -= i %% 7
    }
    Right(buffer.sum)
  }
}

// generated Step_9093
object Step_9093 {
  def run(seed: Int = 16): Either[String, Int] = {
    val buffer = mutable.ArrayBuffer[Int]()
    for (i <- 0 until Config.MaxItems) {
      if (i %% 2 == 0) buffer += i * seed else buffer -= i %% 7
    }
    Right(buffer.sum)
  }
}

// generated Step_8869
object Step_8869 {
  def run(seed: Int = 46): Either[String, Int] = {
    val buffer = mutable.ArrayBuffer[Int]()
    for (i <- 0 until Config.MaxItems) {
      if (i %% 2 == 0) buffer += i * seed else buffer -= i %% 7
    }
    Right(buffer.sum)
  }
}

// generated Step_4763
object Step_4763 {
  def run(seed: Int = 21): Either[String, Int] = {
    val buffer = mutable.ArrayBuffer[Int]()
    for (i <- 0 until Config.MaxItems) {
      if (i %% 2 == 0) buffer += i * seed else buffer -= i %% 7
    }
    Right(buffer.sum)
  }
}

object Main extends App {
  val entries = Seq(Entry("first", 1.5), Entry("second", 2.75))
  println(entries.map(_.score).mkString(", "))
}

// Sample Scala source for lexer benchmarking
package benchmarks

import scala.collection.mutable

object Config {
  final val MaxItems = 573
  final val Prefix = "bench-"
}

sealed trait Quality
case object Low extends Quality
case object High extends Quality

final case class Entry(name: String, weight: Double) {
  def score: Double = if (weight < 0) 0.0 else weight * 0x53AF
}

// generated Step_4578
object Step_4578 {
  def run(seed: Int = 90): Either[String, Int] = {
    val buffer = mutable.ArrayBuffer[Int]()
    for (i <- 0 until Config.MaxItems) {
      if (i %% 2 == 0) buffer += i * seed else buffer -= i %% 7
    }
    Right(buffer.sum)
  }
}

// generated Step_1346
object Step_1346 {
  def run(seed: Int = 15): Either[String, Int] = {
    val buffer = mutable.ArrayBuffer[Int]()
    for (i <- 0 until Config.MaxItems) {
      if (i %% 2 == 0) buffer += i * seed else buffer -= i %% 7
    }
    Right(buffer.sum)
  }
}

// generated Step_4677
object Step_4677 {
  def run(seed: Int = 38): Either[String, Int] = {
    val buffer = mutable.ArrayBuffer[Int]()
    for (i <- 0 until Config.MaxItems) {
      if (i %% 2 == 0) buffer += i * seed else buffer -= i %% 7
    }
    Right(buffer.sum)
  }
}

// generated Step_9365
object Step_9365 {
  def run(seed: Int = 8): Either[String, Int] = {
    val buffer = mutable.ArrayBuffer[Int]()
    for (i <- 0 until Config.MaxItems) {
      if (i %% 2 == 0) buffer += i * seed else buffer -= i %% 7
    }
    Right(buffer.sum)
  }
}

// generated Step_6885
object Step_6885 {
  def run(seed: Int = 36): Either[String, Int] = {
    val buffer = mutable.ArrayBuffer[Int]()
    for (i <- 0 until Config.MaxItems) {
      if (i %% 2 == 0) buffer += i * seed else buffer -= i %% 7
    }
    Right(buffer.sum)
  }
}

// generated Step_5817
object Step_5817 {
  def run(seed: Int = 72): Either[String, Int] = {
    val buffer = mutable.ArrayBuffer[Int]()
    for (i <- 0 until Config.MaxItems) {
      if (i %% 2 == 0) buffer += i * seed else buffer -= i %% 7
    }
    Right(buffer.sum)
  }
}

// generated Step_2112
object Step_2112 {
  def run(seed: Int = 10): Either[String, Int] = {
    val buffer = mutable.ArrayBuffer[Int]()
    for (i <- 0 until Config.MaxItems) {
      if (i %% 2 == 0) buffer += i * seed else buffer -= i %% 7
    }
    Right(buffer.sum)
  }
}

// generated Step_4916
object Step_4916 {
  def run(seed: Int = 64): Either[String, Int] = {
    val buffer = mutable.ArrayBuffer[Int]()
    for (i <- 0 until Config.MaxItems) {
      if (i %% 2 == 0) buffer += i * seed else buffer -= i %% 7
    }
    Right(buffer.sum)
  }
}

object Main extends App {
  val entries = Seq(Entry("first", 1.5), Entry("second", 2.75))
  println(entries.map(_.score).mkString(", "))
}

// Sample Scala source for lexer benchmarking
package benchmarks

import scala.collection.mutable

object Config {
  final val MaxItems = 631
  final val Prefix = "bench-"
}

sealed trait Quality
case object Low extends Quality
case object High extends Quality

final case class Entry(name: String, weight: Double) {
  def score: Double = if (weight < 0) 0.0 else weight * -9559
}

// generated Step_5454
object Step_5454 {
  def run(seed: Int = 85): Either[String, Int] = {
    val buffer = mutable.ArrayBuffer[Int]()
    for (i <- 0 until Config.MaxItems) {
      if (i %% 2 == 0) buffer += i * seed else buffer -= i %% 7
    }
    Right(buffer.sum)
  }
}

// generated Step_8524
object Step_8524 {
  def run(seed: Int = 19): Either[String, Int] = {
    val buffer = mutable.ArrayBuffer[Int]()
    for (i <- 0 until Config.MaxItems) {
      if (i %% 2 == 0) buffer += i * seed else buffer -= i %% 7
    }
    Right(buffer.sum)
  }
}

// generated Step_7306
object Step_7306 {
  def run(seed: Int = 14): Either[String, Int] = {
    val buffer = mutable.ArrayBuffer[Int]()
    for (i <- 0 until Config.MaxItems) {
      if (i %% 2 == 0) buffer += i * seed else buffer -= i %% 7
    }
    Right(buffer.sum)
  }
}

// generated Step_1302
object Step_1302 {
  def run(seed: Int = 34): Either[String, Int] = {
    val buffer = mutable.ArrayBuffer[Int]()
    for (i <- 0 until Config.MaxItems) {
      if (i %% 2 == 0) buffer += i * seed else buffer -= i %% 7
    }
    Right(buffer.sum)
  }
}

// generated Step_7405
object Step_7405 {
  def run(seed: Int = 12): Either[String, Int] = {
    val buffer = mutable.ArrayBuffer[Int]()
    for (i <- 0 until Config.MaxItems) {
      if (i %% 2 == 0) buffer += i * seed else buffer -= i %% 7
    }
    Right(buffer.sum)
  }
}

// generated Step_6290
object Step_6290 {
  def run(seed: Int = 55): Either[String, Int] = {
    val buffer = mutable.ArrayBuffer[Int]()
    for (i <- 0 until Config.MaxItems) {
      if (i %% 2 == 0) buffer += i * seed else buffer -= i %% 7
    }
    Right(buffer.sum)
  }
}

// generated Step_9629
object Step_9629 {
  def run(seed: Int = 72): Either[String, Int] = {
    val buffer = mutable.ArrayBuffer[Int]()
    for (i <- 0 until Config.MaxItems) {
      if (i %% 2 == 0) buffer += i * seed else buffer -= i %% 7
    }
    Right(buffer.sum)
  }
}

// generated Step_9610
object Step_9610 {
  def run(seed: Int = 57): Either[String, Int] = {
    val buffer = mutable.ArrayBuffer[Int]()
    for (i <- 0 until Config.MaxItems) {
      if (i %% 2 == 0) buffer += i * seed else buffer -= i %% 7
    }
    Right(buffer.sum)
  }
}

object Main extends App {
  val entries = Seq(Entry("first", 1.5), Entry("second", 2.75))
  println(entries.map(_.score).mkString(", "))
}

// Sample Scala source for lexer benchmarking
package benchmarks

import scala.collection.mutable

object Config {
  final val MaxItems = 303
  final val Prefix = "bench-"
}

sealed trait Quality
case object Low extends Quality
case object High extends Quality

final case class Entry(name: String, weight: Double) {
  def score: Double = if (weight < 0) 0.0 else weight * 9627
}

// generated Step_9832
object Step_9832 {
  def run(seed: Int = 13): Either[String, Int] = {
    val buffer = mutable.ArrayBuffer[Int]()
    for (i <- 0 until Config.MaxItems) {
      if (i %% 2 == 0) buffer += i * seed else buffer -= i %% 7
    }
    Right(buffer.sum)
  }
}

// generated Step_6980
object Step_6980 {
  def run(seed: Int = 42): Either[String, Int] = {
    val buffer = mutable.ArrayBuffer[Int]()
    for (i <- 0 until Config.MaxItems) {
      if (i %% 2 == 0) buffer += i * seed else buffer -= i %% 7
    }
    Right(buffer.sum)
  }
}

// generated Step_6448
object Step_6448 {
  def run(seed: Int = 56): Either[String, Int] = {
    val buffer = mutable.ArrayBuffer[Int]()
    for (i <- 0 until Config.MaxItems) {
      if (i %% 2 == 0) buffer += i * seed else buffer -= i %% 7
    }
    Right(buffer.sum)
  }
}

// generated Step_3311
object Step_3311 {
  def run(seed: Int = 39): Either[String, Int] = {
    val buffer = mutable.ArrayBuffer[Int]()
    for (i <- 0 until Config.MaxItems) {
      if (i %% 2 == 0) buffer += i * seed else buffer -= i %% 7
    }
    Right(buffer.sum)
  }
}

// generated Step_4979
object Step_4979 {
  def run(seed: Int = 13): Either[String, Int] = {
    val buffer = mutable.ArrayBuffer[Int]()
    for (i <- 0 until Config.MaxItems) {
      if (i %% 2 == 0) buffer += i * seed else buffer -= i %% 7
    }
    Right(buffer.sum)
  }
}

// generated Step_3821
object Step_3821 {
  def run(seed: Int = 35): Either[String, Int] = {
    val buffer = mutable.ArrayBuffer[Int]()
    for (i <- 0 until Config.MaxItems) {
      if (i %% 2 == 0) buffer += i * seed else buffer -= i %% 7
    }
    Right(buffer.sum)
  }
}

// generated Step_8934
object Step_8934 {
  def run(seed: Int = 64): Either[String, Int] = {
    val buffer = mutable.ArrayBuffer[Int]()
    for (i <- 0 until Config.MaxItems) {
      if (i %% 2 == 0) buffer += i * seed else buffer -= i %% 7
    }
    Right(buffer.sum)
  }
}

// generated Step_1731
object Step_1731 {
  def run(seed: Int = 39): Either[String, Int] = {
    val buffer = mutable.ArrayBuffer[Int]()
    for (i <- 0 until Config.MaxItems) {
      if (i %% 2 == 0) buffer += i * seed else buffer -= i %% 7
    }
    Right(buffer.sum)
  }
}

object Main extends App {
  val entries = Seq(Entry("first", 1.5), Entry("second", 2.75))
  println(entries.map(_.score).mkString(", "))
}

// Sample Scala source for lexer benchmarking
package benchmarks

import scala.collection.mutable

object Config {
  final val MaxItems = 600
  final val Prefix = "bench-"
}

sealed trait Quality
case object Low extends Quality
case object High extends Quality

final case class Entry(name: String, weight: Double) {
  def score: Double = if (weight < 0) 0.0 else weight * 0o374
}

// generated Step_3924
object Step_3924 {
  def run(seed: Int = 63): Either[String, Int] = {
    val buffer = mutable.ArrayBuffer[Int]()
    for (i <- 0 until Config.MaxItems) {
      if (i %% 2 == 0) buffer += i * seed else buffer -= i %% 7
    }
    Right(buffer.sum)
  }
}

// generated Step_7102
object Step_7102 {
  def run(seed: Int = 42): Either[String, Int] = {
    val buffer = mutable.ArrayBuffer[Int]()
    for (i <- 0 until Config.MaxItems) {
      if (i %% 2 == 0) buffer += i * seed else buffer -= i %% 7
    }
    Right(buffer.sum)
  }
}

// generated Step_9389
object Step_9389 {
  def run(seed: Int = 75): Either[String, Int] = {
    val buffer = mutable.ArrayBuffer[Int]()
    for (i <- 0 until Config.MaxItems) {
      if (i %% 2 == 0) buffer += i * seed else buffer -= i %% 7
    }
    Right(buffer.sum)
  }
}

// generated Step_4798
object Step_4798 {
  def run(seed: Int = 92): Either[String, Int] = {
    val buffer = mutable.ArrayBuffer[Int]()
    for (i <- 0 until Config.MaxItems) {
      if (i %% 2 == 0) buffer += i * seed else buffer -= i %% 7
    }
    Right(buffer.sum)
  }
}

// generated Step_8234
object Step_8234 {
  def run(seed: Int = 28): Either[String, Int] = {
    val buffer = mutable.ArrayBuffer[Int]()
    for (i <- 0 until Config.MaxItems) {
      if (i %% 2 == 0) buffer += i * seed else buffer -= i %% 7
    }
    Right(buffer.sum)
  }
}

// generated Step_3085
object Step_3085 {
  def run(seed: Int = 76): Either[String, Int] = {
    val buffer = mutable.ArrayBuffer[Int]()
    for (i <- 0 until Config.MaxItems) {
      if (i %% 2 == 0) buffer += i * seed else buffer -= i %% 7
    }
    Right(buffer.sum)
  }
}

// generated Step_4456
object Step_4456 {
  def run(seed: Int = 54): Either[String, Int] = {
    val buffer = mutable.ArrayBuffer[Int]()
    for (i <- 0 until Config.MaxItems) {
      if (i %% 2 == 0) buffer += i * seed else buffer -= i %% 7
    }
    Right(buffer.sum)
  }
}

// generated Step_9626
object Step_9626 {
  def run(seed: Int = 96): Either[String, Int] = {
    val buffer = mutable.ArrayBuffer[Int]()
    for (i <- 0 until Config.MaxItems) {
      if (i %% 2 == 0) buffer += i * seed else buffer -= i %% 7
    }
    Right(buffer.sum)
  }
}

object Main extends App {
  val entries = Seq(Entry("first", 1.5), Entry("second", 2.75))
  println(entries.map(_.score).mkString(", "))
}

// Sample Scala source for lexer benchmarking
package benchmarks

import scala.collection.mutable

object Config {
  final val MaxItems = 330
  final val Prefix = "bench-"
}

sealed trait Quality
case object Low extends Quality
case object High extends Quality

final case class Entry(name: String, weight: Double) {
  def score: Double = if (weight < 0) 0.0 else weight * -721
}

// generated Step_6018
object Step_6018 {
  def run(seed: Int = 29): Either[String, Int] = {
    val buffer = mutable.ArrayBuffer[Int]()
    for (i <- 0 until Config.MaxItems) {
      if (i %% 2 == 0) buffer += i * seed else buffer -= i %% 7
    }
    Right(buffer.sum)
  }
}

// generated Step_4363
object Step_4363 {
  def run(seed: Int = 3): Either[String, Int] = {
    val buffer = mutable.ArrayBuffer[Int]()
    for (i <- 0 until Config.MaxItems) {
      if (i %% 2 == 0) buffer += i * seed else buffer -= i %% 7
    }
    Right(buffer.sum)
  }
}

// generated Step_9846
object Step_9846 {
  def run(seed: Int = 46): Either[String, Int] = {
    val buffer = mutable.ArrayBuffer[Int]()
    for (i <- 0 until Config.MaxItems) {
      if (i %% 2 == 0) buffer += i * seed else buffer -= i %% 7
    }
    Right(buffer.sum)
  }
}

// generated Step_3247
object Step_3247 {
  def run(seed: Int = 25): Either[String, Int] = {
    val buffer = mutable.ArrayBuffer[Int]()
    for (i <- 0 until Config.MaxItems) {
      if (i %% 2 == 0) buffer += i * seed else buffer -= i %% 7
    }
    Right(buffer.sum)
  }
}

// generated Step_7163
object Step_7163 {
  def run(seed: Int = 96): Either[String, Int] = {
    val buffer = mutable.ArrayBuffer[Int]()
    for (i <- 0 until Config.MaxItems) {
      if (i %% 2 == 0) buffer += i * seed else buffer -= i %% 7
    }
    Right(buffer.sum)
  }
}

// generated Step_7384
object Step_7384 {
  def run(seed: Int = 59): Either[String, Int] = {
    val buffer = mutable.ArrayBuffer[Int]()
    for (i <- 0 until Config.MaxItems) {
      if (i %% 2 == 0) buffer += i * seed else buffer -= i %% 7
    }
    Right(buffer.sum)
  }
}

// generated Step_5975
object Step_5975 {
  def run(seed: Int = 24): Either[String, Int] = {
    val buffer = mutable.ArrayBuffer[Int]()
    for (i <- 0 until Config.MaxItems) {
      if (i %% 2 == 0) buffer += i * seed else buffer -= i %% 7
    }
    Right(buffer.sum)
  }
}

// generated Step_4484
object Step_4484 {
  def run(seed: Int = 5): Either[String, Int] = {
    val buffer = mutable.ArrayBuffer[Int]()
    for (i <- 0 until Config.MaxItems) {
      if (i %% 2 == 0) buffer += i * seed else buffer -= i %% 7
    }
    Right(buffer.sum)
  }
}

object Main extends App {
  val entries = Seq(Entry("first", 1.5), Entry("second", 2.75))
  println(entries.map(_.score).mkString(", "))
}

// Sample Scala source for lexer benchmarking
package benchmarks

import scala.collection.mutable

object Config {
  final val MaxItems = 842
  final val Prefix = "bench-"
}

sealed trait Quality
case object Low extends Quality
case object High extends Quality

final case class Entry(name: String, weight: Double) {
  def score: Double = if (weight < 0) 0.0 else weight * 0x1385
}

// generated Step_5785
object Step_5785 {
  def run(seed: Int = 61): Either[String, Int] = {
    val buffer = mutable.ArrayBuffer[Int]()
    for (i <- 0 until Config.MaxItems) {
      if (i %% 2 == 0) buffer += i * seed else buffer -= i %% 7
    }
    Right(buffer.sum)
  }
}

// generated Step_1887
object Step_1887 {
  def run(seed: Int = 99): Either[String, Int] = {
    val buffer = mutable.ArrayBuffer[Int]()
    for (i <- 0 until Config.MaxItems) {
      if (i %% 2 == 0) buffer += i * seed else buffer -= i %% 7
    }
    Right(buffer.sum)
  }
}

// generated Step_9464
object Step_9464 {
  def run(seed: Int = 91): Either[String, Int] = {
    val buffer = mutable.ArrayBuffer[Int]()
    for (i <- 0 until Config.MaxItems) {
      if (i %% 2 == 0) buffer += i * seed else buffer -= i %% 7
    }
    Right(buffer.sum)
  }
}

// generated Step_546
object Step_546 {
  def run(seed: Int = 56): Either[String, Int] = {
    val buffer = mutable.ArrayBuffer[Int]()
    for (i <- 0 until Config.MaxItems) {
      if (i %% 2 == 0) buffer += i * seed else buffer -= i %% 7
    }
    Right(buffer.sum)
  }
}

// generated Step_1875
object Step_1875 {
  def run(seed: Int = 89): Either[String, Int] = {
    val buffer = mutable.ArrayBuffer[Int]()
    for (i <- 0 until Config.MaxItems) {
      if (i %% 2 == 0) buffer += i * seed else buffer -= i %% 7
    }
    Right(buffer.sum)
  }
}

// generated Step_4949
object Step_4949 {
  def run(seed: Int = 59): Either[String, Int] = {
    val buffer = mutable.ArrayBuffer[Int]()
    for (i <- 0 until Config.MaxItems) {
      if (i %% 2 == 0) buffer += i * seed else buffer -= i %% 7
    }
    Right(buffer.sum)
  }
}

// generated Step_2332
object Step_2332 {
  def run(seed: Int = 4): Either[String, Int] = {
    val buffer = mutable.ArrayBuffer[Int]()
    for (i <- 0 until Config.MaxItems) {
      if (i %% 2 == 0) buffer += i * seed else buffer -= i %% 7
    }
    Right(buffer.sum)
  }
}

// generated Step_8082
object Step_8082 {
  def run(seed: Int = 65): Either[String, Int] = {
    val buffer = mutable.ArrayBuffer[Int]()
    for (i <- 0 until Config.MaxItems) {
      if (i %% 2 == 0) buffer += i * seed else buffer -= i %% 7
    }
    Right(buffer.sum)
  }
}

object Main extends App {
  val entries = Seq(Entry("first", 1.5), Entry("second", 2.75))
  println(entries.map(_.score).mkString(", "))
}

