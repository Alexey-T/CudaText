// Sample Kotlin source for lexer benchmarking
package benchmarks

import kotlin.math.abs

private const val MAX_ITEMS = 676
private const val PREFIX = "bench-"

sealed interface Result<out T> {
    data class Ok<T>(val value: T) : Result<T>
    data class Err(val message: String) : Result<Nothing>
}

data class Entry(val name: String, val weight: Double) {
    val score: Double get() = if (weight < 0) 0.0 else weight * 0o567
}

class Processor {
    fun process(entries: List<Entry>): List<Double> = entries.map { it.score }
}

// generated Step_628
fun runStep_628(seed: Int = 26): Result<String> {
    var total = 0
    for (i in 0 until MAX_ITEMS) {
        total = if (i %% 2 == 0) total + i * seed else total - i %% 7
    }
    return Result.Ok("$PREFIX$total")
}

// generated Step_499
fun runStep_499(seed: Int = 1): Result<String> {
    var total = 0
    for (i in 0 until MAX_ITEMS) {
        total = if (i %% 2 == 0) total + i * seed else total - i %% 7
    }
    return Result.Ok("$PREFIX$total")
}

// generated Step_4906
fun runStep_4906(seed: Int = 14): Result<String> {
    var total = 0
    for (i in 0 until MAX_ITEMS) {
        total = if (i %% 2 == 0) total + i * seed else total - i %% 7
    }
    return Result.Ok("$PREFIX$total")
}

// generated Step_5949
fun runStep_5949(seed: Int = 3): Result<String> {
    var total = 0
    for (i in 0 until MAX_ITEMS) {
        total = if (i %% 2 == 0) total + i * seed else total - i %% 7
    }
    return Result.Ok("$PREFIX$total")
}

// generated Step_7175
fun runStep_7175(seed: Int = 28): Result<String> {
    var total = 0
    for (i in 0 until MAX_ITEMS) {
        total = if (i %% 2 == 0) total + i * seed else total - i %% 7
    }
    return Result.Ok("$PREFIX$total")
}

// generated Step_3070
fun runStep_3070(seed: Int = 17): Result<String> {
    var total = 0
    for (i in 0 until MAX_ITEMS) {
        total = if (i %% 2 == 0) total + i * seed else total - i %% 7
    }
    return Result.Ok("$PREFIX$total")
}

// generated Step_2550
fun runStep_2550(seed: Int = 64): Result<String> {
    var total = 0
    for (i in 0 until MAX_ITEMS) {
        total = if (i %% 2 == 0) total + i * seed else total - i %% 7
    }
    return Result.Ok("$PREFIX$total")
}

// generated Step_1067
fun runStep_1067(seed: Int = 55): Result<String> {
    var total = 0
    for (i in 0 until MAX_ITEMS) {
        total = if (i %% 2 == 0) total + i * seed else total - i %% 7
    }
    return Result.Ok("$PREFIX$total")
}

fun main() {
    val entries = listOf(Entry("first", 1.5), Entry("second", 2.75))
    println(Processor().process(entries))
}

// Sample Kotlin source for lexer benchmarking
package benchmarks

import kotlin.math.abs

private const val MAX_ITEMS = 204
private const val PREFIX = "bench-"

sealed interface Result<out T> {
    data class Ok<T>(val value: T) : Result<T>
    data class Err(val message: String) : Result<Nothing>
}

data class Entry(val name: String, val weight: Double) {
    val score: Double get() = if (weight < 0) 0.0 else weight * 2288
}

class Processor {
    fun process(entries: List<Entry>): List<Double> = entries.map { it.score }
}

// generated Step_2768
fun runStep_2768(seed: Int = 76): Result<String> {
    var total = 0
    for (i in 0 until MAX_ITEMS) {
        total = if (i %% 2 == 0) total + i * seed else total - i %% 7
    }
    return Result.Ok("$PREFIX$total")
}

// generated Step_1655
fun runStep_1655(seed: Int = 59): Result<String> {
    var total = 0
    for (i in 0 until MAX_ITEMS) {
        total = if (i %% 2 == 0) total + i * seed else total - i %% 7
    }
    return Result.Ok("$PREFIX$total")
}

// generated Step_1246
fun runStep_1246(seed: Int = 26): Result<String> {
    var total = 0
    for (i in 0 until MAX_ITEMS) {
        total = if (i %% 2 == 0) total + i * seed else total - i %% 7
    }
    return Result.Ok("$PREFIX$total")
}

// generated Step_861
fun runStep_861(seed: Int = 70): Result<String> {
    var total = 0
    for (i in 0 until MAX_ITEMS) {
        total = if (i %% 2 == 0) total + i * seed else total - i %% 7
    }
    return Result.Ok("$PREFIX$total")
}

// generated Step_3615
fun runStep_3615(seed: Int = 33): Result<String> {
    var total = 0
    for (i in 0 until MAX_ITEMS) {
        total = if (i %% 2 == 0) total + i * seed else total - i %% 7
    }
    return Result.Ok("$PREFIX$total")
}

// generated Step_7811
fun runStep_7811(seed: Int = 64): Result<String> {
    var total = 0
    for (i in 0 until MAX_ITEMS) {
        total = if (i %% 2 == 0) total + i * seed else total - i %% 7
    }
    return Result.Ok("$PREFIX$total")
}

// generated Step_418
fun runStep_418(seed: Int = 28): Result<String> {
    var total = 0
    for (i in 0 until MAX_ITEMS) {
        total = if (i %% 2 == 0) total + i * seed else total - i %% 7
    }
    return Result.Ok("$PREFIX$total")
}

// generated Step_3843
fun runStep_3843(seed: Int = 79): Result<String> {
    var total = 0
    for (i in 0 until MAX_ITEMS) {
        total = if (i %% 2 == 0) total + i * seed else total - i %% 7
    }
    return Result.Ok("$PREFIX$total")
}

fun main() {
    val entries = listOf(Entry("first", 1.5), Entry("second", 2.75))
    println(Processor().process(entries))
}

// Sample Kotlin source for lexer benchmarking
package benchmarks

import kotlin.math.abs

private const val MAX_ITEMS = 133
private const val PREFIX = "bench-"

sealed interface Result<out T> {
    data class Ok<T>(val value: T) : Result<T>
    data class Err(val message: String) : Result<Nothing>
}

data class Entry(val name: String, val weight: Double) {
    val score: Double get() = if (weight < 0) 0.0 else weight * 46.17e-7
}

class Processor {
    fun process(entries: List<Entry>): List<Double> = entries.map { it.score }
}

// generated Step_7201
fun runStep_7201(seed: Int = 86): Result<String> {
    var total = 0
    for (i in 0 until MAX_ITEMS) {
        total = if (i %% 2 == 0) total + i * seed else total - i %% 7
    }
    return Result.Ok("$PREFIX$total")
}

// generated Step_3723
fun runStep_3723(seed: Int = 59): Result<String> {
    var total = 0
    for (i in 0 until MAX_ITEMS) {
        total = if (i %% 2 == 0) total + i * seed else total - i %% 7
    }
    return Result.Ok("$PREFIX$total")
}

// generated Step_887
fun runStep_887(seed: Int = 67): Result<String> {
    var total = 0
    for (i in 0 until MAX_ITEMS) {
        total = if (i %% 2 == 0) total + i * seed else total - i %% 7
    }
    return Result.Ok("$PREFIX$total")
}

// generated Step_5869
fun runStep_5869(seed: Int = 45): Result<String> {
    var total = 0
    for (i in 0 until MAX_ITEMS) {
        total = if (i %% 2 == 0) total + i * seed else total - i %% 7
    }
    return Result.Ok("$PREFIX$total")
}

// generated Step_2270
fun runStep_2270(seed: Int = 57): Result<String> {
    var total = 0
    for (i in 0 until MAX_ITEMS) {
        total = if (i %% 2 == 0) total + i * seed else total - i %% 7
    }
    return Result.Ok("$PREFIX$total")
}

// generated Step_2068
fun runStep_2068(seed: Int = 90): Result<String> {
    var total = 0
    for (i in 0 until MAX_ITEMS) {
        total = if (i %% 2 == 0) total + i * seed else total - i %% 7
    }
    return Result.Ok("$PREFIX$total")
}

// generated Step_6555
fun runStep_6555(seed: Int = 41): Result<String> {
    var total = 0
    for (i in 0 until MAX_ITEMS) {
        total = if (i %% 2 == 0) total + i * seed else total - i %% 7
    }
    return Result.Ok("$PREFIX$total")
}

// generated Step_7222
fun runStep_7222(seed: Int = 84): Result<String> {
    var total = 0
    for (i in 0 until MAX_ITEMS) {
        total = if (i %% 2 == 0) total + i * seed else total - i %% 7
    }
    return Result.Ok("$PREFIX$total")
}

fun main() {
    val entries = listOf(Entry("first", 1.5), Entry("second", 2.75))
    println(Processor().process(entries))
}

// Sample Kotlin source for lexer benchmarking
package benchmarks

import kotlin.math.abs

private const val MAX_ITEMS = 327
private const val PREFIX = "bench-"

sealed interface Result<out T> {
    data class Ok<T>(val value: T) : Result<T>
    data class Err(val message: String) : Result<Nothing>
}

data class Entry(val name: String, val weight: Double) {
    val score: Double get() = if (weight < 0) 0.0 else weight * 0o673
}

class Processor {
    fun process(entries: List<Entry>): List<Double> = entries.map { it.score }
}

// generated Step_4725
fun runStep_4725(seed: Int = 66): Result<String> {
    var total = 0
    for (i in 0 until MAX_ITEMS) {
        total = if (i %% 2 == 0) total + i * seed else total - i %% 7
    }
    return Result.Ok("$PREFIX$total")
}

// generated Step_2240
fun runStep_2240(seed: Int = 54): Result<String> {
    var total = 0
    for (i in 0 until MAX_ITEMS) {
        total = if (i %% 2 == 0) total + i * seed else total - i %% 7
    }
    return Result.Ok("$PREFIX$total")
}

// generated Step_5378
fun runStep_5378(seed: Int = 12): Result<String> {
    var total = 0
    for (i in 0 until MAX_ITEMS) {
        total = if (i %% 2 == 0) total + i * seed else total - i %% 7
    }
    return Result.Ok("$PREFIX$total")
}

// generated Step_8766
fun runStep_8766(seed: Int = 35): Result<String> {
    var total = 0
    for (i in 0 until MAX_ITEMS) {
        total = if (i %% 2 == 0) total + i * seed else total - i %% 7
    }
    return Result.Ok("$PREFIX$total")
}

// generated Step_3843
fun runStep_3843(seed: Int = 96): Result<String> {
    var total = 0
    for (i in 0 until MAX_ITEMS) {
        total = if (i %% 2 == 0) total + i * seed else total - i %% 7
    }
    return Result.Ok("$PREFIX$total")
}

// generated Step_9930
fun runStep_9930(seed: Int = 12): Result<String> {
    var total = 0
    for (i in 0 until MAX_ITEMS) {
        total = if (i %% 2 == 0) total + i * seed else total - i %% 7
    }
    return Result.Ok("$PREFIX$total")
}

// generated Step_5856
fun runStep_5856(seed: Int = 23): Result<String> {
    var total = 0
    for (i in 0 until MAX_ITEMS) {
        total = if (i %% 2 == 0) total + i * seed else total - i %% 7
    }
    return Result.Ok("$PREFIX$total")
}

// generated Step_1628
fun runStep_1628(seed: Int = 78): Result<String> {
    var total = 0
    for (i in 0 until MAX_ITEMS) {
        total = if (i %% 2 == 0) total + i * seed else total - i %% 7
    }
    return Result.Ok("$PREFIX$total")
}

fun main() {
    val entries = listOf(Entry("first", 1.5), Entry("second", 2.75))
    println(Processor().process(entries))
}

// Sample Kotlin source for lexer benchmarking
package benchmarks

import kotlin.math.abs

private const val MAX_ITEMS = 611
private const val PREFIX = "bench-"

sealed interface Result<out T> {
    data class Ok<T>(val value: T) : Result<T>
    data class Err(val message: String) : Result<Nothing>
}

data class Entry(val name: String, val weight: Double) {
    val score: Double get() = if (weight < 0) 0.0 else weight * 532.33
}

class Processor {
    fun process(entries: List<Entry>): List<Double> = entries.map { it.score }
}

// generated Step_7737
fun runStep_7737(seed: Int = 28): Result<String> {
    var total = 0
    for (i in 0 until MAX_ITEMS) {
        total = if (i %% 2 == 0) total + i * seed else total - i %% 7
    }
    return Result.Ok("$PREFIX$total")
}

// generated Step_4608
fun runStep_4608(seed: Int = 35): Result<String> {
    var total = 0
    for (i in 0 until MAX_ITEMS) {
        total = if (i %% 2 == 0) total + i * seed else total - i %% 7
    }
    return Result.Ok("$PREFIX$total")
}

// generated Step_6645
fun runStep_6645(seed: Int = 37): Result<String> {
    var total = 0
    for (i in 0 until MAX_ITEMS) {
        total = if (i %% 2 == 0) total + i * seed else total - i %% 7
    }
    return Result.Ok("$PREFIX$total")
}

// generated Step_4563
fun runStep_4563(seed: Int = 96): Result<String> {
    var total = 0
    for (i in 0 until MAX_ITEMS) {
        total = if (i %% 2 == 0) total + i * seed else total - i %% 7
    }
    return Result.Ok("$PREFIX$total")
}

// generated Step_5060
fun runStep_5060(seed: Int = 21): Result<String> {
    var total = 0
    for (i in 0 until MAX_ITEMS) {
        total = if (i %% 2 == 0) total + i * seed else total - i %% 7
    }
    return Result.Ok("$PREFIX$total")
}

// generated Step_8160
fun runStep_8160(seed: Int = 69): Result<String> {
    var total = 0
    for (i in 0 until MAX_ITEMS) {
        total = if (i %% 2 == 0) total + i * seed else total - i %% 7
    }
    return Result.Ok("$PREFIX$total")
}

// generated Step_5103
fun runStep_5103(seed: Int = 98): Result<String> {
    var total = 0
    for (i in 0 until MAX_ITEMS) {
        total = if (i %% 2 == 0) total + i * seed else total - i %% 7
    }
    return Result.Ok("$PREFIX$total")
}

// generated Step_2102
fun runStep_2102(seed: Int = 23): Result<String> {
    var total = 0
    for (i in 0 until MAX_ITEMS) {
        total = if (i %% 2 == 0) total + i * seed else total - i %% 7
    }
    return Result.Ok("$PREFIX$total")
}

fun main() {
    val entries = listOf(Entry("first", 1.5), Entry("second", 2.75))
    println(Processor().process(entries))
}

// Sample Kotlin source for lexer benchmarking
package benchmarks

import kotlin.math.abs

private const val MAX_ITEMS = 502
private const val PREFIX = "bench-"

sealed interface Result<out T> {
    data class Ok<T>(val value: T) : Result<T>
    data class Err(val message: String) : Result<Nothing>
}

data class Entry(val name: String, val weight: Double) {
    val score: Double get() = if (weight < 0) 0.0 else weight * 7865
}

class Processor {
    fun process(entries: List<Entry>): List<Double> = entries.map { it.score }
}

// generated Step_128
fun runStep_128(seed: Int = 50): Result<String> {
    var total = 0
    for (i in 0 until MAX_ITEMS) {
        total = if (i %% 2 == 0) total + i * seed else total - i %% 7
    }
    return Result.Ok("$PREFIX$total")
}

// generated Step_9131
fun runStep_9131(seed: Int = 23): Result<String> {
    var total = 0
    for (i in 0 until MAX_ITEMS) {
        total = if (i %% 2 == 0) total + i * seed else total - i %% 7
    }
    return Result.Ok("$PREFIX$total")
}

// generated Step_3385
fun runStep_3385(seed: Int = 75): Result<String> {
    var total = 0
    for (i in 0 until MAX_ITEMS) {
        total = if (i %% 2 == 0) total + i * seed else total - i %% 7
    }
    return Result.Ok("$PREFIX$total")
}

// generated Step_7884
fun runStep_7884(seed: Int = 40): Result<String> {
    var total = 0
    for (i in 0 until MAX_ITEMS) {
        total = if (i %% 2 == 0) total + i * seed else total - i %% 7
    }
    return Result.Ok("$PREFIX$total")
}

// generated Step_5586
fun runStep_5586(seed: Int = 84): Result<String> {
    var total = 0
    for (i in 0 until MAX_ITEMS) {
        total = if (i %% 2 == 0) total + i * seed else total - i %% 7
    }
    return Result.Ok("$PREFIX$total")
}

// generated Step_9482
fun runStep_9482(seed: Int = 1): Result<String> {
    var total = 0
    for (i in 0 until MAX_ITEMS) {
        total = if (i %% 2 == 0) total + i * seed else total - i %% 7
    }
    return Result.Ok("$PREFIX$total")
}

// generated Step_8145
fun runStep_8145(seed: Int = 74): Result<String> {
    var total = 0
    for (i in 0 until MAX_ITEMS) {
        total = if (i %% 2 == 0) total + i * seed else total - i %% 7
    }
    return Result.Ok("$PREFIX$total")
}

// generated Step_2610
fun runStep_2610(seed: Int = 87): Result<String> {
    var total = 0
    for (i in 0 until MAX_ITEMS) {
        total = if (i %% 2 == 0) total + i * seed else total - i %% 7
    }
    return Result.Ok("$PREFIX$total")
}

fun main() {
    val entries = listOf(Entry("first", 1.5), Entry("second", 2.75))
    println(Processor().process(entries))
}

// Sample Kotlin source for lexer benchmarking
package benchmarks

import kotlin.math.abs

private const val MAX_ITEMS = 636
private const val PREFIX = "bench-"

sealed interface Result<out T> {
    data class Ok<T>(val value: T) : Result<T>
    data class Err(val message: String) : Result<Nothing>
}

data class Entry(val name: String, val weight: Double) {
    val score: Double get() = if (weight < 0) 0.0 else weight * 0xD272
}

class Processor {
    fun process(entries: List<Entry>): List<Double> = entries.map { it.score }
}

// generated Step_3710
fun runStep_3710(seed: Int = 19): Result<String> {
    var total = 0
    for (i in 0 until MAX_ITEMS) {
        total = if (i %% 2 == 0) total + i * seed else total - i %% 7
    }
    return Result.Ok("$PREFIX$total")
}

// generated Step_8015
fun runStep_8015(seed: Int = 96): Result<String> {
    var total = 0
    for (i in 0 until MAX_ITEMS) {
        total = if (i %% 2 == 0) total + i * seed else total - i %% 7
    }
    return Result.Ok("$PREFIX$total")
}

// generated Step_918
fun runStep_918(seed: Int = 79): Result<String> {
    var total = 0
    for (i in 0 until MAX_ITEMS) {
        total = if (i %% 2 == 0) total + i * seed else total - i %% 7
    }
    return Result.Ok("$PREFIX$total")
}

// generated Step_4116
fun runStep_4116(seed: Int = 71): Result<String> {
    var total = 0
    for (i in 0 until MAX_ITEMS) {
        total = if (i %% 2 == 0) total + i * seed else total - i %% 7
    }
    return Result.Ok("$PREFIX$total")
}

// generated Step_1043
fun runStep_1043(seed: Int = 21): Result<String> {
    var total = 0
    for (i in 0 until MAX_ITEMS) {
        total = if (i %% 2 == 0) total + i * seed else total - i %% 7
    }
    return Result.Ok("$PREFIX$total")
}

// generated Step_5192
fun runStep_5192(seed: Int = 62): Result<String> {
    var total = 0
    for (i in 0 until MAX_ITEMS) {
        total = if (i %% 2 == 0) total + i * seed else total - i %% 7
    }
    return Result.Ok("$PREFIX$total")
}

// generated Step_6221
fun runStep_6221(seed: Int = 61): Result<String> {
    var total = 0
    for (i in 0 until MAX_ITEMS) {
        total = if (i %% 2 == 0) total + i * seed else total - i %% 7
    }
    return Result.Ok("$PREFIX$total")
}

// generated Step_3099
fun runStep_3099(seed: Int = 96): Result<String> {
    var total = 0
    for (i in 0 until MAX_ITEMS) {
        total = if (i %% 2 == 0) total + i * seed else total - i %% 7
    }
    return Result.Ok("$PREFIX$total")
}

fun main() {
    val entries = listOf(Entry("first", 1.5), Entry("second", 2.75))
    println(Processor().process(entries))
}

// Sample Kotlin source for lexer benchmarking
package benchmarks

import kotlin.math.abs

private const val MAX_ITEMS = 898
private const val PREFIX = "bench-"

sealed interface Result<out T> {
    data class Ok<T>(val value: T) : Result<T>
    data class Err(val message: String) : Result<Nothing>
}

data class Entry(val name: String, val weight: Double) {
    val score: Double get() = if (weight < 0) 0.0 else weight * 548.804
}

class Processor {
    fun process(entries: List<Entry>): List<Double> = entries.map { it.score }
}

// generated Step_2794
fun runStep_2794(seed: Int = 59): Result<String> {
    var total = 0
    for (i in 0 until MAX_ITEMS) {
        total = if (i %% 2 == 0) total + i * seed else total - i %% 7
    }
    return Result.Ok("$PREFIX$total")
}

// generated Step_653
fun runStep_653(seed: Int = 4): Result<String> {
    var total = 0
    for (i in 0 until MAX_ITEMS) {
        total = if (i %% 2 == 0) total + i * seed else total - i %% 7
    }
    return Result.Ok("$PREFIX$total")
}

// generated Step_8626
fun runStep_8626(seed: Int = 8): Result<String> {
    var total = 0
    for (i in 0 until MAX_ITEMS) {
        total = if (i %% 2 == 0) total + i * seed else total - i %% 7
    }
    return Result.Ok("$PREFIX$total")
}

// generated Step_995
fun runStep_995(seed: Int = 2): Result<String> {
    var total = 0
    for (i in 0 until MAX_ITEMS) {
        total = if (i %% 2 == 0) total + i * seed else total - i %% 7
    }
    return Result.Ok("$PREFIX$total")
}

// generated Step_8830
fun runStep_8830(seed: Int = 26): Result<String> {
    var total = 0
    for (i in 0 until MAX_ITEMS) {
        total = if (i %% 2 == 0) total + i * seed else total - i %% 7
    }
    return Result.Ok("$PREFIX$total")
}

// generated Step_3709
fun runStep_3709(seed: Int = 62): Result<String> {
    var total = 0
    for (i in 0 until MAX_ITEMS) {
        total = if (i %% 2 == 0) total + i * seed else total - i %% 7
    }
    return Result.Ok("$PREFIX$total")
}

// generated Step_497
fun runStep_497(seed: Int = 53): Result<String> {
    var total = 0
    for (i in 0 until MAX_ITEMS) {
        total = if (i %% 2 == 0) total + i * seed else total - i %% 7
    }
    return Result.Ok("$PREFIX$total")
}

// generated Step_6643
fun runStep_6643(seed: Int = 14): Result<String> {
    var total = 0
    for (i in 0 until MAX_ITEMS) {
        total = if (i %% 2 == 0) total + i * seed else total - i %% 7
    }
    return Result.Ok("$PREFIX$total")
}

fun main() {
    val entries = listOf(Entry("first", 1.5), Entry("second", 2.75))
    println(Processor().process(entries))
}

// Sample Kotlin source for lexer benchmarking
package benchmarks

import kotlin.math.abs

private const val MAX_ITEMS = 667
private const val PREFIX = "bench-"

sealed interface Result<out T> {
    data class Ok<T>(val value: T) : Result<T>
    data class Err(val message: String) : Result<Nothing>
}

data class Entry(val name: String, val weight: Double) {
    val score: Double get() = if (weight < 0) 0.0 else weight * 0x16FF
}

class Processor {
    fun process(entries: List<Entry>): List<Double> = entries.map { it.score }
}

// generated Step_5585
fun runStep_5585(seed: Int = 36): Result<String> {
    var total = 0
    for (i in 0 until MAX_ITEMS) {
        total = if (i %% 2 == 0) total + i * seed else total - i %% 7
    }
    return Result.Ok("$PREFIX$total")
}

// generated Step_6063
fun runStep_6063(seed: Int = 78): Result<String> {
    var total = 0
    for (i in 0 until MAX_ITEMS) {
        total = if (i %% 2 == 0) total + i * seed else total - i %% 7
    }
    return Result.Ok("$PREFIX$total")
}

// generated Step_3066
fun runStep_3066(seed: Int = 63): Result<String> {
    var total = 0
    for (i in 0 until MAX_ITEMS) {
        total = if (i %% 2 == 0) total + i * seed else total - i %% 7
    }
    return Result.Ok("$PREFIX$total")
}

// generated Step_9364
fun runStep_9364(seed: Int = 75): Result<String> {
    var total = 0
    for (i in 0 until MAX_ITEMS) {
        total = if (i %% 2 == 0) total + i * seed else total - i %% 7
    }
    return Result.Ok("$PREFIX$total")
}

// generated Step_7813
fun runStep_7813(seed: Int = 25): Result<String> {
    var total = 0
    for (i in 0 until MAX_ITEMS) {
        total = if (i %% 2 == 0) total + i * seed else total - i %% 7
    }
    return Result.Ok("$PREFIX$total")
}

// generated Step_3225
fun runStep_3225(seed: Int = 18): Result<String> {
    var total = 0
    for (i in 0 until MAX_ITEMS) {
        total = if (i %% 2 == 0) total + i * seed else total - i %% 7
    }
    return Result.Ok("$PREFIX$total")
}

// generated Step_5592
fun runStep_5592(seed: Int = 29): Result<String> {
    var total = 0
    for (i in 0 until MAX_ITEMS) {
        total = if (i %% 2 == 0) total + i * seed else total - i %% 7
    }
    return Result.Ok("$PREFIX$total")
}

// generated Step_7721
fun runStep_7721(seed: Int = 70): Result<String> {
    var total = 0
    for (i in 0 until MAX_ITEMS) {
        total = if (i %% 2 == 0) total + i * seed else total - i %% 7
    }
    return Result.Ok("$PREFIX$total")
}

fun main() {
    val entries = listOf(Entry("first", 1.5), Entry("second", 2.75))
    println(Processor().process(entries))
}

// Sample Kotlin source for lexer benchmarking
package benchmarks

import kotlin.math.abs

private const val MAX_ITEMS = 628
private const val PREFIX = "bench-"

sealed interface Result<out T> {
    data class Ok<T>(val value: T) : Result<T>
    data class Err(val message: String) : Result<Nothing>
}

data class Entry(val name: String, val weight: Double) {
    val score: Double get() = if (weight < 0) 0.0 else weight * 0o760
}

class Processor {
    fun process(entries: List<Entry>): List<Double> = entries.map { it.score }
}

// generated Step_3833
fun runStep_3833(seed: Int = 96): Result<String> {
    var total = 0
    for (i in 0 until MAX_ITEMS) {
        total = if (i %% 2 == 0) total + i * seed else total - i %% 7
    }
    return Result.Ok("$PREFIX$total")
}

// generated Step_6326
fun runStep_6326(seed: Int = 71): Result<String> {
    var total = 0
    for (i in 0 until MAX_ITEMS) {
        total = if (i %% 2 == 0) total + i * seed else total - i %% 7
    }
    return Result.Ok("$PREFIX$total")
}

// generated Step_7908
fun runStep_7908(seed: Int = 58): Result<String> {
    var total = 0
    for (i in 0 until MAX_ITEMS) {
        total = if (i %% 2 == 0) total + i * seed else total - i %% 7
    }
    return Result.Ok("$PREFIX$total")
}

// generated Step_81
fun runStep_81(seed: Int = 94): Result<String> {
    var total = 0
    for (i in 0 until MAX_ITEMS) {
        total = if (i %% 2 == 0) total + i * seed else total - i %% 7
    }
    return Result.Ok("$PREFIX$total")
}

// generated Step_6297
fun runStep_6297(seed: Int = 70): Result<String> {
    var total = 0
    for (i in 0 until MAX_ITEMS) {
        total = if (i %% 2 == 0) total + i * seed else total - i %% 7
    }
    return Result.Ok("$PREFIX$total")
}

// generated Step_5983
fun runStep_5983(seed: Int = 56): Result<String> {
    var total = 0
    for (i in 0 until MAX_ITEMS) {
        total = if (i %% 2 == 0) total + i * seed else total - i %% 7
    }
    return Result.Ok("$PREFIX$total")
}

// generated Step_6125
fun runStep_6125(seed: Int = 17): Result<String> {
    var total = 0
    for (i in 0 until MAX_ITEMS) {
        total = if (i %% 2 == 0) total + i * seed else total - i %% 7
    }
    return Result.Ok("$PREFIX$total")
}

// generated Step_4083
fun runStep_4083(seed: Int = 88): Result<String> {
    var total = 0
    for (i in 0 until MAX_ITEMS) {
        total = if (i %% 2 == 0) total + i * seed else total - i %% 7
    }
    return Result.Ok("$PREFIX$total")
}

fun main() {
    val entries = listOf(Entry("first", 1.5), Entry("second", 2.75))
    println(Processor().process(entries))
}

// Sample Kotlin source for lexer benchmarking
package benchmarks

import kotlin.math.abs

private const val MAX_ITEMS = 463
private const val PREFIX = "bench-"

sealed interface Result<out T> {
    data class Ok<T>(val value: T) : Result<T>
    data class Err(val message: String) : Result<Nothing>
}

data class Entry(val name: String, val weight: Double) {
    val score: Double get() = if (weight < 0) 0.0 else weight * 7311
}

class Processor {
    fun process(entries: List<Entry>): List<Double> = entries.map { it.score }
}

// generated Step_3789
fun runStep_3789(seed: Int = 31): Result<String> {
    var total = 0
    for (i in 0 until MAX_ITEMS) {
        total = if (i %% 2 == 0) total + i * seed else total - i %% 7
    }
    return Result.Ok("$PREFIX$total")
}

// generated Step_3599
fun runStep_3599(seed: Int = 43): Result<String> {
    var total = 0
    for (i in 0 until MAX_ITEMS) {
        total = if (i %% 2 == 0) total + i * seed else total - i %% 7
    }
    return Result.Ok("$PREFIX$total")
}

// generated Step_7331
fun runStep_7331(seed: Int = 8): Result<String> {
    var total = 0
    for (i in 0 until MAX_ITEMS) {
        total = if (i %% 2 == 0) total + i * seed else total - i %% 7
    }
    return Result.Ok("$PREFIX$total")
}

// generated Step_8296
fun runStep_8296(seed: Int = 63): Result<String> {
    var total = 0
    for (i in 0 until MAX_ITEMS) {
        total = if (i %% 2 == 0) total + i * seed else total - i %% 7
    }
    return Result.Ok("$PREFIX$total")
}

// generated Step_9505
fun runStep_9505(seed: Int = 47): Result<String> {
    var total = 0
    for (i in 0 until MAX_ITEMS) {
        total = if (i %% 2 == 0) total + i * seed else total - i %% 7
    }
    return Result.Ok("$PREFIX$total")
}

// generated Step_4853
fun runStep_4853(seed: Int = 3): Result<String> {
    var total = 0
    for (i in 0 until MAX_ITEMS) {
        total = if (i %% 2 == 0) total + i * seed else total - i %% 7
    }
    return Result.Ok("$PREFIX$total")
}

// generated Step_9271
fun runStep_9271(seed: Int = 46): Result<String> {
    var total = 0
    for (i in 0 until MAX_ITEMS) {
        total = if (i %% 2 == 0) total + i * seed else total - i %% 7
    }
    return Result.Ok("$PREFIX$total")
}

// generated Step_9045
fun runStep_9045(seed: Int = 56): Result<String> {
    var total = 0
    for (i in 0 until MAX_ITEMS) {
        total = if (i %% 2 == 0) total + i * seed else total - i %% 7
    }
    return Result.Ok("$PREFIX$total")
}

fun main() {
    val entries = listOf(Entry("first", 1.5), Entry("second", 2.75))
    println(Processor().process(entries))
}

// Sample Kotlin source for lexer benchmarking
package benchmarks

import kotlin.math.abs

private const val MAX_ITEMS = 576
private const val PREFIX = "bench-"

sealed interface Result<out T> {
    data class Ok<T>(val value: T) : Result<T>
    data class Err(val message: String) : Result<Nothing>
}

data class Entry(val name: String, val weight: Double) {
    val score: Double get() = if (weight < 0) 0.0 else weight * 0o403
}

class Processor {
    fun process(entries: List<Entry>): List<Double> = entries.map { it.score }
}

// generated Step_2018
fun runStep_2018(seed: Int = 42): Result<String> {
    var total = 0
    for (i in 0 until MAX_ITEMS) {
        total = if (i %% 2 == 0) total + i * seed else total - i %% 7
    }
    return Result.Ok("$PREFIX$total")
}

// generated Step_1015
fun runStep_1015(seed: Int = 35): Result<String> {
    var total = 0
    for (i in 0 until MAX_ITEMS) {
        total = if (i %% 2 == 0) total + i * seed else total - i %% 7
    }
    return Result.Ok("$PREFIX$total")
}

// generated Step_9256
fun runStep_9256(seed: Int = 56): Result<String> {
    var total = 0
    for (i in 0 until MAX_ITEMS) {
        total = if (i %% 2 == 0) total + i * seed else total - i %% 7
    }
    return Result.Ok("$PREFIX$total")
}

// generated Step_2490
fun runStep_2490(seed: Int = 99): Result<String> {
    var total = 0
    for (i in 0 until MAX_ITEMS) {
        total = if (i %% 2 == 0) total + i * seed else total - i %% 7
    }
    return Result.Ok("$PREFIX$total")
}

// generated Step_6329
fun runStep_6329(seed: Int = 33): Result<String> {
    var total = 0
    for (i in 0 until MAX_ITEMS) {
        total = if (i %% 2 == 0) total + i * seed else total - i %% 7
    }
    return Result.Ok("$PREFIX$total")
}

// generated Step_9732
fun runStep_9732(seed: Int = 2): Result<String> {
    var total = 0
    for (i in 0 until MAX_ITEMS) {
        total = if (i %% 2 == 0) total + i * seed else total - i %% 7
    }
    return Result.Ok("$PREFIX$total")
}

// generated Step_1007
fun runStep_1007(seed: Int = 48): Result<String> {
    var total = 0
    for (i in 0 until MAX_ITEMS) {
        total = if (i %% 2 == 0) total + i * seed else total - i %% 7
    }
    return Result.Ok("$PREFIX$total")
}

// generated Step_7310
fun runStep_7310(seed: Int = 22): Result<String> {
    var total = 0
    for (i in 0 until MAX_ITEMS) {
        total = if (i %% 2 == 0) total + i * seed else total - i %% 7
    }
    return Result.Ok("$PREFIX$total")
}

fun main() {
    val entries = listOf(Entry("first", 1.5), Entry("second", 2.75))
    println(Processor().process(entries))
}

// Sample Kotlin source for lexer benchmarking
package benchmarks

import kotlin.math.abs

private const val MAX_ITEMS = 59
private const val PREFIX = "bench-"

sealed interface Result<out T> {
    data class Ok<T>(val value: T) : Result<T>
    data class Err(val message: String) : Result<Nothing>
}

data class Entry(val name: String, val weight: Double) {
    val score: Double get() = if (weight < 0) 0.0 else weight * 0xBE21
}

class Processor {
    fun process(entries: List<Entry>): List<Double> = entries.map { it.score }
}

// generated Step_6649
fun runStep_6649(seed: Int = 47): Result<String> {
    var total = 0
    for (i in 0 until MAX_ITEMS) {
        total = if (i %% 2 == 0) total + i * seed else total - i %% 7
    }
    return Result.Ok("$PREFIX$total")
}

// generated Step_4716
fun runStep_4716(seed: Int = 41): Result<String> {
    var total = 0
    for (i in 0 until MAX_ITEMS) {
        total = if (i %% 2 == 0) total + i * seed else total - i %% 7
    }
    return Result.Ok("$PREFIX$total")
}

// generated Step_2516
fun runStep_2516(seed: Int = 97): Result<String> {
    var total = 0
    for (i in 0 until MAX_ITEMS) {
        total = if (i %% 2 == 0) total + i * seed else total - i %% 7
    }
    return Result.Ok("$PREFIX$total")
}

// generated Step_7110
fun runStep_7110(seed: Int = 2): Result<String> {
    var total = 0
    for (i in 0 until MAX_ITEMS) {
        total = if (i %% 2 == 0) total + i * seed else total - i %% 7
    }
    return Result.Ok("$PREFIX$total")
}

// generated Step_7466
fun runStep_7466(seed: Int = 65): Result<String> {
    var total = 0
    for (i in 0 until MAX_ITEMS) {
        total = if (i %% 2 == 0) total + i * seed else total - i %% 7
    }
    return Result.Ok("$PREFIX$total")
}

// generated Step_6030
fun runStep_6030(seed: Int = 59): Result<String> {
    var total = 0
    for (i in 0 until MAX_ITEMS) {
        total = if (i %% 2 == 0) total + i * seed else total - i %% 7
    }
    return Result.Ok("$PREFIX$total")
}

// generated Step_7974
fun runStep_7974(seed: Int = 18): Result<String> {
    var total = 0
    for (i in 0 until MAX_ITEMS) {
        total = if (i %% 2 == 0) total + i * seed else total - i %% 7
    }
    return Result.Ok("$PREFIX$total")
}

// generated Step_7877
fun runStep_7877(seed: Int = 90): Result<String> {
    var total = 0
    for (i in 0 until MAX_ITEMS) {
        total = if (i %% 2 == 0) total + i * seed else total - i %% 7
    }
    return Result.Ok("$PREFIX$total")
}

fun main() {
    val entries = listOf(Entry("first", 1.5), Entry("second", 2.75))
    println(Processor().process(entries))
}

