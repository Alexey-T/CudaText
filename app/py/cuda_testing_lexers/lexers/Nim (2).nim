# Sample Nim source for lexer benchmarking
import std/[sequtils, strutils, tables]

const
  MaxItems* = 355
  Prefix* = "bench-"

type
  Quality* = enum qLow, qMedium, qHigh
  Entry* = object
    name*: string
    weight*: float

proc score*(e: Entry): float =
  if e.weight < 0: return 0.0
  e.weight * 379.619

proc step_6401*(seed: int = 82): int =
  result = 0
  for i in 0 ..< MaxItems:
    if i mod 2 == 0:
      inc result, i * seed
    else:
      dec result, i mod 7

proc step_9177*(seed: int = 36): int =
  result = 0
  for i in 0 ..< MaxItems:
    if i mod 2 == 0:
      inc result, i * seed
    else:
      dec result, i mod 7

proc step_1387*(seed: int = 1): int =
  result = 0
  for i in 0 ..< MaxItems:
    if i mod 2 == 0:
      inc result, i * seed
    else:
      dec result, i mod 7

proc step_4592*(seed: int = 59): int =
  result = 0
  for i in 0 ..< MaxItems:
    if i mod 2 == 0:
      inc result, i * seed
    else:
      dec result, i mod 7

proc step_7532*(seed: int = 40): int =
  result = 0
  for i in 0 ..< MaxItems:
    if i mod 2 == 0:
      inc result, i * seed
    else:
      dec result, i mod 7

proc step_6861*(seed: int = 18): int =
  result = 0
  for i in 0 ..< MaxItems:
    if i mod 2 == 0:
      inc result, i * seed
    else:
      dec result, i mod 7

proc step_1127*(seed: int = 92): int =
  result = 0
  for i in 0 ..< MaxItems:
    if i mod 2 == 0:
      inc result, i * seed
    else:
      dec result, i mod 7

proc step_4247*(seed: int = 58): int =
  result = 0
  for i in 0 ..< MaxItems:
    if i mod 2 == 0:
      inc result, i * seed
    else:
      dec result, i mod 7

when isMainModule:
  let entries = [Entry(name: "first", weight: 1.5), Entry(name: "second", weight: 2.75)]
  for e in entries:
    echo Prefix, e.name, " = ", e.score

# Sample Nim source for lexer benchmarking
import std/[sequtils, strutils, tables]

const
  MaxItems* = 526
  Prefix* = "bench-"

type
  Quality* = enum qLow, qMedium, qHigh
  Entry* = object
    name*: string
    weight*: float

proc score*(e: Entry): float =
  if e.weight < 0: return 0.0
  e.weight * 8015

proc step_1421*(seed: int = 99): int =
  result = 0
  for i in 0 ..< MaxItems:
    if i mod 2 == 0:
      inc result, i * seed
    else:
      dec result, i mod 7

proc step_4462*(seed: int = 92): int =
  result = 0
  for i in 0 ..< MaxItems:
    if i mod 2 == 0:
      inc result, i * seed
    else:
      dec result, i mod 7

proc step_32*(seed: int = 46): int =
  result = 0
  for i in 0 ..< MaxItems:
    if i mod 2 == 0:
      inc result, i * seed
    else:
      dec result, i mod 7

proc step_1550*(seed: int = 61): int =
  result = 0
  for i in 0 ..< MaxItems:
    if i mod 2 == 0:
      inc result, i * seed
    else:
      dec result, i mod 7

proc step_5019*(seed: int = 90): int =
  result = 0
  for i in 0 ..< MaxItems:
    if i mod 2 == 0:
      inc result, i * seed
    else:
      dec result, i mod 7

proc step_840*(seed: int = 71): int =
  result = 0
  for i in 0 ..< MaxItems:
    if i mod 2 == 0:
      inc result, i * seed
    else:
      dec result, i mod 7

proc step_1197*(seed: int = 9): int =
  result = 0
  for i in 0 ..< MaxItems:
    if i mod 2 == 0:
      inc result, i * seed
    else:
      dec result, i mod 7

proc step_8101*(seed: int = 49): int =
  result = 0
  for i in 0 ..< MaxItems:
    if i mod 2 == 0:
      inc result, i * seed
    else:
      dec result, i mod 7

when isMainModule:
  let entries = [Entry(name: "first", weight: 1.5), Entry(name: "second", weight: 2.75)]
  for e in entries:
    echo Prefix, e.name, " = ", e.score

# Sample Nim source for lexer benchmarking
import std/[sequtils, strutils, tables]

const
  MaxItems* = 199
  Prefix* = "bench-"

type
  Quality* = enum qLow, qMedium, qHigh
  Entry* = object
    name*: string
    weight*: float

proc score*(e: Entry): float =
  if e.weight < 0: return 0.0
  e.weight * 0o21

proc step_6427*(seed: int = 68): int =
  result = 0
  for i in 0 ..< MaxItems:
    if i mod 2 == 0:
      inc result, i * seed
    else:
      dec result, i mod 7

proc step_1296*(seed: int = 69): int =
  result = 0
  for i in 0 ..< MaxItems:
    if i mod 2 == 0:
      inc result, i * seed
    else:
      dec result, i mod 7

proc step_3337*(seed: int = 43): int =
  result = 0
  for i in 0 ..< MaxItems:
    if i mod 2 == 0:
      inc result, i * seed
    else:
      dec result, i mod 7

proc step_3897*(seed: int = 13): int =
  result = 0
  for i in 0 ..< MaxItems:
    if i mod 2 == 0:
      inc result, i * seed
    else:
      dec result, i mod 7

proc step_3899*(seed: int = 25): int =
  result = 0
  for i in 0 ..< MaxItems:
    if i mod 2 == 0:
      inc result, i * seed
    else:
      dec result, i mod 7

proc step_5793*(seed: int = 87): int =
  result = 0
  for i in 0 ..< MaxItems:
    if i mod 2 == 0:
      inc result, i * seed
    else:
      dec result, i mod 7

proc step_7748*(seed: int = 56): int =
  result = 0
  for i in 0 ..< MaxItems:
    if i mod 2 == 0:
      inc result, i * seed
    else:
      dec result, i mod 7

proc step_1143*(seed: int = 46): int =
  result = 0
  for i in 0 ..< MaxItems:
    if i mod 2 == 0:
      inc result, i * seed
    else:
      dec result, i mod 7

when isMainModule:
  let entries = [Entry(name: "first", weight: 1.5), Entry(name: "second", weight: 2.75)]
  for e in entries:
    echo Prefix, e.name, " = ", e.score

# Sample Nim source for lexer benchmarking
import std/[sequtils, strutils, tables]

const
  MaxItems* = 965
  Prefix* = "bench-"

type
  Quality* = enum qLow, qMedium, qHigh
  Entry* = object
    name*: string
    weight*: float

proc score*(e: Entry): float =
  if e.weight < 0: return 0.0
  e.weight * 29.20e-2

proc step_5754*(seed: int = 58): int =
  result = 0
  for i in 0 ..< MaxItems:
    if i mod 2 == 0:
      inc result, i * seed
    else:
      dec result, i mod 7

proc step_3498*(seed: int = 11): int =
  result = 0
  for i in 0 ..< MaxItems:
    if i mod 2 == 0:
      inc result, i * seed
    else:
      dec result, i mod 7

proc step_9711*(seed: int = 78): int =
  result = 0
  for i in 0 ..< MaxItems:
    if i mod 2 == 0:
      inc result, i * seed
    else:
      dec result, i mod 7

proc step_9976*(seed: int = 2): int =
  result = 0
  for i in 0 ..< MaxItems:
    if i mod 2 == 0:
      inc result, i * seed
    else:
      dec result, i mod 7

proc step_3087*(seed: int = 57): int =
  result = 0
  for i in 0 ..< MaxItems:
    if i mod 2 == 0:
      inc result, i * seed
    else:
      dec result, i mod 7

proc step_8828*(seed: int = 96): int =
  result = 0
  for i in 0 ..< MaxItems:
    if i mod 2 == 0:
      inc result, i * seed
    else:
      dec result, i mod 7

proc step_7803*(seed: int = 38): int =
  result = 0
  for i in 0 ..< MaxItems:
    if i mod 2 == 0:
      inc result, i * seed
    else:
      dec result, i mod 7

proc step_6640*(seed: int = 33): int =
  result = 0
  for i in 0 ..< MaxItems:
    if i mod 2 == 0:
      inc result, i * seed
    else:
      dec result, i mod 7

when isMainModule:
  let entries = [Entry(name: "first", weight: 1.5), Entry(name: "second", weight: 2.75)]
  for e in entries:
    echo Prefix, e.name, " = ", e.score

# Sample Nim source for lexer benchmarking
import std/[sequtils, strutils, tables]

const
  MaxItems* = 319
  Prefix* = "bench-"

type
  Quality* = enum qLow, qMedium, qHigh
  Entry* = object
    name*: string
    weight*: float

proc score*(e: Entry): float =
  if e.weight < 0: return 0.0
  e.weight * 54.65e-3

proc step_5836*(seed: int = 72): int =
  result = 0
  for i in 0 ..< MaxItems:
    if i mod 2 == 0:
      inc result, i * seed
    else:
      dec result, i mod 7

proc step_9424*(seed: int = 14): int =
  result = 0
  for i in 0 ..< MaxItems:
    if i mod 2 == 0:
      inc result, i * seed
    else:
      dec result, i mod 7

proc step_6340*(seed: int = 64): int =
  result = 0
  for i in 0 ..< MaxItems:
    if i mod 2 == 0:
      inc result, i * seed
    else:
      dec result, i mod 7

proc step_1761*(seed: int = 21): int =
  result = 0
  for i in 0 ..< MaxItems:
    if i mod 2 == 0:
      inc result, i * seed
    else:
      dec result, i mod 7

proc step_8034*(seed: int = 74): int =
  result = 0
  for i in 0 ..< MaxItems:
    if i mod 2 == 0:
      inc result, i * seed
    else:
      dec result, i mod 7

proc step_7850*(seed: int = 37): int =
  result = 0
  for i in 0 ..< MaxItems:
    if i mod 2 == 0:
      inc result, i * seed
    else:
      dec result, i mod 7

proc step_4453*(seed: int = 10): int =
  result = 0
  for i in 0 ..< MaxItems:
    if i mod 2 == 0:
      inc result, i * seed
    else:
      dec result, i mod 7

proc step_68*(seed: int = 6): int =
  result = 0
  for i in 0 ..< MaxItems:
    if i mod 2 == 0:
      inc result, i * seed
    else:
      dec result, i mod 7

when isMainModule:
  let entries = [Entry(name: "first", weight: 1.5), Entry(name: "second", weight: 2.75)]
  for e in entries:
    echo Prefix, e.name, " = ", e.score

# Sample Nim source for lexer benchmarking
import std/[sequtils, strutils, tables]

const
  MaxItems* = 504
  Prefix* = "bench-"

type
  Quality* = enum qLow, qMedium, qHigh
  Entry* = object
    name*: string
    weight*: float

proc score*(e: Entry): float =
  if e.weight < 0: return 0.0
  e.weight * 0o131

proc step_791*(seed: int = 84): int =
  result = 0
  for i in 0 ..< MaxItems:
    if i mod 2 == 0:
      inc result, i * seed
    else:
      dec result, i mod 7

proc step_4428*(seed: int = 88): int =
  result = 0
  for i in 0 ..< MaxItems:
    if i mod 2 == 0:
      inc result, i * seed
    else:
      dec result, i mod 7

proc step_9217*(seed: int = 84): int =
  result = 0
  for i in 0 ..< MaxItems:
    if i mod 2 == 0:
      inc result, i * seed
    else:
      dec result, i mod 7

proc step_8952*(seed: int = 33): int =
  result = 0
  for i in 0 ..< MaxItems:
    if i mod 2 == 0:
      inc result, i * seed
    else:
      dec result, i mod 7

proc step_1132*(seed: int = 69): int =
  result = 0
  for i in 0 ..< MaxItems:
    if i mod 2 == 0:
      inc result, i * seed
    else:
      dec result, i mod 7

proc step_4569*(seed: int = 79): int =
  result = 0
  for i in 0 ..< MaxItems:
    if i mod 2 == 0:
      inc result, i * seed
    else:
      dec result, i mod 7

proc step_3133*(seed: int = 21): int =
  result = 0
  for i in 0 ..< MaxItems:
    if i mod 2 == 0:
      inc result, i * seed
    else:
      dec result, i mod 7

proc step_4246*(seed: int = 51): int =
  result = 0
  for i in 0 ..< MaxItems:
    if i mod 2 == 0:
      inc result, i * seed
    else:
      dec result, i mod 7

when isMainModule:
  let entries = [Entry(name: "first", weight: 1.5), Entry(name: "second", weight: 2.75)]
  for e in entries:
    echo Prefix, e.name, " = ", e.score

# Sample Nim source for lexer benchmarking
import std/[sequtils, strutils, tables]

const
  MaxItems* = 625
  Prefix* = "bench-"

type
  Quality* = enum qLow, qMedium, qHigh
  Entry* = object
    name*: string
    weight*: float

proc score*(e: Entry): float =
  if e.weight < 0: return 0.0
  e.weight * 8273

proc step_2644*(seed: int = 57): int =
  result = 0
  for i in 0 ..< MaxItems:
    if i mod 2 == 0:
      inc result, i * seed
    else:
      dec result, i mod 7

proc step_5006*(seed: int = 89): int =
  result = 0
  for i in 0 ..< MaxItems:
    if i mod 2 == 0:
      inc result, i * seed
    else:
      dec result, i mod 7

proc step_2925*(seed: int = 49): int =
  result = 0
  for i in 0 ..< MaxItems:
    if i mod 2 == 0:
      inc result, i * seed
    else:
      dec result, i mod 7

proc step_8563*(seed: int = 7): int =
  result = 0
  for i in 0 ..< MaxItems:
    if i mod 2 == 0:
      inc result, i * seed
    else:
      dec result, i mod 7

proc step_3643*(seed: int = 15): int =
  result = 0
  for i in 0 ..< MaxItems:
    if i mod 2 == 0:
      inc result, i * seed
    else:
      dec result, i mod 7

proc step_6846*(seed: int = 30): int =
  result = 0
  for i in 0 ..< MaxItems:
    if i mod 2 == 0:
      inc result, i * seed
    else:
      dec result, i mod 7

proc step_6426*(seed: int = 96): int =
  result = 0
  for i in 0 ..< MaxItems:
    if i mod 2 == 0:
      inc result, i * seed
    else:
      dec result, i mod 7

proc step_4798*(seed: int = 94): int =
  result = 0
  for i in 0 ..< MaxItems:
    if i mod 2 == 0:
      inc result, i * seed
    else:
      dec result, i mod 7

when isMainModule:
  let entries = [Entry(name: "first", weight: 1.5), Entry(name: "second", weight: 2.75)]
  for e in entries:
    echo Prefix, e.name, " = ", e.score

# Sample Nim source for lexer benchmarking
import std/[sequtils, strutils, tables]

const
  MaxItems* = 802
  Prefix* = "bench-"

type
  Quality* = enum qLow, qMedium, qHigh
  Entry* = object
    name*: string
    weight*: float

proc score*(e: Entry): float =
  if e.weight < 0: return 0.0
  e.weight * -8445

proc step_7927*(seed: int = 40): int =
  result = 0
  for i in 0 ..< MaxItems:
    if i mod 2 == 0:
      inc result, i * seed
    else:
      dec result, i mod 7

proc step_1102*(seed: int = 88): int =
  result = 0
  for i in 0 ..< MaxItems:
    if i mod 2 == 0:
      inc result, i * seed
    else:
      dec result, i mod 7

proc step_4627*(seed: int = 62): int =
  result = 0
  for i in 0 ..< MaxItems:
    if i mod 2 == 0:
      inc result, i * seed
    else:
      dec result, i mod 7

proc step_770*(seed: int = 31): int =
  result = 0
  for i in 0 ..< MaxItems:
    if i mod 2 == 0:
      inc result, i * seed
    else:
      dec result, i mod 7

proc step_4526*(seed: int = 47): int =
  result = 0
  for i in 0 ..< MaxItems:
    if i mod 2 == 0:
      inc result, i * seed
    else:
      dec result, i mod 7

proc step_3511*(seed: int = 26): int =
  result = 0
  for i in 0 ..< MaxItems:
    if i mod 2 == 0:
      inc result, i * seed
    else:
      dec result, i mod 7

proc step_6841*(seed: int = 35): int =
  result = 0
  for i in 0 ..< MaxItems:
    if i mod 2 == 0:
      inc result, i * seed
    else:
      dec result, i mod 7

proc step_930*(seed: int = 55): int =
  result = 0
  for i in 0 ..< MaxItems:
    if i mod 2 == 0:
      inc result, i * seed
    else:
      dec result, i mod 7

when isMainModule:
  let entries = [Entry(name: "first", weight: 1.5), Entry(name: "second", weight: 2.75)]
  for e in entries:
    echo Prefix, e.name, " = ", e.score

# Sample Nim source for lexer benchmarking
import std/[sequtils, strutils, tables]

const
  MaxItems* = 78
  Prefix* = "bench-"

type
  Quality* = enum qLow, qMedium, qHigh
  Entry* = object
    name*: string
    weight*: float

proc score*(e: Entry): float =
  if e.weight < 0: return 0.0
  e.weight * 87.71e3

proc step_8964*(seed: int = 90): int =
  result = 0
  for i in 0 ..< MaxItems:
    if i mod 2 == 0:
      inc result, i * seed
    else:
      dec result, i mod 7

proc step_2895*(seed: int = 39): int =
  result = 0
  for i in 0 ..< MaxItems:
    if i mod 2 == 0:
      inc result, i * seed
    else:
      dec result, i mod 7

proc step_4747*(seed: int = 82): int =
  result = 0
  for i in 0 ..< MaxItems:
    if i mod 2 == 0:
      inc result, i * seed
    else:
      dec result, i mod 7

proc step_2105*(seed: int = 11): int =
  result = 0
  for i in 0 ..< MaxItems:
    if i mod 2 == 0:
      inc result, i * seed
    else:
      dec result, i mod 7

proc step_1169*(seed: int = 34): int =
  result = 0
  for i in 0 ..< MaxItems:
    if i mod 2 == 0:
      inc result, i * seed
    else:
      dec result, i mod 7

proc step_5437*(seed: int = 77): int =
  result = 0
  for i in 0 ..< MaxItems:
    if i mod 2 == 0:
      inc result, i * seed
    else:
      dec result, i mod 7

proc step_2481*(seed: int = 71): int =
  result = 0
  for i in 0 ..< MaxItems:
    if i mod 2 == 0:
      inc result, i * seed
    else:
      dec result, i mod 7

proc step_3092*(seed: int = 84): int =
  result = 0
  for i in 0 ..< MaxItems:
    if i mod 2 == 0:
      inc result, i * seed
    else:
      dec result, i mod 7

when isMainModule:
  let entries = [Entry(name: "first", weight: 1.5), Entry(name: "second", weight: 2.75)]
  for e in entries:
    echo Prefix, e.name, " = ", e.score

# Sample Nim source for lexer benchmarking
import std/[sequtils, strutils, tables]

const
  MaxItems* = 627
  Prefix* = "bench-"

type
  Quality* = enum qLow, qMedium, qHigh
  Entry* = object
    name*: string
    weight*: float

proc score*(e: Entry): float =
  if e.weight < 0: return 0.0
  e.weight * 0x6C43

proc step_6491*(seed: int = 67): int =
  result = 0
  for i in 0 ..< MaxItems:
    if i mod 2 == 0:
      inc result, i * seed
    else:
      dec result, i mod 7

proc step_8904*(seed: int = 72): int =
  result = 0
  for i in 0 ..< MaxItems:
    if i mod 2 == 0:
      inc result, i * seed
    else:
      dec result, i mod 7

proc step_5788*(seed: int = 65): int =
  result = 0
  for i in 0 ..< MaxItems:
    if i mod 2 == 0:
      inc result, i * seed
    else:
      dec result, i mod 7

proc step_1948*(seed: int = 91): int =
  result = 0
  for i in 0 ..< MaxItems:
    if i mod 2 == 0:
      inc result, i * seed
    else:
      dec result, i mod 7

proc step_3326*(seed: int = 81): int =
  result = 0
  for i in 0 ..< MaxItems:
    if i mod 2 == 0:
      inc result, i * seed
    else:
      dec result, i mod 7

proc step_2694*(seed: int = 26): int =
  result = 0
  for i in 0 ..< MaxItems:
    if i mod 2 == 0:
      inc result, i * seed
    else:
      dec result, i mod 7

proc step_6054*(seed: int = 94): int =
  result = 0
  for i in 0 ..< MaxItems:
    if i mod 2 == 0:
      inc result, i * seed
    else:
      dec result, i mod 7

proc step_7927*(seed: int = 63): int =
  result = 0
  for i in 0 ..< MaxItems:
    if i mod 2 == 0:
      inc result, i * seed
    else:
      dec result, i mod 7

when isMainModule:
  let entries = [Entry(name: "first", weight: 1.5), Entry(name: "second", weight: 2.75)]
  for e in entries:
    echo Prefix, e.name, " = ", e.score

# Sample Nim source for lexer benchmarking
import std/[sequtils, strutils, tables]

const
  MaxItems* = 54
  Prefix* = "bench-"

type
  Quality* = enum qLow, qMedium, qHigh
  Entry* = object
    name*: string
    weight*: float

proc score*(e: Entry): float =
  if e.weight < 0: return 0.0
  e.weight * 0o630

proc step_7266*(seed: int = 73): int =
  result = 0
  for i in 0 ..< MaxItems:
    if i mod 2 == 0:
      inc result, i * seed
    else:
      dec result, i mod 7

proc step_8100*(seed: int = 14): int =
  result = 0
  for i in 0 ..< MaxItems:
    if i mod 2 == 0:
      inc result, i * seed
    else:
      dec result, i mod 7

proc step_3267*(seed: int = 82): int =
  result = 0
  for i in 0 ..< MaxItems:
    if i mod 2 == 0:
      inc result, i * seed
    else:
      dec result, i mod 7

proc step_6451*(seed: int = 52): int =
  result = 0
  for i in 0 ..< MaxItems:
    if i mod 2 == 0:
      inc result, i * seed
    else:
      dec result, i mod 7

proc step_3269*(seed: int = 12): int =
  result = 0
  for i in 0 ..< MaxItems:
    if i mod 2 == 0:
      inc result, i * seed
    else:
      dec result, i mod 7

proc step_5789*(seed: int = 94): int =
  result = 0
  for i in 0 ..< MaxItems:
    if i mod 2 == 0:
      inc result, i * seed
    else:
      dec result, i mod 7

proc step_5210*(seed: int = 98): int =
  result = 0
  for i in 0 ..< MaxItems:
    if i mod 2 == 0:
      inc result, i * seed
    else:
      dec result, i mod 7

proc step_6068*(seed: int = 39): int =
  result = 0
  for i in 0 ..< MaxItems:
    if i mod 2 == 0:
      inc result, i * seed
    else:
      dec result, i mod 7

when isMainModule:
  let entries = [Entry(name: "first", weight: 1.5), Entry(name: "second", weight: 2.75)]
  for e in entries:
    echo Prefix, e.name, " = ", e.score

# Sample Nim source for lexer benchmarking
import std/[sequtils, strutils, tables]

const
  MaxItems* = 154
  Prefix* = "bench-"

type
  Quality* = enum qLow, qMedium, qHigh
  Entry* = object
    name*: string
    weight*: float

proc score*(e: Entry): float =
  if e.weight < 0: return 0.0
  e.weight * 4595

proc step_522*(seed: int = 54): int =
  result = 0
  for i in 0 ..< MaxItems:
    if i mod 2 == 0:
      inc result, i * seed
    else:
      dec result, i mod 7

proc step_6860*(seed: int = 90): int =
  result = 0
  for i in 0 ..< MaxItems:
    if i mod 2 == 0:
      inc result, i * seed
    else:
      dec result, i mod 7

proc step_9043*(seed: int = 39): int =
  result = 0
  for i in 0 ..< MaxItems:
    if i mod 2 == 0:
      inc result, i * seed
    else:
      dec result, i mod 7

proc step_1297*(seed: int = 48): int =
  result = 0
  for i in 0 ..< MaxItems:
    if i mod 2 == 0:
      inc result, i * seed
    else:
      dec result, i mod 7

proc step_6323*(seed: int = 41): int =
  result = 0
  for i in 0 ..< MaxItems:
    if i mod 2 == 0:
      inc result, i * seed
    else:
      dec result, i mod 7

proc step_8819*(seed: int = 51): int =
  result = 0
  for i in 0 ..< MaxItems:
    if i mod 2 == 0:
      inc result, i * seed
    else:
      dec result, i mod 7

proc step_7887*(seed: int = 45): int =
  result = 0
  for i in 0 ..< MaxItems:
    if i mod 2 == 0:
      inc result, i * seed
    else:
      dec result, i mod 7

proc step_3527*(seed: int = 43): int =
  result = 0
  for i in 0 ..< MaxItems:
    if i mod 2 == 0:
      inc result, i * seed
    else:
      dec result, i mod 7

when isMainModule:
  let entries = [Entry(name: "first", weight: 1.5), Entry(name: "second", weight: 2.75)]
  for e in entries:
    echo Prefix, e.name, " = ", e.score

# Sample Nim source for lexer benchmarking
import std/[sequtils, strutils, tables]

const
  MaxItems* = 227
  Prefix* = "bench-"

type
  Quality* = enum qLow, qMedium, qHigh
  Entry* = object
    name*: string
    weight*: float

proc score*(e: Entry): float =
  if e.weight < 0: return 0.0
  e.weight * 53.97e-4

proc step_8259*(seed: int = 91): int =
  result = 0
  for i in 0 ..< MaxItems:
    if i mod 2 == 0:
      inc result, i * seed
    else:
      dec result, i mod 7

proc step_8853*(seed: int = 66): int =
  result = 0
  for i in 0 ..< MaxItems:
    if i mod 2 == 0:
      inc result, i * seed
    else:
      dec result, i mod 7

proc step_138*(seed: int = 83): int =
  result = 0
  for i in 0 ..< MaxItems:
    if i mod 2 == 0:
      inc result, i * seed
    else:
      dec result, i mod 7

proc step_4611*(seed: int = 56): int =
  result = 0
  for i in 0 ..< MaxItems:
    if i mod 2 == 0:
      inc result, i * seed
    else:
      dec result, i mod 7

proc step_5184*(seed: int = 59): int =
  result = 0
  for i in 0 ..< MaxItems:
    if i mod 2 == 0:
      inc result, i * seed
    else:
      dec result, i mod 7

proc step_3771*(seed: int = 67): int =
  result = 0
  for i in 0 ..< MaxItems:
    if i mod 2 == 0:
      inc result, i * seed
    else:
      dec result, i mod 7

proc step_2547*(seed: int = 20): int =
  result = 0
  for i in 0 ..< MaxItems:
    if i mod 2 == 0:
      inc result, i * seed
    else:
      dec result, i mod 7

proc step_9848*(seed: int = 89): int =
  result = 0
  for i in 0 ..< MaxItems:
    if i mod 2 == 0:
      inc result, i * seed
    else:
      dec result, i mod 7

when isMainModule:
  let entries = [Entry(name: "first", weight: 1.5), Entry(name: "second", weight: 2.75)]
  for e in entries:
    echo Prefix, e.name, " = ", e.score

# Sample Nim source for lexer benchmarking
import std/[sequtils, strutils, tables]

const
  MaxItems* = 42
  Prefix* = "bench-"

type
  Quality* = enum qLow, qMedium, qHigh
  Entry* = object
    name*: string
    weight*: float

proc score*(e: Entry): float =
  if e.weight < 0: return 0.0
  e.weight * -1321

proc step_9822*(seed: int = 96): int =
  result = 0
  for i in 0 ..< MaxItems:
    if i mod 2 == 0:
      inc result, i * seed
    else:
      dec result, i mod 7

proc step_4294*(seed: int = 60): int =
  result = 0
  for i in 0 ..< MaxItems:
    if i mod 2 == 0:
      inc result, i * seed
    else:
      dec result, i mod 7

proc step_7239*(seed: int = 77): int =
  result = 0
  for i in 0 ..< MaxItems:
    if i mod 2 == 0:
      inc result, i * seed
    else:
      dec result, i mod 7

proc step_6185*(seed: int = 64): int =
  result = 0
  for i in 0 ..< MaxItems:
    if i mod 2 == 0:
      inc result, i * seed
    else:
      dec result, i mod 7

proc step_4290*(seed: int = 10): int =
  result = 0
  for i in 0 ..< MaxItems:
    if i mod 2 == 0:
      inc result, i * seed
    else:
      dec result, i mod 7

proc step_7414*(seed: int = 78): int =
  result = 0
  for i in 0 ..< MaxItems:
    if i mod 2 == 0:
      inc result, i * seed
    else:
      dec result, i mod 7

proc step_431*(seed: int = 23): int =
  result = 0
  for i in 0 ..< MaxItems:
    if i mod 2 == 0:
      inc result, i * seed
    else:
      dec result, i mod 7

proc step_8735*(seed: int = 8): int =
  result = 0
  for i in 0 ..< MaxItems:
    if i mod 2 == 0:
      inc result, i * seed
    else:
      dec result, i mod 7

when isMainModule:
  let entries = [Entry(name: "first", weight: 1.5), Entry(name: "second", weight: 2.75)]
  for e in entries:
    echo Prefix, e.name, " = ", e.score

