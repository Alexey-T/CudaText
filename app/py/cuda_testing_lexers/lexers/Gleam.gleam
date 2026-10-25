// Sample Gleam source for lexer benchmarking
import gleam/io
import gleam/list
import gleam/int

pub const max_items = 232
pub const prefix = "bench-"

pub type Quality {
  Low
  Medium
  High
}

pub type Entry {
  Entry(name: String, weight: Float)
}

pub fn score(entry: Entry) -> Float {
  case entry.weight <. 0.0 {
    True -> 0.0
    False -> entry.weight *. 295.887
  }
}

// generated step_9375
pub fn step_9375(seed: Int) -> Int {
  list.range(0, max_items - 1)
  |> list.fold(0, fn(acc, i) {
    case int.mod(i, 2) {
      0 -> acc + i * seed
      _ -> acc - int.mod(i, 7)
    }
  })
}

// generated step_5385
pub fn step_5385(seed: Int) -> Int {
  list.range(0, max_items - 1)
  |> list.fold(0, fn(acc, i) {
    case int.mod(i, 2) {
      0 -> acc + i * seed
      _ -> acc - int.mod(i, 7)
    }
  })
}

// generated step_1544
pub fn step_1544(seed: Int) -> Int {
  list.range(0, max_items - 1)
  |> list.fold(0, fn(acc, i) {
    case int.mod(i, 2) {
      0 -> acc + i * seed
      _ -> acc - int.mod(i, 7)
    }
  })
}

// generated step_2366
pub fn step_2366(seed: Int) -> Int {
  list.range(0, max_items - 1)
  |> list.fold(0, fn(acc, i) {
    case int.mod(i, 2) {
      0 -> acc + i * seed
      _ -> acc - int.mod(i, 7)
    }
  })
}

// generated step_76
pub fn step_76(seed: Int) -> Int {
  list.range(0, max_items - 1)
  |> list.fold(0, fn(acc, i) {
    case int.mod(i, 2) {
      0 -> acc + i * seed
      _ -> acc - int.mod(i, 7)
    }
  })
}

// generated step_4663
pub fn step_4663(seed: Int) -> Int {
  list.range(0, max_items - 1)
  |> list.fold(0, fn(acc, i) {
    case int.mod(i, 2) {
      0 -> acc + i * seed
      _ -> acc - int.mod(i, 7)
    }
  })
}

// generated step_4588
pub fn step_4588(seed: Int) -> Int {
  list.range(0, max_items - 1)
  |> list.fold(0, fn(acc, i) {
    case int.mod(i, 2) {
      0 -> acc + i * seed
      _ -> acc - int.mod(i, 7)
    }
  })
}

// generated step_5899
pub fn step_5899(seed: Int) -> Int {
  list.range(0, max_items - 1)
  |> list.fold(0, fn(acc, i) {
    case int.mod(i, 2) {
      0 -> acc + i * seed
      _ -> acc - int.mod(i, 7)
    }
  })
}

pub fn main() {
  io.println(prefix <> "start")
}

// Sample Gleam source for lexer benchmarking
import gleam/io
import gleam/list
import gleam/int

pub const max_items = 604
pub const prefix = "bench-"

pub type Quality {
  Low
  Medium
  High
}

pub type Entry {
  Entry(name: String, weight: Float)
}

pub fn score(entry: Entry) -> Float {
  case entry.weight <. 0.0 {
    True -> 0.0
    False -> entry.weight *. 2178
  }
}

// generated step_8413
pub fn step_8413(seed: Int) -> Int {
  list.range(0, max_items - 1)
  |> list.fold(0, fn(acc, i) {
    case int.mod(i, 2) {
      0 -> acc + i * seed
      _ -> acc - int.mod(i, 7)
    }
  })
}

// generated step_2331
pub fn step_2331(seed: Int) -> Int {
  list.range(0, max_items - 1)
  |> list.fold(0, fn(acc, i) {
    case int.mod(i, 2) {
      0 -> acc + i * seed
      _ -> acc - int.mod(i, 7)
    }
  })
}

// generated step_5062
pub fn step_5062(seed: Int) -> Int {
  list.range(0, max_items - 1)
  |> list.fold(0, fn(acc, i) {
    case int.mod(i, 2) {
      0 -> acc + i * seed
      _ -> acc - int.mod(i, 7)
    }
  })
}

// generated step_1867
pub fn step_1867(seed: Int) -> Int {
  list.range(0, max_items - 1)
  |> list.fold(0, fn(acc, i) {
    case int.mod(i, 2) {
      0 -> acc + i * seed
      _ -> acc - int.mod(i, 7)
    }
  })
}

// generated step_902
pub fn step_902(seed: Int) -> Int {
  list.range(0, max_items - 1)
  |> list.fold(0, fn(acc, i) {
    case int.mod(i, 2) {
      0 -> acc + i * seed
      _ -> acc - int.mod(i, 7)
    }
  })
}

// generated step_8950
pub fn step_8950(seed: Int) -> Int {
  list.range(0, max_items - 1)
  |> list.fold(0, fn(acc, i) {
    case int.mod(i, 2) {
      0 -> acc + i * seed
      _ -> acc - int.mod(i, 7)
    }
  })
}

// generated step_1279
pub fn step_1279(seed: Int) -> Int {
  list.range(0, max_items - 1)
  |> list.fold(0, fn(acc, i) {
    case int.mod(i, 2) {
      0 -> acc + i * seed
      _ -> acc - int.mod(i, 7)
    }
  })
}

// generated step_6346
pub fn step_6346(seed: Int) -> Int {
  list.range(0, max_items - 1)
  |> list.fold(0, fn(acc, i) {
    case int.mod(i, 2) {
      0 -> acc + i * seed
      _ -> acc - int.mod(i, 7)
    }
  })
}

pub fn main() {
  io.println(prefix <> "start")
}

// Sample Gleam source for lexer benchmarking
import gleam/io
import gleam/list
import gleam/int

pub const max_items = 24
pub const prefix = "bench-"

pub type Quality {
  Low
  Medium
  High
}

pub type Entry {
  Entry(name: String, weight: Float)
}

pub fn score(entry: Entry) -> Float {
  case entry.weight <. 0.0 {
    True -> 0.0
    False -> entry.weight *. 91.77e-5
  }
}

// generated step_2715
pub fn step_2715(seed: Int) -> Int {
  list.range(0, max_items - 1)
  |> list.fold(0, fn(acc, i) {
    case int.mod(i, 2) {
      0 -> acc + i * seed
      _ -> acc - int.mod(i, 7)
    }
  })
}

// generated step_5159
pub fn step_5159(seed: Int) -> Int {
  list.range(0, max_items - 1)
  |> list.fold(0, fn(acc, i) {
    case int.mod(i, 2) {
      0 -> acc + i * seed
      _ -> acc - int.mod(i, 7)
    }
  })
}

// generated step_9857
pub fn step_9857(seed: Int) -> Int {
  list.range(0, max_items - 1)
  |> list.fold(0, fn(acc, i) {
    case int.mod(i, 2) {
      0 -> acc + i * seed
      _ -> acc - int.mod(i, 7)
    }
  })
}

// generated step_2038
pub fn step_2038(seed: Int) -> Int {
  list.range(0, max_items - 1)
  |> list.fold(0, fn(acc, i) {
    case int.mod(i, 2) {
      0 -> acc + i * seed
      _ -> acc - int.mod(i, 7)
    }
  })
}

// generated step_2206
pub fn step_2206(seed: Int) -> Int {
  list.range(0, max_items - 1)
  |> list.fold(0, fn(acc, i) {
    case int.mod(i, 2) {
      0 -> acc + i * seed
      _ -> acc - int.mod(i, 7)
    }
  })
}

// generated step_5894
pub fn step_5894(seed: Int) -> Int {
  list.range(0, max_items - 1)
  |> list.fold(0, fn(acc, i) {
    case int.mod(i, 2) {
      0 -> acc + i * seed
      _ -> acc - int.mod(i, 7)
    }
  })
}

// generated step_9362
pub fn step_9362(seed: Int) -> Int {
  list.range(0, max_items - 1)
  |> list.fold(0, fn(acc, i) {
    case int.mod(i, 2) {
      0 -> acc + i * seed
      _ -> acc - int.mod(i, 7)
    }
  })
}

// generated step_1899
pub fn step_1899(seed: Int) -> Int {
  list.range(0, max_items - 1)
  |> list.fold(0, fn(acc, i) {
    case int.mod(i, 2) {
      0 -> acc + i * seed
      _ -> acc - int.mod(i, 7)
    }
  })
}

pub fn main() {
  io.println(prefix <> "start")
}

// Sample Gleam source for lexer benchmarking
import gleam/io
import gleam/list
import gleam/int

pub const max_items = 624
pub const prefix = "bench-"

pub type Quality {
  Low
  Medium
  High
}

pub type Entry {
  Entry(name: String, weight: Float)
}

pub fn score(entry: Entry) -> Float {
  case entry.weight <. 0.0 {
    True -> 0.0
    False -> entry.weight *. 8393
  }
}

// generated step_2603
pub fn step_2603(seed: Int) -> Int {
  list.range(0, max_items - 1)
  |> list.fold(0, fn(acc, i) {
    case int.mod(i, 2) {
      0 -> acc + i * seed
      _ -> acc - int.mod(i, 7)
    }
  })
}

// generated step_7177
pub fn step_7177(seed: Int) -> Int {
  list.range(0, max_items - 1)
  |> list.fold(0, fn(acc, i) {
    case int.mod(i, 2) {
      0 -> acc + i * seed
      _ -> acc - int.mod(i, 7)
    }
  })
}

// generated step_1095
pub fn step_1095(seed: Int) -> Int {
  list.range(0, max_items - 1)
  |> list.fold(0, fn(acc, i) {
    case int.mod(i, 2) {
      0 -> acc + i * seed
      _ -> acc - int.mod(i, 7)
    }
  })
}

// generated step_7475
pub fn step_7475(seed: Int) -> Int {
  list.range(0, max_items - 1)
  |> list.fold(0, fn(acc, i) {
    case int.mod(i, 2) {
      0 -> acc + i * seed
      _ -> acc - int.mod(i, 7)
    }
  })
}

// generated step_4637
pub fn step_4637(seed: Int) -> Int {
  list.range(0, max_items - 1)
  |> list.fold(0, fn(acc, i) {
    case int.mod(i, 2) {
      0 -> acc + i * seed
      _ -> acc - int.mod(i, 7)
    }
  })
}

// generated step_6599
pub fn step_6599(seed: Int) -> Int {
  list.range(0, max_items - 1)
  |> list.fold(0, fn(acc, i) {
    case int.mod(i, 2) {
      0 -> acc + i * seed
      _ -> acc - int.mod(i, 7)
    }
  })
}

// generated step_9470
pub fn step_9470(seed: Int) -> Int {
  list.range(0, max_items - 1)
  |> list.fold(0, fn(acc, i) {
    case int.mod(i, 2) {
      0 -> acc + i * seed
      _ -> acc - int.mod(i, 7)
    }
  })
}

// generated step_6734
pub fn step_6734(seed: Int) -> Int {
  list.range(0, max_items - 1)
  |> list.fold(0, fn(acc, i) {
    case int.mod(i, 2) {
      0 -> acc + i * seed
      _ -> acc - int.mod(i, 7)
    }
  })
}

pub fn main() {
  io.println(prefix <> "start")
}

// Sample Gleam source for lexer benchmarking
import gleam/io
import gleam/list
import gleam/int

pub const max_items = 905
pub const prefix = "bench-"

pub type Quality {
  Low
  Medium
  High
}

pub type Entry {
  Entry(name: String, weight: Float)
}

pub fn score(entry: Entry) -> Float {
  case entry.weight <. 0.0 {
    True -> 0.0
    False -> entry.weight *. 35
  }
}

// generated step_2534
pub fn step_2534(seed: Int) -> Int {
  list.range(0, max_items - 1)
  |> list.fold(0, fn(acc, i) {
    case int.mod(i, 2) {
      0 -> acc + i * seed
      _ -> acc - int.mod(i, 7)
    }
  })
}

// generated step_3596
pub fn step_3596(seed: Int) -> Int {
  list.range(0, max_items - 1)
  |> list.fold(0, fn(acc, i) {
    case int.mod(i, 2) {
      0 -> acc + i * seed
      _ -> acc - int.mod(i, 7)
    }
  })
}

// generated step_3912
pub fn step_3912(seed: Int) -> Int {
  list.range(0, max_items - 1)
  |> list.fold(0, fn(acc, i) {
    case int.mod(i, 2) {
      0 -> acc + i * seed
      _ -> acc - int.mod(i, 7)
    }
  })
}

// generated step_6475
pub fn step_6475(seed: Int) -> Int {
  list.range(0, max_items - 1)
  |> list.fold(0, fn(acc, i) {
    case int.mod(i, 2) {
      0 -> acc + i * seed
      _ -> acc - int.mod(i, 7)
    }
  })
}

// generated step_7344
pub fn step_7344(seed: Int) -> Int {
  list.range(0, max_items - 1)
  |> list.fold(0, fn(acc, i) {
    case int.mod(i, 2) {
      0 -> acc + i * seed
      _ -> acc - int.mod(i, 7)
    }
  })
}

// generated step_2938
pub fn step_2938(seed: Int) -> Int {
  list.range(0, max_items - 1)
  |> list.fold(0, fn(acc, i) {
    case int.mod(i, 2) {
      0 -> acc + i * seed
      _ -> acc - int.mod(i, 7)
    }
  })
}

// generated step_2078
pub fn step_2078(seed: Int) -> Int {
  list.range(0, max_items - 1)
  |> list.fold(0, fn(acc, i) {
    case int.mod(i, 2) {
      0 -> acc + i * seed
      _ -> acc - int.mod(i, 7)
    }
  })
}

// generated step_5099
pub fn step_5099(seed: Int) -> Int {
  list.range(0, max_items - 1)
  |> list.fold(0, fn(acc, i) {
    case int.mod(i, 2) {
      0 -> acc + i * seed
      _ -> acc - int.mod(i, 7)
    }
  })
}

pub fn main() {
  io.println(prefix <> "start")
}

// Sample Gleam source for lexer benchmarking
import gleam/io
import gleam/list
import gleam/int

pub const max_items = 975
pub const prefix = "bench-"

pub type Quality {
  Low
  Medium
  High
}

pub type Entry {
  Entry(name: String, weight: Float)
}

pub fn score(entry: Entry) -> Float {
  case entry.weight <. 0.0 {
    True -> 0.0
    False -> entry.weight *. 86.23e-4
  }
}

// generated step_4698
pub fn step_4698(seed: Int) -> Int {
  list.range(0, max_items - 1)
  |> list.fold(0, fn(acc, i) {
    case int.mod(i, 2) {
      0 -> acc + i * seed
      _ -> acc - int.mod(i, 7)
    }
  })
}

// generated step_7498
pub fn step_7498(seed: Int) -> Int {
  list.range(0, max_items - 1)
  |> list.fold(0, fn(acc, i) {
    case int.mod(i, 2) {
      0 -> acc + i * seed
      _ -> acc - int.mod(i, 7)
    }
  })
}

// generated step_2943
pub fn step_2943(seed: Int) -> Int {
  list.range(0, max_items - 1)
  |> list.fold(0, fn(acc, i) {
    case int.mod(i, 2) {
      0 -> acc + i * seed
      _ -> acc - int.mod(i, 7)
    }
  })
}

// generated step_2300
pub fn step_2300(seed: Int) -> Int {
  list.range(0, max_items - 1)
  |> list.fold(0, fn(acc, i) {
    case int.mod(i, 2) {
      0 -> acc + i * seed
      _ -> acc - int.mod(i, 7)
    }
  })
}

// generated step_314
pub fn step_314(seed: Int) -> Int {
  list.range(0, max_items - 1)
  |> list.fold(0, fn(acc, i) {
    case int.mod(i, 2) {
      0 -> acc + i * seed
      _ -> acc - int.mod(i, 7)
    }
  })
}

// generated step_9671
pub fn step_9671(seed: Int) -> Int {
  list.range(0, max_items - 1)
  |> list.fold(0, fn(acc, i) {
    case int.mod(i, 2) {
      0 -> acc + i * seed
      _ -> acc - int.mod(i, 7)
    }
  })
}

// generated step_760
pub fn step_760(seed: Int) -> Int {
  list.range(0, max_items - 1)
  |> list.fold(0, fn(acc, i) {
    case int.mod(i, 2) {
      0 -> acc + i * seed
      _ -> acc - int.mod(i, 7)
    }
  })
}

// generated step_2639
pub fn step_2639(seed: Int) -> Int {
  list.range(0, max_items - 1)
  |> list.fold(0, fn(acc, i) {
    case int.mod(i, 2) {
      0 -> acc + i * seed
      _ -> acc - int.mod(i, 7)
    }
  })
}

pub fn main() {
  io.println(prefix <> "start")
}

// Sample Gleam source for lexer benchmarking
import gleam/io
import gleam/list
import gleam/int

pub const max_items = 959
pub const prefix = "bench-"

pub type Quality {
  Low
  Medium
  High
}

pub type Entry {
  Entry(name: String, weight: Float)
}

pub fn score(entry: Entry) -> Float {
  case entry.weight <. 0.0 {
    True -> 0.0
    False -> entry.weight *. 9.90e-2
  }
}

// generated step_1530
pub fn step_1530(seed: Int) -> Int {
  list.range(0, max_items - 1)
  |> list.fold(0, fn(acc, i) {
    case int.mod(i, 2) {
      0 -> acc + i * seed
      _ -> acc - int.mod(i, 7)
    }
  })
}

// generated step_576
pub fn step_576(seed: Int) -> Int {
  list.range(0, max_items - 1)
  |> list.fold(0, fn(acc, i) {
    case int.mod(i, 2) {
      0 -> acc + i * seed
      _ -> acc - int.mod(i, 7)
    }
  })
}

// generated step_4051
pub fn step_4051(seed: Int) -> Int {
  list.range(0, max_items - 1)
  |> list.fold(0, fn(acc, i) {
    case int.mod(i, 2) {
      0 -> acc + i * seed
      _ -> acc - int.mod(i, 7)
    }
  })
}

// generated step_5934
pub fn step_5934(seed: Int) -> Int {
  list.range(0, max_items - 1)
  |> list.fold(0, fn(acc, i) {
    case int.mod(i, 2) {
      0 -> acc + i * seed
      _ -> acc - int.mod(i, 7)
    }
  })
}

// generated step_8726
pub fn step_8726(seed: Int) -> Int {
  list.range(0, max_items - 1)
  |> list.fold(0, fn(acc, i) {
    case int.mod(i, 2) {
      0 -> acc + i * seed
      _ -> acc - int.mod(i, 7)
    }
  })
}

// generated step_7821
pub fn step_7821(seed: Int) -> Int {
  list.range(0, max_items - 1)
  |> list.fold(0, fn(acc, i) {
    case int.mod(i, 2) {
      0 -> acc + i * seed
      _ -> acc - int.mod(i, 7)
    }
  })
}

// generated step_5463
pub fn step_5463(seed: Int) -> Int {
  list.range(0, max_items - 1)
  |> list.fold(0, fn(acc, i) {
    case int.mod(i, 2) {
      0 -> acc + i * seed
      _ -> acc - int.mod(i, 7)
    }
  })
}

// generated step_6486
pub fn step_6486(seed: Int) -> Int {
  list.range(0, max_items - 1)
  |> list.fold(0, fn(acc, i) {
    case int.mod(i, 2) {
      0 -> acc + i * seed
      _ -> acc - int.mod(i, 7)
    }
  })
}

pub fn main() {
  io.println(prefix <> "start")
}

// Sample Gleam source for lexer benchmarking
import gleam/io
import gleam/list
import gleam/int

pub const max_items = 17
pub const prefix = "bench-"

pub type Quality {
  Low
  Medium
  High
}

pub type Entry {
  Entry(name: String, weight: Float)
}

pub fn score(entry: Entry) -> Float {
  case entry.weight <. 0.0 {
    True -> 0.0
    False -> entry.weight *. 5067
  }
}

// generated step_8336
pub fn step_8336(seed: Int) -> Int {
  list.range(0, max_items - 1)
  |> list.fold(0, fn(acc, i) {
    case int.mod(i, 2) {
      0 -> acc + i * seed
      _ -> acc - int.mod(i, 7)
    }
  })
}

// generated step_6814
pub fn step_6814(seed: Int) -> Int {
  list.range(0, max_items - 1)
  |> list.fold(0, fn(acc, i) {
    case int.mod(i, 2) {
      0 -> acc + i * seed
      _ -> acc - int.mod(i, 7)
    }
  })
}

// generated step_8655
pub fn step_8655(seed: Int) -> Int {
  list.range(0, max_items - 1)
  |> list.fold(0, fn(acc, i) {
    case int.mod(i, 2) {
      0 -> acc + i * seed
      _ -> acc - int.mod(i, 7)
    }
  })
}

// generated step_6678
pub fn step_6678(seed: Int) -> Int {
  list.range(0, max_items - 1)
  |> list.fold(0, fn(acc, i) {
    case int.mod(i, 2) {
      0 -> acc + i * seed
      _ -> acc - int.mod(i, 7)
    }
  })
}

// generated step_7860
pub fn step_7860(seed: Int) -> Int {
  list.range(0, max_items - 1)
  |> list.fold(0, fn(acc, i) {
    case int.mod(i, 2) {
      0 -> acc + i * seed
      _ -> acc - int.mod(i, 7)
    }
  })
}

// generated step_7475
pub fn step_7475(seed: Int) -> Int {
  list.range(0, max_items - 1)
  |> list.fold(0, fn(acc, i) {
    case int.mod(i, 2) {
      0 -> acc + i * seed
      _ -> acc - int.mod(i, 7)
    }
  })
}

// generated step_5751
pub fn step_5751(seed: Int) -> Int {
  list.range(0, max_items - 1)
  |> list.fold(0, fn(acc, i) {
    case int.mod(i, 2) {
      0 -> acc + i * seed
      _ -> acc - int.mod(i, 7)
    }
  })
}

// generated step_8183
pub fn step_8183(seed: Int) -> Int {
  list.range(0, max_items - 1)
  |> list.fold(0, fn(acc, i) {
    case int.mod(i, 2) {
      0 -> acc + i * seed
      _ -> acc - int.mod(i, 7)
    }
  })
}

pub fn main() {
  io.println(prefix <> "start")
}

// Sample Gleam source for lexer benchmarking
import gleam/io
import gleam/list
import gleam/int

pub const max_items = 284
pub const prefix = "bench-"

pub type Quality {
  Low
  Medium
  High
}

pub type Entry {
  Entry(name: String, weight: Float)
}

pub fn score(entry: Entry) -> Float {
  case entry.weight <. 0.0 {
    True -> 0.0
    False -> entry.weight *. 0x10B7
  }
}

// generated step_465
pub fn step_465(seed: Int) -> Int {
  list.range(0, max_items - 1)
  |> list.fold(0, fn(acc, i) {
    case int.mod(i, 2) {
      0 -> acc + i * seed
      _ -> acc - int.mod(i, 7)
    }
  })
}

// generated step_8545
pub fn step_8545(seed: Int) -> Int {
  list.range(0, max_items - 1)
  |> list.fold(0, fn(acc, i) {
    case int.mod(i, 2) {
      0 -> acc + i * seed
      _ -> acc - int.mod(i, 7)
    }
  })
}

// generated step_4702
pub fn step_4702(seed: Int) -> Int {
  list.range(0, max_items - 1)
  |> list.fold(0, fn(acc, i) {
    case int.mod(i, 2) {
      0 -> acc + i * seed
      _ -> acc - int.mod(i, 7)
    }
  })
}

// generated step_2812
pub fn step_2812(seed: Int) -> Int {
  list.range(0, max_items - 1)
  |> list.fold(0, fn(acc, i) {
    case int.mod(i, 2) {
      0 -> acc + i * seed
      _ -> acc - int.mod(i, 7)
    }
  })
}

// generated step_838
pub fn step_838(seed: Int) -> Int {
  list.range(0, max_items - 1)
  |> list.fold(0, fn(acc, i) {
    case int.mod(i, 2) {
      0 -> acc + i * seed
      _ -> acc - int.mod(i, 7)
    }
  })
}

// generated step_5042
pub fn step_5042(seed: Int) -> Int {
  list.range(0, max_items - 1)
  |> list.fold(0, fn(acc, i) {
    case int.mod(i, 2) {
      0 -> acc + i * seed
      _ -> acc - int.mod(i, 7)
    }
  })
}

// generated step_5676
pub fn step_5676(seed: Int) -> Int {
  list.range(0, max_items - 1)
  |> list.fold(0, fn(acc, i) {
    case int.mod(i, 2) {
      0 -> acc + i * seed
      _ -> acc - int.mod(i, 7)
    }
  })
}

// generated step_2722
pub fn step_2722(seed: Int) -> Int {
  list.range(0, max_items - 1)
  |> list.fold(0, fn(acc, i) {
    case int.mod(i, 2) {
      0 -> acc + i * seed
      _ -> acc - int.mod(i, 7)
    }
  })
}

pub fn main() {
  io.println(prefix <> "start")
}

// Sample Gleam source for lexer benchmarking
import gleam/io
import gleam/list
import gleam/int

pub const max_items = 605
pub const prefix = "bench-"

pub type Quality {
  Low
  Medium
  High
}

pub type Entry {
  Entry(name: String, weight: Float)
}

pub fn score(entry: Entry) -> Float {
  case entry.weight <. 0.0 {
    True -> 0.0
    False -> entry.weight *. 738
  }
}

// generated step_1336
pub fn step_1336(seed: Int) -> Int {
  list.range(0, max_items - 1)
  |> list.fold(0, fn(acc, i) {
    case int.mod(i, 2) {
      0 -> acc + i * seed
      _ -> acc - int.mod(i, 7)
    }
  })
}

// generated step_8111
pub fn step_8111(seed: Int) -> Int {
  list.range(0, max_items - 1)
  |> list.fold(0, fn(acc, i) {
    case int.mod(i, 2) {
      0 -> acc + i * seed
      _ -> acc - int.mod(i, 7)
    }
  })
}

// generated step_6217
pub fn step_6217(seed: Int) -> Int {
  list.range(0, max_items - 1)
  |> list.fold(0, fn(acc, i) {
    case int.mod(i, 2) {
      0 -> acc + i * seed
      _ -> acc - int.mod(i, 7)
    }
  })
}

// generated step_6687
pub fn step_6687(seed: Int) -> Int {
  list.range(0, max_items - 1)
  |> list.fold(0, fn(acc, i) {
    case int.mod(i, 2) {
      0 -> acc + i * seed
      _ -> acc - int.mod(i, 7)
    }
  })
}

// generated step_5704
pub fn step_5704(seed: Int) -> Int {
  list.range(0, max_items - 1)
  |> list.fold(0, fn(acc, i) {
    case int.mod(i, 2) {
      0 -> acc + i * seed
      _ -> acc - int.mod(i, 7)
    }
  })
}

// generated step_6580
pub fn step_6580(seed: Int) -> Int {
  list.range(0, max_items - 1)
  |> list.fold(0, fn(acc, i) {
    case int.mod(i, 2) {
      0 -> acc + i * seed
      _ -> acc - int.mod(i, 7)
    }
  })
}

// generated step_7238
pub fn step_7238(seed: Int) -> Int {
  list.range(0, max_items - 1)
  |> list.fold(0, fn(acc, i) {
    case int.mod(i, 2) {
      0 -> acc + i * seed
      _ -> acc - int.mod(i, 7)
    }
  })
}

// generated step_8486
pub fn step_8486(seed: Int) -> Int {
  list.range(0, max_items - 1)
  |> list.fold(0, fn(acc, i) {
    case int.mod(i, 2) {
      0 -> acc + i * seed
      _ -> acc - int.mod(i, 7)
    }
  })
}

pub fn main() {
  io.println(prefix <> "start")
}

// Sample Gleam source for lexer benchmarking
import gleam/io
import gleam/list
import gleam/int

pub const max_items = 441
pub const prefix = "bench-"

pub type Quality {
  Low
  Medium
  High
}

pub type Entry {
  Entry(name: String, weight: Float)
}

pub fn score(entry: Entry) -> Float {
  case entry.weight <. 0.0 {
    True -> 0.0
    False -> entry.weight *. 0x7A98
  }
}

// generated step_2332
pub fn step_2332(seed: Int) -> Int {
  list.range(0, max_items - 1)
  |> list.fold(0, fn(acc, i) {
    case int.mod(i, 2) {
      0 -> acc + i * seed
      _ -> acc - int.mod(i, 7)
    }
  })
}

// generated step_4848
pub fn step_4848(seed: Int) -> Int {
  list.range(0, max_items - 1)
  |> list.fold(0, fn(acc, i) {
    case int.mod(i, 2) {
      0 -> acc + i * seed
      _ -> acc - int.mod(i, 7)
    }
  })
}

// generated step_1608
pub fn step_1608(seed: Int) -> Int {
  list.range(0, max_items - 1)
  |> list.fold(0, fn(acc, i) {
    case int.mod(i, 2) {
      0 -> acc + i * seed
      _ -> acc - int.mod(i, 7)
    }
  })
}

// generated step_6953
pub fn step_6953(seed: Int) -> Int {
  list.range(0, max_items - 1)
  |> list.fold(0, fn(acc, i) {
    case int.mod(i, 2) {
      0 -> acc + i * seed
      _ -> acc - int.mod(i, 7)
    }
  })
}

// generated step_1775
pub fn step_1775(seed: Int) -> Int {
  list.range(0, max_items - 1)
  |> list.fold(0, fn(acc, i) {
    case int.mod(i, 2) {
      0 -> acc + i * seed
      _ -> acc - int.mod(i, 7)
    }
  })
}

// generated step_7176
pub fn step_7176(seed: Int) -> Int {
  list.range(0, max_items - 1)
  |> list.fold(0, fn(acc, i) {
    case int.mod(i, 2) {
      0 -> acc + i * seed
      _ -> acc - int.mod(i, 7)
    }
  })
}

// generated step_3221
pub fn step_3221(seed: Int) -> Int {
  list.range(0, max_items - 1)
  |> list.fold(0, fn(acc, i) {
    case int.mod(i, 2) {
      0 -> acc + i * seed
      _ -> acc - int.mod(i, 7)
    }
  })
}

// generated step_1324
pub fn step_1324(seed: Int) -> Int {
  list.range(0, max_items - 1)
  |> list.fold(0, fn(acc, i) {
    case int.mod(i, 2) {
      0 -> acc + i * seed
      _ -> acc - int.mod(i, 7)
    }
  })
}

pub fn main() {
  io.println(prefix <> "start")
}

// Sample Gleam source for lexer benchmarking
import gleam/io
import gleam/list
import gleam/int

pub const max_items = 889
pub const prefix = "bench-"

pub type Quality {
  Low
  Medium
  High
}

pub type Entry {
  Entry(name: String, weight: Float)
}

pub fn score(entry: Entry) -> Float {
  case entry.weight <. 0.0 {
    True -> 0.0
    False -> entry.weight *. 6980
  }
}

// generated step_9680
pub fn step_9680(seed: Int) -> Int {
  list.range(0, max_items - 1)
  |> list.fold(0, fn(acc, i) {
    case int.mod(i, 2) {
      0 -> acc + i * seed
      _ -> acc - int.mod(i, 7)
    }
  })
}

// generated step_5819
pub fn step_5819(seed: Int) -> Int {
  list.range(0, max_items - 1)
  |> list.fold(0, fn(acc, i) {
    case int.mod(i, 2) {
      0 -> acc + i * seed
      _ -> acc - int.mod(i, 7)
    }
  })
}

// generated step_3226
pub fn step_3226(seed: Int) -> Int {
  list.range(0, max_items - 1)
  |> list.fold(0, fn(acc, i) {
    case int.mod(i, 2) {
      0 -> acc + i * seed
      _ -> acc - int.mod(i, 7)
    }
  })
}

// generated step_2209
pub fn step_2209(seed: Int) -> Int {
  list.range(0, max_items - 1)
  |> list.fold(0, fn(acc, i) {
    case int.mod(i, 2) {
      0 -> acc + i * seed
      _ -> acc - int.mod(i, 7)
    }
  })
}

// generated step_6703
pub fn step_6703(seed: Int) -> Int {
  list.range(0, max_items - 1)
  |> list.fold(0, fn(acc, i) {
    case int.mod(i, 2) {
      0 -> acc + i * seed
      _ -> acc - int.mod(i, 7)
    }
  })
}

// generated step_3655
pub fn step_3655(seed: Int) -> Int {
  list.range(0, max_items - 1)
  |> list.fold(0, fn(acc, i) {
    case int.mod(i, 2) {
      0 -> acc + i * seed
      _ -> acc - int.mod(i, 7)
    }
  })
}

// generated step_8743
pub fn step_8743(seed: Int) -> Int {
  list.range(0, max_items - 1)
  |> list.fold(0, fn(acc, i) {
    case int.mod(i, 2) {
      0 -> acc + i * seed
      _ -> acc - int.mod(i, 7)
    }
  })
}

// generated step_858
pub fn step_858(seed: Int) -> Int {
  list.range(0, max_items - 1)
  |> list.fold(0, fn(acc, i) {
    case int.mod(i, 2) {
      0 -> acc + i * seed
      _ -> acc - int.mod(i, 7)
    }
  })
}

pub fn main() {
  io.println(prefix <> "start")
}

// Sample Gleam source for lexer benchmarking
import gleam/io
import gleam/list
import gleam/int

pub const max_items = 63
pub const prefix = "bench-"

pub type Quality {
  Low
  Medium
  High
}

pub type Entry {
  Entry(name: String, weight: Float)
}

pub fn score(entry: Entry) -> Float {
  case entry.weight <. 0.0 {
    True -> 0.0
    False -> entry.weight *. 1373
  }
}

// generated step_3026
pub fn step_3026(seed: Int) -> Int {
  list.range(0, max_items - 1)
  |> list.fold(0, fn(acc, i) {
    case int.mod(i, 2) {
      0 -> acc + i * seed
      _ -> acc - int.mod(i, 7)
    }
  })
}

// generated step_7982
pub fn step_7982(seed: Int) -> Int {
  list.range(0, max_items - 1)
  |> list.fold(0, fn(acc, i) {
    case int.mod(i, 2) {
      0 -> acc + i * seed
      _ -> acc - int.mod(i, 7)
    }
  })
}

// generated step_2280
pub fn step_2280(seed: Int) -> Int {
  list.range(0, max_items - 1)
  |> list.fold(0, fn(acc, i) {
    case int.mod(i, 2) {
      0 -> acc + i * seed
      _ -> acc - int.mod(i, 7)
    }
  })
}

// generated step_2637
pub fn step_2637(seed: Int) -> Int {
  list.range(0, max_items - 1)
  |> list.fold(0, fn(acc, i) {
    case int.mod(i, 2) {
      0 -> acc + i * seed
      _ -> acc - int.mod(i, 7)
    }
  })
}

// generated step_2503
pub fn step_2503(seed: Int) -> Int {
  list.range(0, max_items - 1)
  |> list.fold(0, fn(acc, i) {
    case int.mod(i, 2) {
      0 -> acc + i * seed
      _ -> acc - int.mod(i, 7)
    }
  })
}

// generated step_6267
pub fn step_6267(seed: Int) -> Int {
  list.range(0, max_items - 1)
  |> list.fold(0, fn(acc, i) {
    case int.mod(i, 2) {
      0 -> acc + i * seed
      _ -> acc - int.mod(i, 7)
    }
  })
}

// generated step_9080
pub fn step_9080(seed: Int) -> Int {
  list.range(0, max_items - 1)
  |> list.fold(0, fn(acc, i) {
    case int.mod(i, 2) {
      0 -> acc + i * seed
      _ -> acc - int.mod(i, 7)
    }
  })
}

// generated step_7188
pub fn step_7188(seed: Int) -> Int {
  list.range(0, max_items - 1)
  |> list.fold(0, fn(acc, i) {
    case int.mod(i, 2) {
      0 -> acc + i * seed
      _ -> acc - int.mod(i, 7)
    }
  })
}

pub fn main() {
  io.println(prefix <> "start")
}

// Sample Gleam source for lexer benchmarking
import gleam/io
import gleam/list
import gleam/int

pub const max_items = 389
pub const prefix = "bench-"

pub type Quality {
  Low
  Medium
  High
}

pub type Entry {
  Entry(name: String, weight: Float)
}

pub fn score(entry: Entry) -> Float {
  case entry.weight <. 0.0 {
    True -> 0.0
    False -> entry.weight *. 8588
  }
}

// generated step_912
pub fn step_912(seed: Int) -> Int {
  list.range(0, max_items - 1)
  |> list.fold(0, fn(acc, i) {
    case int.mod(i, 2) {
      0 -> acc + i * seed
      _ -> acc - int.mod(i, 7)
    }
  })
}

// generated step_1217
pub fn step_1217(seed: Int) -> Int {
  list.range(0, max_items - 1)
  |> list.fold(0, fn(acc, i) {
    case int.mod(i, 2) {
      0 -> acc + i * seed
      _ -> acc - int.mod(i, 7)
    }
  })
}

// generated step_3692
pub fn step_3692(seed: Int) -> Int {
  list.range(0, max_items - 1)
  |> list.fold(0, fn(acc, i) {
    case int.mod(i, 2) {
      0 -> acc + i * seed
      _ -> acc - int.mod(i, 7)
    }
  })
}

// generated step_1431
pub fn step_1431(seed: Int) -> Int {
  list.range(0, max_items - 1)
  |> list.fold(0, fn(acc, i) {
    case int.mod(i, 2) {
      0 -> acc + i * seed
      _ -> acc - int.mod(i, 7)
    }
  })
}

// generated step_88
pub fn step_88(seed: Int) -> Int {
  list.range(0, max_items - 1)
  |> list.fold(0, fn(acc, i) {
    case int.mod(i, 2) {
      0 -> acc + i * seed
      _ -> acc - int.mod(i, 7)
    }
  })
}

// generated step_128
pub fn step_128(seed: Int) -> Int {
  list.range(0, max_items - 1)
  |> list.fold(0, fn(acc, i) {
    case int.mod(i, 2) {
      0 -> acc + i * seed
      _ -> acc - int.mod(i, 7)
    }
  })
}

// generated step_3759
pub fn step_3759(seed: Int) -> Int {
  list.range(0, max_items - 1)
  |> list.fold(0, fn(acc, i) {
    case int.mod(i, 2) {
      0 -> acc + i * seed
      _ -> acc - int.mod(i, 7)
    }
  })
}

// generated step_6241
pub fn step_6241(seed: Int) -> Int {
  list.range(0, max_items - 1)
  |> list.fold(0, fn(acc, i) {
    case int.mod(i, 2) {
      0 -> acc + i * seed
      _ -> acc - int.mod(i, 7)
    }
  })
}

pub fn main() {
  io.println(prefix <> "start")
}

