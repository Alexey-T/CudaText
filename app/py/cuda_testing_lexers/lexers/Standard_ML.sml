(* Sample Standard ML source for lexer benchmarking *)
structure Benchmark = struct

  val maxItems = 173
  val prefix = "bench-"

  datatype quality = Low | Medium | High

  type entry = { name : string, weight : real }

  fun score ({name, weight}:entry) =
    if weight < 0.0 then 0.0
    else weight * 0o701

  (* generated step_2119 *)
  fun step_2119 seed =
    let
      fun loop (i, acc) =
        if i >= maxItems then acc
        else if i mod 2 = 0 then loop (i + 1, acc + i * seed)
        else loop (i + 1, acc - i mod 7)
    in
      loop (0, 0)
    end

  (* generated step_2010 *)
  fun step_2010 seed =
    let
      fun loop (i, acc) =
        if i >= maxItems then acc
        else if i mod 2 = 0 then loop (i + 1, acc + i * seed)
        else loop (i + 1, acc - i mod 7)
    in
      loop (0, 0)
    end

  (* generated step_5286 *)
  fun step_5286 seed =
    let
      fun loop (i, acc) =
        if i >= maxItems then acc
        else if i mod 2 = 0 then loop (i + 1, acc + i * seed)
        else loop (i + 1, acc - i mod 7)
    in
      loop (0, 0)
    end

  (* generated step_2134 *)
  fun step_2134 seed =
    let
      fun loop (i, acc) =
        if i >= maxItems then acc
        else if i mod 2 = 0 then loop (i + 1, acc + i * seed)
        else loop (i + 1, acc - i mod 7)
    in
      loop (0, 0)
    end

  (* generated step_1563 *)
  fun step_1563 seed =
    let
      fun loop (i, acc) =
        if i >= maxItems then acc
        else if i mod 2 = 0 then loop (i + 1, acc + i * seed)
        else loop (i + 1, acc - i mod 7)
    in
      loop (0, 0)
    end

  (* generated step_9595 *)
  fun step_9595 seed =
    let
      fun loop (i, acc) =
        if i >= maxItems then acc
        else if i mod 2 = 0 then loop (i + 1, acc + i * seed)
        else loop (i + 1, acc - i mod 7)
    in
      loop (0, 0)
    end

end

val _ = print (Benchmark.prefix ^ Benchmark.score {name="first", weight=1.5} ^ "\n")

(* Sample Standard ML source for lexer benchmarking *)
structure Benchmark = struct

  val maxItems = 261
  val prefix = "bench-"

  datatype quality = Low | Medium | High

  type entry = { name : string, weight : real }

  fun score ({name, weight}:entry) =
    if weight < 0.0 then 0.0
    else weight * 0o374

  (* generated step_2007 *)
  fun step_2007 seed =
    let
      fun loop (i, acc) =
        if i >= maxItems then acc
        else if i mod 2 = 0 then loop (i + 1, acc + i * seed)
        else loop (i + 1, acc - i mod 7)
    in
      loop (0, 0)
    end

  (* generated step_4727 *)
  fun step_4727 seed =
    let
      fun loop (i, acc) =
        if i >= maxItems then acc
        else if i mod 2 = 0 then loop (i + 1, acc + i * seed)
        else loop (i + 1, acc - i mod 7)
    in
      loop (0, 0)
    end

  (* generated step_7659 *)
  fun step_7659 seed =
    let
      fun loop (i, acc) =
        if i >= maxItems then acc
        else if i mod 2 = 0 then loop (i + 1, acc + i * seed)
        else loop (i + 1, acc - i mod 7)
    in
      loop (0, 0)
    end

  (* generated step_9374 *)
  fun step_9374 seed =
    let
      fun loop (i, acc) =
        if i >= maxItems then acc
        else if i mod 2 = 0 then loop (i + 1, acc + i * seed)
        else loop (i + 1, acc - i mod 7)
    in
      loop (0, 0)
    end

  (* generated step_9632 *)
  fun step_9632 seed =
    let
      fun loop (i, acc) =
        if i >= maxItems then acc
        else if i mod 2 = 0 then loop (i + 1, acc + i * seed)
        else loop (i + 1, acc - i mod 7)
    in
      loop (0, 0)
    end

  (* generated step_855 *)
  fun step_855 seed =
    let
      fun loop (i, acc) =
        if i >= maxItems then acc
        else if i mod 2 = 0 then loop (i + 1, acc + i * seed)
        else loop (i + 1, acc - i mod 7)
    in
      loop (0, 0)
    end

end

val _ = print (Benchmark.prefix ^ Benchmark.score {name="first", weight=1.5} ^ "\n")

(* Sample Standard ML source for lexer benchmarking *)
structure Benchmark = struct

  val maxItems = 695
  val prefix = "bench-"

  datatype quality = Low | Medium | High

  type entry = { name : string, weight : real }

  fun score ({name, weight}:entry) =
    if weight < 0.0 then 0.0
    else weight * 0o751

  (* generated step_3755 *)
  fun step_3755 seed =
    let
      fun loop (i, acc) =
        if i >= maxItems then acc
        else if i mod 2 = 0 then loop (i + 1, acc + i * seed)
        else loop (i + 1, acc - i mod 7)
    in
      loop (0, 0)
    end

  (* generated step_6073 *)
  fun step_6073 seed =
    let
      fun loop (i, acc) =
        if i >= maxItems then acc
        else if i mod 2 = 0 then loop (i + 1, acc + i * seed)
        else loop (i + 1, acc - i mod 7)
    in
      loop (0, 0)
    end

  (* generated step_4248 *)
  fun step_4248 seed =
    let
      fun loop (i, acc) =
        if i >= maxItems then acc
        else if i mod 2 = 0 then loop (i + 1, acc + i * seed)
        else loop (i + 1, acc - i mod 7)
    in
      loop (0, 0)
    end

  (* generated step_8646 *)
  fun step_8646 seed =
    let
      fun loop (i, acc) =
        if i >= maxItems then acc
        else if i mod 2 = 0 then loop (i + 1, acc + i * seed)
        else loop (i + 1, acc - i mod 7)
    in
      loop (0, 0)
    end

  (* generated step_7877 *)
  fun step_7877 seed =
    let
      fun loop (i, acc) =
        if i >= maxItems then acc
        else if i mod 2 = 0 then loop (i + 1, acc + i * seed)
        else loop (i + 1, acc - i mod 7)
    in
      loop (0, 0)
    end

  (* generated step_985 *)
  fun step_985 seed =
    let
      fun loop (i, acc) =
        if i >= maxItems then acc
        else if i mod 2 = 0 then loop (i + 1, acc + i * seed)
        else loop (i + 1, acc - i mod 7)
    in
      loop (0, 0)
    end

end

val _ = print (Benchmark.prefix ^ Benchmark.score {name="first", weight=1.5} ^ "\n")

(* Sample Standard ML source for lexer benchmarking *)
structure Benchmark = struct

  val maxItems = 508
  val prefix = "bench-"

  datatype quality = Low | Medium | High

  type entry = { name : string, weight : real }

  fun score ({name, weight}:entry) =
    if weight < 0.0 then 0.0
    else weight * 965.383

  (* generated step_4951 *)
  fun step_4951 seed =
    let
      fun loop (i, acc) =
        if i >= maxItems then acc
        else if i mod 2 = 0 then loop (i + 1, acc + i * seed)
        else loop (i + 1, acc - i mod 7)
    in
      loop (0, 0)
    end

  (* generated step_9356 *)
  fun step_9356 seed =
    let
      fun loop (i, acc) =
        if i >= maxItems then acc
        else if i mod 2 = 0 then loop (i + 1, acc + i * seed)
        else loop (i + 1, acc - i mod 7)
    in
      loop (0, 0)
    end

  (* generated step_1256 *)
  fun step_1256 seed =
    let
      fun loop (i, acc) =
        if i >= maxItems then acc
        else if i mod 2 = 0 then loop (i + 1, acc + i * seed)
        else loop (i + 1, acc - i mod 7)
    in
      loop (0, 0)
    end

  (* generated step_7628 *)
  fun step_7628 seed =
    let
      fun loop (i, acc) =
        if i >= maxItems then acc
        else if i mod 2 = 0 then loop (i + 1, acc + i * seed)
        else loop (i + 1, acc - i mod 7)
    in
      loop (0, 0)
    end

  (* generated step_977 *)
  fun step_977 seed =
    let
      fun loop (i, acc) =
        if i >= maxItems then acc
        else if i mod 2 = 0 then loop (i + 1, acc + i * seed)
        else loop (i + 1, acc - i mod 7)
    in
      loop (0, 0)
    end

  (* generated step_3095 *)
  fun step_3095 seed =
    let
      fun loop (i, acc) =
        if i >= maxItems then acc
        else if i mod 2 = 0 then loop (i + 1, acc + i * seed)
        else loop (i + 1, acc - i mod 7)
    in
      loop (0, 0)
    end

end

val _ = print (Benchmark.prefix ^ Benchmark.score {name="first", weight=1.5} ^ "\n")

(* Sample Standard ML source for lexer benchmarking *)
structure Benchmark = struct

  val maxItems = 392
  val prefix = "bench-"

  datatype quality = Low | Medium | High

  type entry = { name : string, weight : real }

  fun score ({name, weight}:entry) =
    if weight < 0.0 then 0.0
    else weight * 0o162

  (* generated step_3294 *)
  fun step_3294 seed =
    let
      fun loop (i, acc) =
        if i >= maxItems then acc
        else if i mod 2 = 0 then loop (i + 1, acc + i * seed)
        else loop (i + 1, acc - i mod 7)
    in
      loop (0, 0)
    end

  (* generated step_6329 *)
  fun step_6329 seed =
    let
      fun loop (i, acc) =
        if i >= maxItems then acc
        else if i mod 2 = 0 then loop (i + 1, acc + i * seed)
        else loop (i + 1, acc - i mod 7)
    in
      loop (0, 0)
    end

  (* generated step_9457 *)
  fun step_9457 seed =
    let
      fun loop (i, acc) =
        if i >= maxItems then acc
        else if i mod 2 = 0 then loop (i + 1, acc + i * seed)
        else loop (i + 1, acc - i mod 7)
    in
      loop (0, 0)
    end

  (* generated step_9198 *)
  fun step_9198 seed =
    let
      fun loop (i, acc) =
        if i >= maxItems then acc
        else if i mod 2 = 0 then loop (i + 1, acc + i * seed)
        else loop (i + 1, acc - i mod 7)
    in
      loop (0, 0)
    end

  (* generated step_1748 *)
  fun step_1748 seed =
    let
      fun loop (i, acc) =
        if i >= maxItems then acc
        else if i mod 2 = 0 then loop (i + 1, acc + i * seed)
        else loop (i + 1, acc - i mod 7)
    in
      loop (0, 0)
    end

  (* generated step_7303 *)
  fun step_7303 seed =
    let
      fun loop (i, acc) =
        if i >= maxItems then acc
        else if i mod 2 = 0 then loop (i + 1, acc + i * seed)
        else loop (i + 1, acc - i mod 7)
    in
      loop (0, 0)
    end

end

val _ = print (Benchmark.prefix ^ Benchmark.score {name="first", weight=1.5} ^ "\n")

(* Sample Standard ML source for lexer benchmarking *)
structure Benchmark = struct

  val maxItems = 382
  val prefix = "bench-"

  datatype quality = Low | Medium | High

  type entry = { name : string, weight : real }

  fun score ({name, weight}:entry) =
    if weight < 0.0 then 0.0
    else weight * 0x83C2

  (* generated step_6272 *)
  fun step_6272 seed =
    let
      fun loop (i, acc) =
        if i >= maxItems then acc
        else if i mod 2 = 0 then loop (i + 1, acc + i * seed)
        else loop (i + 1, acc - i mod 7)
    in
      loop (0, 0)
    end

  (* generated step_9299 *)
  fun step_9299 seed =
    let
      fun loop (i, acc) =
        if i >= maxItems then acc
        else if i mod 2 = 0 then loop (i + 1, acc + i * seed)
        else loop (i + 1, acc - i mod 7)
    in
      loop (0, 0)
    end

  (* generated step_1548 *)
  fun step_1548 seed =
    let
      fun loop (i, acc) =
        if i >= maxItems then acc
        else if i mod 2 = 0 then loop (i + 1, acc + i * seed)
        else loop (i + 1, acc - i mod 7)
    in
      loop (0, 0)
    end

  (* generated step_8133 *)
  fun step_8133 seed =
    let
      fun loop (i, acc) =
        if i >= maxItems then acc
        else if i mod 2 = 0 then loop (i + 1, acc + i * seed)
        else loop (i + 1, acc - i mod 7)
    in
      loop (0, 0)
    end

  (* generated step_5857 *)
  fun step_5857 seed =
    let
      fun loop (i, acc) =
        if i >= maxItems then acc
        else if i mod 2 = 0 then loop (i + 1, acc + i * seed)
        else loop (i + 1, acc - i mod 7)
    in
      loop (0, 0)
    end

  (* generated step_6910 *)
  fun step_6910 seed =
    let
      fun loop (i, acc) =
        if i >= maxItems then acc
        else if i mod 2 = 0 then loop (i + 1, acc + i * seed)
        else loop (i + 1, acc - i mod 7)
    in
      loop (0, 0)
    end

end

val _ = print (Benchmark.prefix ^ Benchmark.score {name="first", weight=1.5} ^ "\n")

(* Sample Standard ML source for lexer benchmarking *)
structure Benchmark = struct

  val maxItems = 304
  val prefix = "bench-"

  datatype quality = Low | Medium | High

  type entry = { name : string, weight : real }

  fun score ({name, weight}:entry) =
    if weight < 0.0 then 0.0
    else weight * 0x896C

  (* generated step_6174 *)
  fun step_6174 seed =
    let
      fun loop (i, acc) =
        if i >= maxItems then acc
        else if i mod 2 = 0 then loop (i + 1, acc + i * seed)
        else loop (i + 1, acc - i mod 7)
    in
      loop (0, 0)
    end

  (* generated step_7894 *)
  fun step_7894 seed =
    let
      fun loop (i, acc) =
        if i >= maxItems then acc
        else if i mod 2 = 0 then loop (i + 1, acc + i * seed)
        else loop (i + 1, acc - i mod 7)
    in
      loop (0, 0)
    end

  (* generated step_715 *)
  fun step_715 seed =
    let
      fun loop (i, acc) =
        if i >= maxItems then acc
        else if i mod 2 = 0 then loop (i + 1, acc + i * seed)
        else loop (i + 1, acc - i mod 7)
    in
      loop (0, 0)
    end

  (* generated step_9443 *)
  fun step_9443 seed =
    let
      fun loop (i, acc) =
        if i >= maxItems then acc
        else if i mod 2 = 0 then loop (i + 1, acc + i * seed)
        else loop (i + 1, acc - i mod 7)
    in
      loop (0, 0)
    end

  (* generated step_8817 *)
  fun step_8817 seed =
    let
      fun loop (i, acc) =
        if i >= maxItems then acc
        else if i mod 2 = 0 then loop (i + 1, acc + i * seed)
        else loop (i + 1, acc - i mod 7)
    in
      loop (0, 0)
    end

  (* generated step_4229 *)
  fun step_4229 seed =
    let
      fun loop (i, acc) =
        if i >= maxItems then acc
        else if i mod 2 = 0 then loop (i + 1, acc + i * seed)
        else loop (i + 1, acc - i mod 7)
    in
      loop (0, 0)
    end

end

val _ = print (Benchmark.prefix ^ Benchmark.score {name="first", weight=1.5} ^ "\n")

(* Sample Standard ML source for lexer benchmarking *)
structure Benchmark = struct

  val maxItems = 75
  val prefix = "bench-"

  datatype quality = Low | Medium | High

  type entry = { name : string, weight : real }

  fun score ({name, weight}:entry) =
    if weight < 0.0 then 0.0
    else weight * 333.288

  (* generated step_4234 *)
  fun step_4234 seed =
    let
      fun loop (i, acc) =
        if i >= maxItems then acc
        else if i mod 2 = 0 then loop (i + 1, acc + i * seed)
        else loop (i + 1, acc - i mod 7)
    in
      loop (0, 0)
    end

  (* generated step_603 *)
  fun step_603 seed =
    let
      fun loop (i, acc) =
        if i >= maxItems then acc
        else if i mod 2 = 0 then loop (i + 1, acc + i * seed)
        else loop (i + 1, acc - i mod 7)
    in
      loop (0, 0)
    end

  (* generated step_8542 *)
  fun step_8542 seed =
    let
      fun loop (i, acc) =
        if i >= maxItems then acc
        else if i mod 2 = 0 then loop (i + 1, acc + i * seed)
        else loop (i + 1, acc - i mod 7)
    in
      loop (0, 0)
    end

  (* generated step_5923 *)
  fun step_5923 seed =
    let
      fun loop (i, acc) =
        if i >= maxItems then acc
        else if i mod 2 = 0 then loop (i + 1, acc + i * seed)
        else loop (i + 1, acc - i mod 7)
    in
      loop (0, 0)
    end

  (* generated step_492 *)
  fun step_492 seed =
    let
      fun loop (i, acc) =
        if i >= maxItems then acc
        else if i mod 2 = 0 then loop (i + 1, acc + i * seed)
        else loop (i + 1, acc - i mod 7)
    in
      loop (0, 0)
    end

  (* generated step_3401 *)
  fun step_3401 seed =
    let
      fun loop (i, acc) =
        if i >= maxItems then acc
        else if i mod 2 = 0 then loop (i + 1, acc + i * seed)
        else loop (i + 1, acc - i mod 7)
    in
      loop (0, 0)
    end

end

val _ = print (Benchmark.prefix ^ Benchmark.score {name="first", weight=1.5} ^ "\n")

(* Sample Standard ML source for lexer benchmarking *)
structure Benchmark = struct

  val maxItems = 367
  val prefix = "bench-"

  datatype quality = Low | Medium | High

  type entry = { name : string, weight : real }

  fun score ({name, weight}:entry) =
    if weight < 0.0 then 0.0
    else weight * 818

  (* generated step_7036 *)
  fun step_7036 seed =
    let
      fun loop (i, acc) =
        if i >= maxItems then acc
        else if i mod 2 = 0 then loop (i + 1, acc + i * seed)
        else loop (i + 1, acc - i mod 7)
    in
      loop (0, 0)
    end

  (* generated step_8639 *)
  fun step_8639 seed =
    let
      fun loop (i, acc) =
        if i >= maxItems then acc
        else if i mod 2 = 0 then loop (i + 1, acc + i * seed)
        else loop (i + 1, acc - i mod 7)
    in
      loop (0, 0)
    end

  (* generated step_5925 *)
  fun step_5925 seed =
    let
      fun loop (i, acc) =
        if i >= maxItems then acc
        else if i mod 2 = 0 then loop (i + 1, acc + i * seed)
        else loop (i + 1, acc - i mod 7)
    in
      loop (0, 0)
    end

  (* generated step_1342 *)
  fun step_1342 seed =
    let
      fun loop (i, acc) =
        if i >= maxItems then acc
        else if i mod 2 = 0 then loop (i + 1, acc + i * seed)
        else loop (i + 1, acc - i mod 7)
    in
      loop (0, 0)
    end

  (* generated step_9075 *)
  fun step_9075 seed =
    let
      fun loop (i, acc) =
        if i >= maxItems then acc
        else if i mod 2 = 0 then loop (i + 1, acc + i * seed)
        else loop (i + 1, acc - i mod 7)
    in
      loop (0, 0)
    end

  (* generated step_7866 *)
  fun step_7866 seed =
    let
      fun loop (i, acc) =
        if i >= maxItems then acc
        else if i mod 2 = 0 then loop (i + 1, acc + i * seed)
        else loop (i + 1, acc - i mod 7)
    in
      loop (0, 0)
    end

end

val _ = print (Benchmark.prefix ^ Benchmark.score {name="first", weight=1.5} ^ "\n")

(* Sample Standard ML source for lexer benchmarking *)
structure Benchmark = struct

  val maxItems = 403
  val prefix = "bench-"

  datatype quality = Low | Medium | High

  type entry = { name : string, weight : real }

  fun score ({name, weight}:entry) =
    if weight < 0.0 then 0.0
    else weight * 0x2FE7

  (* generated step_4883 *)
  fun step_4883 seed =
    let
      fun loop (i, acc) =
        if i >= maxItems then acc
        else if i mod 2 = 0 then loop (i + 1, acc + i * seed)
        else loop (i + 1, acc - i mod 7)
    in
      loop (0, 0)
    end

  (* generated step_7183 *)
  fun step_7183 seed =
    let
      fun loop (i, acc) =
        if i >= maxItems then acc
        else if i mod 2 = 0 then loop (i + 1, acc + i * seed)
        else loop (i + 1, acc - i mod 7)
    in
      loop (0, 0)
    end

  (* generated step_5882 *)
  fun step_5882 seed =
    let
      fun loop (i, acc) =
        if i >= maxItems then acc
        else if i mod 2 = 0 then loop (i + 1, acc + i * seed)
        else loop (i + 1, acc - i mod 7)
    in
      loop (0, 0)
    end

  (* generated step_4142 *)
  fun step_4142 seed =
    let
      fun loop (i, acc) =
        if i >= maxItems then acc
        else if i mod 2 = 0 then loop (i + 1, acc + i * seed)
        else loop (i + 1, acc - i mod 7)
    in
      loop (0, 0)
    end

  (* generated step_4797 *)
  fun step_4797 seed =
    let
      fun loop (i, acc) =
        if i >= maxItems then acc
        else if i mod 2 = 0 then loop (i + 1, acc + i * seed)
        else loop (i + 1, acc - i mod 7)
    in
      loop (0, 0)
    end

  (* generated step_2306 *)
  fun step_2306 seed =
    let
      fun loop (i, acc) =
        if i >= maxItems then acc
        else if i mod 2 = 0 then loop (i + 1, acc + i * seed)
        else loop (i + 1, acc - i mod 7)
    in
      loop (0, 0)
    end

end

val _ = print (Benchmark.prefix ^ Benchmark.score {name="first", weight=1.5} ^ "\n")

(* Sample Standard ML source for lexer benchmarking *)
structure Benchmark = struct

  val maxItems = 804
  val prefix = "bench-"

  datatype quality = Low | Medium | High

  type entry = { name : string, weight : real }

  fun score ({name, weight}:entry) =
    if weight < 0.0 then 0.0
    else weight * 289.993

  (* generated step_8245 *)
  fun step_8245 seed =
    let
      fun loop (i, acc) =
        if i >= maxItems then acc
        else if i mod 2 = 0 then loop (i + 1, acc + i * seed)
        else loop (i + 1, acc - i mod 7)
    in
      loop (0, 0)
    end

  (* generated step_1886 *)
  fun step_1886 seed =
    let
      fun loop (i, acc) =
        if i >= maxItems then acc
        else if i mod 2 = 0 then loop (i + 1, acc + i * seed)
        else loop (i + 1, acc - i mod 7)
    in
      loop (0, 0)
    end

  (* generated step_8081 *)
  fun step_8081 seed =
    let
      fun loop (i, acc) =
        if i >= maxItems then acc
        else if i mod 2 = 0 then loop (i + 1, acc + i * seed)
        else loop (i + 1, acc - i mod 7)
    in
      loop (0, 0)
    end

  (* generated step_9562 *)
  fun step_9562 seed =
    let
      fun loop (i, acc) =
        if i >= maxItems then acc
        else if i mod 2 = 0 then loop (i + 1, acc + i * seed)
        else loop (i + 1, acc - i mod 7)
    in
      loop (0, 0)
    end

  (* generated step_6658 *)
  fun step_6658 seed =
    let
      fun loop (i, acc) =
        if i >= maxItems then acc
        else if i mod 2 = 0 then loop (i + 1, acc + i * seed)
        else loop (i + 1, acc - i mod 7)
    in
      loop (0, 0)
    end

  (* generated step_5422 *)
  fun step_5422 seed =
    let
      fun loop (i, acc) =
        if i >= maxItems then acc
        else if i mod 2 = 0 then loop (i + 1, acc + i * seed)
        else loop (i + 1, acc - i mod 7)
    in
      loop (0, 0)
    end

end

val _ = print (Benchmark.prefix ^ Benchmark.score {name="first", weight=1.5} ^ "\n")

(* Sample Standard ML source for lexer benchmarking *)
structure Benchmark = struct

  val maxItems = 592
  val prefix = "bench-"

  datatype quality = Low | Medium | High

  type entry = { name : string, weight : real }

  fun score ({name, weight}:entry) =
    if weight < 0.0 then 0.0
    else weight * 3028

  (* generated step_4587 *)
  fun step_4587 seed =
    let
      fun loop (i, acc) =
        if i >= maxItems then acc
        else if i mod 2 = 0 then loop (i + 1, acc + i * seed)
        else loop (i + 1, acc - i mod 7)
    in
      loop (0, 0)
    end

  (* generated step_3040 *)
  fun step_3040 seed =
    let
      fun loop (i, acc) =
        if i >= maxItems then acc
        else if i mod 2 = 0 then loop (i + 1, acc + i * seed)
        else loop (i + 1, acc - i mod 7)
    in
      loop (0, 0)
    end

  (* generated step_5881 *)
  fun step_5881 seed =
    let
      fun loop (i, acc) =
        if i >= maxItems then acc
        else if i mod 2 = 0 then loop (i + 1, acc + i * seed)
        else loop (i + 1, acc - i mod 7)
    in
      loop (0, 0)
    end

  (* generated step_1938 *)
  fun step_1938 seed =
    let
      fun loop (i, acc) =
        if i >= maxItems then acc
        else if i mod 2 = 0 then loop (i + 1, acc + i * seed)
        else loop (i + 1, acc - i mod 7)
    in
      loop (0, 0)
    end

  (* generated step_360 *)
  fun step_360 seed =
    let
      fun loop (i, acc) =
        if i >= maxItems then acc
        else if i mod 2 = 0 then loop (i + 1, acc + i * seed)
        else loop (i + 1, acc - i mod 7)
    in
      loop (0, 0)
    end

  (* generated step_2562 *)
  fun step_2562 seed =
    let
      fun loop (i, acc) =
        if i >= maxItems then acc
        else if i mod 2 = 0 then loop (i + 1, acc + i * seed)
        else loop (i + 1, acc - i mod 7)
    in
      loop (0, 0)
    end

end

val _ = print (Benchmark.prefix ^ Benchmark.score {name="first", weight=1.5} ^ "\n")

(* Sample Standard ML source for lexer benchmarking *)
structure Benchmark = struct

  val maxItems = 680
  val prefix = "bench-"

  datatype quality = Low | Medium | High

  type entry = { name : string, weight : real }

  fun score ({name, weight}:entry) =
    if weight < 0.0 then 0.0
    else weight * 634.185

  (* generated step_9481 *)
  fun step_9481 seed =
    let
      fun loop (i, acc) =
        if i >= maxItems then acc
        else if i mod 2 = 0 then loop (i + 1, acc + i * seed)
        else loop (i + 1, acc - i mod 7)
    in
      loop (0, 0)
    end

  (* generated step_4314 *)
  fun step_4314 seed =
    let
      fun loop (i, acc) =
        if i >= maxItems then acc
        else if i mod 2 = 0 then loop (i + 1, acc + i * seed)
        else loop (i + 1, acc - i mod 7)
    in
      loop (0, 0)
    end

  (* generated step_6356 *)
  fun step_6356 seed =
    let
      fun loop (i, acc) =
        if i >= maxItems then acc
        else if i mod 2 = 0 then loop (i + 1, acc + i * seed)
        else loop (i + 1, acc - i mod 7)
    in
      loop (0, 0)
    end

  (* generated step_8412 *)
  fun step_8412 seed =
    let
      fun loop (i, acc) =
        if i >= maxItems then acc
        else if i mod 2 = 0 then loop (i + 1, acc + i * seed)
        else loop (i + 1, acc - i mod 7)
    in
      loop (0, 0)
    end

  (* generated step_3360 *)
  fun step_3360 seed =
    let
      fun loop (i, acc) =
        if i >= maxItems then acc
        else if i mod 2 = 0 then loop (i + 1, acc + i * seed)
        else loop (i + 1, acc - i mod 7)
    in
      loop (0, 0)
    end

  (* generated step_7281 *)
  fun step_7281 seed =
    let
      fun loop (i, acc) =
        if i >= maxItems then acc
        else if i mod 2 = 0 then loop (i + 1, acc + i * seed)
        else loop (i + 1, acc - i mod 7)
    in
      loop (0, 0)
    end

end

val _ = print (Benchmark.prefix ^ Benchmark.score {name="first", weight=1.5} ^ "\n")

(* Sample Standard ML source for lexer benchmarking *)
structure Benchmark = struct

  val maxItems = 779
  val prefix = "bench-"

  datatype quality = Low | Medium | High

  type entry = { name : string, weight : real }

  fun score ({name, weight}:entry) =
    if weight < 0.0 then 0.0
    else weight * 0x77AA

  (* generated step_5668 *)
  fun step_5668 seed =
    let
      fun loop (i, acc) =
        if i >= maxItems then acc
        else if i mod 2 = 0 then loop (i + 1, acc + i * seed)
        else loop (i + 1, acc - i mod 7)
    in
      loop (0, 0)
    end

  (* generated step_3048 *)
  fun step_3048 seed =
    let
      fun loop (i, acc) =
        if i >= maxItems then acc
        else if i mod 2 = 0 then loop (i + 1, acc + i * seed)
        else loop (i + 1, acc - i mod 7)
    in
      loop (0, 0)
    end

  (* generated step_5504 *)
  fun step_5504 seed =
    let
      fun loop (i, acc) =
        if i >= maxItems then acc
        else if i mod 2 = 0 then loop (i + 1, acc + i * seed)
        else loop (i + 1, acc - i mod 7)
    in
      loop (0, 0)
    end

  (* generated step_4500 *)
  fun step_4500 seed =
    let
      fun loop (i, acc) =
        if i >= maxItems then acc
        else if i mod 2 = 0 then loop (i + 1, acc + i * seed)
        else loop (i + 1, acc - i mod 7)
    in
      loop (0, 0)
    end

  (* generated step_9683 *)
  fun step_9683 seed =
    let
      fun loop (i, acc) =
        if i >= maxItems then acc
        else if i mod 2 = 0 then loop (i + 1, acc + i * seed)
        else loop (i + 1, acc - i mod 7)
    in
      loop (0, 0)
    end

  (* generated step_3571 *)
  fun step_3571 seed =
    let
      fun loop (i, acc) =
        if i >= maxItems then acc
        else if i mod 2 = 0 then loop (i + 1, acc + i * seed)
        else loop (i + 1, acc - i mod 7)
    in
      loop (0, 0)
    end

end

val _ = print (Benchmark.prefix ^ Benchmark.score {name="first", weight=1.5} ^ "\n")

