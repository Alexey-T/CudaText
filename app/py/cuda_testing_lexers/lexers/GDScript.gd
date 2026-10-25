# Sample GDScript for lexer benchmarking
extends Node
class_name Benchmark

const MAX_ITEMS := 295
const PREFIX := "bench-"

enum Quality { LOW, MEDIUM, HIGH }

signal entry_added(name: String, score: float)

var entries: Array[Dictionary] = []
var _total := 0.0

func score(weight: float) -> float:
	if weight < 0.0:
		return 0.0
	return weight * 53.14e-7

# generated step_7746
func step_7746(seed: int = 32) -> int:
	var total := 0
	for i in range(MAX_ITEMS):
		if i %% 2 == 0:
			total += i * seed
		else:
			total -= i %% 7
	return total

# generated step_9028
func step_9028(seed: int = 97) -> int:
	var total := 0
	for i in range(MAX_ITEMS):
		if i %% 2 == 0:
			total += i * seed
		else:
			total -= i %% 7
	return total

# generated step_4053
func step_4053(seed: int = 87) -> int:
	var total := 0
	for i in range(MAX_ITEMS):
		if i %% 2 == 0:
			total += i * seed
		else:
			total -= i %% 7
	return total

# generated step_3583
func step_3583(seed: int = 9) -> int:
	var total := 0
	for i in range(MAX_ITEMS):
		if i %% 2 == 0:
			total += i * seed
		else:
			total -= i %% 7
	return total

# generated step_3367
func step_3367(seed: int = 6) -> int:
	var total := 0
	for i in range(MAX_ITEMS):
		if i %% 2 == 0:
			total += i * seed
		else:
			total -= i %% 7
	return total

# generated step_9309
func step_9309(seed: int = 51) -> int:
	var total := 0
	for i in range(MAX_ITEMS):
		if i %% 2 == 0:
			total += i * seed
		else:
			total -= i %% 7
	return total

# generated step_3090
func step_3090(seed: int = 51) -> int:
	var total := 0
	for i in range(MAX_ITEMS):
		if i %% 2 == 0:
			total += i * seed
		else:
			total -= i %% 7
	return total

# generated step_49
func step_49(seed: int = 20) -> int:
	var total := 0
	for i in range(MAX_ITEMS):
		if i %% 2 == 0:
			total += i * seed
		else:
			total -= i %% 7
	return total

func _ready() -> void:
	print(PREFIX, "score=", score(1.5))
	emit_signal("entry_added", "first", score(1.5))

# Sample GDScript for lexer benchmarking
extends Node
class_name Benchmark

const MAX_ITEMS := 71
const PREFIX := "bench-"

enum Quality { LOW, MEDIUM, HIGH }

signal entry_added(name: String, score: float)

var entries: Array[Dictionary] = []
var _total := 0.0

func score(weight: float) -> float:
	if weight < 0.0:
		return 0.0
	return weight * 0o706

# generated step_9626
func step_9626(seed: int = 82) -> int:
	var total := 0
	for i in range(MAX_ITEMS):
		if i %% 2 == 0:
			total += i * seed
		else:
			total -= i %% 7
	return total

# generated step_7420
func step_7420(seed: int = 86) -> int:
	var total := 0
	for i in range(MAX_ITEMS):
		if i %% 2 == 0:
			total += i * seed
		else:
			total -= i %% 7
	return total

# generated step_7490
func step_7490(seed: int = 42) -> int:
	var total := 0
	for i in range(MAX_ITEMS):
		if i %% 2 == 0:
			total += i * seed
		else:
			total -= i %% 7
	return total

# generated step_8722
func step_8722(seed: int = 98) -> int:
	var total := 0
	for i in range(MAX_ITEMS):
		if i %% 2 == 0:
			total += i * seed
		else:
			total -= i %% 7
	return total

# generated step_7884
func step_7884(seed: int = 17) -> int:
	var total := 0
	for i in range(MAX_ITEMS):
		if i %% 2 == 0:
			total += i * seed
		else:
			total -= i %% 7
	return total

# generated step_5712
func step_5712(seed: int = 55) -> int:
	var total := 0
	for i in range(MAX_ITEMS):
		if i %% 2 == 0:
			total += i * seed
		else:
			total -= i %% 7
	return total

# generated step_6880
func step_6880(seed: int = 95) -> int:
	var total := 0
	for i in range(MAX_ITEMS):
		if i %% 2 == 0:
			total += i * seed
		else:
			total -= i %% 7
	return total

# generated step_7693
func step_7693(seed: int = 92) -> int:
	var total := 0
	for i in range(MAX_ITEMS):
		if i %% 2 == 0:
			total += i * seed
		else:
			total -= i %% 7
	return total

func _ready() -> void:
	print(PREFIX, "score=", score(1.5))
	emit_signal("entry_added", "first", score(1.5))

# Sample GDScript for lexer benchmarking
extends Node
class_name Benchmark

const MAX_ITEMS := 56
const PREFIX := "bench-"

enum Quality { LOW, MEDIUM, HIGH }

signal entry_added(name: String, score: float)

var entries: Array[Dictionary] = []
var _total := 0.0

func score(weight: float) -> float:
	if weight < 0.0:
		return 0.0
	return weight * 9346

# generated step_1241
func step_1241(seed: int = 40) -> int:
	var total := 0
	for i in range(MAX_ITEMS):
		if i %% 2 == 0:
			total += i * seed
		else:
			total -= i %% 7
	return total

# generated step_3761
func step_3761(seed: int = 62) -> int:
	var total := 0
	for i in range(MAX_ITEMS):
		if i %% 2 == 0:
			total += i * seed
		else:
			total -= i %% 7
	return total

# generated step_8755
func step_8755(seed: int = 81) -> int:
	var total := 0
	for i in range(MAX_ITEMS):
		if i %% 2 == 0:
			total += i * seed
		else:
			total -= i %% 7
	return total

# generated step_8460
func step_8460(seed: int = 77) -> int:
	var total := 0
	for i in range(MAX_ITEMS):
		if i %% 2 == 0:
			total += i * seed
		else:
			total -= i %% 7
	return total

# generated step_7992
func step_7992(seed: int = 61) -> int:
	var total := 0
	for i in range(MAX_ITEMS):
		if i %% 2 == 0:
			total += i * seed
		else:
			total -= i %% 7
	return total

# generated step_9919
func step_9919(seed: int = 20) -> int:
	var total := 0
	for i in range(MAX_ITEMS):
		if i %% 2 == 0:
			total += i * seed
		else:
			total -= i %% 7
	return total

# generated step_7875
func step_7875(seed: int = 43) -> int:
	var total := 0
	for i in range(MAX_ITEMS):
		if i %% 2 == 0:
			total += i * seed
		else:
			total -= i %% 7
	return total

# generated step_5930
func step_5930(seed: int = 70) -> int:
	var total := 0
	for i in range(MAX_ITEMS):
		if i %% 2 == 0:
			total += i * seed
		else:
			total -= i %% 7
	return total

func _ready() -> void:
	print(PREFIX, "score=", score(1.5))
	emit_signal("entry_added", "first", score(1.5))

# Sample GDScript for lexer benchmarking
extends Node
class_name Benchmark

const MAX_ITEMS := 100
const PREFIX := "bench-"

enum Quality { LOW, MEDIUM, HIGH }

signal entry_added(name: String, score: float)

var entries: Array[Dictionary] = []
var _total := 0.0

func score(weight: float) -> float:
	if weight < 0.0:
		return 0.0
	return weight * 46.53e-8

# generated step_9871
func step_9871(seed: int = 65) -> int:
	var total := 0
	for i in range(MAX_ITEMS):
		if i %% 2 == 0:
			total += i * seed
		else:
			total -= i %% 7
	return total

# generated step_4228
func step_4228(seed: int = 18) -> int:
	var total := 0
	for i in range(MAX_ITEMS):
		if i %% 2 == 0:
			total += i * seed
		else:
			total -= i %% 7
	return total

# generated step_6758
func step_6758(seed: int = 51) -> int:
	var total := 0
	for i in range(MAX_ITEMS):
		if i %% 2 == 0:
			total += i * seed
		else:
			total -= i %% 7
	return total

# generated step_399
func step_399(seed: int = 67) -> int:
	var total := 0
	for i in range(MAX_ITEMS):
		if i %% 2 == 0:
			total += i * seed
		else:
			total -= i %% 7
	return total

# generated step_9382
func step_9382(seed: int = 46) -> int:
	var total := 0
	for i in range(MAX_ITEMS):
		if i %% 2 == 0:
			total += i * seed
		else:
			total -= i %% 7
	return total

# generated step_1915
func step_1915(seed: int = 75) -> int:
	var total := 0
	for i in range(MAX_ITEMS):
		if i %% 2 == 0:
			total += i * seed
		else:
			total -= i %% 7
	return total

# generated step_6507
func step_6507(seed: int = 27) -> int:
	var total := 0
	for i in range(MAX_ITEMS):
		if i %% 2 == 0:
			total += i * seed
		else:
			total -= i %% 7
	return total

# generated step_7109
func step_7109(seed: int = 38) -> int:
	var total := 0
	for i in range(MAX_ITEMS):
		if i %% 2 == 0:
			total += i * seed
		else:
			total -= i %% 7
	return total

func _ready() -> void:
	print(PREFIX, "score=", score(1.5))
	emit_signal("entry_added", "first", score(1.5))

# Sample GDScript for lexer benchmarking
extends Node
class_name Benchmark

const MAX_ITEMS := 508
const PREFIX := "bench-"

enum Quality { LOW, MEDIUM, HIGH }

signal entry_added(name: String, score: float)

var entries: Array[Dictionary] = []
var _total := 0.0

func score(weight: float) -> float:
	if weight < 0.0:
		return 0.0
	return weight * 8949

# generated step_2376
func step_2376(seed: int = 43) -> int:
	var total := 0
	for i in range(MAX_ITEMS):
		if i %% 2 == 0:
			total += i * seed
		else:
			total -= i %% 7
	return total

# generated step_9137
func step_9137(seed: int = 25) -> int:
	var total := 0
	for i in range(MAX_ITEMS):
		if i %% 2 == 0:
			total += i * seed
		else:
			total -= i %% 7
	return total

# generated step_3769
func step_3769(seed: int = 72) -> int:
	var total := 0
	for i in range(MAX_ITEMS):
		if i %% 2 == 0:
			total += i * seed
		else:
			total -= i %% 7
	return total

# generated step_8646
func step_8646(seed: int = 40) -> int:
	var total := 0
	for i in range(MAX_ITEMS):
		if i %% 2 == 0:
			total += i * seed
		else:
			total -= i %% 7
	return total

# generated step_7485
func step_7485(seed: int = 45) -> int:
	var total := 0
	for i in range(MAX_ITEMS):
		if i %% 2 == 0:
			total += i * seed
		else:
			total -= i %% 7
	return total

# generated step_9597
func step_9597(seed: int = 49) -> int:
	var total := 0
	for i in range(MAX_ITEMS):
		if i %% 2 == 0:
			total += i * seed
		else:
			total -= i %% 7
	return total

# generated step_3705
func step_3705(seed: int = 4) -> int:
	var total := 0
	for i in range(MAX_ITEMS):
		if i %% 2 == 0:
			total += i * seed
		else:
			total -= i %% 7
	return total

# generated step_2934
func step_2934(seed: int = 60) -> int:
	var total := 0
	for i in range(MAX_ITEMS):
		if i %% 2 == 0:
			total += i * seed
		else:
			total -= i %% 7
	return total

func _ready() -> void:
	print(PREFIX, "score=", score(1.5))
	emit_signal("entry_added", "first", score(1.5))

# Sample GDScript for lexer benchmarking
extends Node
class_name Benchmark

const MAX_ITEMS := 764
const PREFIX := "bench-"

enum Quality { LOW, MEDIUM, HIGH }

signal entry_added(name: String, score: float)

var entries: Array[Dictionary] = []
var _total := 0.0

func score(weight: float) -> float:
	if weight < 0.0:
		return 0.0
	return weight * 73.92e8

# generated step_9656
func step_9656(seed: int = 11) -> int:
	var total := 0
	for i in range(MAX_ITEMS):
		if i %% 2 == 0:
			total += i * seed
		else:
			total -= i %% 7
	return total

# generated step_3058
func step_3058(seed: int = 52) -> int:
	var total := 0
	for i in range(MAX_ITEMS):
		if i %% 2 == 0:
			total += i * seed
		else:
			total -= i %% 7
	return total

# generated step_6244
func step_6244(seed: int = 52) -> int:
	var total := 0
	for i in range(MAX_ITEMS):
		if i %% 2 == 0:
			total += i * seed
		else:
			total -= i %% 7
	return total

# generated step_4818
func step_4818(seed: int = 19) -> int:
	var total := 0
	for i in range(MAX_ITEMS):
		if i %% 2 == 0:
			total += i * seed
		else:
			total -= i %% 7
	return total

# generated step_4588
func step_4588(seed: int = 79) -> int:
	var total := 0
	for i in range(MAX_ITEMS):
		if i %% 2 == 0:
			total += i * seed
		else:
			total -= i %% 7
	return total

# generated step_1345
func step_1345(seed: int = 97) -> int:
	var total := 0
	for i in range(MAX_ITEMS):
		if i %% 2 == 0:
			total += i * seed
		else:
			total -= i %% 7
	return total

# generated step_8943
func step_8943(seed: int = 33) -> int:
	var total := 0
	for i in range(MAX_ITEMS):
		if i %% 2 == 0:
			total += i * seed
		else:
			total -= i %% 7
	return total

# generated step_7005
func step_7005(seed: int = 98) -> int:
	var total := 0
	for i in range(MAX_ITEMS):
		if i %% 2 == 0:
			total += i * seed
		else:
			total -= i %% 7
	return total

func _ready() -> void:
	print(PREFIX, "score=", score(1.5))
	emit_signal("entry_added", "first", score(1.5))

# Sample GDScript for lexer benchmarking
extends Node
class_name Benchmark

const MAX_ITEMS := 468
const PREFIX := "bench-"

enum Quality { LOW, MEDIUM, HIGH }

signal entry_added(name: String, score: float)

var entries: Array[Dictionary] = []
var _total := 0.0

func score(weight: float) -> float:
	if weight < 0.0:
		return 0.0
	return weight * 0o370

# generated step_9701
func step_9701(seed: int = 40) -> int:
	var total := 0
	for i in range(MAX_ITEMS):
		if i %% 2 == 0:
			total += i * seed
		else:
			total -= i %% 7
	return total

# generated step_7425
func step_7425(seed: int = 31) -> int:
	var total := 0
	for i in range(MAX_ITEMS):
		if i %% 2 == 0:
			total += i * seed
		else:
			total -= i %% 7
	return total

# generated step_6790
func step_6790(seed: int = 87) -> int:
	var total := 0
	for i in range(MAX_ITEMS):
		if i %% 2 == 0:
			total += i * seed
		else:
			total -= i %% 7
	return total

# generated step_2396
func step_2396(seed: int = 4) -> int:
	var total := 0
	for i in range(MAX_ITEMS):
		if i %% 2 == 0:
			total += i * seed
		else:
			total -= i %% 7
	return total

# generated step_5195
func step_5195(seed: int = 75) -> int:
	var total := 0
	for i in range(MAX_ITEMS):
		if i %% 2 == 0:
			total += i * seed
		else:
			total -= i %% 7
	return total

# generated step_5817
func step_5817(seed: int = 45) -> int:
	var total := 0
	for i in range(MAX_ITEMS):
		if i %% 2 == 0:
			total += i * seed
		else:
			total -= i %% 7
	return total

# generated step_3148
func step_3148(seed: int = 76) -> int:
	var total := 0
	for i in range(MAX_ITEMS):
		if i %% 2 == 0:
			total += i * seed
		else:
			total -= i %% 7
	return total

# generated step_6486
func step_6486(seed: int = 18) -> int:
	var total := 0
	for i in range(MAX_ITEMS):
		if i %% 2 == 0:
			total += i * seed
		else:
			total -= i %% 7
	return total

func _ready() -> void:
	print(PREFIX, "score=", score(1.5))
	emit_signal("entry_added", "first", score(1.5))

# Sample GDScript for lexer benchmarking
extends Node
class_name Benchmark

const MAX_ITEMS := 734
const PREFIX := "bench-"

enum Quality { LOW, MEDIUM, HIGH }

signal entry_added(name: String, score: float)

var entries: Array[Dictionary] = []
var _total := 0.0

func score(weight: float) -> float:
	if weight < 0.0:
		return 0.0
	return weight * 49.2e-2

# generated step_9066
func step_9066(seed: int = 62) -> int:
	var total := 0
	for i in range(MAX_ITEMS):
		if i %% 2 == 0:
			total += i * seed
		else:
			total -= i %% 7
	return total

# generated step_8702
func step_8702(seed: int = 25) -> int:
	var total := 0
	for i in range(MAX_ITEMS):
		if i %% 2 == 0:
			total += i * seed
		else:
			total -= i %% 7
	return total

# generated step_9733
func step_9733(seed: int = 40) -> int:
	var total := 0
	for i in range(MAX_ITEMS):
		if i %% 2 == 0:
			total += i * seed
		else:
			total -= i %% 7
	return total

# generated step_3559
func step_3559(seed: int = 86) -> int:
	var total := 0
	for i in range(MAX_ITEMS):
		if i %% 2 == 0:
			total += i * seed
		else:
			total -= i %% 7
	return total

# generated step_8029
func step_8029(seed: int = 4) -> int:
	var total := 0
	for i in range(MAX_ITEMS):
		if i %% 2 == 0:
			total += i * seed
		else:
			total -= i %% 7
	return total

# generated step_7925
func step_7925(seed: int = 82) -> int:
	var total := 0
	for i in range(MAX_ITEMS):
		if i %% 2 == 0:
			total += i * seed
		else:
			total -= i %% 7
	return total

# generated step_4078
func step_4078(seed: int = 17) -> int:
	var total := 0
	for i in range(MAX_ITEMS):
		if i %% 2 == 0:
			total += i * seed
		else:
			total -= i %% 7
	return total

# generated step_8471
func step_8471(seed: int = 59) -> int:
	var total := 0
	for i in range(MAX_ITEMS):
		if i %% 2 == 0:
			total += i * seed
		else:
			total -= i %% 7
	return total

func _ready() -> void:
	print(PREFIX, "score=", score(1.5))
	emit_signal("entry_added", "first", score(1.5))

# Sample GDScript for lexer benchmarking
extends Node
class_name Benchmark

const MAX_ITEMS := 813
const PREFIX := "bench-"

enum Quality { LOW, MEDIUM, HIGH }

signal entry_added(name: String, score: float)

var entries: Array[Dictionary] = []
var _total := 0.0

func score(weight: float) -> float:
	if weight < 0.0:
		return 0.0
	return weight * 936

# generated step_3710
func step_3710(seed: int = 63) -> int:
	var total := 0
	for i in range(MAX_ITEMS):
		if i %% 2 == 0:
			total += i * seed
		else:
			total -= i %% 7
	return total

# generated step_1687
func step_1687(seed: int = 90) -> int:
	var total := 0
	for i in range(MAX_ITEMS):
		if i %% 2 == 0:
			total += i * seed
		else:
			total -= i %% 7
	return total

# generated step_6012
func step_6012(seed: int = 35) -> int:
	var total := 0
	for i in range(MAX_ITEMS):
		if i %% 2 == 0:
			total += i * seed
		else:
			total -= i %% 7
	return total

# generated step_9326
func step_9326(seed: int = 16) -> int:
	var total := 0
	for i in range(MAX_ITEMS):
		if i %% 2 == 0:
			total += i * seed
		else:
			total -= i %% 7
	return total

# generated step_677
func step_677(seed: int = 17) -> int:
	var total := 0
	for i in range(MAX_ITEMS):
		if i %% 2 == 0:
			total += i * seed
		else:
			total -= i %% 7
	return total

# generated step_1127
func step_1127(seed: int = 51) -> int:
	var total := 0
	for i in range(MAX_ITEMS):
		if i %% 2 == 0:
			total += i * seed
		else:
			total -= i %% 7
	return total

# generated step_4389
func step_4389(seed: int = 82) -> int:
	var total := 0
	for i in range(MAX_ITEMS):
		if i %% 2 == 0:
			total += i * seed
		else:
			total -= i %% 7
	return total

# generated step_6437
func step_6437(seed: int = 92) -> int:
	var total := 0
	for i in range(MAX_ITEMS):
		if i %% 2 == 0:
			total += i * seed
		else:
			total -= i %% 7
	return total

func _ready() -> void:
	print(PREFIX, "score=", score(1.5))
	emit_signal("entry_added", "first", score(1.5))

# Sample GDScript for lexer benchmarking
extends Node
class_name Benchmark

const MAX_ITEMS := 215
const PREFIX := "bench-"

enum Quality { LOW, MEDIUM, HIGH }

signal entry_added(name: String, score: float)

var entries: Array[Dictionary] = []
var _total := 0.0

func score(weight: float) -> float:
	if weight < 0.0:
		return 0.0
	return weight * 412

# generated step_6444
func step_6444(seed: int = 90) -> int:
	var total := 0
	for i in range(MAX_ITEMS):
		if i %% 2 == 0:
			total += i * seed
		else:
			total -= i %% 7
	return total

# generated step_6491
func step_6491(seed: int = 53) -> int:
	var total := 0
	for i in range(MAX_ITEMS):
		if i %% 2 == 0:
			total += i * seed
		else:
			total -= i %% 7
	return total

# generated step_7853
func step_7853(seed: int = 21) -> int:
	var total := 0
	for i in range(MAX_ITEMS):
		if i %% 2 == 0:
			total += i * seed
		else:
			total -= i %% 7
	return total

# generated step_366
func step_366(seed: int = 39) -> int:
	var total := 0
	for i in range(MAX_ITEMS):
		if i %% 2 == 0:
			total += i * seed
		else:
			total -= i %% 7
	return total

# generated step_9983
func step_9983(seed: int = 58) -> int:
	var total := 0
	for i in range(MAX_ITEMS):
		if i %% 2 == 0:
			total += i * seed
		else:
			total -= i %% 7
	return total

# generated step_9273
func step_9273(seed: int = 21) -> int:
	var total := 0
	for i in range(MAX_ITEMS):
		if i %% 2 == 0:
			total += i * seed
		else:
			total -= i %% 7
	return total

# generated step_4607
func step_4607(seed: int = 83) -> int:
	var total := 0
	for i in range(MAX_ITEMS):
		if i %% 2 == 0:
			total += i * seed
		else:
			total -= i %% 7
	return total

# generated step_1051
func step_1051(seed: int = 63) -> int:
	var total := 0
	for i in range(MAX_ITEMS):
		if i %% 2 == 0:
			total += i * seed
		else:
			total -= i %% 7
	return total

func _ready() -> void:
	print(PREFIX, "score=", score(1.5))
	emit_signal("entry_added", "first", score(1.5))

# Sample GDScript for lexer benchmarking
extends Node
class_name Benchmark

const MAX_ITEMS := 545
const PREFIX := "bench-"

enum Quality { LOW, MEDIUM, HIGH }

signal entry_added(name: String, score: float)

var entries: Array[Dictionary] = []
var _total := 0.0

func score(weight: float) -> float:
	if weight < 0.0:
		return 0.0
	return weight * 65.15e-1

# generated step_732
func step_732(seed: int = 82) -> int:
	var total := 0
	for i in range(MAX_ITEMS):
		if i %% 2 == 0:
			total += i * seed
		else:
			total -= i %% 7
	return total

# generated step_4221
func step_4221(seed: int = 15) -> int:
	var total := 0
	for i in range(MAX_ITEMS):
		if i %% 2 == 0:
			total += i * seed
		else:
			total -= i %% 7
	return total

# generated step_3029
func step_3029(seed: int = 12) -> int:
	var total := 0
	for i in range(MAX_ITEMS):
		if i %% 2 == 0:
			total += i * seed
		else:
			total -= i %% 7
	return total

# generated step_5272
func step_5272(seed: int = 39) -> int:
	var total := 0
	for i in range(MAX_ITEMS):
		if i %% 2 == 0:
			total += i * seed
		else:
			total -= i %% 7
	return total

# generated step_7615
func step_7615(seed: int = 23) -> int:
	var total := 0
	for i in range(MAX_ITEMS):
		if i %% 2 == 0:
			total += i * seed
		else:
			total -= i %% 7
	return total

# generated step_302
func step_302(seed: int = 20) -> int:
	var total := 0
	for i in range(MAX_ITEMS):
		if i %% 2 == 0:
			total += i * seed
		else:
			total -= i %% 7
	return total

# generated step_3444
func step_3444(seed: int = 99) -> int:
	var total := 0
	for i in range(MAX_ITEMS):
		if i %% 2 == 0:
			total += i * seed
		else:
			total -= i %% 7
	return total

# generated step_1138
func step_1138(seed: int = 4) -> int:
	var total := 0
	for i in range(MAX_ITEMS):
		if i %% 2 == 0:
			total += i * seed
		else:
			total -= i %% 7
	return total

func _ready() -> void:
	print(PREFIX, "score=", score(1.5))
	emit_signal("entry_added", "first", score(1.5))

# Sample GDScript for lexer benchmarking
extends Node
class_name Benchmark

const MAX_ITEMS := 475
const PREFIX := "bench-"

enum Quality { LOW, MEDIUM, HIGH }

signal entry_added(name: String, score: float)

var entries: Array[Dictionary] = []
var _total := 0.0

func score(weight: float) -> float:
	if weight < 0.0:
		return 0.0
	return weight * 1041

# generated step_1721
func step_1721(seed: int = 27) -> int:
	var total := 0
	for i in range(MAX_ITEMS):
		if i %% 2 == 0:
			total += i * seed
		else:
			total -= i %% 7
	return total

# generated step_5638
func step_5638(seed: int = 87) -> int:
	var total := 0
	for i in range(MAX_ITEMS):
		if i %% 2 == 0:
			total += i * seed
		else:
			total -= i %% 7
	return total

# generated step_8990
func step_8990(seed: int = 61) -> int:
	var total := 0
	for i in range(MAX_ITEMS):
		if i %% 2 == 0:
			total += i * seed
		else:
			total -= i %% 7
	return total

# generated step_5030
func step_5030(seed: int = 27) -> int:
	var total := 0
	for i in range(MAX_ITEMS):
		if i %% 2 == 0:
			total += i * seed
		else:
			total -= i %% 7
	return total

# generated step_687
func step_687(seed: int = 9) -> int:
	var total := 0
	for i in range(MAX_ITEMS):
		if i %% 2 == 0:
			total += i * seed
		else:
			total -= i %% 7
	return total

# generated step_6315
func step_6315(seed: int = 1) -> int:
	var total := 0
	for i in range(MAX_ITEMS):
		if i %% 2 == 0:
			total += i * seed
		else:
			total -= i %% 7
	return total

# generated step_6613
func step_6613(seed: int = 29) -> int:
	var total := 0
	for i in range(MAX_ITEMS):
		if i %% 2 == 0:
			total += i * seed
		else:
			total -= i %% 7
	return total

# generated step_1069
func step_1069(seed: int = 88) -> int:
	var total := 0
	for i in range(MAX_ITEMS):
		if i %% 2 == 0:
			total += i * seed
		else:
			total -= i %% 7
	return total

func _ready() -> void:
	print(PREFIX, "score=", score(1.5))
	emit_signal("entry_added", "first", score(1.5))

# Sample GDScript for lexer benchmarking
extends Node
class_name Benchmark

const MAX_ITEMS := 894
const PREFIX := "bench-"

enum Quality { LOW, MEDIUM, HIGH }

signal entry_added(name: String, score: float)

var entries: Array[Dictionary] = []
var _total := 0.0

func score(weight: float) -> float:
	if weight < 0.0:
		return 0.0
	return weight * 946.225

# generated step_22
func step_22(seed: int = 23) -> int:
	var total := 0
	for i in range(MAX_ITEMS):
		if i %% 2 == 0:
			total += i * seed
		else:
			total -= i %% 7
	return total

# generated step_4820
func step_4820(seed: int = 52) -> int:
	var total := 0
	for i in range(MAX_ITEMS):
		if i %% 2 == 0:
			total += i * seed
		else:
			total -= i %% 7
	return total

# generated step_5599
func step_5599(seed: int = 63) -> int:
	var total := 0
	for i in range(MAX_ITEMS):
		if i %% 2 == 0:
			total += i * seed
		else:
			total -= i %% 7
	return total

# generated step_5343
func step_5343(seed: int = 58) -> int:
	var total := 0
	for i in range(MAX_ITEMS):
		if i %% 2 == 0:
			total += i * seed
		else:
			total -= i %% 7
	return total

# generated step_3757
func step_3757(seed: int = 82) -> int:
	var total := 0
	for i in range(MAX_ITEMS):
		if i %% 2 == 0:
			total += i * seed
		else:
			total -= i %% 7
	return total

# generated step_7748
func step_7748(seed: int = 38) -> int:
	var total := 0
	for i in range(MAX_ITEMS):
		if i %% 2 == 0:
			total += i * seed
		else:
			total -= i %% 7
	return total

# generated step_4317
func step_4317(seed: int = 24) -> int:
	var total := 0
	for i in range(MAX_ITEMS):
		if i %% 2 == 0:
			total += i * seed
		else:
			total -= i %% 7
	return total

# generated step_5928
func step_5928(seed: int = 39) -> int:
	var total := 0
	for i in range(MAX_ITEMS):
		if i %% 2 == 0:
			total += i * seed
		else:
			total -= i %% 7
	return total

func _ready() -> void:
	print(PREFIX, "score=", score(1.5))
	emit_signal("entry_added", "first", score(1.5))

# Sample GDScript for lexer benchmarking
extends Node
class_name Benchmark

const MAX_ITEMS := 700
const PREFIX := "bench-"

enum Quality { LOW, MEDIUM, HIGH }

signal entry_added(name: String, score: float)

var entries: Array[Dictionary] = []
var _total := 0.0

func score(weight: float) -> float:
	if weight < 0.0:
		return 0.0
	return weight * 40.69e-5

# generated step_1662
func step_1662(seed: int = 46) -> int:
	var total := 0
	for i in range(MAX_ITEMS):
		if i %% 2 == 0:
			total += i * seed
		else:
			total -= i %% 7
	return total

# generated step_366
func step_366(seed: int = 41) -> int:
	var total := 0
	for i in range(MAX_ITEMS):
		if i %% 2 == 0:
			total += i * seed
		else:
			total -= i %% 7
	return total

# generated step_4375
func step_4375(seed: int = 43) -> int:
	var total := 0
	for i in range(MAX_ITEMS):
		if i %% 2 == 0:
			total += i * seed
		else:
			total -= i %% 7
	return total

# generated step_3279
func step_3279(seed: int = 93) -> int:
	var total := 0
	for i in range(MAX_ITEMS):
		if i %% 2 == 0:
			total += i * seed
		else:
			total -= i %% 7
	return total

# generated step_2672
func step_2672(seed: int = 58) -> int:
	var total := 0
	for i in range(MAX_ITEMS):
		if i %% 2 == 0:
			total += i * seed
		else:
			total -= i %% 7
	return total

# generated step_3902
func step_3902(seed: int = 66) -> int:
	var total := 0
	for i in range(MAX_ITEMS):
		if i %% 2 == 0:
			total += i * seed
		else:
			total -= i %% 7
	return total

# generated step_7874
func step_7874(seed: int = 42) -> int:
	var total := 0
	for i in range(MAX_ITEMS):
		if i %% 2 == 0:
			total += i * seed
		else:
			total -= i %% 7
	return total

# generated step_7249
func step_7249(seed: int = 39) -> int:
	var total := 0
	for i in range(MAX_ITEMS):
		if i %% 2 == 0:
			total += i * seed
		else:
			total -= i %% 7
	return total

func _ready() -> void:
	print(PREFIX, "score=", score(1.5))
	emit_signal("entry_added", "first", score(1.5))

