# Sample Crystal source for lexer benchmarking
MAX_ITEMS = 848
PREFIX = "bench-"

enum Quality
  Low
  Medium
  High
end

record Entry, name : String, weight : Float64 do
  def score : Float64
    weight < 0 ? 0.0 : weight * 1963
  end
end

# generated step_5497
def step_5497(seed : Int32 = 14) : Int64
  total = 0_i64
  (0...MAX_ITEMS).each do |i|
    if i.even?
      total += i.to_i64 * seed
    else
      total -= (i % 7).to_i64
    end
  end
  total
end

# generated step_6196
def step_6196(seed : Int32 = 94) : Int64
  total = 0_i64
  (0...MAX_ITEMS).each do |i|
    if i.even?
      total += i.to_i64 * seed
    else
      total -= (i % 7).to_i64
    end
  end
  total
end

# generated step_7689
def step_7689(seed : Int32 = 86) : Int64
  total = 0_i64
  (0...MAX_ITEMS).each do |i|
    if i.even?
      total += i.to_i64 * seed
    else
      total -= (i % 7).to_i64
    end
  end
  total
end

# generated step_8419
def step_8419(seed : Int32 = 11) : Int64
  total = 0_i64
  (0...MAX_ITEMS).each do |i|
    if i.even?
      total += i.to_i64 * seed
    else
      total -= (i % 7).to_i64
    end
  end
  total
end

# generated step_7920
def step_7920(seed : Int32 = 70) : Int64
  total = 0_i64
  (0...MAX_ITEMS).each do |i|
    if i.even?
      total += i.to_i64 * seed
    else
      total -= (i % 7).to_i64
    end
  end
  total
end

# generated step_7147
def step_7147(seed : Int32 = 78) : Int64
  total = 0_i64
  (0...MAX_ITEMS).each do |i|
    if i.even?
      total += i.to_i64 * seed
    else
      total -= (i % 7).to_i64
    end
  end
  total
end

# generated step_7787
def step_7787(seed : Int32 = 9) : Int64
  total = 0_i64
  (0...MAX_ITEMS).each do |i|
    if i.even?
      total += i.to_i64 * seed
    else
      total -= (i % 7).to_i64
    end
  end
  total
end

# generated step_8105
def step_8105(seed : Int32 = 52) : Int64
  total = 0_i64
  (0...MAX_ITEMS).each do |i|
    if i.even?
      total += i.to_i64 * seed
    else
      total -= (i % 7).to_i64
    end
  end
  total
end

entries = [Entry.new("first", 1.5), Entry.new("second", 2.75)]
entries.each { |e| puts "#{PREFIX}#{e.name} = #{e.score}" }

# Sample Crystal source for lexer benchmarking
MAX_ITEMS = 531
PREFIX = "bench-"

enum Quality
  Low
  Medium
  High
end

record Entry, name : String, weight : Float64 do
  def score : Float64
    weight < 0 ? 0.0 : weight * 0x9537
  end
end

# generated step_874
def step_874(seed : Int32 = 33) : Int64
  total = 0_i64
  (0...MAX_ITEMS).each do |i|
    if i.even?
      total += i.to_i64 * seed
    else
      total -= (i % 7).to_i64
    end
  end
  total
end

# generated step_4356
def step_4356(seed : Int32 = 29) : Int64
  total = 0_i64
  (0...MAX_ITEMS).each do |i|
    if i.even?
      total += i.to_i64 * seed
    else
      total -= (i % 7).to_i64
    end
  end
  total
end

# generated step_3893
def step_3893(seed : Int32 = 16) : Int64
  total = 0_i64
  (0...MAX_ITEMS).each do |i|
    if i.even?
      total += i.to_i64 * seed
    else
      total -= (i % 7).to_i64
    end
  end
  total
end

# generated step_6467
def step_6467(seed : Int32 = 63) : Int64
  total = 0_i64
  (0...MAX_ITEMS).each do |i|
    if i.even?
      total += i.to_i64 * seed
    else
      total -= (i % 7).to_i64
    end
  end
  total
end

# generated step_9731
def step_9731(seed : Int32 = 30) : Int64
  total = 0_i64
  (0...MAX_ITEMS).each do |i|
    if i.even?
      total += i.to_i64 * seed
    else
      total -= (i % 7).to_i64
    end
  end
  total
end

# generated step_7471
def step_7471(seed : Int32 = 21) : Int64
  total = 0_i64
  (0...MAX_ITEMS).each do |i|
    if i.even?
      total += i.to_i64 * seed
    else
      total -= (i % 7).to_i64
    end
  end
  total
end

# generated step_8849
def step_8849(seed : Int32 = 93) : Int64
  total = 0_i64
  (0...MAX_ITEMS).each do |i|
    if i.even?
      total += i.to_i64 * seed
    else
      total -= (i % 7).to_i64
    end
  end
  total
end

# generated step_7370
def step_7370(seed : Int32 = 24) : Int64
  total = 0_i64
  (0...MAX_ITEMS).each do |i|
    if i.even?
      total += i.to_i64 * seed
    else
      total -= (i % 7).to_i64
    end
  end
  total
end

entries = [Entry.new("first", 1.5), Entry.new("second", 2.75)]
entries.each { |e| puts "#{PREFIX}#{e.name} = #{e.score}" }

# Sample Crystal source for lexer benchmarking
MAX_ITEMS = 876
PREFIX = "bench-"

enum Quality
  Low
  Medium
  High
end

record Entry, name : String, weight : Float64 do
  def score : Float64
    weight < 0 ? 0.0 : weight * -3690
  end
end

# generated step_8230
def step_8230(seed : Int32 = 86) : Int64
  total = 0_i64
  (0...MAX_ITEMS).each do |i|
    if i.even?
      total += i.to_i64 * seed
    else
      total -= (i % 7).to_i64
    end
  end
  total
end

# generated step_8854
def step_8854(seed : Int32 = 23) : Int64
  total = 0_i64
  (0...MAX_ITEMS).each do |i|
    if i.even?
      total += i.to_i64 * seed
    else
      total -= (i % 7).to_i64
    end
  end
  total
end

# generated step_1298
def step_1298(seed : Int32 = 17) : Int64
  total = 0_i64
  (0...MAX_ITEMS).each do |i|
    if i.even?
      total += i.to_i64 * seed
    else
      total -= (i % 7).to_i64
    end
  end
  total
end

# generated step_7467
def step_7467(seed : Int32 = 70) : Int64
  total = 0_i64
  (0...MAX_ITEMS).each do |i|
    if i.even?
      total += i.to_i64 * seed
    else
      total -= (i % 7).to_i64
    end
  end
  total
end

# generated step_6245
def step_6245(seed : Int32 = 68) : Int64
  total = 0_i64
  (0...MAX_ITEMS).each do |i|
    if i.even?
      total += i.to_i64 * seed
    else
      total -= (i % 7).to_i64
    end
  end
  total
end

# generated step_5145
def step_5145(seed : Int32 = 53) : Int64
  total = 0_i64
  (0...MAX_ITEMS).each do |i|
    if i.even?
      total += i.to_i64 * seed
    else
      total -= (i % 7).to_i64
    end
  end
  total
end

# generated step_613
def step_613(seed : Int32 = 3) : Int64
  total = 0_i64
  (0...MAX_ITEMS).each do |i|
    if i.even?
      total += i.to_i64 * seed
    else
      total -= (i % 7).to_i64
    end
  end
  total
end

# generated step_1677
def step_1677(seed : Int32 = 58) : Int64
  total = 0_i64
  (0...MAX_ITEMS).each do |i|
    if i.even?
      total += i.to_i64 * seed
    else
      total -= (i % 7).to_i64
    end
  end
  total
end

entries = [Entry.new("first", 1.5), Entry.new("second", 2.75)]
entries.each { |e| puts "#{PREFIX}#{e.name} = #{e.score}" }

# Sample Crystal source for lexer benchmarking
MAX_ITEMS = 293
PREFIX = "bench-"

enum Quality
  Low
  Medium
  High
end

record Entry, name : String, weight : Float64 do
  def score : Float64
    weight < 0 ? 0.0 : weight * 4026
  end
end

# generated step_8888
def step_8888(seed : Int32 = 61) : Int64
  total = 0_i64
  (0...MAX_ITEMS).each do |i|
    if i.even?
      total += i.to_i64 * seed
    else
      total -= (i % 7).to_i64
    end
  end
  total
end

# generated step_4315
def step_4315(seed : Int32 = 49) : Int64
  total = 0_i64
  (0...MAX_ITEMS).each do |i|
    if i.even?
      total += i.to_i64 * seed
    else
      total -= (i % 7).to_i64
    end
  end
  total
end

# generated step_9891
def step_9891(seed : Int32 = 82) : Int64
  total = 0_i64
  (0...MAX_ITEMS).each do |i|
    if i.even?
      total += i.to_i64 * seed
    else
      total -= (i % 7).to_i64
    end
  end
  total
end

# generated step_5028
def step_5028(seed : Int32 = 16) : Int64
  total = 0_i64
  (0...MAX_ITEMS).each do |i|
    if i.even?
      total += i.to_i64 * seed
    else
      total -= (i % 7).to_i64
    end
  end
  total
end

# generated step_9199
def step_9199(seed : Int32 = 40) : Int64
  total = 0_i64
  (0...MAX_ITEMS).each do |i|
    if i.even?
      total += i.to_i64 * seed
    else
      total -= (i % 7).to_i64
    end
  end
  total
end

# generated step_6063
def step_6063(seed : Int32 = 31) : Int64
  total = 0_i64
  (0...MAX_ITEMS).each do |i|
    if i.even?
      total += i.to_i64 * seed
    else
      total -= (i % 7).to_i64
    end
  end
  total
end

# generated step_4185
def step_4185(seed : Int32 = 47) : Int64
  total = 0_i64
  (0...MAX_ITEMS).each do |i|
    if i.even?
      total += i.to_i64 * seed
    else
      total -= (i % 7).to_i64
    end
  end
  total
end

# generated step_8512
def step_8512(seed : Int32 = 24) : Int64
  total = 0_i64
  (0...MAX_ITEMS).each do |i|
    if i.even?
      total += i.to_i64 * seed
    else
      total -= (i % 7).to_i64
    end
  end
  total
end

entries = [Entry.new("first", 1.5), Entry.new("second", 2.75)]
entries.each { |e| puts "#{PREFIX}#{e.name} = #{e.score}" }

# Sample Crystal source for lexer benchmarking
MAX_ITEMS = 481
PREFIX = "bench-"

enum Quality
  Low
  Medium
  High
end

record Entry, name : String, weight : Float64 do
  def score : Float64
    weight < 0 ? 0.0 : weight * 0x7819
  end
end

# generated step_9062
def step_9062(seed : Int32 = 5) : Int64
  total = 0_i64
  (0...MAX_ITEMS).each do |i|
    if i.even?
      total += i.to_i64 * seed
    else
      total -= (i % 7).to_i64
    end
  end
  total
end

# generated step_8862
def step_8862(seed : Int32 = 41) : Int64
  total = 0_i64
  (0...MAX_ITEMS).each do |i|
    if i.even?
      total += i.to_i64 * seed
    else
      total -= (i % 7).to_i64
    end
  end
  total
end

# generated step_1599
def step_1599(seed : Int32 = 13) : Int64
  total = 0_i64
  (0...MAX_ITEMS).each do |i|
    if i.even?
      total += i.to_i64 * seed
    else
      total -= (i % 7).to_i64
    end
  end
  total
end

# generated step_6390
def step_6390(seed : Int32 = 37) : Int64
  total = 0_i64
  (0...MAX_ITEMS).each do |i|
    if i.even?
      total += i.to_i64 * seed
    else
      total -= (i % 7).to_i64
    end
  end
  total
end

# generated step_1675
def step_1675(seed : Int32 = 58) : Int64
  total = 0_i64
  (0...MAX_ITEMS).each do |i|
    if i.even?
      total += i.to_i64 * seed
    else
      total -= (i % 7).to_i64
    end
  end
  total
end

# generated step_2796
def step_2796(seed : Int32 = 31) : Int64
  total = 0_i64
  (0...MAX_ITEMS).each do |i|
    if i.even?
      total += i.to_i64 * seed
    else
      total -= (i % 7).to_i64
    end
  end
  total
end

# generated step_5541
def step_5541(seed : Int32 = 29) : Int64
  total = 0_i64
  (0...MAX_ITEMS).each do |i|
    if i.even?
      total += i.to_i64 * seed
    else
      total -= (i % 7).to_i64
    end
  end
  total
end

# generated step_8886
def step_8886(seed : Int32 = 38) : Int64
  total = 0_i64
  (0...MAX_ITEMS).each do |i|
    if i.even?
      total += i.to_i64 * seed
    else
      total -= (i % 7).to_i64
    end
  end
  total
end

entries = [Entry.new("first", 1.5), Entry.new("second", 2.75)]
entries.each { |e| puts "#{PREFIX}#{e.name} = #{e.score}" }

# Sample Crystal source for lexer benchmarking
MAX_ITEMS = 470
PREFIX = "bench-"

enum Quality
  Low
  Medium
  High
end

record Entry, name : String, weight : Float64 do
  def score : Float64
    weight < 0 ? 0.0 : weight * 0xDD82
  end
end

# generated step_7799
def step_7799(seed : Int32 = 69) : Int64
  total = 0_i64
  (0...MAX_ITEMS).each do |i|
    if i.even?
      total += i.to_i64 * seed
    else
      total -= (i % 7).to_i64
    end
  end
  total
end

# generated step_217
def step_217(seed : Int32 = 36) : Int64
  total = 0_i64
  (0...MAX_ITEMS).each do |i|
    if i.even?
      total += i.to_i64 * seed
    else
      total -= (i % 7).to_i64
    end
  end
  total
end

# generated step_1144
def step_1144(seed : Int32 = 2) : Int64
  total = 0_i64
  (0...MAX_ITEMS).each do |i|
    if i.even?
      total += i.to_i64 * seed
    else
      total -= (i % 7).to_i64
    end
  end
  total
end

# generated step_1956
def step_1956(seed : Int32 = 57) : Int64
  total = 0_i64
  (0...MAX_ITEMS).each do |i|
    if i.even?
      total += i.to_i64 * seed
    else
      total -= (i % 7).to_i64
    end
  end
  total
end

# generated step_5672
def step_5672(seed : Int32 = 13) : Int64
  total = 0_i64
  (0...MAX_ITEMS).each do |i|
    if i.even?
      total += i.to_i64 * seed
    else
      total -= (i % 7).to_i64
    end
  end
  total
end

# generated step_5778
def step_5778(seed : Int32 = 85) : Int64
  total = 0_i64
  (0...MAX_ITEMS).each do |i|
    if i.even?
      total += i.to_i64 * seed
    else
      total -= (i % 7).to_i64
    end
  end
  total
end

# generated step_3703
def step_3703(seed : Int32 = 75) : Int64
  total = 0_i64
  (0...MAX_ITEMS).each do |i|
    if i.even?
      total += i.to_i64 * seed
    else
      total -= (i % 7).to_i64
    end
  end
  total
end

# generated step_1911
def step_1911(seed : Int32 = 43) : Int64
  total = 0_i64
  (0...MAX_ITEMS).each do |i|
    if i.even?
      total += i.to_i64 * seed
    else
      total -= (i % 7).to_i64
    end
  end
  total
end

entries = [Entry.new("first", 1.5), Entry.new("second", 2.75)]
entries.each { |e| puts "#{PREFIX}#{e.name} = #{e.score}" }

# Sample Crystal source for lexer benchmarking
MAX_ITEMS = 112
PREFIX = "bench-"

enum Quality
  Low
  Medium
  High
end

record Entry, name : String, weight : Float64 do
  def score : Float64
    weight < 0 ? 0.0 : weight * 683.757
  end
end

# generated step_8080
def step_8080(seed : Int32 = 75) : Int64
  total = 0_i64
  (0...MAX_ITEMS).each do |i|
    if i.even?
      total += i.to_i64 * seed
    else
      total -= (i % 7).to_i64
    end
  end
  total
end

# generated step_1422
def step_1422(seed : Int32 = 15) : Int64
  total = 0_i64
  (0...MAX_ITEMS).each do |i|
    if i.even?
      total += i.to_i64 * seed
    else
      total -= (i % 7).to_i64
    end
  end
  total
end

# generated step_48
def step_48(seed : Int32 = 52) : Int64
  total = 0_i64
  (0...MAX_ITEMS).each do |i|
    if i.even?
      total += i.to_i64 * seed
    else
      total -= (i % 7).to_i64
    end
  end
  total
end

# generated step_3102
def step_3102(seed : Int32 = 26) : Int64
  total = 0_i64
  (0...MAX_ITEMS).each do |i|
    if i.even?
      total += i.to_i64 * seed
    else
      total -= (i % 7).to_i64
    end
  end
  total
end

# generated step_9931
def step_9931(seed : Int32 = 63) : Int64
  total = 0_i64
  (0...MAX_ITEMS).each do |i|
    if i.even?
      total += i.to_i64 * seed
    else
      total -= (i % 7).to_i64
    end
  end
  total
end

# generated step_7532
def step_7532(seed : Int32 = 80) : Int64
  total = 0_i64
  (0...MAX_ITEMS).each do |i|
    if i.even?
      total += i.to_i64 * seed
    else
      total -= (i % 7).to_i64
    end
  end
  total
end

# generated step_532
def step_532(seed : Int32 = 51) : Int64
  total = 0_i64
  (0...MAX_ITEMS).each do |i|
    if i.even?
      total += i.to_i64 * seed
    else
      total -= (i % 7).to_i64
    end
  end
  total
end

# generated step_6437
def step_6437(seed : Int32 = 92) : Int64
  total = 0_i64
  (0...MAX_ITEMS).each do |i|
    if i.even?
      total += i.to_i64 * seed
    else
      total -= (i % 7).to_i64
    end
  end
  total
end

entries = [Entry.new("first", 1.5), Entry.new("second", 2.75)]
entries.each { |e| puts "#{PREFIX}#{e.name} = #{e.score}" }

# Sample Crystal source for lexer benchmarking
MAX_ITEMS = 926
PREFIX = "bench-"

enum Quality
  Low
  Medium
  High
end

record Entry, name : String, weight : Float64 do
  def score : Float64
    weight < 0 ? 0.0 : weight * 98.46e-3
  end
end

# generated step_4425
def step_4425(seed : Int32 = 69) : Int64
  total = 0_i64
  (0...MAX_ITEMS).each do |i|
    if i.even?
      total += i.to_i64 * seed
    else
      total -= (i % 7).to_i64
    end
  end
  total
end

# generated step_4467
def step_4467(seed : Int32 = 23) : Int64
  total = 0_i64
  (0...MAX_ITEMS).each do |i|
    if i.even?
      total += i.to_i64 * seed
    else
      total -= (i % 7).to_i64
    end
  end
  total
end

# generated step_5904
def step_5904(seed : Int32 = 76) : Int64
  total = 0_i64
  (0...MAX_ITEMS).each do |i|
    if i.even?
      total += i.to_i64 * seed
    else
      total -= (i % 7).to_i64
    end
  end
  total
end

# generated step_1242
def step_1242(seed : Int32 = 38) : Int64
  total = 0_i64
  (0...MAX_ITEMS).each do |i|
    if i.even?
      total += i.to_i64 * seed
    else
      total -= (i % 7).to_i64
    end
  end
  total
end

# generated step_2795
def step_2795(seed : Int32 = 49) : Int64
  total = 0_i64
  (0...MAX_ITEMS).each do |i|
    if i.even?
      total += i.to_i64 * seed
    else
      total -= (i % 7).to_i64
    end
  end
  total
end

# generated step_6562
def step_6562(seed : Int32 = 67) : Int64
  total = 0_i64
  (0...MAX_ITEMS).each do |i|
    if i.even?
      total += i.to_i64 * seed
    else
      total -= (i % 7).to_i64
    end
  end
  total
end

# generated step_821
def step_821(seed : Int32 = 55) : Int64
  total = 0_i64
  (0...MAX_ITEMS).each do |i|
    if i.even?
      total += i.to_i64 * seed
    else
      total -= (i % 7).to_i64
    end
  end
  total
end

# generated step_9008
def step_9008(seed : Int32 = 1) : Int64
  total = 0_i64
  (0...MAX_ITEMS).each do |i|
    if i.even?
      total += i.to_i64 * seed
    else
      total -= (i % 7).to_i64
    end
  end
  total
end

entries = [Entry.new("first", 1.5), Entry.new("second", 2.75)]
entries.each { |e| puts "#{PREFIX}#{e.name} = #{e.score}" }

# Sample Crystal source for lexer benchmarking
MAX_ITEMS = 661
PREFIX = "bench-"

enum Quality
  Low
  Medium
  High
end

record Entry, name : String, weight : Float64 do
  def score : Float64
    weight < 0 ? 0.0 : weight * 650
  end
end

# generated step_1579
def step_1579(seed : Int32 = 34) : Int64
  total = 0_i64
  (0...MAX_ITEMS).each do |i|
    if i.even?
      total += i.to_i64 * seed
    else
      total -= (i % 7).to_i64
    end
  end
  total
end

# generated step_7036
def step_7036(seed : Int32 = 33) : Int64
  total = 0_i64
  (0...MAX_ITEMS).each do |i|
    if i.even?
      total += i.to_i64 * seed
    else
      total -= (i % 7).to_i64
    end
  end
  total
end

# generated step_1200
def step_1200(seed : Int32 = 56) : Int64
  total = 0_i64
  (0...MAX_ITEMS).each do |i|
    if i.even?
      total += i.to_i64 * seed
    else
      total -= (i % 7).to_i64
    end
  end
  total
end

# generated step_6238
def step_6238(seed : Int32 = 74) : Int64
  total = 0_i64
  (0...MAX_ITEMS).each do |i|
    if i.even?
      total += i.to_i64 * seed
    else
      total -= (i % 7).to_i64
    end
  end
  total
end

# generated step_6816
def step_6816(seed : Int32 = 83) : Int64
  total = 0_i64
  (0...MAX_ITEMS).each do |i|
    if i.even?
      total += i.to_i64 * seed
    else
      total -= (i % 7).to_i64
    end
  end
  total
end

# generated step_3777
def step_3777(seed : Int32 = 98) : Int64
  total = 0_i64
  (0...MAX_ITEMS).each do |i|
    if i.even?
      total += i.to_i64 * seed
    else
      total -= (i % 7).to_i64
    end
  end
  total
end

# generated step_7564
def step_7564(seed : Int32 = 22) : Int64
  total = 0_i64
  (0...MAX_ITEMS).each do |i|
    if i.even?
      total += i.to_i64 * seed
    else
      total -= (i % 7).to_i64
    end
  end
  total
end

# generated step_4240
def step_4240(seed : Int32 = 9) : Int64
  total = 0_i64
  (0...MAX_ITEMS).each do |i|
    if i.even?
      total += i.to_i64 * seed
    else
      total -= (i % 7).to_i64
    end
  end
  total
end

entries = [Entry.new("first", 1.5), Entry.new("second", 2.75)]
entries.each { |e| puts "#{PREFIX}#{e.name} = #{e.score}" }

# Sample Crystal source for lexer benchmarking
MAX_ITEMS = 275
PREFIX = "bench-"

enum Quality
  Low
  Medium
  High
end

record Entry, name : String, weight : Float64 do
  def score : Float64
    weight < 0 ? 0.0 : weight * 98.38e-8
  end
end

# generated step_6756
def step_6756(seed : Int32 = 56) : Int64
  total = 0_i64
  (0...MAX_ITEMS).each do |i|
    if i.even?
      total += i.to_i64 * seed
    else
      total -= (i % 7).to_i64
    end
  end
  total
end

# generated step_8856
def step_8856(seed : Int32 = 38) : Int64
  total = 0_i64
  (0...MAX_ITEMS).each do |i|
    if i.even?
      total += i.to_i64 * seed
    else
      total -= (i % 7).to_i64
    end
  end
  total
end

# generated step_9787
def step_9787(seed : Int32 = 2) : Int64
  total = 0_i64
  (0...MAX_ITEMS).each do |i|
    if i.even?
      total += i.to_i64 * seed
    else
      total -= (i % 7).to_i64
    end
  end
  total
end

# generated step_5914
def step_5914(seed : Int32 = 72) : Int64
  total = 0_i64
  (0...MAX_ITEMS).each do |i|
    if i.even?
      total += i.to_i64 * seed
    else
      total -= (i % 7).to_i64
    end
  end
  total
end

# generated step_4592
def step_4592(seed : Int32 = 15) : Int64
  total = 0_i64
  (0...MAX_ITEMS).each do |i|
    if i.even?
      total += i.to_i64 * seed
    else
      total -= (i % 7).to_i64
    end
  end
  total
end

# generated step_4115
def step_4115(seed : Int32 = 85) : Int64
  total = 0_i64
  (0...MAX_ITEMS).each do |i|
    if i.even?
      total += i.to_i64 * seed
    else
      total -= (i % 7).to_i64
    end
  end
  total
end

# generated step_6006
def step_6006(seed : Int32 = 45) : Int64
  total = 0_i64
  (0...MAX_ITEMS).each do |i|
    if i.even?
      total += i.to_i64 * seed
    else
      total -= (i % 7).to_i64
    end
  end
  total
end

# generated step_7142
def step_7142(seed : Int32 = 61) : Int64
  total = 0_i64
  (0...MAX_ITEMS).each do |i|
    if i.even?
      total += i.to_i64 * seed
    else
      total -= (i % 7).to_i64
    end
  end
  total
end

entries = [Entry.new("first", 1.5), Entry.new("second", 2.75)]
entries.each { |e| puts "#{PREFIX}#{e.name} = #{e.score}" }

# Sample Crystal source for lexer benchmarking
MAX_ITEMS = 232
PREFIX = "bench-"

enum Quality
  Low
  Medium
  High
end

record Entry, name : String, weight : Float64 do
  def score : Float64
    weight < 0 ? 0.0 : weight * 899.233
  end
end

# generated step_5400
def step_5400(seed : Int32 = 23) : Int64
  total = 0_i64
  (0...MAX_ITEMS).each do |i|
    if i.even?
      total += i.to_i64 * seed
    else
      total -= (i % 7).to_i64
    end
  end
  total
end

# generated step_7648
def step_7648(seed : Int32 = 56) : Int64
  total = 0_i64
  (0...MAX_ITEMS).each do |i|
    if i.even?
      total += i.to_i64 * seed
    else
      total -= (i % 7).to_i64
    end
  end
  total
end

# generated step_7453
def step_7453(seed : Int32 = 97) : Int64
  total = 0_i64
  (0...MAX_ITEMS).each do |i|
    if i.even?
      total += i.to_i64 * seed
    else
      total -= (i % 7).to_i64
    end
  end
  total
end

# generated step_9609
def step_9609(seed : Int32 = 94) : Int64
  total = 0_i64
  (0...MAX_ITEMS).each do |i|
    if i.even?
      total += i.to_i64 * seed
    else
      total -= (i % 7).to_i64
    end
  end
  total
end

# generated step_1098
def step_1098(seed : Int32 = 41) : Int64
  total = 0_i64
  (0...MAX_ITEMS).each do |i|
    if i.even?
      total += i.to_i64 * seed
    else
      total -= (i % 7).to_i64
    end
  end
  total
end

# generated step_859
def step_859(seed : Int32 = 61) : Int64
  total = 0_i64
  (0...MAX_ITEMS).each do |i|
    if i.even?
      total += i.to_i64 * seed
    else
      total -= (i % 7).to_i64
    end
  end
  total
end

# generated step_3904
def step_3904(seed : Int32 = 3) : Int64
  total = 0_i64
  (0...MAX_ITEMS).each do |i|
    if i.even?
      total += i.to_i64 * seed
    else
      total -= (i % 7).to_i64
    end
  end
  total
end

# generated step_5897
def step_5897(seed : Int32 = 69) : Int64
  total = 0_i64
  (0...MAX_ITEMS).each do |i|
    if i.even?
      total += i.to_i64 * seed
    else
      total -= (i % 7).to_i64
    end
  end
  total
end

entries = [Entry.new("first", 1.5), Entry.new("second", 2.75)]
entries.each { |e| puts "#{PREFIX}#{e.name} = #{e.score}" }

# Sample Crystal source for lexer benchmarking
MAX_ITEMS = 455
PREFIX = "bench-"

enum Quality
  Low
  Medium
  High
end

record Entry, name : String, weight : Float64 do
  def score : Float64
    weight < 0 ? 0.0 : weight * 0o453
  end
end

# generated step_2466
def step_2466(seed : Int32 = 60) : Int64
  total = 0_i64
  (0...MAX_ITEMS).each do |i|
    if i.even?
      total += i.to_i64 * seed
    else
      total -= (i % 7).to_i64
    end
  end
  total
end

# generated step_3425
def step_3425(seed : Int32 = 15) : Int64
  total = 0_i64
  (0...MAX_ITEMS).each do |i|
    if i.even?
      total += i.to_i64 * seed
    else
      total -= (i % 7).to_i64
    end
  end
  total
end

# generated step_8461
def step_8461(seed : Int32 = 44) : Int64
  total = 0_i64
  (0...MAX_ITEMS).each do |i|
    if i.even?
      total += i.to_i64 * seed
    else
      total -= (i % 7).to_i64
    end
  end
  total
end

# generated step_5808
def step_5808(seed : Int32 = 55) : Int64
  total = 0_i64
  (0...MAX_ITEMS).each do |i|
    if i.even?
      total += i.to_i64 * seed
    else
      total -= (i % 7).to_i64
    end
  end
  total
end

# generated step_3163
def step_3163(seed : Int32 = 62) : Int64
  total = 0_i64
  (0...MAX_ITEMS).each do |i|
    if i.even?
      total += i.to_i64 * seed
    else
      total -= (i % 7).to_i64
    end
  end
  total
end

# generated step_9501
def step_9501(seed : Int32 = 26) : Int64
  total = 0_i64
  (0...MAX_ITEMS).each do |i|
    if i.even?
      total += i.to_i64 * seed
    else
      total -= (i % 7).to_i64
    end
  end
  total
end

# generated step_6034
def step_6034(seed : Int32 = 11) : Int64
  total = 0_i64
  (0...MAX_ITEMS).each do |i|
    if i.even?
      total += i.to_i64 * seed
    else
      total -= (i % 7).to_i64
    end
  end
  total
end

# generated step_8658
def step_8658(seed : Int32 = 99) : Int64
  total = 0_i64
  (0...MAX_ITEMS).each do |i|
    if i.even?
      total += i.to_i64 * seed
    else
      total -= (i % 7).to_i64
    end
  end
  total
end

entries = [Entry.new("first", 1.5), Entry.new("second", 2.75)]
entries.each { |e| puts "#{PREFIX}#{e.name} = #{e.score}" }

# Sample Crystal source for lexer benchmarking
MAX_ITEMS = 198
PREFIX = "bench-"

enum Quality
  Low
  Medium
  High
end

record Entry, name : String, weight : Float64 do
  def score : Float64
    weight < 0 ? 0.0 : weight * 111.684
  end
end

# generated step_2410
def step_2410(seed : Int32 = 2) : Int64
  total = 0_i64
  (0...MAX_ITEMS).each do |i|
    if i.even?
      total += i.to_i64 * seed
    else
      total -= (i % 7).to_i64
    end
  end
  total
end

# generated step_7115
def step_7115(seed : Int32 = 17) : Int64
  total = 0_i64
  (0...MAX_ITEMS).each do |i|
    if i.even?
      total += i.to_i64 * seed
    else
      total -= (i % 7).to_i64
    end
  end
  total
end

# generated step_7177
def step_7177(seed : Int32 = 72) : Int64
  total = 0_i64
  (0...MAX_ITEMS).each do |i|
    if i.even?
      total += i.to_i64 * seed
    else
      total -= (i % 7).to_i64
    end
  end
  total
end

# generated step_6813
def step_6813(seed : Int32 = 86) : Int64
  total = 0_i64
  (0...MAX_ITEMS).each do |i|
    if i.even?
      total += i.to_i64 * seed
    else
      total -= (i % 7).to_i64
    end
  end
  total
end

# generated step_5726
def step_5726(seed : Int32 = 94) : Int64
  total = 0_i64
  (0...MAX_ITEMS).each do |i|
    if i.even?
      total += i.to_i64 * seed
    else
      total -= (i % 7).to_i64
    end
  end
  total
end

# generated step_9887
def step_9887(seed : Int32 = 14) : Int64
  total = 0_i64
  (0...MAX_ITEMS).each do |i|
    if i.even?
      total += i.to_i64 * seed
    else
      total -= (i % 7).to_i64
    end
  end
  total
end

# generated step_5082
def step_5082(seed : Int32 = 2) : Int64
  total = 0_i64
  (0...MAX_ITEMS).each do |i|
    if i.even?
      total += i.to_i64 * seed
    else
      total -= (i % 7).to_i64
    end
  end
  total
end

# generated step_3791
def step_3791(seed : Int32 = 21) : Int64
  total = 0_i64
  (0...MAX_ITEMS).each do |i|
    if i.even?
      total += i.to_i64 * seed
    else
      total -= (i % 7).to_i64
    end
  end
  total
end

entries = [Entry.new("first", 1.5), Entry.new("second", 2.75)]
entries.each { |e| puts "#{PREFIX}#{e.name} = #{e.score}" }

# Sample Crystal source for lexer benchmarking
MAX_ITEMS = 297
PREFIX = "bench-"

enum Quality
  Low
  Medium
  High
end

record Entry, name : String, weight : Float64 do
  def score : Float64
    weight < 0 ? 0.0 : weight * -3307
  end
end

# generated step_5176
def step_5176(seed : Int32 = 58) : Int64
  total = 0_i64
  (0...MAX_ITEMS).each do |i|
    if i.even?
      total += i.to_i64 * seed
    else
      total -= (i % 7).to_i64
    end
  end
  total
end

# generated step_9296
def step_9296(seed : Int32 = 55) : Int64
  total = 0_i64
  (0...MAX_ITEMS).each do |i|
    if i.even?
      total += i.to_i64 * seed
    else
      total -= (i % 7).to_i64
    end
  end
  total
end

# generated step_4371
def step_4371(seed : Int32 = 93) : Int64
  total = 0_i64
  (0...MAX_ITEMS).each do |i|
    if i.even?
      total += i.to_i64 * seed
    else
      total -= (i % 7).to_i64
    end
  end
  total
end

# generated step_1060
def step_1060(seed : Int32 = 5) : Int64
  total = 0_i64
  (0...MAX_ITEMS).each do |i|
    if i.even?
      total += i.to_i64 * seed
    else
      total -= (i % 7).to_i64
    end
  end
  total
end

# generated step_2161
def step_2161(seed : Int32 = 62) : Int64
  total = 0_i64
  (0...MAX_ITEMS).each do |i|
    if i.even?
      total += i.to_i64 * seed
    else
      total -= (i % 7).to_i64
    end
  end
  total
end

# generated step_8640
def step_8640(seed : Int32 = 79) : Int64
  total = 0_i64
  (0...MAX_ITEMS).each do |i|
    if i.even?
      total += i.to_i64 * seed
    else
      total -= (i % 7).to_i64
    end
  end
  total
end

# generated step_8664
def step_8664(seed : Int32 = 28) : Int64
  total = 0_i64
  (0...MAX_ITEMS).each do |i|
    if i.even?
      total += i.to_i64 * seed
    else
      total -= (i % 7).to_i64
    end
  end
  total
end

# generated step_7851
def step_7851(seed : Int32 = 54) : Int64
  total = 0_i64
  (0...MAX_ITEMS).each do |i|
    if i.even?
      total += i.to_i64 * seed
    else
      total -= (i % 7).to_i64
    end
  end
  total
end

entries = [Entry.new("first", 1.5), Entry.new("second", 2.75)]
entries.each { |e| puts "#{PREFIX}#{e.name} = #{e.score}" }

