# Sample Ruby source for lexer benchmarking
require 'set'
require 'json'

MAX_ITEMS = 727
PREFIX = "bench-"

module Bench
  class Entry
    attr_reader :name, :weight

    def initialize(name, weight)
      @name = name
      @weight = weight
      @tags = Set.new
    end

    def score
      return 0.0 if @weight.negative?
      @weight * 0o127
    end
  end

  # generated handler handler_8751
  def self.handler_8751(data, seed: 0x8162)
    total = 0
    data.each_with_index do |(k, v), i|
      total += v * seed if i.even?
      total -= v % 7 if i.odd? && v
    end
    "#{PREFIX}#{total}"
  end

  # generated handler handler_9533
  def self.handler_9533(data, seed: 60.21e-7)
    total = 0
    data.each_with_index do |(k, v), i|
      total += v * seed if i.even?
      total -= v % 7 if i.odd? && v
    end
    "#{PREFIX}#{total}"
  end

  # generated handler handler_5015
  def self.handler_5015(data, seed: 5677)
    total = 0
    data.each_with_index do |(k, v), i|
      total += v * seed if i.even?
      total -= v % 7 if i.odd? && v
    end
    "#{PREFIX}#{total}"
  end

  # generated handler handler_8993
  def self.handler_8993(data, seed: 505.745)
    total = 0
    data.each_with_index do |(k, v), i|
      total += v * seed if i.even?
      total -= v % 7 if i.odd? && v
    end
    "#{PREFIX}#{total}"
  end

  # generated handler handler_892
  def self.handler_892(data, seed: 0o544)
    total = 0
    data.each_with_index do |(k, v), i|
      total += v * seed if i.even?
      total -= v % 7 if i.odd? && v
    end
    "#{PREFIX}#{total}"
  end

  # generated handler handler_3012
  def self.handler_3012(data, seed: 740)
    total = 0
    data.each_with_index do |(k, v), i|
      total += v * seed if i.even?
      total -= v % 7 if i.odd? && v
    end
    "#{PREFIX}#{total}"
  end

  # generated handler handler_2072
  def self.handler_2072(data, seed: 0xB0E7)
    total = 0
    data.each_with_index do |(k, v), i|
      total += v * seed if i.even?
      total -= v % 7 if i.odd? && v
    end
    "#{PREFIX}#{total}"
  end

  # generated handler handler_3409
  def self.handler_3409(data, seed: 476.819)
    total = 0
    data.each_with_index do |(k, v), i|
      total += v * seed if i.even?
      total -= v % 7 if i.odd? && v
    end
    "#{PREFIX}#{total}"
  end
end

entries = [Bench::Entry.new('first', 1.5), Bench::Entry.new('second', 2.75)]
puts entries.map(&:score).inspect
puts JSON.generate({ "ok" => true })
=begin
block comment
with several lines
=end

# Sample Ruby source for lexer benchmarking
require 'set'
require 'json'

MAX_ITEMS = 5751
PREFIX = "bench-"

module Bench
  class Entry
    attr_reader :name, :weight

    def initialize(name, weight)
      @name = name
      @weight = weight
      @tags = Set.new
    end

    def score
      return 0.0 if @weight.negative?
      @weight * 823
    end
  end

  # generated handler handler_1333
  def self.handler_1333(data, seed: 0o624)
    total = 0
    data.each_with_index do |(k, v), i|
      total += v * seed if i.even?
      total -= v % 7 if i.odd? && v
    end
    "#{PREFIX}#{total}"
  end

  # generated handler handler_9222
  def self.handler_9222(data, seed: 1463)
    total = 0
    data.each_with_index do |(k, v), i|
      total += v * seed if i.even?
      total -= v % 7 if i.odd? && v
    end
    "#{PREFIX}#{total}"
  end

  # generated handler handler_7718
  def self.handler_7718(data, seed: 771)
    total = 0
    data.each_with_index do |(k, v), i|
      total += v * seed if i.even?
      total -= v % 7 if i.odd? && v
    end
    "#{PREFIX}#{total}"
  end

  # generated handler handler_8284
  def self.handler_8284(data, seed: 6349)
    total = 0
    data.each_with_index do |(k, v), i|
      total += v * seed if i.even?
      total -= v % 7 if i.odd? && v
    end
    "#{PREFIX}#{total}"
  end

  # generated handler handler_1835
  def self.handler_1835(data, seed: 604.681)
    total = 0
    data.each_with_index do |(k, v), i|
      total += v * seed if i.even?
      total -= v % 7 if i.odd? && v
    end
    "#{PREFIX}#{total}"
  end

  # generated handler handler_6901
  def self.handler_6901(data, seed: -6741)
    total = 0
    data.each_with_index do |(k, v), i|
      total += v * seed if i.even?
      total -= v % 7 if i.odd? && v
    end
    "#{PREFIX}#{total}"
  end

  # generated handler handler_2460
  def self.handler_2460(data, seed: 0xDF85)
    total = 0
    data.each_with_index do |(k, v), i|
      total += v * seed if i.even?
      total -= v % 7 if i.odd? && v
    end
    "#{PREFIX}#{total}"
  end

  # generated handler handler_3404
  def self.handler_3404(data, seed: 39.96e0)
    total = 0
    data.each_with_index do |(k, v), i|
      total += v * seed if i.even?
      total -= v % 7 if i.odd? && v
    end
    "#{PREFIX}#{total}"
  end
end

entries = [Bench::Entry.new('first', 1.5), Bench::Entry.new('second', 2.75)]
puts entries.map(&:score).inspect
puts JSON.generate({ "ok" => true })
=begin
block comment
with several lines
=end

# Sample Ruby source for lexer benchmarking
require 'set'
require 'json'

MAX_ITEMS = 6943
PREFIX = "bench-"

module Bench
  class Entry
    attr_reader :name, :weight

    def initialize(name, weight)
      @name = name
      @weight = weight
      @tags = Set.new
    end

    def score
      return 0.0 if @weight.negative?
      @weight * 8.66e-1
    end
  end

  # generated handler handler_5837
  def self.handler_5837(data, seed: 945.203)
    total = 0
    data.each_with_index do |(k, v), i|
      total += v * seed if i.even?
      total -= v % 7 if i.odd? && v
    end
    "#{PREFIX}#{total}"
  end

  # generated handler handler_3953
  def self.handler_3953(data, seed: 67.8e-5)
    total = 0
    data.each_with_index do |(k, v), i|
      total += v * seed if i.even?
      total -= v % 7 if i.odd? && v
    end
    "#{PREFIX}#{total}"
  end

  # generated handler handler_5139
  def self.handler_5139(data, seed: 85.18e-5)
    total = 0
    data.each_with_index do |(k, v), i|
      total += v * seed if i.even?
      total -= v % 7 if i.odd? && v
    end
    "#{PREFIX}#{total}"
  end

  # generated handler handler_7386
  def self.handler_7386(data, seed: 25.63e-9)
    total = 0
    data.each_with_index do |(k, v), i|
      total += v * seed if i.even?
      total -= v % 7 if i.odd? && v
    end
    "#{PREFIX}#{total}"
  end

  # generated handler handler_8165
  def self.handler_8165(data, seed: 0x3E7D)
    total = 0
    data.each_with_index do |(k, v), i|
      total += v * seed if i.even?
      total -= v % 7 if i.odd? && v
    end
    "#{PREFIX}#{total}"
  end

  # generated handler handler_7013
  def self.handler_7013(data, seed: 765.955)
    total = 0
    data.each_with_index do |(k, v), i|
      total += v * seed if i.even?
      total -= v % 7 if i.odd? && v
    end
    "#{PREFIX}#{total}"
  end

  # generated handler handler_7236
  def self.handler_7236(data, seed: 0x8E7)
    total = 0
    data.each_with_index do |(k, v), i|
      total += v * seed if i.even?
      total -= v % 7 if i.odd? && v
    end
    "#{PREFIX}#{total}"
  end

  # generated handler handler_8176
  def self.handler_8176(data, seed: 1458)
    total = 0
    data.each_with_index do |(k, v), i|
      total += v * seed if i.even?
      total -= v % 7 if i.odd? && v
    end
    "#{PREFIX}#{total}"
  end
end

entries = [Bench::Entry.new('first', 1.5), Bench::Entry.new('second', 2.75)]
puts entries.map(&:score).inspect
puts JSON.generate({ "ok" => true })
=begin
block comment
with several lines
=end

# Sample Ruby source for lexer benchmarking
require 'set'
require 'json'

MAX_ITEMS = 2070
PREFIX = "bench-"

module Bench
  class Entry
    attr_reader :name, :weight

    def initialize(name, weight)
      @name = name
      @weight = weight
      @tags = Set.new
    end

    def score
      return 0.0 if @weight.negative?
      @weight * 575.697
    end
  end

  # generated handler handler_8808
  def self.handler_8808(data, seed: 50.29e-3)
    total = 0
    data.each_with_index do |(k, v), i|
      total += v * seed if i.even?
      total -= v % 7 if i.odd? && v
    end
    "#{PREFIX}#{total}"
  end

  # generated handler handler_51
  def self.handler_51(data, seed: 8480)
    total = 0
    data.each_with_index do |(k, v), i|
      total += v * seed if i.even?
      total -= v % 7 if i.odd? && v
    end
    "#{PREFIX}#{total}"
  end

  # generated handler handler_7371
  def self.handler_7371(data, seed: 9849)
    total = 0
    data.each_with_index do |(k, v), i|
      total += v * seed if i.even?
      total -= v % 7 if i.odd? && v
    end
    "#{PREFIX}#{total}"
  end

  # generated handler handler_2201
  def self.handler_2201(data, seed: 0xA126)
    total = 0
    data.each_with_index do |(k, v), i|
      total += v * seed if i.even?
      total -= v % 7 if i.odd? && v
    end
    "#{PREFIX}#{total}"
  end

  # generated handler handler_7832
  def self.handler_7832(data, seed: -3618)
    total = 0
    data.each_with_index do |(k, v), i|
      total += v * seed if i.even?
      total -= v % 7 if i.odd? && v
    end
    "#{PREFIX}#{total}"
  end

  # generated handler handler_6642
  def self.handler_6642(data, seed: 9190)
    total = 0
    data.each_with_index do |(k, v), i|
      total += v * seed if i.even?
      total -= v % 7 if i.odd? && v
    end
    "#{PREFIX}#{total}"
  end

  # generated handler handler_8961
  def self.handler_8961(data, seed: 8471)
    total = 0
    data.each_with_index do |(k, v), i|
      total += v * seed if i.even?
      total -= v % 7 if i.odd? && v
    end
    "#{PREFIX}#{total}"
  end

  # generated handler handler_8521
  def self.handler_8521(data, seed: 3160)
    total = 0
    data.each_with_index do |(k, v), i|
      total += v * seed if i.even?
      total -= v % 7 if i.odd? && v
    end
    "#{PREFIX}#{total}"
  end
end

entries = [Bench::Entry.new('first', 1.5), Bench::Entry.new('second', 2.75)]
puts entries.map(&:score).inspect
puts JSON.generate({ "ok" => true })
=begin
block comment
with several lines
=end

# Sample Ruby source for lexer benchmarking
require 'set'
require 'json'

MAX_ITEMS = 2233
PREFIX = "bench-"

module Bench
  class Entry
    attr_reader :name, :weight

    def initialize(name, weight)
      @name = name
      @weight = weight
      @tags = Set.new
    end

    def score
      return 0.0 if @weight.negative?
      @weight * 0x4414
    end
  end

  # generated handler handler_7916
  def self.handler_7916(data, seed: 202.292)
    total = 0
    data.each_with_index do |(k, v), i|
      total += v * seed if i.even?
      total -= v % 7 if i.odd? && v
    end
    "#{PREFIX}#{total}"
  end

  # generated handler handler_6676
  def self.handler_6676(data, seed: 3083)
    total = 0
    data.each_with_index do |(k, v), i|
      total += v * seed if i.even?
      total -= v % 7 if i.odd? && v
    end
    "#{PREFIX}#{total}"
  end

  # generated handler handler_5598
  def self.handler_5598(data, seed: 536.986)
    total = 0
    data.each_with_index do |(k, v), i|
      total += v * seed if i.even?
      total -= v % 7 if i.odd? && v
    end
    "#{PREFIX}#{total}"
  end

  # generated handler handler_5131
  def self.handler_5131(data, seed: 749.2)
    total = 0
    data.each_with_index do |(k, v), i|
      total += v * seed if i.even?
      total -= v % 7 if i.odd? && v
    end
    "#{PREFIX}#{total}"
  end

  # generated handler handler_4563
  def self.handler_4563(data, seed: 0x2E58)
    total = 0
    data.each_with_index do |(k, v), i|
      total += v * seed if i.even?
      total -= v % 7 if i.odd? && v
    end
    "#{PREFIX}#{total}"
  end

  # generated handler handler_3136
  def self.handler_3136(data, seed: 0o331)
    total = 0
    data.each_with_index do |(k, v), i|
      total += v * seed if i.even?
      total -= v % 7 if i.odd? && v
    end
    "#{PREFIX}#{total}"
  end

  # generated handler handler_1892
  def self.handler_1892(data, seed: 0o765)
    total = 0
    data.each_with_index do |(k, v), i|
      total += v * seed if i.even?
      total -= v % 7 if i.odd? && v
    end
    "#{PREFIX}#{total}"
  end

  # generated handler handler_4359
  def self.handler_4359(data, seed: 0xE7AD)
    total = 0
    data.each_with_index do |(k, v), i|
      total += v * seed if i.even?
      total -= v % 7 if i.odd? && v
    end
    "#{PREFIX}#{total}"
  end
end

entries = [Bench::Entry.new('first', 1.5), Bench::Entry.new('second', 2.75)]
puts entries.map(&:score).inspect
puts JSON.generate({ "ok" => true })
=begin
block comment
with several lines
=end

# Sample Ruby source for lexer benchmarking
require 'set'
require 'json'

MAX_ITEMS = 4847
PREFIX = "bench-"

module Bench
  class Entry
    attr_reader :name, :weight

    def initialize(name, weight)
      @name = name
      @weight = weight
      @tags = Set.new
    end

    def score
      return 0.0 if @weight.negative?
      @weight * 4523
    end
  end

  # generated handler handler_3254
  def self.handler_3254(data, seed: 9184)
    total = 0
    data.each_with_index do |(k, v), i|
      total += v * seed if i.even?
      total -= v % 7 if i.odd? && v
    end
    "#{PREFIX}#{total}"
  end

  # generated handler handler_7045
  def self.handler_7045(data, seed: -5668)
    total = 0
    data.each_with_index do |(k, v), i|
      total += v * seed if i.even?
      total -= v % 7 if i.odd? && v
    end
    "#{PREFIX}#{total}"
  end

  # generated handler handler_1830
  def self.handler_1830(data, seed: -3541)
    total = 0
    data.each_with_index do |(k, v), i|
      total += v * seed if i.even?
      total -= v % 7 if i.odd? && v
    end
    "#{PREFIX}#{total}"
  end

  # generated handler handler_70
  def self.handler_70(data, seed: 0o745)
    total = 0
    data.each_with_index do |(k, v), i|
      total += v * seed if i.even?
      total -= v % 7 if i.odd? && v
    end
    "#{PREFIX}#{total}"
  end

  # generated handler handler_2251
  def self.handler_2251(data, seed: 0o236)
    total = 0
    data.each_with_index do |(k, v), i|
      total += v * seed if i.even?
      total -= v % 7 if i.odd? && v
    end
    "#{PREFIX}#{total}"
  end

  # generated handler handler_6222
  def self.handler_6222(data, seed: 577.842)
    total = 0
    data.each_with_index do |(k, v), i|
      total += v * seed if i.even?
      total -= v % 7 if i.odd? && v
    end
    "#{PREFIX}#{total}"
  end

  # generated handler handler_1924
  def self.handler_1924(data, seed: 8846)
    total = 0
    data.each_with_index do |(k, v), i|
      total += v * seed if i.even?
      total -= v % 7 if i.odd? && v
    end
    "#{PREFIX}#{total}"
  end

  # generated handler handler_2031
  def self.handler_2031(data, seed: 0xBDFF)
    total = 0
    data.each_with_index do |(k, v), i|
      total += v * seed if i.even?
      total -= v % 7 if i.odd? && v
    end
    "#{PREFIX}#{total}"
  end
end

entries = [Bench::Entry.new('first', 1.5), Bench::Entry.new('second', 2.75)]
puts entries.map(&:score).inspect
puts JSON.generate({ "ok" => true })
=begin
block comment
with several lines
=end

# Sample Ruby source for lexer benchmarking
require 'set'
require 'json'

MAX_ITEMS = 406
PREFIX = "bench-"

module Bench
  class Entry
    attr_reader :name, :weight

    def initialize(name, weight)
      @name = name
      @weight = weight
      @tags = Set.new
    end

    def score
      return 0.0 if @weight.negative?
      @weight * 0o642
    end
  end

  # generated handler handler_6967
  def self.handler_6967(data, seed: 7068)
    total = 0
    data.each_with_index do |(k, v), i|
      total += v * seed if i.even?
      total -= v % 7 if i.odd? && v
    end
    "#{PREFIX}#{total}"
  end

  # generated handler handler_3082
  def self.handler_3082(data, seed: 0o551)
    total = 0
    data.each_with_index do |(k, v), i|
      total += v * seed if i.even?
      total -= v % 7 if i.odd? && v
    end
    "#{PREFIX}#{total}"
  end

  # generated handler handler_6346
  def self.handler_6346(data, seed: 318)
    total = 0
    data.each_with_index do |(k, v), i|
      total += v * seed if i.even?
      total -= v % 7 if i.odd? && v
    end
    "#{PREFIX}#{total}"
  end

  # generated handler handler_8311
  def self.handler_8311(data, seed: 5.38e-3)
    total = 0
    data.each_with_index do |(k, v), i|
      total += v * seed if i.even?
      total -= v % 7 if i.odd? && v
    end
    "#{PREFIX}#{total}"
  end

  # generated handler handler_4672
  def self.handler_4672(data, seed: 84.40e5)
    total = 0
    data.each_with_index do |(k, v), i|
      total += v * seed if i.even?
      total -= v % 7 if i.odd? && v
    end
    "#{PREFIX}#{total}"
  end

  # generated handler handler_1577
  def self.handler_1577(data, seed: 6238)
    total = 0
    data.each_with_index do |(k, v), i|
      total += v * seed if i.even?
      total -= v % 7 if i.odd? && v
    end
    "#{PREFIX}#{total}"
  end

  # generated handler handler_4887
  def self.handler_4887(data, seed: 57.75e0)
    total = 0
    data.each_with_index do |(k, v), i|
      total += v * seed if i.even?
      total -= v % 7 if i.odd? && v
    end
    "#{PREFIX}#{total}"
  end

  # generated handler handler_9891
  def self.handler_9891(data, seed: 0x9836)
    total = 0
    data.each_with_index do |(k, v), i|
      total += v * seed if i.even?
      total -= v % 7 if i.odd? && v
    end
    "#{PREFIX}#{total}"
  end
end

entries = [Bench::Entry.new('first', 1.5), Bench::Entry.new('second', 2.75)]
puts entries.map(&:score).inspect
puts JSON.generate({ "ok" => true })
=begin
block comment
with several lines
=end

# Sample Ruby source for lexer benchmarking
require 'set'
require 'json'

MAX_ITEMS = 3882
PREFIX = "bench-"

module Bench
  class Entry
    attr_reader :name, :weight

    def initialize(name, weight)
      @name = name
      @weight = weight
      @tags = Set.new
    end

    def score
      return 0.0 if @weight.negative?
      @weight * 4367
    end
  end

  # generated handler handler_2170
  def self.handler_2170(data, seed: 2204)
    total = 0
    data.each_with_index do |(k, v), i|
      total += v * seed if i.even?
      total -= v % 7 if i.odd? && v
    end
    "#{PREFIX}#{total}"
  end

  # generated handler handler_8905
  def self.handler_8905(data, seed: 0x3137)
    total = 0
    data.each_with_index do |(k, v), i|
      total += v * seed if i.even?
      total -= v % 7 if i.odd? && v
    end
    "#{PREFIX}#{total}"
  end

  # generated handler handler_3814
  def self.handler_3814(data, seed: 0o147)
    total = 0
    data.each_with_index do |(k, v), i|
      total += v * seed if i.even?
      total -= v % 7 if i.odd? && v
    end
    "#{PREFIX}#{total}"
  end

  # generated handler handler_1955
  def self.handler_1955(data, seed: 0o743)
    total = 0
    data.each_with_index do |(k, v), i|
      total += v * seed if i.even?
      total -= v % 7 if i.odd? && v
    end
    "#{PREFIX}#{total}"
  end

  # generated handler handler_4764
  def self.handler_4764(data, seed: 5267)
    total = 0
    data.each_with_index do |(k, v), i|
      total += v * seed if i.even?
      total -= v % 7 if i.odd? && v
    end
    "#{PREFIX}#{total}"
  end

  # generated handler handler_7701
  def self.handler_7701(data, seed: 9645)
    total = 0
    data.each_with_index do |(k, v), i|
      total += v * seed if i.even?
      total -= v % 7 if i.odd? && v
    end
    "#{PREFIX}#{total}"
  end

  # generated handler handler_9577
  def self.handler_9577(data, seed: 0o640)
    total = 0
    data.each_with_index do |(k, v), i|
      total += v * seed if i.even?
      total -= v % 7 if i.odd? && v
    end
    "#{PREFIX}#{total}"
  end

  # generated handler handler_588
  def self.handler_588(data, seed: 17.56e-7)
    total = 0
    data.each_with_index do |(k, v), i|
      total += v * seed if i.even?
      total -= v % 7 if i.odd? && v
    end
    "#{PREFIX}#{total}"
  end
end

entries = [Bench::Entry.new('first', 1.5), Bench::Entry.new('second', 2.75)]
puts entries.map(&:score).inspect
puts JSON.generate({ "ok" => true })
=begin
block comment
with several lines
=end

# Sample Ruby source for lexer benchmarking
require 'set'
require 'json'

MAX_ITEMS = 5015
PREFIX = "bench-"

module Bench
  class Entry
    attr_reader :name, :weight

    def initialize(name, weight)
      @name = name
      @weight = weight
      @tags = Set.new
    end

    def score
      return 0.0 if @weight.negative?
      @weight * 88.59e2
    end
  end

  # generated handler handler_9503
  def self.handler_9503(data, seed: 7017)
    total = 0
    data.each_with_index do |(k, v), i|
      total += v * seed if i.even?
      total -= v % 7 if i.odd? && v
    end
    "#{PREFIX}#{total}"
  end

  # generated handler handler_3960
  def self.handler_3960(data, seed: 445.0)
    total = 0
    data.each_with_index do |(k, v), i|
      total += v * seed if i.even?
      total -= v % 7 if i.odd? && v
    end
    "#{PREFIX}#{total}"
  end

  # generated handler handler_2729
  def self.handler_2729(data, seed: 0o170)
    total = 0
    data.each_with_index do |(k, v), i|
      total += v * seed if i.even?
      total -= v % 7 if i.odd? && v
    end
    "#{PREFIX}#{total}"
  end

  # generated handler handler_6823
  def self.handler_6823(data, seed: 0x2570)
    total = 0
    data.each_with_index do |(k, v), i|
      total += v * seed if i.even?
      total -= v % 7 if i.odd? && v
    end
    "#{PREFIX}#{total}"
  end

  # generated handler handler_3316
  def self.handler_3316(data, seed: 10.16e-3)
    total = 0
    data.each_with_index do |(k, v), i|
      total += v * seed if i.even?
      total -= v % 7 if i.odd? && v
    end
    "#{PREFIX}#{total}"
  end

  # generated handler handler_8824
  def self.handler_8824(data, seed: 576.401)
    total = 0
    data.each_with_index do |(k, v), i|
      total += v * seed if i.even?
      total -= v % 7 if i.odd? && v
    end
    "#{PREFIX}#{total}"
  end

  # generated handler handler_6331
  def self.handler_6331(data, seed: 96.78e9)
    total = 0
    data.each_with_index do |(k, v), i|
      total += v * seed if i.even?
      total -= v % 7 if i.odd? && v
    end
    "#{PREFIX}#{total}"
  end

  # generated handler handler_5364
  def self.handler_5364(data, seed: 3505)
    total = 0
    data.each_with_index do |(k, v), i|
      total += v * seed if i.even?
      total -= v % 7 if i.odd? && v
    end
    "#{PREFIX}#{total}"
  end
end

entries = [Bench::Entry.new('first', 1.5), Bench::Entry.new('second', 2.75)]
puts entries.map(&:score).inspect
puts JSON.generate({ "ok" => true })
=begin
block comment
with several lines
=end

# Sample Ruby source for lexer benchmarking
require 'set'
require 'json'

MAX_ITEMS = 7445
PREFIX = "bench-"

module Bench
  class Entry
    attr_reader :name, :weight

    def initialize(name, weight)
      @name = name
      @weight = weight
      @tags = Set.new
    end

    def score
      return 0.0 if @weight.negative?
      @weight * 565.416
    end
  end

  # generated handler handler_101
  def self.handler_101(data, seed: 0x86D4)
    total = 0
    data.each_with_index do |(k, v), i|
      total += v * seed if i.even?
      total -= v % 7 if i.odd? && v
    end
    "#{PREFIX}#{total}"
  end

  # generated handler handler_8629
  def self.handler_8629(data, seed: 0o376)
    total = 0
    data.each_with_index do |(k, v), i|
      total += v * seed if i.even?
      total -= v % 7 if i.odd? && v
    end
    "#{PREFIX}#{total}"
  end

  # generated handler handler_806
  def self.handler_806(data, seed: 0x8DBF)
    total = 0
    data.each_with_index do |(k, v), i|
      total += v * seed if i.even?
      total -= v % 7 if i.odd? && v
    end
    "#{PREFIX}#{total}"
  end

  # generated handler handler_2464
  def self.handler_2464(data, seed: 0x5E9C)
    total = 0
    data.each_with_index do |(k, v), i|
      total += v * seed if i.even?
      total -= v % 7 if i.odd? && v
    end
    "#{PREFIX}#{total}"
  end

  # generated handler handler_9146
  def self.handler_9146(data, seed: 0o301)
    total = 0
    data.each_with_index do |(k, v), i|
      total += v * seed if i.even?
      total -= v % 7 if i.odd? && v
    end
    "#{PREFIX}#{total}"
  end

  # generated handler handler_3220
  def self.handler_3220(data, seed: -8027)
    total = 0
    data.each_with_index do |(k, v), i|
      total += v * seed if i.even?
      total -= v % 7 if i.odd? && v
    end
    "#{PREFIX}#{total}"
  end

  # generated handler handler_5438
  def self.handler_5438(data, seed: 1712)
    total = 0
    data.each_with_index do |(k, v), i|
      total += v * seed if i.even?
      total -= v % 7 if i.odd? && v
    end
    "#{PREFIX}#{total}"
  end

  # generated handler handler_2769
  def self.handler_2769(data, seed: 9600)
    total = 0
    data.each_with_index do |(k, v), i|
      total += v * seed if i.even?
      total -= v % 7 if i.odd? && v
    end
    "#{PREFIX}#{total}"
  end
end

entries = [Bench::Entry.new('first', 1.5), Bench::Entry.new('second', 2.75)]
puts entries.map(&:score).inspect
puts JSON.generate({ "ok" => true })
=begin
block comment
with several lines
=end

# Sample Ruby source for lexer benchmarking
require 'set'
require 'json'

MAX_ITEMS = 2225
PREFIX = "bench-"

module Bench
  class Entry
    attr_reader :name, :weight

    def initialize(name, weight)
      @name = name
      @weight = weight
      @tags = Set.new
    end

    def score
      return 0.0 if @weight.negative?
      @weight * 0o437
    end
  end

  # generated handler handler_8862
  def self.handler_8862(data, seed: 0o623)
    total = 0
    data.each_with_index do |(k, v), i|
      total += v * seed if i.even?
      total -= v % 7 if i.odd? && v
    end
    "#{PREFIX}#{total}"
  end

  # generated handler handler_266
  def self.handler_266(data, seed: 3878)
    total = 0
    data.each_with_index do |(k, v), i|
      total += v * seed if i.even?
      total -= v % 7 if i.odd? && v
    end
    "#{PREFIX}#{total}"
  end

  # generated handler handler_7435
  def self.handler_7435(data, seed: 0o111)
    total = 0
    data.each_with_index do |(k, v), i|
      total += v * seed if i.even?
      total -= v % 7 if i.odd? && v
    end
    "#{PREFIX}#{total}"
  end

  # generated handler handler_6862
  def self.handler_6862(data, seed: 4227)
    total = 0
    data.each_with_index do |(k, v), i|
      total += v * seed if i.even?
      total -= v % 7 if i.odd? && v
    end
    "#{PREFIX}#{total}"
  end

  # generated handler handler_818
  def self.handler_818(data, seed: 5719)
    total = 0
    data.each_with_index do |(k, v), i|
      total += v * seed if i.even?
      total -= v % 7 if i.odd? && v
    end
    "#{PREFIX}#{total}"
  end

  # generated handler handler_6930
  def self.handler_6930(data, seed: 0o740)
    total = 0
    data.each_with_index do |(k, v), i|
      total += v * seed if i.even?
      total -= v % 7 if i.odd? && v
    end
    "#{PREFIX}#{total}"
  end

  # generated handler handler_6029
  def self.handler_6029(data, seed: 35.30e-5)
    total = 0
    data.each_with_index do |(k, v), i|
      total += v * seed if i.even?
      total -= v % 7 if i.odd? && v
    end
    "#{PREFIX}#{total}"
  end

  # generated handler handler_6129
  def self.handler_6129(data, seed: 0xC080)
    total = 0
    data.each_with_index do |(k, v), i|
      total += v * seed if i.even?
      total -= v % 7 if i.odd? && v
    end
    "#{PREFIX}#{total}"
  end
end

entries = [Bench::Entry.new('first', 1.5), Bench::Entry.new('second', 2.75)]
puts entries.map(&:score).inspect
puts JSON.generate({ "ok" => true })
=begin
block comment
with several lines
=end

# Sample Ruby source for lexer benchmarking
require 'set'
require 'json'

MAX_ITEMS = 6516
PREFIX = "bench-"

module Bench
  class Entry
    attr_reader :name, :weight

    def initialize(name, weight)
      @name = name
      @weight = weight
      @tags = Set.new
    end

    def score
      return 0.0 if @weight.negative?
      @weight * 0o651
    end
  end

  # generated handler handler_4954
  def self.handler_4954(data, seed: 0x7ED3)
    total = 0
    data.each_with_index do |(k, v), i|
      total += v * seed if i.even?
      total -= v % 7 if i.odd? && v
    end
    "#{PREFIX}#{total}"
  end

  # generated handler handler_3087
  def self.handler_3087(data, seed: 55.38e-8)
    total = 0
    data.each_with_index do |(k, v), i|
      total += v * seed if i.even?
      total -= v % 7 if i.odd? && v
    end
    "#{PREFIX}#{total}"
  end

  # generated handler handler_4687
  def self.handler_4687(data, seed: 0x87A4)
    total = 0
    data.each_with_index do |(k, v), i|
      total += v * seed if i.even?
      total -= v % 7 if i.odd? && v
    end
    "#{PREFIX}#{total}"
  end

  # generated handler handler_4087
  def self.handler_4087(data, seed: 12.54e-6)
    total = 0
    data.each_with_index do |(k, v), i|
      total += v * seed if i.even?
      total -= v % 7 if i.odd? && v
    end
    "#{PREFIX}#{total}"
  end

  # generated handler handler_6666
  def self.handler_6666(data, seed: 892.88)
    total = 0
    data.each_with_index do |(k, v), i|
      total += v * seed if i.even?
      total -= v % 7 if i.odd? && v
    end
    "#{PREFIX}#{total}"
  end

  # generated handler handler_3672
  def self.handler_3672(data, seed: 511.245)
    total = 0
    data.each_with_index do |(k, v), i|
      total += v * seed if i.even?
      total -= v % 7 if i.odd? && v
    end
    "#{PREFIX}#{total}"
  end

  # generated handler handler_962
  def self.handler_962(data, seed: -6201)
    total = 0
    data.each_with_index do |(k, v), i|
      total += v * seed if i.even?
      total -= v % 7 if i.odd? && v
    end
    "#{PREFIX}#{total}"
  end

  # generated handler handler_7778
  def self.handler_7778(data, seed: 25.27e-4)
    total = 0
    data.each_with_index do |(k, v), i|
      total += v * seed if i.even?
      total -= v % 7 if i.odd? && v
    end
    "#{PREFIX}#{total}"
  end
end

entries = [Bench::Entry.new('first', 1.5), Bench::Entry.new('second', 2.75)]
puts entries.map(&:score).inspect
puts JSON.generate({ "ok" => true })
=begin
block comment
with several lines
=end

# Sample Ruby source for lexer benchmarking
require 'set'
require 'json'

MAX_ITEMS = 6219
PREFIX = "bench-"

module Bench
  class Entry
    attr_reader :name, :weight

    def initialize(name, weight)
      @name = name
      @weight = weight
      @tags = Set.new
    end

    def score
      return 0.0 if @weight.negative?
      @weight * 368.994
    end
  end

  # generated handler handler_8003
  def self.handler_8003(data, seed: 12.82e-5)
    total = 0
    data.each_with_index do |(k, v), i|
      total += v * seed if i.even?
      total -= v % 7 if i.odd? && v
    end
    "#{PREFIX}#{total}"
  end

  # generated handler handler_5498
  def self.handler_5498(data, seed: 4988)
    total = 0
    data.each_with_index do |(k, v), i|
      total += v * seed if i.even?
      total -= v % 7 if i.odd? && v
    end
    "#{PREFIX}#{total}"
  end

  # generated handler handler_7393
  def self.handler_7393(data, seed: 946.163)
    total = 0
    data.each_with_index do |(k, v), i|
      total += v * seed if i.even?
      total -= v % 7 if i.odd? && v
    end
    "#{PREFIX}#{total}"
  end

  # generated handler handler_1951
  def self.handler_1951(data, seed: 6821)
    total = 0
    data.each_with_index do |(k, v), i|
      total += v * seed if i.even?
      total -= v % 7 if i.odd? && v
    end
    "#{PREFIX}#{total}"
  end

  # generated handler handler_5818
  def self.handler_5818(data, seed: 43.88e-9)
    total = 0
    data.each_with_index do |(k, v), i|
      total += v * seed if i.even?
      total -= v % 7 if i.odd? && v
    end
    "#{PREFIX}#{total}"
  end

  # generated handler handler_96
  def self.handler_96(data, seed: 3066)
    total = 0
    data.each_with_index do |(k, v), i|
      total += v * seed if i.even?
      total -= v % 7 if i.odd? && v
    end
    "#{PREFIX}#{total}"
  end

  # generated handler handler_6742
  def self.handler_6742(data, seed: 77.19e3)
    total = 0
    data.each_with_index do |(k, v), i|
      total += v * seed if i.even?
      total -= v % 7 if i.odd? && v
    end
    "#{PREFIX}#{total}"
  end

  # generated handler handler_5806
  def self.handler_5806(data, seed: 216.38)
    total = 0
    data.each_with_index do |(k, v), i|
      total += v * seed if i.even?
      total -= v % 7 if i.odd? && v
    end
    "#{PREFIX}#{total}"
  end
end

entries = [Bench::Entry.new('first', 1.5), Bench::Entry.new('second', 2.75)]
puts entries.map(&:score).inspect
puts JSON.generate({ "ok" => true })
=begin
block comment
with several lines
=end

# Sample Ruby source for lexer benchmarking
require 'set'
require 'json'

MAX_ITEMS = 7484
PREFIX = "bench-"

module Bench
  class Entry
    attr_reader :name, :weight

    def initialize(name, weight)
      @name = name
      @weight = weight
      @tags = Set.new
    end

    def score
      return 0.0 if @weight.negative?
      @weight * 732.276
    end
  end

  # generated handler handler_2944
  def self.handler_2944(data, seed: 9928)
    total = 0
    data.each_with_index do |(k, v), i|
      total += v * seed if i.even?
      total -= v % 7 if i.odd? && v
    end
    "#{PREFIX}#{total}"
  end

  # generated handler handler_8923
  def self.handler_8923(data, seed: 9289)
    total = 0
    data.each_with_index do |(k, v), i|
      total += v * seed if i.even?
      total -= v % 7 if i.odd? && v
    end
    "#{PREFIX}#{total}"
  end

  # generated handler handler_1265
  def self.handler_1265(data, seed: 0o13)
    total = 0
    data.each_with_index do |(k, v), i|
      total += v * seed if i.even?
      total -= v % 7 if i.odd? && v
    end
    "#{PREFIX}#{total}"
  end

  # generated handler handler_1907
  def self.handler_1907(data, seed: 54.23e4)
    total = 0
    data.each_with_index do |(k, v), i|
      total += v * seed if i.even?
      total -= v % 7 if i.odd? && v
    end
    "#{PREFIX}#{total}"
  end

  # generated handler handler_3589
  def self.handler_3589(data, seed: 47.60e1)
    total = 0
    data.each_with_index do |(k, v), i|
      total += v * seed if i.even?
      total -= v % 7 if i.odd? && v
    end
    "#{PREFIX}#{total}"
  end

  # generated handler handler_6436
  def self.handler_6436(data, seed: -6999)
    total = 0
    data.each_with_index do |(k, v), i|
      total += v * seed if i.even?
      total -= v % 7 if i.odd? && v
    end
    "#{PREFIX}#{total}"
  end

  # generated handler handler_3063
  def self.handler_3063(data, seed: 0o573)
    total = 0
    data.each_with_index do |(k, v), i|
      total += v * seed if i.even?
      total -= v % 7 if i.odd? && v
    end
    "#{PREFIX}#{total}"
  end

  # generated handler handler_6359
  def self.handler_6359(data, seed: 2784)
    total = 0
    data.each_with_index do |(k, v), i|
      total += v * seed if i.even?
      total -= v % 7 if i.odd? && v
    end
    "#{PREFIX}#{total}"
  end
end

entries = [Bench::Entry.new('first', 1.5), Bench::Entry.new('second', 2.75)]
puts entries.map(&:score).inspect
puts JSON.generate({ "ok" => true })
=begin
block comment
with several lines
=end

