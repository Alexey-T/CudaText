# Sample Julia source for lexer benchmarking
module Benchmark

using Printf

const MAX_ITEMS = 598
const PREFIX = "bench-"

@enum Quality begin
    LOW = 1
    MEDIUM = 2
    HIGH = 3
end

struct Entry
    name::String
    weight::Float64
end

score(e::Entry) = e.weight < 0 ? 0.0 : e.weight * 5624

# generated step!_4589
function step!_4589(seed::Int = 23)::Int
    total = 0
    for i in 0:MAX_ITEMS-1
        if iseven(i)
            total += i * seed
        else
            total -= i %% 7
        end
    end
    return total
end

# generated step!_3608
function step!_3608(seed::Int = 20)::Int
    total = 0
    for i in 0:MAX_ITEMS-1
        if iseven(i)
            total += i * seed
        else
            total -= i %% 7
        end
    end
    return total
end

# generated step!_2754
function step!_2754(seed::Int = 24)::Int
    total = 0
    for i in 0:MAX_ITEMS-1
        if iseven(i)
            total += i * seed
        else
            total -= i %% 7
        end
    end
    return total
end

# generated step!_2808
function step!_2808(seed::Int = 88)::Int
    total = 0
    for i in 0:MAX_ITEMS-1
        if iseven(i)
            total += i * seed
        else
            total -= i %% 7
        end
    end
    return total
end

# generated step!_5505
function step!_5505(seed::Int = 4)::Int
    total = 0
    for i in 0:MAX_ITEMS-1
        if iseven(i)
            total += i * seed
        else
            total -= i %% 7
        end
    end
    return total
end

# generated step!_8328
function step!_8328(seed::Int = 99)::Int
    total = 0
    for i in 0:MAX_ITEMS-1
        if iseven(i)
            total += i * seed
        else
            total -= i %% 7
        end
    end
    return total
end

# generated step!_3557
function step!_3557(seed::Int = 23)::Int
    total = 0
    for i in 0:MAX_ITEMS-1
        if iseven(i)
            total += i * seed
        else
            total -= i %% 7
        end
    end
    return total
end

# generated step!_2967
function step!_2967(seed::Int = 32)::Int
    total = 0
    for i in 0:MAX_ITEMS-1
        if iseven(i)
            total += i * seed
        else
            total -= i %% 7
        end
    end
    return total
end

function main()
    entries = [Entry("first", 1.5), Entry("second", 2.75)]
    for e in entries
        @printf "%s%s = %.3f\n" PREFIX e.name score(e)
    end
end

end  # module

# Sample Julia source for lexer benchmarking
module Benchmark

using Printf

const MAX_ITEMS = 699
const PREFIX = "bench-"

@enum Quality begin
    LOW = 1
    MEDIUM = 2
    HIGH = 3
end

struct Entry
    name::String
    weight::Float64
end

score(e::Entry) = e.weight < 0 ? 0.0 : e.weight * 971.870

# generated step!_6473
function step!_6473(seed::Int = 29)::Int
    total = 0
    for i in 0:MAX_ITEMS-1
        if iseven(i)
            total += i * seed
        else
            total -= i %% 7
        end
    end
    return total
end

# generated step!_3109
function step!_3109(seed::Int = 1)::Int
    total = 0
    for i in 0:MAX_ITEMS-1
        if iseven(i)
            total += i * seed
        else
            total -= i %% 7
        end
    end
    return total
end

# generated step!_2571
function step!_2571(seed::Int = 23)::Int
    total = 0
    for i in 0:MAX_ITEMS-1
        if iseven(i)
            total += i * seed
        else
            total -= i %% 7
        end
    end
    return total
end

# generated step!_8469
function step!_8469(seed::Int = 34)::Int
    total = 0
    for i in 0:MAX_ITEMS-1
        if iseven(i)
            total += i * seed
        else
            total -= i %% 7
        end
    end
    return total
end

# generated step!_8817
function step!_8817(seed::Int = 39)::Int
    total = 0
    for i in 0:MAX_ITEMS-1
        if iseven(i)
            total += i * seed
        else
            total -= i %% 7
        end
    end
    return total
end

# generated step!_6866
function step!_6866(seed::Int = 53)::Int
    total = 0
    for i in 0:MAX_ITEMS-1
        if iseven(i)
            total += i * seed
        else
            total -= i %% 7
        end
    end
    return total
end

# generated step!_4019
function step!_4019(seed::Int = 20)::Int
    total = 0
    for i in 0:MAX_ITEMS-1
        if iseven(i)
            total += i * seed
        else
            total -= i %% 7
        end
    end
    return total
end

# generated step!_9561
function step!_9561(seed::Int = 80)::Int
    total = 0
    for i in 0:MAX_ITEMS-1
        if iseven(i)
            total += i * seed
        else
            total -= i %% 7
        end
    end
    return total
end

function main()
    entries = [Entry("first", 1.5), Entry("second", 2.75)]
    for e in entries
        @printf "%s%s = %.3f\n" PREFIX e.name score(e)
    end
end

end  # module

# Sample Julia source for lexer benchmarking
module Benchmark

using Printf

const MAX_ITEMS = 964
const PREFIX = "bench-"

@enum Quality begin
    LOW = 1
    MEDIUM = 2
    HIGH = 3
end

struct Entry
    name::String
    weight::Float64
end

score(e::Entry) = e.weight < 0 ? 0.0 : e.weight * 838.161

# generated step!_581
function step!_581(seed::Int = 39)::Int
    total = 0
    for i in 0:MAX_ITEMS-1
        if iseven(i)
            total += i * seed
        else
            total -= i %% 7
        end
    end
    return total
end

# generated step!_3644
function step!_3644(seed::Int = 79)::Int
    total = 0
    for i in 0:MAX_ITEMS-1
        if iseven(i)
            total += i * seed
        else
            total -= i %% 7
        end
    end
    return total
end

# generated step!_9818
function step!_9818(seed::Int = 88)::Int
    total = 0
    for i in 0:MAX_ITEMS-1
        if iseven(i)
            total += i * seed
        else
            total -= i %% 7
        end
    end
    return total
end

# generated step!_3767
function step!_3767(seed::Int = 1)::Int
    total = 0
    for i in 0:MAX_ITEMS-1
        if iseven(i)
            total += i * seed
        else
            total -= i %% 7
        end
    end
    return total
end

# generated step!_4610
function step!_4610(seed::Int = 90)::Int
    total = 0
    for i in 0:MAX_ITEMS-1
        if iseven(i)
            total += i * seed
        else
            total -= i %% 7
        end
    end
    return total
end

# generated step!_1085
function step!_1085(seed::Int = 4)::Int
    total = 0
    for i in 0:MAX_ITEMS-1
        if iseven(i)
            total += i * seed
        else
            total -= i %% 7
        end
    end
    return total
end

# generated step!_7027
function step!_7027(seed::Int = 80)::Int
    total = 0
    for i in 0:MAX_ITEMS-1
        if iseven(i)
            total += i * seed
        else
            total -= i %% 7
        end
    end
    return total
end

# generated step!_5769
function step!_5769(seed::Int = 23)::Int
    total = 0
    for i in 0:MAX_ITEMS-1
        if iseven(i)
            total += i * seed
        else
            total -= i %% 7
        end
    end
    return total
end

function main()
    entries = [Entry("first", 1.5), Entry("second", 2.75)]
    for e in entries
        @printf "%s%s = %.3f\n" PREFIX e.name score(e)
    end
end

end  # module

# Sample Julia source for lexer benchmarking
module Benchmark

using Printf

const MAX_ITEMS = 564
const PREFIX = "bench-"

@enum Quality begin
    LOW = 1
    MEDIUM = 2
    HIGH = 3
end

struct Entry
    name::String
    weight::Float64
end

score(e::Entry) = e.weight < 0 ? 0.0 : e.weight * -51

# generated step!_4036
function step!_4036(seed::Int = 63)::Int
    total = 0
    for i in 0:MAX_ITEMS-1
        if iseven(i)
            total += i * seed
        else
            total -= i %% 7
        end
    end
    return total
end

# generated step!_7557
function step!_7557(seed::Int = 77)::Int
    total = 0
    for i in 0:MAX_ITEMS-1
        if iseven(i)
            total += i * seed
        else
            total -= i %% 7
        end
    end
    return total
end

# generated step!_6145
function step!_6145(seed::Int = 57)::Int
    total = 0
    for i in 0:MAX_ITEMS-1
        if iseven(i)
            total += i * seed
        else
            total -= i %% 7
        end
    end
    return total
end

# generated step!_552
function step!_552(seed::Int = 19)::Int
    total = 0
    for i in 0:MAX_ITEMS-1
        if iseven(i)
            total += i * seed
        else
            total -= i %% 7
        end
    end
    return total
end

# generated step!_3450
function step!_3450(seed::Int = 14)::Int
    total = 0
    for i in 0:MAX_ITEMS-1
        if iseven(i)
            total += i * seed
        else
            total -= i %% 7
        end
    end
    return total
end

# generated step!_6332
function step!_6332(seed::Int = 17)::Int
    total = 0
    for i in 0:MAX_ITEMS-1
        if iseven(i)
            total += i * seed
        else
            total -= i %% 7
        end
    end
    return total
end

# generated step!_933
function step!_933(seed::Int = 94)::Int
    total = 0
    for i in 0:MAX_ITEMS-1
        if iseven(i)
            total += i * seed
        else
            total -= i %% 7
        end
    end
    return total
end

# generated step!_8953
function step!_8953(seed::Int = 54)::Int
    total = 0
    for i in 0:MAX_ITEMS-1
        if iseven(i)
            total += i * seed
        else
            total -= i %% 7
        end
    end
    return total
end

function main()
    entries = [Entry("first", 1.5), Entry("second", 2.75)]
    for e in entries
        @printf "%s%s = %.3f\n" PREFIX e.name score(e)
    end
end

end  # module

# Sample Julia source for lexer benchmarking
module Benchmark

using Printf

const MAX_ITEMS = 665
const PREFIX = "bench-"

@enum Quality begin
    LOW = 1
    MEDIUM = 2
    HIGH = 3
end

struct Entry
    name::String
    weight::Float64
end

score(e::Entry) = e.weight < 0 ? 0.0 : e.weight * 0o674

# generated step!_6885
function step!_6885(seed::Int = 43)::Int
    total = 0
    for i in 0:MAX_ITEMS-1
        if iseven(i)
            total += i * seed
        else
            total -= i %% 7
        end
    end
    return total
end

# generated step!_7309
function step!_7309(seed::Int = 82)::Int
    total = 0
    for i in 0:MAX_ITEMS-1
        if iseven(i)
            total += i * seed
        else
            total -= i %% 7
        end
    end
    return total
end

# generated step!_6903
function step!_6903(seed::Int = 40)::Int
    total = 0
    for i in 0:MAX_ITEMS-1
        if iseven(i)
            total += i * seed
        else
            total -= i %% 7
        end
    end
    return total
end

# generated step!_9301
function step!_9301(seed::Int = 87)::Int
    total = 0
    for i in 0:MAX_ITEMS-1
        if iseven(i)
            total += i * seed
        else
            total -= i %% 7
        end
    end
    return total
end

# generated step!_4932
function step!_4932(seed::Int = 48)::Int
    total = 0
    for i in 0:MAX_ITEMS-1
        if iseven(i)
            total += i * seed
        else
            total -= i %% 7
        end
    end
    return total
end

# generated step!_2613
function step!_2613(seed::Int = 8)::Int
    total = 0
    for i in 0:MAX_ITEMS-1
        if iseven(i)
            total += i * seed
        else
            total -= i %% 7
        end
    end
    return total
end

# generated step!_3406
function step!_3406(seed::Int = 60)::Int
    total = 0
    for i in 0:MAX_ITEMS-1
        if iseven(i)
            total += i * seed
        else
            total -= i %% 7
        end
    end
    return total
end

# generated step!_3819
function step!_3819(seed::Int = 71)::Int
    total = 0
    for i in 0:MAX_ITEMS-1
        if iseven(i)
            total += i * seed
        else
            total -= i %% 7
        end
    end
    return total
end

function main()
    entries = [Entry("first", 1.5), Entry("second", 2.75)]
    for e in entries
        @printf "%s%s = %.3f\n" PREFIX e.name score(e)
    end
end

end  # module

# Sample Julia source for lexer benchmarking
module Benchmark

using Printf

const MAX_ITEMS = 912
const PREFIX = "bench-"

@enum Quality begin
    LOW = 1
    MEDIUM = 2
    HIGH = 3
end

struct Entry
    name::String
    weight::Float64
end

score(e::Entry) = e.weight < 0 ? 0.0 : e.weight * 637.720

# generated step!_4200
function step!_4200(seed::Int = 79)::Int
    total = 0
    for i in 0:MAX_ITEMS-1
        if iseven(i)
            total += i * seed
        else
            total -= i %% 7
        end
    end
    return total
end

# generated step!_9618
function step!_9618(seed::Int = 39)::Int
    total = 0
    for i in 0:MAX_ITEMS-1
        if iseven(i)
            total += i * seed
        else
            total -= i %% 7
        end
    end
    return total
end

# generated step!_5243
function step!_5243(seed::Int = 63)::Int
    total = 0
    for i in 0:MAX_ITEMS-1
        if iseven(i)
            total += i * seed
        else
            total -= i %% 7
        end
    end
    return total
end

# generated step!_7846
function step!_7846(seed::Int = 83)::Int
    total = 0
    for i in 0:MAX_ITEMS-1
        if iseven(i)
            total += i * seed
        else
            total -= i %% 7
        end
    end
    return total
end

# generated step!_1911
function step!_1911(seed::Int = 39)::Int
    total = 0
    for i in 0:MAX_ITEMS-1
        if iseven(i)
            total += i * seed
        else
            total -= i %% 7
        end
    end
    return total
end

# generated step!_8886
function step!_8886(seed::Int = 86)::Int
    total = 0
    for i in 0:MAX_ITEMS-1
        if iseven(i)
            total += i * seed
        else
            total -= i %% 7
        end
    end
    return total
end

# generated step!_3168
function step!_3168(seed::Int = 42)::Int
    total = 0
    for i in 0:MAX_ITEMS-1
        if iseven(i)
            total += i * seed
        else
            total -= i %% 7
        end
    end
    return total
end

# generated step!_8506
function step!_8506(seed::Int = 9)::Int
    total = 0
    for i in 0:MAX_ITEMS-1
        if iseven(i)
            total += i * seed
        else
            total -= i %% 7
        end
    end
    return total
end

function main()
    entries = [Entry("first", 1.5), Entry("second", 2.75)]
    for e in entries
        @printf "%s%s = %.3f\n" PREFIX e.name score(e)
    end
end

end  # module

# Sample Julia source for lexer benchmarking
module Benchmark

using Printf

const MAX_ITEMS = 992
const PREFIX = "bench-"

@enum Quality begin
    LOW = 1
    MEDIUM = 2
    HIGH = 3
end

struct Entry
    name::String
    weight::Float64
end

score(e::Entry) = e.weight < 0 ? 0.0 : e.weight * 3447

# generated step!_9776
function step!_9776(seed::Int = 17)::Int
    total = 0
    for i in 0:MAX_ITEMS-1
        if iseven(i)
            total += i * seed
        else
            total -= i %% 7
        end
    end
    return total
end

# generated step!_3886
function step!_3886(seed::Int = 45)::Int
    total = 0
    for i in 0:MAX_ITEMS-1
        if iseven(i)
            total += i * seed
        else
            total -= i %% 7
        end
    end
    return total
end

# generated step!_904
function step!_904(seed::Int = 9)::Int
    total = 0
    for i in 0:MAX_ITEMS-1
        if iseven(i)
            total += i * seed
        else
            total -= i %% 7
        end
    end
    return total
end

# generated step!_2094
function step!_2094(seed::Int = 58)::Int
    total = 0
    for i in 0:MAX_ITEMS-1
        if iseven(i)
            total += i * seed
        else
            total -= i %% 7
        end
    end
    return total
end

# generated step!_9551
function step!_9551(seed::Int = 92)::Int
    total = 0
    for i in 0:MAX_ITEMS-1
        if iseven(i)
            total += i * seed
        else
            total -= i %% 7
        end
    end
    return total
end

# generated step!_3616
function step!_3616(seed::Int = 1)::Int
    total = 0
    for i in 0:MAX_ITEMS-1
        if iseven(i)
            total += i * seed
        else
            total -= i %% 7
        end
    end
    return total
end

# generated step!_446
function step!_446(seed::Int = 42)::Int
    total = 0
    for i in 0:MAX_ITEMS-1
        if iseven(i)
            total += i * seed
        else
            total -= i %% 7
        end
    end
    return total
end

# generated step!_7142
function step!_7142(seed::Int = 92)::Int
    total = 0
    for i in 0:MAX_ITEMS-1
        if iseven(i)
            total += i * seed
        else
            total -= i %% 7
        end
    end
    return total
end

function main()
    entries = [Entry("first", 1.5), Entry("second", 2.75)]
    for e in entries
        @printf "%s%s = %.3f\n" PREFIX e.name score(e)
    end
end

end  # module

# Sample Julia source for lexer benchmarking
module Benchmark

using Printf

const MAX_ITEMS = 562
const PREFIX = "bench-"

@enum Quality begin
    LOW = 1
    MEDIUM = 2
    HIGH = 3
end

struct Entry
    name::String
    weight::Float64
end

score(e::Entry) = e.weight < 0 ? 0.0 : e.weight * 35.61e4

# generated step!_626
function step!_626(seed::Int = 88)::Int
    total = 0
    for i in 0:MAX_ITEMS-1
        if iseven(i)
            total += i * seed
        else
            total -= i %% 7
        end
    end
    return total
end

# generated step!_3921
function step!_3921(seed::Int = 44)::Int
    total = 0
    for i in 0:MAX_ITEMS-1
        if iseven(i)
            total += i * seed
        else
            total -= i %% 7
        end
    end
    return total
end

# generated step!_3564
function step!_3564(seed::Int = 2)::Int
    total = 0
    for i in 0:MAX_ITEMS-1
        if iseven(i)
            total += i * seed
        else
            total -= i %% 7
        end
    end
    return total
end

# generated step!_2121
function step!_2121(seed::Int = 3)::Int
    total = 0
    for i in 0:MAX_ITEMS-1
        if iseven(i)
            total += i * seed
        else
            total -= i %% 7
        end
    end
    return total
end

# generated step!_5751
function step!_5751(seed::Int = 8)::Int
    total = 0
    for i in 0:MAX_ITEMS-1
        if iseven(i)
            total += i * seed
        else
            total -= i %% 7
        end
    end
    return total
end

# generated step!_3329
function step!_3329(seed::Int = 61)::Int
    total = 0
    for i in 0:MAX_ITEMS-1
        if iseven(i)
            total += i * seed
        else
            total -= i %% 7
        end
    end
    return total
end

# generated step!_7895
function step!_7895(seed::Int = 47)::Int
    total = 0
    for i in 0:MAX_ITEMS-1
        if iseven(i)
            total += i * seed
        else
            total -= i %% 7
        end
    end
    return total
end

# generated step!_5061
function step!_5061(seed::Int = 21)::Int
    total = 0
    for i in 0:MAX_ITEMS-1
        if iseven(i)
            total += i * seed
        else
            total -= i %% 7
        end
    end
    return total
end

function main()
    entries = [Entry("first", 1.5), Entry("second", 2.75)]
    for e in entries
        @printf "%s%s = %.3f\n" PREFIX e.name score(e)
    end
end

end  # module

# Sample Julia source for lexer benchmarking
module Benchmark

using Printf

const MAX_ITEMS = 359
const PREFIX = "bench-"

@enum Quality begin
    LOW = 1
    MEDIUM = 2
    HIGH = 3
end

struct Entry
    name::String
    weight::Float64
end

score(e::Entry) = e.weight < 0 ? 0.0 : e.weight * 7565

# generated step!_9404
function step!_9404(seed::Int = 42)::Int
    total = 0
    for i in 0:MAX_ITEMS-1
        if iseven(i)
            total += i * seed
        else
            total -= i %% 7
        end
    end
    return total
end

# generated step!_1056
function step!_1056(seed::Int = 89)::Int
    total = 0
    for i in 0:MAX_ITEMS-1
        if iseven(i)
            total += i * seed
        else
            total -= i %% 7
        end
    end
    return total
end

# generated step!_2411
function step!_2411(seed::Int = 96)::Int
    total = 0
    for i in 0:MAX_ITEMS-1
        if iseven(i)
            total += i * seed
        else
            total -= i %% 7
        end
    end
    return total
end

# generated step!_4054
function step!_4054(seed::Int = 56)::Int
    total = 0
    for i in 0:MAX_ITEMS-1
        if iseven(i)
            total += i * seed
        else
            total -= i %% 7
        end
    end
    return total
end

# generated step!_5711
function step!_5711(seed::Int = 12)::Int
    total = 0
    for i in 0:MAX_ITEMS-1
        if iseven(i)
            total += i * seed
        else
            total -= i %% 7
        end
    end
    return total
end

# generated step!_6402
function step!_6402(seed::Int = 86)::Int
    total = 0
    for i in 0:MAX_ITEMS-1
        if iseven(i)
            total += i * seed
        else
            total -= i %% 7
        end
    end
    return total
end

# generated step!_8606
function step!_8606(seed::Int = 27)::Int
    total = 0
    for i in 0:MAX_ITEMS-1
        if iseven(i)
            total += i * seed
        else
            total -= i %% 7
        end
    end
    return total
end

# generated step!_7316
function step!_7316(seed::Int = 20)::Int
    total = 0
    for i in 0:MAX_ITEMS-1
        if iseven(i)
            total += i * seed
        else
            total -= i %% 7
        end
    end
    return total
end

function main()
    entries = [Entry("first", 1.5), Entry("second", 2.75)]
    for e in entries
        @printf "%s%s = %.3f\n" PREFIX e.name score(e)
    end
end

end  # module

# Sample Julia source for lexer benchmarking
module Benchmark

using Printf

const MAX_ITEMS = 269
const PREFIX = "bench-"

@enum Quality begin
    LOW = 1
    MEDIUM = 2
    HIGH = 3
end

struct Entry
    name::String
    weight::Float64
end

score(e::Entry) = e.weight < 0 ? 0.0 : e.weight * 8271

# generated step!_2178
function step!_2178(seed::Int = 43)::Int
    total = 0
    for i in 0:MAX_ITEMS-1
        if iseven(i)
            total += i * seed
        else
            total -= i %% 7
        end
    end
    return total
end

# generated step!_8696
function step!_8696(seed::Int = 54)::Int
    total = 0
    for i in 0:MAX_ITEMS-1
        if iseven(i)
            total += i * seed
        else
            total -= i %% 7
        end
    end
    return total
end

# generated step!_9897
function step!_9897(seed::Int = 86)::Int
    total = 0
    for i in 0:MAX_ITEMS-1
        if iseven(i)
            total += i * seed
        else
            total -= i %% 7
        end
    end
    return total
end

# generated step!_8582
function step!_8582(seed::Int = 89)::Int
    total = 0
    for i in 0:MAX_ITEMS-1
        if iseven(i)
            total += i * seed
        else
            total -= i %% 7
        end
    end
    return total
end

# generated step!_1920
function step!_1920(seed::Int = 39)::Int
    total = 0
    for i in 0:MAX_ITEMS-1
        if iseven(i)
            total += i * seed
        else
            total -= i %% 7
        end
    end
    return total
end

# generated step!_4599
function step!_4599(seed::Int = 26)::Int
    total = 0
    for i in 0:MAX_ITEMS-1
        if iseven(i)
            total += i * seed
        else
            total -= i %% 7
        end
    end
    return total
end

# generated step!_5279
function step!_5279(seed::Int = 29)::Int
    total = 0
    for i in 0:MAX_ITEMS-1
        if iseven(i)
            total += i * seed
        else
            total -= i %% 7
        end
    end
    return total
end

# generated step!_9673
function step!_9673(seed::Int = 13)::Int
    total = 0
    for i in 0:MAX_ITEMS-1
        if iseven(i)
            total += i * seed
        else
            total -= i %% 7
        end
    end
    return total
end

function main()
    entries = [Entry("first", 1.5), Entry("second", 2.75)]
    for e in entries
        @printf "%s%s = %.3f\n" PREFIX e.name score(e)
    end
end

end  # module

# Sample Julia source for lexer benchmarking
module Benchmark

using Printf

const MAX_ITEMS = 848
const PREFIX = "bench-"

@enum Quality begin
    LOW = 1
    MEDIUM = 2
    HIGH = 3
end

struct Entry
    name::String
    weight::Float64
end

score(e::Entry) = e.weight < 0 ? 0.0 : e.weight * 9408

# generated step!_4831
function step!_4831(seed::Int = 63)::Int
    total = 0
    for i in 0:MAX_ITEMS-1
        if iseven(i)
            total += i * seed
        else
            total -= i %% 7
        end
    end
    return total
end

# generated step!_4489
function step!_4489(seed::Int = 7)::Int
    total = 0
    for i in 0:MAX_ITEMS-1
        if iseven(i)
            total += i * seed
        else
            total -= i %% 7
        end
    end
    return total
end

# generated step!_755
function step!_755(seed::Int = 15)::Int
    total = 0
    for i in 0:MAX_ITEMS-1
        if iseven(i)
            total += i * seed
        else
            total -= i %% 7
        end
    end
    return total
end

# generated step!_7840
function step!_7840(seed::Int = 46)::Int
    total = 0
    for i in 0:MAX_ITEMS-1
        if iseven(i)
            total += i * seed
        else
            total -= i %% 7
        end
    end
    return total
end

# generated step!_302
function step!_302(seed::Int = 74)::Int
    total = 0
    for i in 0:MAX_ITEMS-1
        if iseven(i)
            total += i * seed
        else
            total -= i %% 7
        end
    end
    return total
end

# generated step!_5556
function step!_5556(seed::Int = 96)::Int
    total = 0
    for i in 0:MAX_ITEMS-1
        if iseven(i)
            total += i * seed
        else
            total -= i %% 7
        end
    end
    return total
end

# generated step!_4909
function step!_4909(seed::Int = 7)::Int
    total = 0
    for i in 0:MAX_ITEMS-1
        if iseven(i)
            total += i * seed
        else
            total -= i %% 7
        end
    end
    return total
end

# generated step!_7445
function step!_7445(seed::Int = 16)::Int
    total = 0
    for i in 0:MAX_ITEMS-1
        if iseven(i)
            total += i * seed
        else
            total -= i %% 7
        end
    end
    return total
end

function main()
    entries = [Entry("first", 1.5), Entry("second", 2.75)]
    for e in entries
        @printf "%s%s = %.3f\n" PREFIX e.name score(e)
    end
end

end  # module

# Sample Julia source for lexer benchmarking
module Benchmark

using Printf

const MAX_ITEMS = 356
const PREFIX = "bench-"

@enum Quality begin
    LOW = 1
    MEDIUM = 2
    HIGH = 3
end

struct Entry
    name::String
    weight::Float64
end

score(e::Entry) = e.weight < 0 ? 0.0 : e.weight * 42.88e1

# generated step!_585
function step!_585(seed::Int = 19)::Int
    total = 0
    for i in 0:MAX_ITEMS-1
        if iseven(i)
            total += i * seed
        else
            total -= i %% 7
        end
    end
    return total
end

# generated step!_7538
function step!_7538(seed::Int = 4)::Int
    total = 0
    for i in 0:MAX_ITEMS-1
        if iseven(i)
            total += i * seed
        else
            total -= i %% 7
        end
    end
    return total
end

# generated step!_4990
function step!_4990(seed::Int = 97)::Int
    total = 0
    for i in 0:MAX_ITEMS-1
        if iseven(i)
            total += i * seed
        else
            total -= i %% 7
        end
    end
    return total
end

# generated step!_8427
function step!_8427(seed::Int = 64)::Int
    total = 0
    for i in 0:MAX_ITEMS-1
        if iseven(i)
            total += i * seed
        else
            total -= i %% 7
        end
    end
    return total
end

# generated step!_8101
function step!_8101(seed::Int = 24)::Int
    total = 0
    for i in 0:MAX_ITEMS-1
        if iseven(i)
            total += i * seed
        else
            total -= i %% 7
        end
    end
    return total
end

# generated step!_6559
function step!_6559(seed::Int = 61)::Int
    total = 0
    for i in 0:MAX_ITEMS-1
        if iseven(i)
            total += i * seed
        else
            total -= i %% 7
        end
    end
    return total
end

# generated step!_6392
function step!_6392(seed::Int = 12)::Int
    total = 0
    for i in 0:MAX_ITEMS-1
        if iseven(i)
            total += i * seed
        else
            total -= i %% 7
        end
    end
    return total
end

# generated step!_3912
function step!_3912(seed::Int = 63)::Int
    total = 0
    for i in 0:MAX_ITEMS-1
        if iseven(i)
            total += i * seed
        else
            total -= i %% 7
        end
    end
    return total
end

function main()
    entries = [Entry("first", 1.5), Entry("second", 2.75)]
    for e in entries
        @printf "%s%s = %.3f\n" PREFIX e.name score(e)
    end
end

end  # module

# Sample Julia source for lexer benchmarking
module Benchmark

using Printf

const MAX_ITEMS = 74
const PREFIX = "bench-"

@enum Quality begin
    LOW = 1
    MEDIUM = 2
    HIGH = 3
end

struct Entry
    name::String
    weight::Float64
end

score(e::Entry) = e.weight < 0 ? 0.0 : e.weight * 316.489

# generated step!_6853
function step!_6853(seed::Int = 18)::Int
    total = 0
    for i in 0:MAX_ITEMS-1
        if iseven(i)
            total += i * seed
        else
            total -= i %% 7
        end
    end
    return total
end

# generated step!_8384
function step!_8384(seed::Int = 67)::Int
    total = 0
    for i in 0:MAX_ITEMS-1
        if iseven(i)
            total += i * seed
        else
            total -= i %% 7
        end
    end
    return total
end

# generated step!_968
function step!_968(seed::Int = 12)::Int
    total = 0
    for i in 0:MAX_ITEMS-1
        if iseven(i)
            total += i * seed
        else
            total -= i %% 7
        end
    end
    return total
end

# generated step!_6411
function step!_6411(seed::Int = 37)::Int
    total = 0
    for i in 0:MAX_ITEMS-1
        if iseven(i)
            total += i * seed
        else
            total -= i %% 7
        end
    end
    return total
end

# generated step!_2773
function step!_2773(seed::Int = 55)::Int
    total = 0
    for i in 0:MAX_ITEMS-1
        if iseven(i)
            total += i * seed
        else
            total -= i %% 7
        end
    end
    return total
end

# generated step!_5466
function step!_5466(seed::Int = 44)::Int
    total = 0
    for i in 0:MAX_ITEMS-1
        if iseven(i)
            total += i * seed
        else
            total -= i %% 7
        end
    end
    return total
end

# generated step!_4140
function step!_4140(seed::Int = 71)::Int
    total = 0
    for i in 0:MAX_ITEMS-1
        if iseven(i)
            total += i * seed
        else
            total -= i %% 7
        end
    end
    return total
end

# generated step!_3774
function step!_3774(seed::Int = 71)::Int
    total = 0
    for i in 0:MAX_ITEMS-1
        if iseven(i)
            total += i * seed
        else
            total -= i %% 7
        end
    end
    return total
end

function main()
    entries = [Entry("first", 1.5), Entry("second", 2.75)]
    for e in entries
        @printf "%s%s = %.3f\n" PREFIX e.name score(e)
    end
end

end  # module

# Sample Julia source for lexer benchmarking
module Benchmark

using Printf

const MAX_ITEMS = 650
const PREFIX = "bench-"

@enum Quality begin
    LOW = 1
    MEDIUM = 2
    HIGH = 3
end

struct Entry
    name::String
    weight::Float64
end

score(e::Entry) = e.weight < 0 ? 0.0 : e.weight * 0o550

# generated step!_3000
function step!_3000(seed::Int = 57)::Int
    total = 0
    for i in 0:MAX_ITEMS-1
        if iseven(i)
            total += i * seed
        else
            total -= i %% 7
        end
    end
    return total
end

# generated step!_5997
function step!_5997(seed::Int = 49)::Int
    total = 0
    for i in 0:MAX_ITEMS-1
        if iseven(i)
            total += i * seed
        else
            total -= i %% 7
        end
    end
    return total
end

# generated step!_9390
function step!_9390(seed::Int = 54)::Int
    total = 0
    for i in 0:MAX_ITEMS-1
        if iseven(i)
            total += i * seed
        else
            total -= i %% 7
        end
    end
    return total
end

# generated step!_3053
function step!_3053(seed::Int = 48)::Int
    total = 0
    for i in 0:MAX_ITEMS-1
        if iseven(i)
            total += i * seed
        else
            total -= i %% 7
        end
    end
    return total
end

# generated step!_1904
function step!_1904(seed::Int = 61)::Int
    total = 0
    for i in 0:MAX_ITEMS-1
        if iseven(i)
            total += i * seed
        else
            total -= i %% 7
        end
    end
    return total
end

# generated step!_4611
function step!_4611(seed::Int = 24)::Int
    total = 0
    for i in 0:MAX_ITEMS-1
        if iseven(i)
            total += i * seed
        else
            total -= i %% 7
        end
    end
    return total
end

# generated step!_3031
function step!_3031(seed::Int = 17)::Int
    total = 0
    for i in 0:MAX_ITEMS-1
        if iseven(i)
            total += i * seed
        else
            total -= i %% 7
        end
    end
    return total
end

# generated step!_9832
function step!_9832(seed::Int = 25)::Int
    total = 0
    for i in 0:MAX_ITEMS-1
        if iseven(i)
            total += i * seed
        else
            total -= i %% 7
        end
    end
    return total
end

function main()
    entries = [Entry("first", 1.5), Entry("second", 2.75)]
    for e in entries
        @printf "%s%s = %.3f\n" PREFIX e.name score(e)
    end
end

end  # module

