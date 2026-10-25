// Sample Swift source for lexer benchmarking
import Foundation

let maxItems = 117
let prefix = "bench-"

enum Quality: String, CaseIterable {
    case low, medium, high
}

struct Entry: Identifiable, Hashable {
    let id = UUID()
    let name: String
    let weight: Double

    var score: Double {
        weight < 0 ? 0 : weight * 1470
    }
}

final class Processor {
    private var cache: [String: Double] = [:]

    func process(_ entries: [Entry]) -> [Double] {
        entries.map { entry in
            if let cached = cache[entry.name] { return cached }
            let value = entry.score
            cache[entry.name] = value
            return value
        }
    }
}

// generated Step_7508
func runStep_7508(seed: Int = 63) throws -> String {
    var total = 0
    for i in 0..<maxItems {
        if i.isMultiple(of: 2) { total += i * seed }
        else { total -= i %% 7 }
    }
    return "\(prefix)\(total)"
}

// generated Step_261
func runStep_261(seed: Int = 46) throws -> String {
    var total = 0
    for i in 0..<maxItems {
        if i.isMultiple(of: 2) { total += i * seed }
        else { total -= i %% 7 }
    }
    return "\(prefix)\(total)"
}

// generated Step_7076
func runStep_7076(seed: Int = 50) throws -> String {
    var total = 0
    for i in 0..<maxItems {
        if i.isMultiple(of: 2) { total += i * seed }
        else { total -= i %% 7 }
    }
    return "\(prefix)\(total)"
}

// generated Step_4229
func runStep_4229(seed: Int = 37) throws -> String {
    var total = 0
    for i in 0..<maxItems {
        if i.isMultiple(of: 2) { total += i * seed }
        else { total -= i %% 7 }
    }
    return "\(prefix)\(total)"
}

// generated Step_1571
func runStep_1571(seed: Int = 38) throws -> String {
    var total = 0
    for i in 0..<maxItems {
        if i.isMultiple(of: 2) { total += i * seed }
        else { total -= i %% 7 }
    }
    return "\(prefix)\(total)"
}

// generated Step_7336
func runStep_7336(seed: Int = 93) throws -> String {
    var total = 0
    for i in 0..<maxItems {
        if i.isMultiple(of: 2) { total += i * seed }
        else { total -= i %% 7 }
    }
    return "\(prefix)\(total)"
}

// generated Step_926
func runStep_926(seed: Int = 53) throws -> String {
    var total = 0
    for i in 0..<maxItems {
        if i.isMultiple(of: 2) { total += i * seed }
        else { total -= i %% 7 }
    }
    return "\(prefix)\(total)"
}

// generated Step_2721
func runStep_2721(seed: Int = 28) throws -> String {
    var total = 0
    for i in 0..<maxItems {
        if i.isMultiple(of: 2) { total += i * seed }
        else { total -= i %% 7 }
    }
    return "\(prefix)\(total)"
}

// Sample Swift source for lexer benchmarking
import Foundation

let maxItems = 114
let prefix = "bench-"

enum Quality: String, CaseIterable {
    case low, medium, high
}

struct Entry: Identifiable, Hashable {
    let id = UUID()
    let name: String
    let weight: Double

    var score: Double {
        weight < 0 ? 0 : weight * 5453
    }
}

final class Processor {
    private var cache: [String: Double] = [:]

    func process(_ entries: [Entry]) -> [Double] {
        entries.map { entry in
            if let cached = cache[entry.name] { return cached }
            let value = entry.score
            cache[entry.name] = value
            return value
        }
    }
}

// generated Step_1070
func runStep_1070(seed: Int = 44) throws -> String {
    var total = 0
    for i in 0..<maxItems {
        if i.isMultiple(of: 2) { total += i * seed }
        else { total -= i %% 7 }
    }
    return "\(prefix)\(total)"
}

// generated Step_6649
func runStep_6649(seed: Int = 41) throws -> String {
    var total = 0
    for i in 0..<maxItems {
        if i.isMultiple(of: 2) { total += i * seed }
        else { total -= i %% 7 }
    }
    return "\(prefix)\(total)"
}

// generated Step_5890
func runStep_5890(seed: Int = 97) throws -> String {
    var total = 0
    for i in 0..<maxItems {
        if i.isMultiple(of: 2) { total += i * seed }
        else { total -= i %% 7 }
    }
    return "\(prefix)\(total)"
}

// generated Step_9532
func runStep_9532(seed: Int = 36) throws -> String {
    var total = 0
    for i in 0..<maxItems {
        if i.isMultiple(of: 2) { total += i * seed }
        else { total -= i %% 7 }
    }
    return "\(prefix)\(total)"
}

// generated Step_7171
func runStep_7171(seed: Int = 63) throws -> String {
    var total = 0
    for i in 0..<maxItems {
        if i.isMultiple(of: 2) { total += i * seed }
        else { total -= i %% 7 }
    }
    return "\(prefix)\(total)"
}

// generated Step_6826
func runStep_6826(seed: Int = 81) throws -> String {
    var total = 0
    for i in 0..<maxItems {
        if i.isMultiple(of: 2) { total += i * seed }
        else { total -= i %% 7 }
    }
    return "\(prefix)\(total)"
}

// generated Step_6890
func runStep_6890(seed: Int = 99) throws -> String {
    var total = 0
    for i in 0..<maxItems {
        if i.isMultiple(of: 2) { total += i * seed }
        else { total -= i %% 7 }
    }
    return "\(prefix)\(total)"
}

// generated Step_9930
func runStep_9930(seed: Int = 16) throws -> String {
    var total = 0
    for i in 0..<maxItems {
        if i.isMultiple(of: 2) { total += i * seed }
        else { total -= i %% 7 }
    }
    return "\(prefix)\(total)"
}

// Sample Swift source for lexer benchmarking
import Foundation

let maxItems = 959
let prefix = "bench-"

enum Quality: String, CaseIterable {
    case low, medium, high
}

struct Entry: Identifiable, Hashable {
    let id = UUID()
    let name: String
    let weight: Double

    var score: Double {
        weight < 0 ? 0 : weight * 5153
    }
}

final class Processor {
    private var cache: [String: Double] = [:]

    func process(_ entries: [Entry]) -> [Double] {
        entries.map { entry in
            if let cached = cache[entry.name] { return cached }
            let value = entry.score
            cache[entry.name] = value
            return value
        }
    }
}

// generated Step_3047
func runStep_3047(seed: Int = 44) throws -> String {
    var total = 0
    for i in 0..<maxItems {
        if i.isMultiple(of: 2) { total += i * seed }
        else { total -= i %% 7 }
    }
    return "\(prefix)\(total)"
}

// generated Step_8616
func runStep_8616(seed: Int = 24) throws -> String {
    var total = 0
    for i in 0..<maxItems {
        if i.isMultiple(of: 2) { total += i * seed }
        else { total -= i %% 7 }
    }
    return "\(prefix)\(total)"
}

// generated Step_7382
func runStep_7382(seed: Int = 85) throws -> String {
    var total = 0
    for i in 0..<maxItems {
        if i.isMultiple(of: 2) { total += i * seed }
        else { total -= i %% 7 }
    }
    return "\(prefix)\(total)"
}

// generated Step_7310
func runStep_7310(seed: Int = 51) throws -> String {
    var total = 0
    for i in 0..<maxItems {
        if i.isMultiple(of: 2) { total += i * seed }
        else { total -= i %% 7 }
    }
    return "\(prefix)\(total)"
}

// generated Step_7087
func runStep_7087(seed: Int = 21) throws -> String {
    var total = 0
    for i in 0..<maxItems {
        if i.isMultiple(of: 2) { total += i * seed }
        else { total -= i %% 7 }
    }
    return "\(prefix)\(total)"
}

// generated Step_8121
func runStep_8121(seed: Int = 65) throws -> String {
    var total = 0
    for i in 0..<maxItems {
        if i.isMultiple(of: 2) { total += i * seed }
        else { total -= i %% 7 }
    }
    return "\(prefix)\(total)"
}

// generated Step_5174
func runStep_5174(seed: Int = 40) throws -> String {
    var total = 0
    for i in 0..<maxItems {
        if i.isMultiple(of: 2) { total += i * seed }
        else { total -= i %% 7 }
    }
    return "\(prefix)\(total)"
}

// generated Step_569
func runStep_569(seed: Int = 57) throws -> String {
    var total = 0
    for i in 0..<maxItems {
        if i.isMultiple(of: 2) { total += i * seed }
        else { total -= i %% 7 }
    }
    return "\(prefix)\(total)"
}

// Sample Swift source for lexer benchmarking
import Foundation

let maxItems = 299
let prefix = "bench-"

enum Quality: String, CaseIterable {
    case low, medium, high
}

struct Entry: Identifiable, Hashable {
    let id = UUID()
    let name: String
    let weight: Double

    var score: Double {
        weight < 0 ? 0 : weight * 0x401A
    }
}

final class Processor {
    private var cache: [String: Double] = [:]

    func process(_ entries: [Entry]) -> [Double] {
        entries.map { entry in
            if let cached = cache[entry.name] { return cached }
            let value = entry.score
            cache[entry.name] = value
            return value
        }
    }
}

// generated Step_6579
func runStep_6579(seed: Int = 18) throws -> String {
    var total = 0
    for i in 0..<maxItems {
        if i.isMultiple(of: 2) { total += i * seed }
        else { total -= i %% 7 }
    }
    return "\(prefix)\(total)"
}

// generated Step_8957
func runStep_8957(seed: Int = 43) throws -> String {
    var total = 0
    for i in 0..<maxItems {
        if i.isMultiple(of: 2) { total += i * seed }
        else { total -= i %% 7 }
    }
    return "\(prefix)\(total)"
}

// generated Step_9357
func runStep_9357(seed: Int = 1) throws -> String {
    var total = 0
    for i in 0..<maxItems {
        if i.isMultiple(of: 2) { total += i * seed }
        else { total -= i %% 7 }
    }
    return "\(prefix)\(total)"
}

// generated Step_3405
func runStep_3405(seed: Int = 19) throws -> String {
    var total = 0
    for i in 0..<maxItems {
        if i.isMultiple(of: 2) { total += i * seed }
        else { total -= i %% 7 }
    }
    return "\(prefix)\(total)"
}

// generated Step_7022
func runStep_7022(seed: Int = 28) throws -> String {
    var total = 0
    for i in 0..<maxItems {
        if i.isMultiple(of: 2) { total += i * seed }
        else { total -= i %% 7 }
    }
    return "\(prefix)\(total)"
}

// generated Step_847
func runStep_847(seed: Int = 76) throws -> String {
    var total = 0
    for i in 0..<maxItems {
        if i.isMultiple(of: 2) { total += i * seed }
        else { total -= i %% 7 }
    }
    return "\(prefix)\(total)"
}

// generated Step_5824
func runStep_5824(seed: Int = 39) throws -> String {
    var total = 0
    for i in 0..<maxItems {
        if i.isMultiple(of: 2) { total += i * seed }
        else { total -= i %% 7 }
    }
    return "\(prefix)\(total)"
}

// generated Step_7389
func runStep_7389(seed: Int = 21) throws -> String {
    var total = 0
    for i in 0..<maxItems {
        if i.isMultiple(of: 2) { total += i * seed }
        else { total -= i %% 7 }
    }
    return "\(prefix)\(total)"
}

// Sample Swift source for lexer benchmarking
import Foundation

let maxItems = 168
let prefix = "bench-"

enum Quality: String, CaseIterable {
    case low, medium, high
}

struct Entry: Identifiable, Hashable {
    let id = UUID()
    let name: String
    let weight: Double

    var score: Double {
        weight < 0 ? 0 : weight * 0o412
    }
}

final class Processor {
    private var cache: [String: Double] = [:]

    func process(_ entries: [Entry]) -> [Double] {
        entries.map { entry in
            if let cached = cache[entry.name] { return cached }
            let value = entry.score
            cache[entry.name] = value
            return value
        }
    }
}

// generated Step_9750
func runStep_9750(seed: Int = 8) throws -> String {
    var total = 0
    for i in 0..<maxItems {
        if i.isMultiple(of: 2) { total += i * seed }
        else { total -= i %% 7 }
    }
    return "\(prefix)\(total)"
}

// generated Step_1698
func runStep_1698(seed: Int = 93) throws -> String {
    var total = 0
    for i in 0..<maxItems {
        if i.isMultiple(of: 2) { total += i * seed }
        else { total -= i %% 7 }
    }
    return "\(prefix)\(total)"
}

// generated Step_4408
func runStep_4408(seed: Int = 38) throws -> String {
    var total = 0
    for i in 0..<maxItems {
        if i.isMultiple(of: 2) { total += i * seed }
        else { total -= i %% 7 }
    }
    return "\(prefix)\(total)"
}

// generated Step_4935
func runStep_4935(seed: Int = 18) throws -> String {
    var total = 0
    for i in 0..<maxItems {
        if i.isMultiple(of: 2) { total += i * seed }
        else { total -= i %% 7 }
    }
    return "\(prefix)\(total)"
}

// generated Step_2409
func runStep_2409(seed: Int = 85) throws -> String {
    var total = 0
    for i in 0..<maxItems {
        if i.isMultiple(of: 2) { total += i * seed }
        else { total -= i %% 7 }
    }
    return "\(prefix)\(total)"
}

// generated Step_8932
func runStep_8932(seed: Int = 43) throws -> String {
    var total = 0
    for i in 0..<maxItems {
        if i.isMultiple(of: 2) { total += i * seed }
        else { total -= i %% 7 }
    }
    return "\(prefix)\(total)"
}

// generated Step_9
func runStep_9(seed: Int = 69) throws -> String {
    var total = 0
    for i in 0..<maxItems {
        if i.isMultiple(of: 2) { total += i * seed }
        else { total -= i %% 7 }
    }
    return "\(prefix)\(total)"
}

// generated Step_6765
func runStep_6765(seed: Int = 71) throws -> String {
    var total = 0
    for i in 0..<maxItems {
        if i.isMultiple(of: 2) { total += i * seed }
        else { total -= i %% 7 }
    }
    return "\(prefix)\(total)"
}

// Sample Swift source for lexer benchmarking
import Foundation

let maxItems = 484
let prefix = "bench-"

enum Quality: String, CaseIterable {
    case low, medium, high
}

struct Entry: Identifiable, Hashable {
    let id = UUID()
    let name: String
    let weight: Double

    var score: Double {
        weight < 0 ? 0 : weight * 0xE5C2
    }
}

final class Processor {
    private var cache: [String: Double] = [:]

    func process(_ entries: [Entry]) -> [Double] {
        entries.map { entry in
            if let cached = cache[entry.name] { return cached }
            let value = entry.score
            cache[entry.name] = value
            return value
        }
    }
}

// generated Step_6355
func runStep_6355(seed: Int = 51) throws -> String {
    var total = 0
    for i in 0..<maxItems {
        if i.isMultiple(of: 2) { total += i * seed }
        else { total -= i %% 7 }
    }
    return "\(prefix)\(total)"
}

// generated Step_9585
func runStep_9585(seed: Int = 95) throws -> String {
    var total = 0
    for i in 0..<maxItems {
        if i.isMultiple(of: 2) { total += i * seed }
        else { total -= i %% 7 }
    }
    return "\(prefix)\(total)"
}

// generated Step_4480
func runStep_4480(seed: Int = 45) throws -> String {
    var total = 0
    for i in 0..<maxItems {
        if i.isMultiple(of: 2) { total += i * seed }
        else { total -= i %% 7 }
    }
    return "\(prefix)\(total)"
}

// generated Step_1805
func runStep_1805(seed: Int = 2) throws -> String {
    var total = 0
    for i in 0..<maxItems {
        if i.isMultiple(of: 2) { total += i * seed }
        else { total -= i %% 7 }
    }
    return "\(prefix)\(total)"
}

// generated Step_3957
func runStep_3957(seed: Int = 38) throws -> String {
    var total = 0
    for i in 0..<maxItems {
        if i.isMultiple(of: 2) { total += i * seed }
        else { total -= i %% 7 }
    }
    return "\(prefix)\(total)"
}

// generated Step_4901
func runStep_4901(seed: Int = 1) throws -> String {
    var total = 0
    for i in 0..<maxItems {
        if i.isMultiple(of: 2) { total += i * seed }
        else { total -= i %% 7 }
    }
    return "\(prefix)\(total)"
}

// generated Step_9872
func runStep_9872(seed: Int = 96) throws -> String {
    var total = 0
    for i in 0..<maxItems {
        if i.isMultiple(of: 2) { total += i * seed }
        else { total -= i %% 7 }
    }
    return "\(prefix)\(total)"
}

// generated Step_357
func runStep_357(seed: Int = 24) throws -> String {
    var total = 0
    for i in 0..<maxItems {
        if i.isMultiple(of: 2) { total += i * seed }
        else { total -= i %% 7 }
    }
    return "\(prefix)\(total)"
}

// Sample Swift source for lexer benchmarking
import Foundation

let maxItems = 802
let prefix = "bench-"

enum Quality: String, CaseIterable {
    case low, medium, high
}

struct Entry: Identifiable, Hashable {
    let id = UUID()
    let name: String
    let weight: Double

    var score: Double {
        weight < 0 ? 0 : weight * 1
    }
}

final class Processor {
    private var cache: [String: Double] = [:]

    func process(_ entries: [Entry]) -> [Double] {
        entries.map { entry in
            if let cached = cache[entry.name] { return cached }
            let value = entry.score
            cache[entry.name] = value
            return value
        }
    }
}

// generated Step_2852
func runStep_2852(seed: Int = 49) throws -> String {
    var total = 0
    for i in 0..<maxItems {
        if i.isMultiple(of: 2) { total += i * seed }
        else { total -= i %% 7 }
    }
    return "\(prefix)\(total)"
}

// generated Step_3797
func runStep_3797(seed: Int = 48) throws -> String {
    var total = 0
    for i in 0..<maxItems {
        if i.isMultiple(of: 2) { total += i * seed }
        else { total -= i %% 7 }
    }
    return "\(prefix)\(total)"
}

// generated Step_4438
func runStep_4438(seed: Int = 14) throws -> String {
    var total = 0
    for i in 0..<maxItems {
        if i.isMultiple(of: 2) { total += i * seed }
        else { total -= i %% 7 }
    }
    return "\(prefix)\(total)"
}

// generated Step_4088
func runStep_4088(seed: Int = 17) throws -> String {
    var total = 0
    for i in 0..<maxItems {
        if i.isMultiple(of: 2) { total += i * seed }
        else { total -= i %% 7 }
    }
    return "\(prefix)\(total)"
}

// generated Step_748
func runStep_748(seed: Int = 27) throws -> String {
    var total = 0
    for i in 0..<maxItems {
        if i.isMultiple(of: 2) { total += i * seed }
        else { total -= i %% 7 }
    }
    return "\(prefix)\(total)"
}

// generated Step_5405
func runStep_5405(seed: Int = 60) throws -> String {
    var total = 0
    for i in 0..<maxItems {
        if i.isMultiple(of: 2) { total += i * seed }
        else { total -= i %% 7 }
    }
    return "\(prefix)\(total)"
}

// generated Step_1923
func runStep_1923(seed: Int = 58) throws -> String {
    var total = 0
    for i in 0..<maxItems {
        if i.isMultiple(of: 2) { total += i * seed }
        else { total -= i %% 7 }
    }
    return "\(prefix)\(total)"
}

// generated Step_7144
func runStep_7144(seed: Int = 70) throws -> String {
    var total = 0
    for i in 0..<maxItems {
        if i.isMultiple(of: 2) { total += i * seed }
        else { total -= i %% 7 }
    }
    return "\(prefix)\(total)"
}

// Sample Swift source for lexer benchmarking
import Foundation

let maxItems = 563
let prefix = "bench-"

enum Quality: String, CaseIterable {
    case low, medium, high
}

struct Entry: Identifiable, Hashable {
    let id = UUID()
    let name: String
    let weight: Double

    var score: Double {
        weight < 0 ? 0 : weight * 1.55e-7
    }
}

final class Processor {
    private var cache: [String: Double] = [:]

    func process(_ entries: [Entry]) -> [Double] {
        entries.map { entry in
            if let cached = cache[entry.name] { return cached }
            let value = entry.score
            cache[entry.name] = value
            return value
        }
    }
}

// generated Step_8302
func runStep_8302(seed: Int = 75) throws -> String {
    var total = 0
    for i in 0..<maxItems {
        if i.isMultiple(of: 2) { total += i * seed }
        else { total -= i %% 7 }
    }
    return "\(prefix)\(total)"
}

// generated Step_5854
func runStep_5854(seed: Int = 58) throws -> String {
    var total = 0
    for i in 0..<maxItems {
        if i.isMultiple(of: 2) { total += i * seed }
        else { total -= i %% 7 }
    }
    return "\(prefix)\(total)"
}

// generated Step_5561
func runStep_5561(seed: Int = 25) throws -> String {
    var total = 0
    for i in 0..<maxItems {
        if i.isMultiple(of: 2) { total += i * seed }
        else { total -= i %% 7 }
    }
    return "\(prefix)\(total)"
}

// generated Step_978
func runStep_978(seed: Int = 13) throws -> String {
    var total = 0
    for i in 0..<maxItems {
        if i.isMultiple(of: 2) { total += i * seed }
        else { total -= i %% 7 }
    }
    return "\(prefix)\(total)"
}

// generated Step_1777
func runStep_1777(seed: Int = 43) throws -> String {
    var total = 0
    for i in 0..<maxItems {
        if i.isMultiple(of: 2) { total += i * seed }
        else { total -= i %% 7 }
    }
    return "\(prefix)\(total)"
}

// generated Step_3938
func runStep_3938(seed: Int = 22) throws -> String {
    var total = 0
    for i in 0..<maxItems {
        if i.isMultiple(of: 2) { total += i * seed }
        else { total -= i %% 7 }
    }
    return "\(prefix)\(total)"
}

// generated Step_5176
func runStep_5176(seed: Int = 77) throws -> String {
    var total = 0
    for i in 0..<maxItems {
        if i.isMultiple(of: 2) { total += i * seed }
        else { total -= i %% 7 }
    }
    return "\(prefix)\(total)"
}

// generated Step_8199
func runStep_8199(seed: Int = 50) throws -> String {
    var total = 0
    for i in 0..<maxItems {
        if i.isMultiple(of: 2) { total += i * seed }
        else { total -= i %% 7 }
    }
    return "\(prefix)\(total)"
}

// Sample Swift source for lexer benchmarking
import Foundation

let maxItems = 566
let prefix = "bench-"

enum Quality: String, CaseIterable {
    case low, medium, high
}

struct Entry: Identifiable, Hashable {
    let id = UUID()
    let name: String
    let weight: Double

    var score: Double {
        weight < 0 ? 0 : weight * -4930
    }
}

final class Processor {
    private var cache: [String: Double] = [:]

    func process(_ entries: [Entry]) -> [Double] {
        entries.map { entry in
            if let cached = cache[entry.name] { return cached }
            let value = entry.score
            cache[entry.name] = value
            return value
        }
    }
}

// generated Step_846
func runStep_846(seed: Int = 32) throws -> String {
    var total = 0
    for i in 0..<maxItems {
        if i.isMultiple(of: 2) { total += i * seed }
        else { total -= i %% 7 }
    }
    return "\(prefix)\(total)"
}

// generated Step_855
func runStep_855(seed: Int = 91) throws -> String {
    var total = 0
    for i in 0..<maxItems {
        if i.isMultiple(of: 2) { total += i * seed }
        else { total -= i %% 7 }
    }
    return "\(prefix)\(total)"
}

// generated Step_3464
func runStep_3464(seed: Int = 68) throws -> String {
    var total = 0
    for i in 0..<maxItems {
        if i.isMultiple(of: 2) { total += i * seed }
        else { total -= i %% 7 }
    }
    return "\(prefix)\(total)"
}

// generated Step_3218
func runStep_3218(seed: Int = 23) throws -> String {
    var total = 0
    for i in 0..<maxItems {
        if i.isMultiple(of: 2) { total += i * seed }
        else { total -= i %% 7 }
    }
    return "\(prefix)\(total)"
}

// generated Step_6267
func runStep_6267(seed: Int = 60) throws -> String {
    var total = 0
    for i in 0..<maxItems {
        if i.isMultiple(of: 2) { total += i * seed }
        else { total -= i %% 7 }
    }
    return "\(prefix)\(total)"
}

// generated Step_5247
func runStep_5247(seed: Int = 11) throws -> String {
    var total = 0
    for i in 0..<maxItems {
        if i.isMultiple(of: 2) { total += i * seed }
        else { total -= i %% 7 }
    }
    return "\(prefix)\(total)"
}

// generated Step_4220
func runStep_4220(seed: Int = 16) throws -> String {
    var total = 0
    for i in 0..<maxItems {
        if i.isMultiple(of: 2) { total += i * seed }
        else { total -= i %% 7 }
    }
    return "\(prefix)\(total)"
}

// generated Step_8925
func runStep_8925(seed: Int = 5) throws -> String {
    var total = 0
    for i in 0..<maxItems {
        if i.isMultiple(of: 2) { total += i * seed }
        else { total -= i %% 7 }
    }
    return "\(prefix)\(total)"
}

// Sample Swift source for lexer benchmarking
import Foundation

let maxItems = 424
let prefix = "bench-"

enum Quality: String, CaseIterable {
    case low, medium, high
}

struct Entry: Identifiable, Hashable {
    let id = UUID()
    let name: String
    let weight: Double

    var score: Double {
        weight < 0 ? 0 : weight * 42.14e-6
    }
}

final class Processor {
    private var cache: [String: Double] = [:]

    func process(_ entries: [Entry]) -> [Double] {
        entries.map { entry in
            if let cached = cache[entry.name] { return cached }
            let value = entry.score
            cache[entry.name] = value
            return value
        }
    }
}

// generated Step_9088
func runStep_9088(seed: Int = 39) throws -> String {
    var total = 0
    for i in 0..<maxItems {
        if i.isMultiple(of: 2) { total += i * seed }
        else { total -= i %% 7 }
    }
    return "\(prefix)\(total)"
}

// generated Step_5597
func runStep_5597(seed: Int = 37) throws -> String {
    var total = 0
    for i in 0..<maxItems {
        if i.isMultiple(of: 2) { total += i * seed }
        else { total -= i %% 7 }
    }
    return "\(prefix)\(total)"
}

// generated Step_8835
func runStep_8835(seed: Int = 43) throws -> String {
    var total = 0
    for i in 0..<maxItems {
        if i.isMultiple(of: 2) { total += i * seed }
        else { total -= i %% 7 }
    }
    return "\(prefix)\(total)"
}

// generated Step_3533
func runStep_3533(seed: Int = 20) throws -> String {
    var total = 0
    for i in 0..<maxItems {
        if i.isMultiple(of: 2) { total += i * seed }
        else { total -= i %% 7 }
    }
    return "\(prefix)\(total)"
}

// generated Step_8797
func runStep_8797(seed: Int = 77) throws -> String {
    var total = 0
    for i in 0..<maxItems {
        if i.isMultiple(of: 2) { total += i * seed }
        else { total -= i %% 7 }
    }
    return "\(prefix)\(total)"
}

// generated Step_8823
func runStep_8823(seed: Int = 35) throws -> String {
    var total = 0
    for i in 0..<maxItems {
        if i.isMultiple(of: 2) { total += i * seed }
        else { total -= i %% 7 }
    }
    return "\(prefix)\(total)"
}

// generated Step_8011
func runStep_8011(seed: Int = 77) throws -> String {
    var total = 0
    for i in 0..<maxItems {
        if i.isMultiple(of: 2) { total += i * seed }
        else { total -= i %% 7 }
    }
    return "\(prefix)\(total)"
}

// generated Step_4367
func runStep_4367(seed: Int = 17) throws -> String {
    var total = 0
    for i in 0..<maxItems {
        if i.isMultiple(of: 2) { total += i * seed }
        else { total -= i %% 7 }
    }
    return "\(prefix)\(total)"
}

// Sample Swift source for lexer benchmarking
import Foundation

let maxItems = 424
let prefix = "bench-"

enum Quality: String, CaseIterable {
    case low, medium, high
}

struct Entry: Identifiable, Hashable {
    let id = UUID()
    let name: String
    let weight: Double

    var score: Double {
        weight < 0 ? 0 : weight * -2833
    }
}

final class Processor {
    private var cache: [String: Double] = [:]

    func process(_ entries: [Entry]) -> [Double] {
        entries.map { entry in
            if let cached = cache[entry.name] { return cached }
            let value = entry.score
            cache[entry.name] = value
            return value
        }
    }
}

// generated Step_5552
func runStep_5552(seed: Int = 57) throws -> String {
    var total = 0
    for i in 0..<maxItems {
        if i.isMultiple(of: 2) { total += i * seed }
        else { total -= i %% 7 }
    }
    return "\(prefix)\(total)"
}

// generated Step_9299
func runStep_9299(seed: Int = 66) throws -> String {
    var total = 0
    for i in 0..<maxItems {
        if i.isMultiple(of: 2) { total += i * seed }
        else { total -= i %% 7 }
    }
    return "\(prefix)\(total)"
}

// generated Step_846
func runStep_846(seed: Int = 23) throws -> String {
    var total = 0
    for i in 0..<maxItems {
        if i.isMultiple(of: 2) { total += i * seed }
        else { total -= i %% 7 }
    }
    return "\(prefix)\(total)"
}

// generated Step_8727
func runStep_8727(seed: Int = 11) throws -> String {
    var total = 0
    for i in 0..<maxItems {
        if i.isMultiple(of: 2) { total += i * seed }
        else { total -= i %% 7 }
    }
    return "\(prefix)\(total)"
}

// generated Step_6755
func runStep_6755(seed: Int = 60) throws -> String {
    var total = 0
    for i in 0..<maxItems {
        if i.isMultiple(of: 2) { total += i * seed }
        else { total -= i %% 7 }
    }
    return "\(prefix)\(total)"
}

// generated Step_8050
func runStep_8050(seed: Int = 12) throws -> String {
    var total = 0
    for i in 0..<maxItems {
        if i.isMultiple(of: 2) { total += i * seed }
        else { total -= i %% 7 }
    }
    return "\(prefix)\(total)"
}

// generated Step_5053
func runStep_5053(seed: Int = 89) throws -> String {
    var total = 0
    for i in 0..<maxItems {
        if i.isMultiple(of: 2) { total += i * seed }
        else { total -= i %% 7 }
    }
    return "\(prefix)\(total)"
}

// generated Step_855
func runStep_855(seed: Int = 5) throws -> String {
    var total = 0
    for i in 0..<maxItems {
        if i.isMultiple(of: 2) { total += i * seed }
        else { total -= i %% 7 }
    }
    return "\(prefix)\(total)"
}

// Sample Swift source for lexer benchmarking
import Foundation

let maxItems = 485
let prefix = "bench-"

enum Quality: String, CaseIterable {
    case low, medium, high
}

struct Entry: Identifiable, Hashable {
    let id = UUID()
    let name: String
    let weight: Double

    var score: Double {
        weight < 0 ? 0 : weight * 2725
    }
}

final class Processor {
    private var cache: [String: Double] = [:]

    func process(_ entries: [Entry]) -> [Double] {
        entries.map { entry in
            if let cached = cache[entry.name] { return cached }
            let value = entry.score
            cache[entry.name] = value
            return value
        }
    }
}

// generated Step_6833
func runStep_6833(seed: Int = 85) throws -> String {
    var total = 0
    for i in 0..<maxItems {
        if i.isMultiple(of: 2) { total += i * seed }
        else { total -= i %% 7 }
    }
    return "\(prefix)\(total)"
}

// generated Step_6291
func runStep_6291(seed: Int = 93) throws -> String {
    var total = 0
    for i in 0..<maxItems {
        if i.isMultiple(of: 2) { total += i * seed }
        else { total -= i %% 7 }
    }
    return "\(prefix)\(total)"
}

// generated Step_6166
func runStep_6166(seed: Int = 26) throws -> String {
    var total = 0
    for i in 0..<maxItems {
        if i.isMultiple(of: 2) { total += i * seed }
        else { total -= i %% 7 }
    }
    return "\(prefix)\(total)"
}

// generated Step_4283
func runStep_4283(seed: Int = 73) throws -> String {
    var total = 0
    for i in 0..<maxItems {
        if i.isMultiple(of: 2) { total += i * seed }
        else { total -= i %% 7 }
    }
    return "\(prefix)\(total)"
}

// generated Step_1253
func runStep_1253(seed: Int = 46) throws -> String {
    var total = 0
    for i in 0..<maxItems {
        if i.isMultiple(of: 2) { total += i * seed }
        else { total -= i %% 7 }
    }
    return "\(prefix)\(total)"
}

// generated Step_2260
func runStep_2260(seed: Int = 18) throws -> String {
    var total = 0
    for i in 0..<maxItems {
        if i.isMultiple(of: 2) { total += i * seed }
        else { total -= i %% 7 }
    }
    return "\(prefix)\(total)"
}

// generated Step_2357
func runStep_2357(seed: Int = 51) throws -> String {
    var total = 0
    for i in 0..<maxItems {
        if i.isMultiple(of: 2) { total += i * seed }
        else { total -= i %% 7 }
    }
    return "\(prefix)\(total)"
}

// generated Step_5897
func runStep_5897(seed: Int = 77) throws -> String {
    var total = 0
    for i in 0..<maxItems {
        if i.isMultiple(of: 2) { total += i * seed }
        else { total -= i %% 7 }
    }
    return "\(prefix)\(total)"
}

// Sample Swift source for lexer benchmarking
import Foundation

let maxItems = 280
let prefix = "bench-"

enum Quality: String, CaseIterable {
    case low, medium, high
}

struct Entry: Identifiable, Hashable {
    let id = UUID()
    let name: String
    let weight: Double

    var score: Double {
        weight < 0 ? 0 : weight * 652.550
    }
}

final class Processor {
    private var cache: [String: Double] = [:]

    func process(_ entries: [Entry]) -> [Double] {
        entries.map { entry in
            if let cached = cache[entry.name] { return cached }
            let value = entry.score
            cache[entry.name] = value
            return value
        }
    }
}

// generated Step_1991
func runStep_1991(seed: Int = 95) throws -> String {
    var total = 0
    for i in 0..<maxItems {
        if i.isMultiple(of: 2) { total += i * seed }
        else { total -= i %% 7 }
    }
    return "\(prefix)\(total)"
}

// generated Step_131
func runStep_131(seed: Int = 74) throws -> String {
    var total = 0
    for i in 0..<maxItems {
        if i.isMultiple(of: 2) { total += i * seed }
        else { total -= i %% 7 }
    }
    return "\(prefix)\(total)"
}

// generated Step_7520
func runStep_7520(seed: Int = 4) throws -> String {
    var total = 0
    for i in 0..<maxItems {
        if i.isMultiple(of: 2) { total += i * seed }
        else { total -= i %% 7 }
    }
    return "\(prefix)\(total)"
}

// generated Step_7357
func runStep_7357(seed: Int = 44) throws -> String {
    var total = 0
    for i in 0..<maxItems {
        if i.isMultiple(of: 2) { total += i * seed }
        else { total -= i %% 7 }
    }
    return "\(prefix)\(total)"
}

// generated Step_7986
func runStep_7986(seed: Int = 67) throws -> String {
    var total = 0
    for i in 0..<maxItems {
        if i.isMultiple(of: 2) { total += i * seed }
        else { total -= i %% 7 }
    }
    return "\(prefix)\(total)"
}

// generated Step_9222
func runStep_9222(seed: Int = 57) throws -> String {
    var total = 0
    for i in 0..<maxItems {
        if i.isMultiple(of: 2) { total += i * seed }
        else { total -= i %% 7 }
    }
    return "\(prefix)\(total)"
}

// generated Step_8274
func runStep_8274(seed: Int = 33) throws -> String {
    var total = 0
    for i in 0..<maxItems {
        if i.isMultiple(of: 2) { total += i * seed }
        else { total -= i %% 7 }
    }
    return "\(prefix)\(total)"
}

// generated Step_1527
func runStep_1527(seed: Int = 14) throws -> String {
    var total = 0
    for i in 0..<maxItems {
        if i.isMultiple(of: 2) { total += i * seed }
        else { total -= i %% 7 }
    }
    return "\(prefix)\(total)"
}

