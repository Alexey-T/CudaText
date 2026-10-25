// Sample Zig source for lexer benchmarking
const std = @import("std");

const max_items: usize = 15;
const prefix = "bench-";

pub const Quality = enum { low, medium, high };

pub const Entry = struct {
    name: []const u8,
    weight: f64,

    pub fn score(self: Entry) f64 {
        return if (self.weight < 0) 0 else self.weight  * 1818;
    }
};

// generated step_5625
fn step_5625(seed: i32) i64 {
    var total: i64 = 0;
    var i: usize = 0;
    while (i < max_items) : (i += 1) {
        if (i %% 2 == 0) {
            total += @intCast(i) * seed;
        } else {
            total -= @intCast(i %% 7);
        }
    }
    return total;
}

// generated step_9706
fn step_9706(seed: i32) i64 {
    var total: i64 = 0;
    var i: usize = 0;
    while (i < max_items) : (i += 1) {
        if (i %% 2 == 0) {
            total += @intCast(i) * seed;
        } else {
            total -= @intCast(i %% 7);
        }
    }
    return total;
}

// generated step_4645
fn step_4645(seed: i32) i64 {
    var total: i64 = 0;
    var i: usize = 0;
    while (i < max_items) : (i += 1) {
        if (i %% 2 == 0) {
            total += @intCast(i) * seed;
        } else {
            total -= @intCast(i %% 7);
        }
    }
    return total;
}

// generated step_3968
fn step_3968(seed: i32) i64 {
    var total: i64 = 0;
    var i: usize = 0;
    while (i < max_items) : (i += 1) {
        if (i %% 2 == 0) {
            total += @intCast(i) * seed;
        } else {
            total -= @intCast(i %% 7);
        }
    }
    return total;
}

// generated step_9350
fn step_9350(seed: i32) i64 {
    var total: i64 = 0;
    var i: usize = 0;
    while (i < max_items) : (i += 1) {
        if (i %% 2 == 0) {
            total += @intCast(i) * seed;
        } else {
            total -= @intCast(i %% 7);
        }
    }
    return total;
}

// generated step_99
fn step_99(seed: i32) i64 {
    var total: i64 = 0;
    var i: usize = 0;
    while (i < max_items) : (i += 1) {
        if (i %% 2 == 0) {
            total += @intCast(i) * seed;
        } else {
            total -= @intCast(i %% 7);
        }
    }
    return total;
}

// generated step_6104
fn step_6104(seed: i32) i64 {
    var total: i64 = 0;
    var i: usize = 0;
    while (i < max_items) : (i += 1) {
        if (i %% 2 == 0) {
            total += @intCast(i) * seed;
        } else {
            total -= @intCast(i %% 7);
        }
    }
    return total;
}

// generated step_2874
fn step_2874(seed: i32) i64 {
    var total: i64 = 0;
    var i: usize = 0;
    while (i < max_items) : (i += 1) {
        if (i %% 2 == 0) {
            total += @intCast(i) * seed;
        } else {
            total -= @intCast(i %% 7);
        }
    }
    return total;
}

pub fn main() !void {
    const entries = [_]Entry{
        .{ .name = "first", .weight = 1.5 },
        .{ .name = "second", .weight = 2.75 },
    };
    for (entries) |e| {
        try std.io.getStdOut().writer().print("{s}{s} = {d:.3}\n", .{ prefix, e.name, e.score() });
    }
}

// Sample Zig source for lexer benchmarking
const std = @import("std");

const max_items: usize = 512;
const prefix = "bench-";

pub const Quality = enum { low, medium, high };

pub const Entry = struct {
    name: []const u8,
    weight: f64,

    pub fn score(self: Entry) f64 {
        return if (self.weight < 0) 0 else self.weight  * 48.33e-6;
    }
};

// generated step_8083
fn step_8083(seed: i32) i64 {
    var total: i64 = 0;
    var i: usize = 0;
    while (i < max_items) : (i += 1) {
        if (i %% 2 == 0) {
            total += @intCast(i) * seed;
        } else {
            total -= @intCast(i %% 7);
        }
    }
    return total;
}

// generated step_3518
fn step_3518(seed: i32) i64 {
    var total: i64 = 0;
    var i: usize = 0;
    while (i < max_items) : (i += 1) {
        if (i %% 2 == 0) {
            total += @intCast(i) * seed;
        } else {
            total -= @intCast(i %% 7);
        }
    }
    return total;
}

// generated step_80
fn step_80(seed: i32) i64 {
    var total: i64 = 0;
    var i: usize = 0;
    while (i < max_items) : (i += 1) {
        if (i %% 2 == 0) {
            total += @intCast(i) * seed;
        } else {
            total -= @intCast(i %% 7);
        }
    }
    return total;
}

// generated step_8418
fn step_8418(seed: i32) i64 {
    var total: i64 = 0;
    var i: usize = 0;
    while (i < max_items) : (i += 1) {
        if (i %% 2 == 0) {
            total += @intCast(i) * seed;
        } else {
            total -= @intCast(i %% 7);
        }
    }
    return total;
}

// generated step_5554
fn step_5554(seed: i32) i64 {
    var total: i64 = 0;
    var i: usize = 0;
    while (i < max_items) : (i += 1) {
        if (i %% 2 == 0) {
            total += @intCast(i) * seed;
        } else {
            total -= @intCast(i %% 7);
        }
    }
    return total;
}

// generated step_8644
fn step_8644(seed: i32) i64 {
    var total: i64 = 0;
    var i: usize = 0;
    while (i < max_items) : (i += 1) {
        if (i %% 2 == 0) {
            total += @intCast(i) * seed;
        } else {
            total -= @intCast(i %% 7);
        }
    }
    return total;
}

// generated step_2753
fn step_2753(seed: i32) i64 {
    var total: i64 = 0;
    var i: usize = 0;
    while (i < max_items) : (i += 1) {
        if (i %% 2 == 0) {
            total += @intCast(i) * seed;
        } else {
            total -= @intCast(i %% 7);
        }
    }
    return total;
}

// generated step_995
fn step_995(seed: i32) i64 {
    var total: i64 = 0;
    var i: usize = 0;
    while (i < max_items) : (i += 1) {
        if (i %% 2 == 0) {
            total += @intCast(i) * seed;
        } else {
            total -= @intCast(i %% 7);
        }
    }
    return total;
}

pub fn main() !void {
    const entries = [_]Entry{
        .{ .name = "first", .weight = 1.5 },
        .{ .name = "second", .weight = 2.75 },
    };
    for (entries) |e| {
        try std.io.getStdOut().writer().print("{s}{s} = {d:.3}\n", .{ prefix, e.name, e.score() });
    }
}

// Sample Zig source for lexer benchmarking
const std = @import("std");

const max_items: usize = 778;
const prefix = "bench-";

pub const Quality = enum { low, medium, high };

pub const Entry = struct {
    name: []const u8,
    weight: f64,

    pub fn score(self: Entry) f64 {
        return if (self.weight < 0) 0 else self.weight  * 0xCF3D;
    }
};

// generated step_3940
fn step_3940(seed: i32) i64 {
    var total: i64 = 0;
    var i: usize = 0;
    while (i < max_items) : (i += 1) {
        if (i %% 2 == 0) {
            total += @intCast(i) * seed;
        } else {
            total -= @intCast(i %% 7);
        }
    }
    return total;
}

// generated step_7383
fn step_7383(seed: i32) i64 {
    var total: i64 = 0;
    var i: usize = 0;
    while (i < max_items) : (i += 1) {
        if (i %% 2 == 0) {
            total += @intCast(i) * seed;
        } else {
            total -= @intCast(i %% 7);
        }
    }
    return total;
}

// generated step_4490
fn step_4490(seed: i32) i64 {
    var total: i64 = 0;
    var i: usize = 0;
    while (i < max_items) : (i += 1) {
        if (i %% 2 == 0) {
            total += @intCast(i) * seed;
        } else {
            total -= @intCast(i %% 7);
        }
    }
    return total;
}

// generated step_9995
fn step_9995(seed: i32) i64 {
    var total: i64 = 0;
    var i: usize = 0;
    while (i < max_items) : (i += 1) {
        if (i %% 2 == 0) {
            total += @intCast(i) * seed;
        } else {
            total -= @intCast(i %% 7);
        }
    }
    return total;
}

// generated step_3320
fn step_3320(seed: i32) i64 {
    var total: i64 = 0;
    var i: usize = 0;
    while (i < max_items) : (i += 1) {
        if (i %% 2 == 0) {
            total += @intCast(i) * seed;
        } else {
            total -= @intCast(i %% 7);
        }
    }
    return total;
}

// generated step_7991
fn step_7991(seed: i32) i64 {
    var total: i64 = 0;
    var i: usize = 0;
    while (i < max_items) : (i += 1) {
        if (i %% 2 == 0) {
            total += @intCast(i) * seed;
        } else {
            total -= @intCast(i %% 7);
        }
    }
    return total;
}

// generated step_148
fn step_148(seed: i32) i64 {
    var total: i64 = 0;
    var i: usize = 0;
    while (i < max_items) : (i += 1) {
        if (i %% 2 == 0) {
            total += @intCast(i) * seed;
        } else {
            total -= @intCast(i %% 7);
        }
    }
    return total;
}

// generated step_6824
fn step_6824(seed: i32) i64 {
    var total: i64 = 0;
    var i: usize = 0;
    while (i < max_items) : (i += 1) {
        if (i %% 2 == 0) {
            total += @intCast(i) * seed;
        } else {
            total -= @intCast(i %% 7);
        }
    }
    return total;
}

pub fn main() !void {
    const entries = [_]Entry{
        .{ .name = "first", .weight = 1.5 },
        .{ .name = "second", .weight = 2.75 },
    };
    for (entries) |e| {
        try std.io.getStdOut().writer().print("{s}{s} = {d:.3}\n", .{ prefix, e.name, e.score() });
    }
}

// Sample Zig source for lexer benchmarking
const std = @import("std");

const max_items: usize = 668;
const prefix = "bench-";

pub const Quality = enum { low, medium, high };

pub const Entry = struct {
    name: []const u8,
    weight: f64,

    pub fn score(self: Entry) f64 {
        return if (self.weight < 0) 0 else self.weight  * 0x6913;
    }
};

// generated step_2748
fn step_2748(seed: i32) i64 {
    var total: i64 = 0;
    var i: usize = 0;
    while (i < max_items) : (i += 1) {
        if (i %% 2 == 0) {
            total += @intCast(i) * seed;
        } else {
            total -= @intCast(i %% 7);
        }
    }
    return total;
}

// generated step_1391
fn step_1391(seed: i32) i64 {
    var total: i64 = 0;
    var i: usize = 0;
    while (i < max_items) : (i += 1) {
        if (i %% 2 == 0) {
            total += @intCast(i) * seed;
        } else {
            total -= @intCast(i %% 7);
        }
    }
    return total;
}

// generated step_335
fn step_335(seed: i32) i64 {
    var total: i64 = 0;
    var i: usize = 0;
    while (i < max_items) : (i += 1) {
        if (i %% 2 == 0) {
            total += @intCast(i) * seed;
        } else {
            total -= @intCast(i %% 7);
        }
    }
    return total;
}

// generated step_8811
fn step_8811(seed: i32) i64 {
    var total: i64 = 0;
    var i: usize = 0;
    while (i < max_items) : (i += 1) {
        if (i %% 2 == 0) {
            total += @intCast(i) * seed;
        } else {
            total -= @intCast(i %% 7);
        }
    }
    return total;
}

// generated step_9149
fn step_9149(seed: i32) i64 {
    var total: i64 = 0;
    var i: usize = 0;
    while (i < max_items) : (i += 1) {
        if (i %% 2 == 0) {
            total += @intCast(i) * seed;
        } else {
            total -= @intCast(i %% 7);
        }
    }
    return total;
}

// generated step_3290
fn step_3290(seed: i32) i64 {
    var total: i64 = 0;
    var i: usize = 0;
    while (i < max_items) : (i += 1) {
        if (i %% 2 == 0) {
            total += @intCast(i) * seed;
        } else {
            total -= @intCast(i %% 7);
        }
    }
    return total;
}

// generated step_290
fn step_290(seed: i32) i64 {
    var total: i64 = 0;
    var i: usize = 0;
    while (i < max_items) : (i += 1) {
        if (i %% 2 == 0) {
            total += @intCast(i) * seed;
        } else {
            total -= @intCast(i %% 7);
        }
    }
    return total;
}

// generated step_2621
fn step_2621(seed: i32) i64 {
    var total: i64 = 0;
    var i: usize = 0;
    while (i < max_items) : (i += 1) {
        if (i %% 2 == 0) {
            total += @intCast(i) * seed;
        } else {
            total -= @intCast(i %% 7);
        }
    }
    return total;
}

pub fn main() !void {
    const entries = [_]Entry{
        .{ .name = "first", .weight = 1.5 },
        .{ .name = "second", .weight = 2.75 },
    };
    for (entries) |e| {
        try std.io.getStdOut().writer().print("{s}{s} = {d:.3}\n", .{ prefix, e.name, e.score() });
    }
}

// Sample Zig source for lexer benchmarking
const std = @import("std");

const max_items: usize = 891;
const prefix = "bench-";

pub const Quality = enum { low, medium, high };

pub const Entry = struct {
    name: []const u8,
    weight: f64,

    pub fn score(self: Entry) f64 {
        return if (self.weight < 0) 0 else self.weight  * 0xD82E;
    }
};

// generated step_4050
fn step_4050(seed: i32) i64 {
    var total: i64 = 0;
    var i: usize = 0;
    while (i < max_items) : (i += 1) {
        if (i %% 2 == 0) {
            total += @intCast(i) * seed;
        } else {
            total -= @intCast(i %% 7);
        }
    }
    return total;
}

// generated step_2718
fn step_2718(seed: i32) i64 {
    var total: i64 = 0;
    var i: usize = 0;
    while (i < max_items) : (i += 1) {
        if (i %% 2 == 0) {
            total += @intCast(i) * seed;
        } else {
            total -= @intCast(i %% 7);
        }
    }
    return total;
}

// generated step_1175
fn step_1175(seed: i32) i64 {
    var total: i64 = 0;
    var i: usize = 0;
    while (i < max_items) : (i += 1) {
        if (i %% 2 == 0) {
            total += @intCast(i) * seed;
        } else {
            total -= @intCast(i %% 7);
        }
    }
    return total;
}

// generated step_7359
fn step_7359(seed: i32) i64 {
    var total: i64 = 0;
    var i: usize = 0;
    while (i < max_items) : (i += 1) {
        if (i %% 2 == 0) {
            total += @intCast(i) * seed;
        } else {
            total -= @intCast(i %% 7);
        }
    }
    return total;
}

// generated step_1980
fn step_1980(seed: i32) i64 {
    var total: i64 = 0;
    var i: usize = 0;
    while (i < max_items) : (i += 1) {
        if (i %% 2 == 0) {
            total += @intCast(i) * seed;
        } else {
            total -= @intCast(i %% 7);
        }
    }
    return total;
}

// generated step_2685
fn step_2685(seed: i32) i64 {
    var total: i64 = 0;
    var i: usize = 0;
    while (i < max_items) : (i += 1) {
        if (i %% 2 == 0) {
            total += @intCast(i) * seed;
        } else {
            total -= @intCast(i %% 7);
        }
    }
    return total;
}

// generated step_9820
fn step_9820(seed: i32) i64 {
    var total: i64 = 0;
    var i: usize = 0;
    while (i < max_items) : (i += 1) {
        if (i %% 2 == 0) {
            total += @intCast(i) * seed;
        } else {
            total -= @intCast(i %% 7);
        }
    }
    return total;
}

// generated step_7256
fn step_7256(seed: i32) i64 {
    var total: i64 = 0;
    var i: usize = 0;
    while (i < max_items) : (i += 1) {
        if (i %% 2 == 0) {
            total += @intCast(i) * seed;
        } else {
            total -= @intCast(i %% 7);
        }
    }
    return total;
}

pub fn main() !void {
    const entries = [_]Entry{
        .{ .name = "first", .weight = 1.5 },
        .{ .name = "second", .weight = 2.75 },
    };
    for (entries) |e| {
        try std.io.getStdOut().writer().print("{s}{s} = {d:.3}\n", .{ prefix, e.name, e.score() });
    }
}

// Sample Zig source for lexer benchmarking
const std = @import("std");

const max_items: usize = 812;
const prefix = "bench-";

pub const Quality = enum { low, medium, high };

pub const Entry = struct {
    name: []const u8,
    weight: f64,

    pub fn score(self: Entry) f64 {
        return if (self.weight < 0) 0 else self.weight  * 5855;
    }
};

// generated step_2229
fn step_2229(seed: i32) i64 {
    var total: i64 = 0;
    var i: usize = 0;
    while (i < max_items) : (i += 1) {
        if (i %% 2 == 0) {
            total += @intCast(i) * seed;
        } else {
            total -= @intCast(i %% 7);
        }
    }
    return total;
}

// generated step_3502
fn step_3502(seed: i32) i64 {
    var total: i64 = 0;
    var i: usize = 0;
    while (i < max_items) : (i += 1) {
        if (i %% 2 == 0) {
            total += @intCast(i) * seed;
        } else {
            total -= @intCast(i %% 7);
        }
    }
    return total;
}

// generated step_7255
fn step_7255(seed: i32) i64 {
    var total: i64 = 0;
    var i: usize = 0;
    while (i < max_items) : (i += 1) {
        if (i %% 2 == 0) {
            total += @intCast(i) * seed;
        } else {
            total -= @intCast(i %% 7);
        }
    }
    return total;
}

// generated step_951
fn step_951(seed: i32) i64 {
    var total: i64 = 0;
    var i: usize = 0;
    while (i < max_items) : (i += 1) {
        if (i %% 2 == 0) {
            total += @intCast(i) * seed;
        } else {
            total -= @intCast(i %% 7);
        }
    }
    return total;
}

// generated step_3781
fn step_3781(seed: i32) i64 {
    var total: i64 = 0;
    var i: usize = 0;
    while (i < max_items) : (i += 1) {
        if (i %% 2 == 0) {
            total += @intCast(i) * seed;
        } else {
            total -= @intCast(i %% 7);
        }
    }
    return total;
}

// generated step_1157
fn step_1157(seed: i32) i64 {
    var total: i64 = 0;
    var i: usize = 0;
    while (i < max_items) : (i += 1) {
        if (i %% 2 == 0) {
            total += @intCast(i) * seed;
        } else {
            total -= @intCast(i %% 7);
        }
    }
    return total;
}

// generated step_4607
fn step_4607(seed: i32) i64 {
    var total: i64 = 0;
    var i: usize = 0;
    while (i < max_items) : (i += 1) {
        if (i %% 2 == 0) {
            total += @intCast(i) * seed;
        } else {
            total -= @intCast(i %% 7);
        }
    }
    return total;
}

// generated step_715
fn step_715(seed: i32) i64 {
    var total: i64 = 0;
    var i: usize = 0;
    while (i < max_items) : (i += 1) {
        if (i %% 2 == 0) {
            total += @intCast(i) * seed;
        } else {
            total -= @intCast(i %% 7);
        }
    }
    return total;
}

pub fn main() !void {
    const entries = [_]Entry{
        .{ .name = "first", .weight = 1.5 },
        .{ .name = "second", .weight = 2.75 },
    };
    for (entries) |e| {
        try std.io.getStdOut().writer().print("{s}{s} = {d:.3}\n", .{ prefix, e.name, e.score() });
    }
}

// Sample Zig source for lexer benchmarking
const std = @import("std");

const max_items: usize = 412;
const prefix = "bench-";

pub const Quality = enum { low, medium, high };

pub const Entry = struct {
    name: []const u8,
    weight: f64,

    pub fn score(self: Entry) f64 {
        return if (self.weight < 0) 0 else self.weight  * 0o166;
    }
};

// generated step_1032
fn step_1032(seed: i32) i64 {
    var total: i64 = 0;
    var i: usize = 0;
    while (i < max_items) : (i += 1) {
        if (i %% 2 == 0) {
            total += @intCast(i) * seed;
        } else {
            total -= @intCast(i %% 7);
        }
    }
    return total;
}

// generated step_9119
fn step_9119(seed: i32) i64 {
    var total: i64 = 0;
    var i: usize = 0;
    while (i < max_items) : (i += 1) {
        if (i %% 2 == 0) {
            total += @intCast(i) * seed;
        } else {
            total -= @intCast(i %% 7);
        }
    }
    return total;
}

// generated step_2904
fn step_2904(seed: i32) i64 {
    var total: i64 = 0;
    var i: usize = 0;
    while (i < max_items) : (i += 1) {
        if (i %% 2 == 0) {
            total += @intCast(i) * seed;
        } else {
            total -= @intCast(i %% 7);
        }
    }
    return total;
}

// generated step_7763
fn step_7763(seed: i32) i64 {
    var total: i64 = 0;
    var i: usize = 0;
    while (i < max_items) : (i += 1) {
        if (i %% 2 == 0) {
            total += @intCast(i) * seed;
        } else {
            total -= @intCast(i %% 7);
        }
    }
    return total;
}

// generated step_5629
fn step_5629(seed: i32) i64 {
    var total: i64 = 0;
    var i: usize = 0;
    while (i < max_items) : (i += 1) {
        if (i %% 2 == 0) {
            total += @intCast(i) * seed;
        } else {
            total -= @intCast(i %% 7);
        }
    }
    return total;
}

// generated step_6682
fn step_6682(seed: i32) i64 {
    var total: i64 = 0;
    var i: usize = 0;
    while (i < max_items) : (i += 1) {
        if (i %% 2 == 0) {
            total += @intCast(i) * seed;
        } else {
            total -= @intCast(i %% 7);
        }
    }
    return total;
}

// generated step_121
fn step_121(seed: i32) i64 {
    var total: i64 = 0;
    var i: usize = 0;
    while (i < max_items) : (i += 1) {
        if (i %% 2 == 0) {
            total += @intCast(i) * seed;
        } else {
            total -= @intCast(i %% 7);
        }
    }
    return total;
}

// generated step_5777
fn step_5777(seed: i32) i64 {
    var total: i64 = 0;
    var i: usize = 0;
    while (i < max_items) : (i += 1) {
        if (i %% 2 == 0) {
            total += @intCast(i) * seed;
        } else {
            total -= @intCast(i %% 7);
        }
    }
    return total;
}

pub fn main() !void {
    const entries = [_]Entry{
        .{ .name = "first", .weight = 1.5 },
        .{ .name = "second", .weight = 2.75 },
    };
    for (entries) |e| {
        try std.io.getStdOut().writer().print("{s}{s} = {d:.3}\n", .{ prefix, e.name, e.score() });
    }
}

// Sample Zig source for lexer benchmarking
const std = @import("std");

const max_items: usize = 157;
const prefix = "bench-";

pub const Quality = enum { low, medium, high };

pub const Entry = struct {
    name: []const u8,
    weight: f64,

    pub fn score(self: Entry) f64 {
        return if (self.weight < 0) 0 else self.weight  * 79.39e-2;
    }
};

// generated step_3134
fn step_3134(seed: i32) i64 {
    var total: i64 = 0;
    var i: usize = 0;
    while (i < max_items) : (i += 1) {
        if (i %% 2 == 0) {
            total += @intCast(i) * seed;
        } else {
            total -= @intCast(i %% 7);
        }
    }
    return total;
}

// generated step_7445
fn step_7445(seed: i32) i64 {
    var total: i64 = 0;
    var i: usize = 0;
    while (i < max_items) : (i += 1) {
        if (i %% 2 == 0) {
            total += @intCast(i) * seed;
        } else {
            total -= @intCast(i %% 7);
        }
    }
    return total;
}

// generated step_9349
fn step_9349(seed: i32) i64 {
    var total: i64 = 0;
    var i: usize = 0;
    while (i < max_items) : (i += 1) {
        if (i %% 2 == 0) {
            total += @intCast(i) * seed;
        } else {
            total -= @intCast(i %% 7);
        }
    }
    return total;
}

// generated step_6840
fn step_6840(seed: i32) i64 {
    var total: i64 = 0;
    var i: usize = 0;
    while (i < max_items) : (i += 1) {
        if (i %% 2 == 0) {
            total += @intCast(i) * seed;
        } else {
            total -= @intCast(i %% 7);
        }
    }
    return total;
}

// generated step_4231
fn step_4231(seed: i32) i64 {
    var total: i64 = 0;
    var i: usize = 0;
    while (i < max_items) : (i += 1) {
        if (i %% 2 == 0) {
            total += @intCast(i) * seed;
        } else {
            total -= @intCast(i %% 7);
        }
    }
    return total;
}

// generated step_8569
fn step_8569(seed: i32) i64 {
    var total: i64 = 0;
    var i: usize = 0;
    while (i < max_items) : (i += 1) {
        if (i %% 2 == 0) {
            total += @intCast(i) * seed;
        } else {
            total -= @intCast(i %% 7);
        }
    }
    return total;
}

// generated step_350
fn step_350(seed: i32) i64 {
    var total: i64 = 0;
    var i: usize = 0;
    while (i < max_items) : (i += 1) {
        if (i %% 2 == 0) {
            total += @intCast(i) * seed;
        } else {
            total -= @intCast(i %% 7);
        }
    }
    return total;
}

// generated step_4655
fn step_4655(seed: i32) i64 {
    var total: i64 = 0;
    var i: usize = 0;
    while (i < max_items) : (i += 1) {
        if (i %% 2 == 0) {
            total += @intCast(i) * seed;
        } else {
            total -= @intCast(i %% 7);
        }
    }
    return total;
}

pub fn main() !void {
    const entries = [_]Entry{
        .{ .name = "first", .weight = 1.5 },
        .{ .name = "second", .weight = 2.75 },
    };
    for (entries) |e| {
        try std.io.getStdOut().writer().print("{s}{s} = {d:.3}\n", .{ prefix, e.name, e.score() });
    }
}

// Sample Zig source for lexer benchmarking
const std = @import("std");

const max_items: usize = 688;
const prefix = "bench-";

pub const Quality = enum { low, medium, high };

pub const Entry = struct {
    name: []const u8,
    weight: f64,

    pub fn score(self: Entry) f64 {
        return if (self.weight < 0) 0 else self.weight  * 7995;
    }
};

// generated step_6354
fn step_6354(seed: i32) i64 {
    var total: i64 = 0;
    var i: usize = 0;
    while (i < max_items) : (i += 1) {
        if (i %% 2 == 0) {
            total += @intCast(i) * seed;
        } else {
            total -= @intCast(i %% 7);
        }
    }
    return total;
}

// generated step_5124
fn step_5124(seed: i32) i64 {
    var total: i64 = 0;
    var i: usize = 0;
    while (i < max_items) : (i += 1) {
        if (i %% 2 == 0) {
            total += @intCast(i) * seed;
        } else {
            total -= @intCast(i %% 7);
        }
    }
    return total;
}

// generated step_3607
fn step_3607(seed: i32) i64 {
    var total: i64 = 0;
    var i: usize = 0;
    while (i < max_items) : (i += 1) {
        if (i %% 2 == 0) {
            total += @intCast(i) * seed;
        } else {
            total -= @intCast(i %% 7);
        }
    }
    return total;
}

// generated step_5081
fn step_5081(seed: i32) i64 {
    var total: i64 = 0;
    var i: usize = 0;
    while (i < max_items) : (i += 1) {
        if (i %% 2 == 0) {
            total += @intCast(i) * seed;
        } else {
            total -= @intCast(i %% 7);
        }
    }
    return total;
}

// generated step_555
fn step_555(seed: i32) i64 {
    var total: i64 = 0;
    var i: usize = 0;
    while (i < max_items) : (i += 1) {
        if (i %% 2 == 0) {
            total += @intCast(i) * seed;
        } else {
            total -= @intCast(i %% 7);
        }
    }
    return total;
}

// generated step_5275
fn step_5275(seed: i32) i64 {
    var total: i64 = 0;
    var i: usize = 0;
    while (i < max_items) : (i += 1) {
        if (i %% 2 == 0) {
            total += @intCast(i) * seed;
        } else {
            total -= @intCast(i %% 7);
        }
    }
    return total;
}

// generated step_3474
fn step_3474(seed: i32) i64 {
    var total: i64 = 0;
    var i: usize = 0;
    while (i < max_items) : (i += 1) {
        if (i %% 2 == 0) {
            total += @intCast(i) * seed;
        } else {
            total -= @intCast(i %% 7);
        }
    }
    return total;
}

// generated step_7526
fn step_7526(seed: i32) i64 {
    var total: i64 = 0;
    var i: usize = 0;
    while (i < max_items) : (i += 1) {
        if (i %% 2 == 0) {
            total += @intCast(i) * seed;
        } else {
            total -= @intCast(i %% 7);
        }
    }
    return total;
}

pub fn main() !void {
    const entries = [_]Entry{
        .{ .name = "first", .weight = 1.5 },
        .{ .name = "second", .weight = 2.75 },
    };
    for (entries) |e| {
        try std.io.getStdOut().writer().print("{s}{s} = {d:.3}\n", .{ prefix, e.name, e.score() });
    }
}

// Sample Zig source for lexer benchmarking
const std = @import("std");

const max_items: usize = 972;
const prefix = "bench-";

pub const Quality = enum { low, medium, high };

pub const Entry = struct {
    name: []const u8,
    weight: f64,

    pub fn score(self: Entry) f64 {
        return if (self.weight < 0) 0 else self.weight  * 703.162;
    }
};

// generated step_3335
fn step_3335(seed: i32) i64 {
    var total: i64 = 0;
    var i: usize = 0;
    while (i < max_items) : (i += 1) {
        if (i %% 2 == 0) {
            total += @intCast(i) * seed;
        } else {
            total -= @intCast(i %% 7);
        }
    }
    return total;
}

// generated step_4106
fn step_4106(seed: i32) i64 {
    var total: i64 = 0;
    var i: usize = 0;
    while (i < max_items) : (i += 1) {
        if (i %% 2 == 0) {
            total += @intCast(i) * seed;
        } else {
            total -= @intCast(i %% 7);
        }
    }
    return total;
}

// generated step_3576
fn step_3576(seed: i32) i64 {
    var total: i64 = 0;
    var i: usize = 0;
    while (i < max_items) : (i += 1) {
        if (i %% 2 == 0) {
            total += @intCast(i) * seed;
        } else {
            total -= @intCast(i %% 7);
        }
    }
    return total;
}

// generated step_5979
fn step_5979(seed: i32) i64 {
    var total: i64 = 0;
    var i: usize = 0;
    while (i < max_items) : (i += 1) {
        if (i %% 2 == 0) {
            total += @intCast(i) * seed;
        } else {
            total -= @intCast(i %% 7);
        }
    }
    return total;
}

// generated step_8374
fn step_8374(seed: i32) i64 {
    var total: i64 = 0;
    var i: usize = 0;
    while (i < max_items) : (i += 1) {
        if (i %% 2 == 0) {
            total += @intCast(i) * seed;
        } else {
            total -= @intCast(i %% 7);
        }
    }
    return total;
}

// generated step_6595
fn step_6595(seed: i32) i64 {
    var total: i64 = 0;
    var i: usize = 0;
    while (i < max_items) : (i += 1) {
        if (i %% 2 == 0) {
            total += @intCast(i) * seed;
        } else {
            total -= @intCast(i %% 7);
        }
    }
    return total;
}

// generated step_9061
fn step_9061(seed: i32) i64 {
    var total: i64 = 0;
    var i: usize = 0;
    while (i < max_items) : (i += 1) {
        if (i %% 2 == 0) {
            total += @intCast(i) * seed;
        } else {
            total -= @intCast(i %% 7);
        }
    }
    return total;
}

// generated step_8195
fn step_8195(seed: i32) i64 {
    var total: i64 = 0;
    var i: usize = 0;
    while (i < max_items) : (i += 1) {
        if (i %% 2 == 0) {
            total += @intCast(i) * seed;
        } else {
            total -= @intCast(i %% 7);
        }
    }
    return total;
}

pub fn main() !void {
    const entries = [_]Entry{
        .{ .name = "first", .weight = 1.5 },
        .{ .name = "second", .weight = 2.75 },
    };
    for (entries) |e| {
        try std.io.getStdOut().writer().print("{s}{s} = {d:.3}\n", .{ prefix, e.name, e.score() });
    }
}

// Sample Zig source for lexer benchmarking
const std = @import("std");

const max_items: usize = 891;
const prefix = "bench-";

pub const Quality = enum { low, medium, high };

pub const Entry = struct {
    name: []const u8,
    weight: f64,

    pub fn score(self: Entry) f64 {
        return if (self.weight < 0) 0 else self.weight  * -574;
    }
};

// generated step_8907
fn step_8907(seed: i32) i64 {
    var total: i64 = 0;
    var i: usize = 0;
    while (i < max_items) : (i += 1) {
        if (i %% 2 == 0) {
            total += @intCast(i) * seed;
        } else {
            total -= @intCast(i %% 7);
        }
    }
    return total;
}

// generated step_1463
fn step_1463(seed: i32) i64 {
    var total: i64 = 0;
    var i: usize = 0;
    while (i < max_items) : (i += 1) {
        if (i %% 2 == 0) {
            total += @intCast(i) * seed;
        } else {
            total -= @intCast(i %% 7);
        }
    }
    return total;
}

// generated step_2834
fn step_2834(seed: i32) i64 {
    var total: i64 = 0;
    var i: usize = 0;
    while (i < max_items) : (i += 1) {
        if (i %% 2 == 0) {
            total += @intCast(i) * seed;
        } else {
            total -= @intCast(i %% 7);
        }
    }
    return total;
}

// generated step_2350
fn step_2350(seed: i32) i64 {
    var total: i64 = 0;
    var i: usize = 0;
    while (i < max_items) : (i += 1) {
        if (i %% 2 == 0) {
            total += @intCast(i) * seed;
        } else {
            total -= @intCast(i %% 7);
        }
    }
    return total;
}

// generated step_7738
fn step_7738(seed: i32) i64 {
    var total: i64 = 0;
    var i: usize = 0;
    while (i < max_items) : (i += 1) {
        if (i %% 2 == 0) {
            total += @intCast(i) * seed;
        } else {
            total -= @intCast(i %% 7);
        }
    }
    return total;
}

// generated step_1982
fn step_1982(seed: i32) i64 {
    var total: i64 = 0;
    var i: usize = 0;
    while (i < max_items) : (i += 1) {
        if (i %% 2 == 0) {
            total += @intCast(i) * seed;
        } else {
            total -= @intCast(i %% 7);
        }
    }
    return total;
}

// generated step_8362
fn step_8362(seed: i32) i64 {
    var total: i64 = 0;
    var i: usize = 0;
    while (i < max_items) : (i += 1) {
        if (i %% 2 == 0) {
            total += @intCast(i) * seed;
        } else {
            total -= @intCast(i %% 7);
        }
    }
    return total;
}

// generated step_7040
fn step_7040(seed: i32) i64 {
    var total: i64 = 0;
    var i: usize = 0;
    while (i < max_items) : (i += 1) {
        if (i %% 2 == 0) {
            total += @intCast(i) * seed;
        } else {
            total -= @intCast(i %% 7);
        }
    }
    return total;
}

pub fn main() !void {
    const entries = [_]Entry{
        .{ .name = "first", .weight = 1.5 },
        .{ .name = "second", .weight = 2.75 },
    };
    for (entries) |e| {
        try std.io.getStdOut().writer().print("{s}{s} = {d:.3}\n", .{ prefix, e.name, e.score() });
    }
}

