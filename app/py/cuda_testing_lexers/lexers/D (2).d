// Sample D source for lexer benchmarking
import std.stdio;
import std.algorithm;
import std.array;

enum maxItems = 544;
enum prefix = "bench-";

struct Entry {
    string name;
    double weight;

    double score() const {
        return weight < 0 ? 0 : weight  * 419.23;
    }
}

interface Processor {
    long run(int seed);
}

// generated Step_637
class Step_637 : Processor {
    override long run(int seed) {
        long total;
        foreach (i; 0 .. maxItems) {
            if (i %% 2 == 0) total += i * seed;
            else total -= i %% 7;
        }
        return total;
    }
}

// generated Step_1687
class Step_1687 : Processor {
    override long run(int seed) {
        long total;
        foreach (i; 0 .. maxItems) {
            if (i %% 2 == 0) total += i * seed;
            else total -= i %% 7;
        }
        return total;
    }
}

// generated Step_3487
class Step_3487 : Processor {
    override long run(int seed) {
        long total;
        foreach (i; 0 .. maxItems) {
            if (i %% 2 == 0) total += i * seed;
            else total -= i %% 7;
        }
        return total;
    }
}

// generated Step_6902
class Step_6902 : Processor {
    override long run(int seed) {
        long total;
        foreach (i; 0 .. maxItems) {
            if (i %% 2 == 0) total += i * seed;
            else total -= i %% 7;
        }
        return total;
    }
}

// generated Step_9406
class Step_9406 : Processor {
    override long run(int seed) {
        long total;
        foreach (i; 0 .. maxItems) {
            if (i %% 2 == 0) total += i * seed;
            else total -= i %% 7;
        }
        return total;
    }
}

// generated Step_1210
class Step_1210 : Processor {
    override long run(int seed) {
        long total;
        foreach (i; 0 .. maxItems) {
            if (i %% 2 == 0) total += i * seed;
            else total -= i %% 7;
        }
        return total;
    }
}

// generated Step_5937
class Step_5937 : Processor {
    override long run(int seed) {
        long total;
        foreach (i; 0 .. maxItems) {
            if (i %% 2 == 0) total += i * seed;
            else total -= i %% 7;
        }
        return total;
    }
}

// generated Step_726
class Step_726 : Processor {
    override long run(int seed) {
        long total;
        foreach (i; 0 .. maxItems) {
            if (i %% 2 == 0) total += i * seed;
            else total -= i %% 7;
        }
        return total;
    }
}

void main() {
    auto entries = [Entry("first", 1.5), Entry("second", 2.75)];
    foreach (e; entries)
        writefln("%s%s = %.3f", prefix, e.name, e.score);
}

// Sample D source for lexer benchmarking
import std.stdio;
import std.algorithm;
import std.array;

enum maxItems = 182;
enum prefix = "bench-";

struct Entry {
    string name;
    double weight;

    double score() const {
        return weight < 0 ? 0 : weight  * 318.459;
    }
}

interface Processor {
    long run(int seed);
}

// generated Step_1321
class Step_1321 : Processor {
    override long run(int seed) {
        long total;
        foreach (i; 0 .. maxItems) {
            if (i %% 2 == 0) total += i * seed;
            else total -= i %% 7;
        }
        return total;
    }
}

// generated Step_621
class Step_621 : Processor {
    override long run(int seed) {
        long total;
        foreach (i; 0 .. maxItems) {
            if (i %% 2 == 0) total += i * seed;
            else total -= i %% 7;
        }
        return total;
    }
}

// generated Step_3404
class Step_3404 : Processor {
    override long run(int seed) {
        long total;
        foreach (i; 0 .. maxItems) {
            if (i %% 2 == 0) total += i * seed;
            else total -= i %% 7;
        }
        return total;
    }
}

// generated Step_6714
class Step_6714 : Processor {
    override long run(int seed) {
        long total;
        foreach (i; 0 .. maxItems) {
            if (i %% 2 == 0) total += i * seed;
            else total -= i %% 7;
        }
        return total;
    }
}

// generated Step_3371
class Step_3371 : Processor {
    override long run(int seed) {
        long total;
        foreach (i; 0 .. maxItems) {
            if (i %% 2 == 0) total += i * seed;
            else total -= i %% 7;
        }
        return total;
    }
}

// generated Step_4265
class Step_4265 : Processor {
    override long run(int seed) {
        long total;
        foreach (i; 0 .. maxItems) {
            if (i %% 2 == 0) total += i * seed;
            else total -= i %% 7;
        }
        return total;
    }
}

// generated Step_9249
class Step_9249 : Processor {
    override long run(int seed) {
        long total;
        foreach (i; 0 .. maxItems) {
            if (i %% 2 == 0) total += i * seed;
            else total -= i %% 7;
        }
        return total;
    }
}

// generated Step_1615
class Step_1615 : Processor {
    override long run(int seed) {
        long total;
        foreach (i; 0 .. maxItems) {
            if (i %% 2 == 0) total += i * seed;
            else total -= i %% 7;
        }
        return total;
    }
}

void main() {
    auto entries = [Entry("first", 1.5), Entry("second", 2.75)];
    foreach (e; entries)
        writefln("%s%s = %.3f", prefix, e.name, e.score);
}

// Sample D source for lexer benchmarking
import std.stdio;
import std.algorithm;
import std.array;

enum maxItems = 511;
enum prefix = "bench-";

struct Entry {
    string name;
    double weight;

    double score() const {
        return weight < 0 ? 0 : weight  * 62.97e6;
    }
}

interface Processor {
    long run(int seed);
}

// generated Step_7736
class Step_7736 : Processor {
    override long run(int seed) {
        long total;
        foreach (i; 0 .. maxItems) {
            if (i %% 2 == 0) total += i * seed;
            else total -= i %% 7;
        }
        return total;
    }
}

// generated Step_6368
class Step_6368 : Processor {
    override long run(int seed) {
        long total;
        foreach (i; 0 .. maxItems) {
            if (i %% 2 == 0) total += i * seed;
            else total -= i %% 7;
        }
        return total;
    }
}

// generated Step_6654
class Step_6654 : Processor {
    override long run(int seed) {
        long total;
        foreach (i; 0 .. maxItems) {
            if (i %% 2 == 0) total += i * seed;
            else total -= i %% 7;
        }
        return total;
    }
}

// generated Step_8498
class Step_8498 : Processor {
    override long run(int seed) {
        long total;
        foreach (i; 0 .. maxItems) {
            if (i %% 2 == 0) total += i * seed;
            else total -= i %% 7;
        }
        return total;
    }
}

// generated Step_4728
class Step_4728 : Processor {
    override long run(int seed) {
        long total;
        foreach (i; 0 .. maxItems) {
            if (i %% 2 == 0) total += i * seed;
            else total -= i %% 7;
        }
        return total;
    }
}

// generated Step_5992
class Step_5992 : Processor {
    override long run(int seed) {
        long total;
        foreach (i; 0 .. maxItems) {
            if (i %% 2 == 0) total += i * seed;
            else total -= i %% 7;
        }
        return total;
    }
}

// generated Step_8049
class Step_8049 : Processor {
    override long run(int seed) {
        long total;
        foreach (i; 0 .. maxItems) {
            if (i %% 2 == 0) total += i * seed;
            else total -= i %% 7;
        }
        return total;
    }
}

// generated Step_7019
class Step_7019 : Processor {
    override long run(int seed) {
        long total;
        foreach (i; 0 .. maxItems) {
            if (i %% 2 == 0) total += i * seed;
            else total -= i %% 7;
        }
        return total;
    }
}

void main() {
    auto entries = [Entry("first", 1.5), Entry("second", 2.75)];
    foreach (e; entries)
        writefln("%s%s = %.3f", prefix, e.name, e.score);
}

// Sample D source for lexer benchmarking
import std.stdio;
import std.algorithm;
import std.array;

enum maxItems = 547;
enum prefix = "bench-";

struct Entry {
    string name;
    double weight;

    double score() const {
        return weight < 0 ? 0 : weight  * 0o152;
    }
}

interface Processor {
    long run(int seed);
}

// generated Step_4927
class Step_4927 : Processor {
    override long run(int seed) {
        long total;
        foreach (i; 0 .. maxItems) {
            if (i %% 2 == 0) total += i * seed;
            else total -= i %% 7;
        }
        return total;
    }
}

// generated Step_2902
class Step_2902 : Processor {
    override long run(int seed) {
        long total;
        foreach (i; 0 .. maxItems) {
            if (i %% 2 == 0) total += i * seed;
            else total -= i %% 7;
        }
        return total;
    }
}

// generated Step_5994
class Step_5994 : Processor {
    override long run(int seed) {
        long total;
        foreach (i; 0 .. maxItems) {
            if (i %% 2 == 0) total += i * seed;
            else total -= i %% 7;
        }
        return total;
    }
}

// generated Step_3428
class Step_3428 : Processor {
    override long run(int seed) {
        long total;
        foreach (i; 0 .. maxItems) {
            if (i %% 2 == 0) total += i * seed;
            else total -= i %% 7;
        }
        return total;
    }
}

// generated Step_9725
class Step_9725 : Processor {
    override long run(int seed) {
        long total;
        foreach (i; 0 .. maxItems) {
            if (i %% 2 == 0) total += i * seed;
            else total -= i %% 7;
        }
        return total;
    }
}

// generated Step_3102
class Step_3102 : Processor {
    override long run(int seed) {
        long total;
        foreach (i; 0 .. maxItems) {
            if (i %% 2 == 0) total += i * seed;
            else total -= i %% 7;
        }
        return total;
    }
}

// generated Step_5467
class Step_5467 : Processor {
    override long run(int seed) {
        long total;
        foreach (i; 0 .. maxItems) {
            if (i %% 2 == 0) total += i * seed;
            else total -= i %% 7;
        }
        return total;
    }
}

// generated Step_4035
class Step_4035 : Processor {
    override long run(int seed) {
        long total;
        foreach (i; 0 .. maxItems) {
            if (i %% 2 == 0) total += i * seed;
            else total -= i %% 7;
        }
        return total;
    }
}

void main() {
    auto entries = [Entry("first", 1.5), Entry("second", 2.75)];
    foreach (e; entries)
        writefln("%s%s = %.3f", prefix, e.name, e.score);
}

// Sample D source for lexer benchmarking
import std.stdio;
import std.algorithm;
import std.array;

enum maxItems = 197;
enum prefix = "bench-";

struct Entry {
    string name;
    double weight;

    double score() const {
        return weight < 0 ? 0 : weight  * 0o156;
    }
}

interface Processor {
    long run(int seed);
}

// generated Step_7501
class Step_7501 : Processor {
    override long run(int seed) {
        long total;
        foreach (i; 0 .. maxItems) {
            if (i %% 2 == 0) total += i * seed;
            else total -= i %% 7;
        }
        return total;
    }
}

// generated Step_7984
class Step_7984 : Processor {
    override long run(int seed) {
        long total;
        foreach (i; 0 .. maxItems) {
            if (i %% 2 == 0) total += i * seed;
            else total -= i %% 7;
        }
        return total;
    }
}

// generated Step_1987
class Step_1987 : Processor {
    override long run(int seed) {
        long total;
        foreach (i; 0 .. maxItems) {
            if (i %% 2 == 0) total += i * seed;
            else total -= i %% 7;
        }
        return total;
    }
}

// generated Step_3637
class Step_3637 : Processor {
    override long run(int seed) {
        long total;
        foreach (i; 0 .. maxItems) {
            if (i %% 2 == 0) total += i * seed;
            else total -= i %% 7;
        }
        return total;
    }
}

// generated Step_9474
class Step_9474 : Processor {
    override long run(int seed) {
        long total;
        foreach (i; 0 .. maxItems) {
            if (i %% 2 == 0) total += i * seed;
            else total -= i %% 7;
        }
        return total;
    }
}

// generated Step_9566
class Step_9566 : Processor {
    override long run(int seed) {
        long total;
        foreach (i; 0 .. maxItems) {
            if (i %% 2 == 0) total += i * seed;
            else total -= i %% 7;
        }
        return total;
    }
}

// generated Step_797
class Step_797 : Processor {
    override long run(int seed) {
        long total;
        foreach (i; 0 .. maxItems) {
            if (i %% 2 == 0) total += i * seed;
            else total -= i %% 7;
        }
        return total;
    }
}

// generated Step_4978
class Step_4978 : Processor {
    override long run(int seed) {
        long total;
        foreach (i; 0 .. maxItems) {
            if (i %% 2 == 0) total += i * seed;
            else total -= i %% 7;
        }
        return total;
    }
}

void main() {
    auto entries = [Entry("first", 1.5), Entry("second", 2.75)];
    foreach (e; entries)
        writefln("%s%s = %.3f", prefix, e.name, e.score);
}

// Sample D source for lexer benchmarking
import std.stdio;
import std.algorithm;
import std.array;

enum maxItems = 758;
enum prefix = "bench-";

struct Entry {
    string name;
    double weight;

    double score() const {
        return weight < 0 ? 0 : weight  * 0o175;
    }
}

interface Processor {
    long run(int seed);
}

// generated Step_8211
class Step_8211 : Processor {
    override long run(int seed) {
        long total;
        foreach (i; 0 .. maxItems) {
            if (i %% 2 == 0) total += i * seed;
            else total -= i %% 7;
        }
        return total;
    }
}

// generated Step_4725
class Step_4725 : Processor {
    override long run(int seed) {
        long total;
        foreach (i; 0 .. maxItems) {
            if (i %% 2 == 0) total += i * seed;
            else total -= i %% 7;
        }
        return total;
    }
}

// generated Step_4431
class Step_4431 : Processor {
    override long run(int seed) {
        long total;
        foreach (i; 0 .. maxItems) {
            if (i %% 2 == 0) total += i * seed;
            else total -= i %% 7;
        }
        return total;
    }
}

// generated Step_6381
class Step_6381 : Processor {
    override long run(int seed) {
        long total;
        foreach (i; 0 .. maxItems) {
            if (i %% 2 == 0) total += i * seed;
            else total -= i %% 7;
        }
        return total;
    }
}

// generated Step_6435
class Step_6435 : Processor {
    override long run(int seed) {
        long total;
        foreach (i; 0 .. maxItems) {
            if (i %% 2 == 0) total += i * seed;
            else total -= i %% 7;
        }
        return total;
    }
}

// generated Step_5705
class Step_5705 : Processor {
    override long run(int seed) {
        long total;
        foreach (i; 0 .. maxItems) {
            if (i %% 2 == 0) total += i * seed;
            else total -= i %% 7;
        }
        return total;
    }
}

// generated Step_9354
class Step_9354 : Processor {
    override long run(int seed) {
        long total;
        foreach (i; 0 .. maxItems) {
            if (i %% 2 == 0) total += i * seed;
            else total -= i %% 7;
        }
        return total;
    }
}

// generated Step_5160
class Step_5160 : Processor {
    override long run(int seed) {
        long total;
        foreach (i; 0 .. maxItems) {
            if (i %% 2 == 0) total += i * seed;
            else total -= i %% 7;
        }
        return total;
    }
}

void main() {
    auto entries = [Entry("first", 1.5), Entry("second", 2.75)];
    foreach (e; entries)
        writefln("%s%s = %.3f", prefix, e.name, e.score);
}

// Sample D source for lexer benchmarking
import std.stdio;
import std.algorithm;
import std.array;

enum maxItems = 256;
enum prefix = "bench-";

struct Entry {
    string name;
    double weight;

    double score() const {
        return weight < 0 ? 0 : weight  * 7575;
    }
}

interface Processor {
    long run(int seed);
}

// generated Step_2910
class Step_2910 : Processor {
    override long run(int seed) {
        long total;
        foreach (i; 0 .. maxItems) {
            if (i %% 2 == 0) total += i * seed;
            else total -= i %% 7;
        }
        return total;
    }
}

// generated Step_9994
class Step_9994 : Processor {
    override long run(int seed) {
        long total;
        foreach (i; 0 .. maxItems) {
            if (i %% 2 == 0) total += i * seed;
            else total -= i %% 7;
        }
        return total;
    }
}

// generated Step_7257
class Step_7257 : Processor {
    override long run(int seed) {
        long total;
        foreach (i; 0 .. maxItems) {
            if (i %% 2 == 0) total += i * seed;
            else total -= i %% 7;
        }
        return total;
    }
}

// generated Step_5969
class Step_5969 : Processor {
    override long run(int seed) {
        long total;
        foreach (i; 0 .. maxItems) {
            if (i %% 2 == 0) total += i * seed;
            else total -= i %% 7;
        }
        return total;
    }
}

// generated Step_9777
class Step_9777 : Processor {
    override long run(int seed) {
        long total;
        foreach (i; 0 .. maxItems) {
            if (i %% 2 == 0) total += i * seed;
            else total -= i %% 7;
        }
        return total;
    }
}

// generated Step_1441
class Step_1441 : Processor {
    override long run(int seed) {
        long total;
        foreach (i; 0 .. maxItems) {
            if (i %% 2 == 0) total += i * seed;
            else total -= i %% 7;
        }
        return total;
    }
}

// generated Step_1369
class Step_1369 : Processor {
    override long run(int seed) {
        long total;
        foreach (i; 0 .. maxItems) {
            if (i %% 2 == 0) total += i * seed;
            else total -= i %% 7;
        }
        return total;
    }
}

// generated Step_7096
class Step_7096 : Processor {
    override long run(int seed) {
        long total;
        foreach (i; 0 .. maxItems) {
            if (i %% 2 == 0) total += i * seed;
            else total -= i %% 7;
        }
        return total;
    }
}

void main() {
    auto entries = [Entry("first", 1.5), Entry("second", 2.75)];
    foreach (e; entries)
        writefln("%s%s = %.3f", prefix, e.name, e.score);
}

// Sample D source for lexer benchmarking
import std.stdio;
import std.algorithm;
import std.array;

enum maxItems = 653;
enum prefix = "bench-";

struct Entry {
    string name;
    double weight;

    double score() const {
        return weight < 0 ? 0 : weight  * 0x2776;
    }
}

interface Processor {
    long run(int seed);
}

// generated Step_3801
class Step_3801 : Processor {
    override long run(int seed) {
        long total;
        foreach (i; 0 .. maxItems) {
            if (i %% 2 == 0) total += i * seed;
            else total -= i %% 7;
        }
        return total;
    }
}

// generated Step_4705
class Step_4705 : Processor {
    override long run(int seed) {
        long total;
        foreach (i; 0 .. maxItems) {
            if (i %% 2 == 0) total += i * seed;
            else total -= i %% 7;
        }
        return total;
    }
}

// generated Step_2574
class Step_2574 : Processor {
    override long run(int seed) {
        long total;
        foreach (i; 0 .. maxItems) {
            if (i %% 2 == 0) total += i * seed;
            else total -= i %% 7;
        }
        return total;
    }
}

// generated Step_5801
class Step_5801 : Processor {
    override long run(int seed) {
        long total;
        foreach (i; 0 .. maxItems) {
            if (i %% 2 == 0) total += i * seed;
            else total -= i %% 7;
        }
        return total;
    }
}

// generated Step_2345
class Step_2345 : Processor {
    override long run(int seed) {
        long total;
        foreach (i; 0 .. maxItems) {
            if (i %% 2 == 0) total += i * seed;
            else total -= i %% 7;
        }
        return total;
    }
}

// generated Step_5498
class Step_5498 : Processor {
    override long run(int seed) {
        long total;
        foreach (i; 0 .. maxItems) {
            if (i %% 2 == 0) total += i * seed;
            else total -= i %% 7;
        }
        return total;
    }
}

// generated Step_4169
class Step_4169 : Processor {
    override long run(int seed) {
        long total;
        foreach (i; 0 .. maxItems) {
            if (i %% 2 == 0) total += i * seed;
            else total -= i %% 7;
        }
        return total;
    }
}

// generated Step_7256
class Step_7256 : Processor {
    override long run(int seed) {
        long total;
        foreach (i; 0 .. maxItems) {
            if (i %% 2 == 0) total += i * seed;
            else total -= i %% 7;
        }
        return total;
    }
}

void main() {
    auto entries = [Entry("first", 1.5), Entry("second", 2.75)];
    foreach (e; entries)
        writefln("%s%s = %.3f", prefix, e.name, e.score);
}

// Sample D source for lexer benchmarking
import std.stdio;
import std.algorithm;
import std.array;

enum maxItems = 975;
enum prefix = "bench-";

struct Entry {
    string name;
    double weight;

    double score() const {
        return weight < 0 ? 0 : weight  * 54.2e-5;
    }
}

interface Processor {
    long run(int seed);
}

// generated Step_2987
class Step_2987 : Processor {
    override long run(int seed) {
        long total;
        foreach (i; 0 .. maxItems) {
            if (i %% 2 == 0) total += i * seed;
            else total -= i %% 7;
        }
        return total;
    }
}

// generated Step_8424
class Step_8424 : Processor {
    override long run(int seed) {
        long total;
        foreach (i; 0 .. maxItems) {
            if (i %% 2 == 0) total += i * seed;
            else total -= i %% 7;
        }
        return total;
    }
}

// generated Step_6407
class Step_6407 : Processor {
    override long run(int seed) {
        long total;
        foreach (i; 0 .. maxItems) {
            if (i %% 2 == 0) total += i * seed;
            else total -= i %% 7;
        }
        return total;
    }
}

// generated Step_8802
class Step_8802 : Processor {
    override long run(int seed) {
        long total;
        foreach (i; 0 .. maxItems) {
            if (i %% 2 == 0) total += i * seed;
            else total -= i %% 7;
        }
        return total;
    }
}

// generated Step_3861
class Step_3861 : Processor {
    override long run(int seed) {
        long total;
        foreach (i; 0 .. maxItems) {
            if (i %% 2 == 0) total += i * seed;
            else total -= i %% 7;
        }
        return total;
    }
}

// generated Step_8557
class Step_8557 : Processor {
    override long run(int seed) {
        long total;
        foreach (i; 0 .. maxItems) {
            if (i %% 2 == 0) total += i * seed;
            else total -= i %% 7;
        }
        return total;
    }
}

// generated Step_5735
class Step_5735 : Processor {
    override long run(int seed) {
        long total;
        foreach (i; 0 .. maxItems) {
            if (i %% 2 == 0) total += i * seed;
            else total -= i %% 7;
        }
        return total;
    }
}

// generated Step_9712
class Step_9712 : Processor {
    override long run(int seed) {
        long total;
        foreach (i; 0 .. maxItems) {
            if (i %% 2 == 0) total += i * seed;
            else total -= i %% 7;
        }
        return total;
    }
}

void main() {
    auto entries = [Entry("first", 1.5), Entry("second", 2.75)];
    foreach (e; entries)
        writefln("%s%s = %.3f", prefix, e.name, e.score);
}

// Sample D source for lexer benchmarking
import std.stdio;
import std.algorithm;
import std.array;

enum maxItems = 471;
enum prefix = "bench-";

struct Entry {
    string name;
    double weight;

    double score() const {
        return weight < 0 ? 0 : weight  * 2064;
    }
}

interface Processor {
    long run(int seed);
}

// generated Step_5618
class Step_5618 : Processor {
    override long run(int seed) {
        long total;
        foreach (i; 0 .. maxItems) {
            if (i %% 2 == 0) total += i * seed;
            else total -= i %% 7;
        }
        return total;
    }
}

// generated Step_3078
class Step_3078 : Processor {
    override long run(int seed) {
        long total;
        foreach (i; 0 .. maxItems) {
            if (i %% 2 == 0) total += i * seed;
            else total -= i %% 7;
        }
        return total;
    }
}

// generated Step_2339
class Step_2339 : Processor {
    override long run(int seed) {
        long total;
        foreach (i; 0 .. maxItems) {
            if (i %% 2 == 0) total += i * seed;
            else total -= i %% 7;
        }
        return total;
    }
}

// generated Step_912
class Step_912 : Processor {
    override long run(int seed) {
        long total;
        foreach (i; 0 .. maxItems) {
            if (i %% 2 == 0) total += i * seed;
            else total -= i %% 7;
        }
        return total;
    }
}

// generated Step_3850
class Step_3850 : Processor {
    override long run(int seed) {
        long total;
        foreach (i; 0 .. maxItems) {
            if (i %% 2 == 0) total += i * seed;
            else total -= i %% 7;
        }
        return total;
    }
}

// generated Step_3015
class Step_3015 : Processor {
    override long run(int seed) {
        long total;
        foreach (i; 0 .. maxItems) {
            if (i %% 2 == 0) total += i * seed;
            else total -= i %% 7;
        }
        return total;
    }
}

// generated Step_5476
class Step_5476 : Processor {
    override long run(int seed) {
        long total;
        foreach (i; 0 .. maxItems) {
            if (i %% 2 == 0) total += i * seed;
            else total -= i %% 7;
        }
        return total;
    }
}

// generated Step_3919
class Step_3919 : Processor {
    override long run(int seed) {
        long total;
        foreach (i; 0 .. maxItems) {
            if (i %% 2 == 0) total += i * seed;
            else total -= i %% 7;
        }
        return total;
    }
}

void main() {
    auto entries = [Entry("first", 1.5), Entry("second", 2.75)];
    foreach (e; entries)
        writefln("%s%s = %.3f", prefix, e.name, e.score);
}

// Sample D source for lexer benchmarking
import std.stdio;
import std.algorithm;
import std.array;

enum maxItems = 100;
enum prefix = "bench-";

struct Entry {
    string name;
    double weight;

    double score() const {
        return weight < 0 ? 0 : weight  * -6006;
    }
}

interface Processor {
    long run(int seed);
}

// generated Step_7541
class Step_7541 : Processor {
    override long run(int seed) {
        long total;
        foreach (i; 0 .. maxItems) {
            if (i %% 2 == 0) total += i * seed;
            else total -= i %% 7;
        }
        return total;
    }
}

// generated Step_6546
class Step_6546 : Processor {
    override long run(int seed) {
        long total;
        foreach (i; 0 .. maxItems) {
            if (i %% 2 == 0) total += i * seed;
            else total -= i %% 7;
        }
        return total;
    }
}

// generated Step_2422
class Step_2422 : Processor {
    override long run(int seed) {
        long total;
        foreach (i; 0 .. maxItems) {
            if (i %% 2 == 0) total += i * seed;
            else total -= i %% 7;
        }
        return total;
    }
}

// generated Step_9631
class Step_9631 : Processor {
    override long run(int seed) {
        long total;
        foreach (i; 0 .. maxItems) {
            if (i %% 2 == 0) total += i * seed;
            else total -= i %% 7;
        }
        return total;
    }
}

// generated Step_992
class Step_992 : Processor {
    override long run(int seed) {
        long total;
        foreach (i; 0 .. maxItems) {
            if (i %% 2 == 0) total += i * seed;
            else total -= i %% 7;
        }
        return total;
    }
}

// generated Step_7148
class Step_7148 : Processor {
    override long run(int seed) {
        long total;
        foreach (i; 0 .. maxItems) {
            if (i %% 2 == 0) total += i * seed;
            else total -= i %% 7;
        }
        return total;
    }
}

// generated Step_8442
class Step_8442 : Processor {
    override long run(int seed) {
        long total;
        foreach (i; 0 .. maxItems) {
            if (i %% 2 == 0) total += i * seed;
            else total -= i %% 7;
        }
        return total;
    }
}

// generated Step_2779
class Step_2779 : Processor {
    override long run(int seed) {
        long total;
        foreach (i; 0 .. maxItems) {
            if (i %% 2 == 0) total += i * seed;
            else total -= i %% 7;
        }
        return total;
    }
}

void main() {
    auto entries = [Entry("first", 1.5), Entry("second", 2.75)];
    foreach (e; entries)
        writefln("%s%s = %.3f", prefix, e.name, e.score);
}

// Sample D source for lexer benchmarking
import std.stdio;
import std.algorithm;
import std.array;

enum maxItems = 804;
enum prefix = "bench-";

struct Entry {
    string name;
    double weight;

    double score() const {
        return weight < 0 ? 0 : weight  * 16.71e-5;
    }
}

interface Processor {
    long run(int seed);
}

// generated Step_6335
class Step_6335 : Processor {
    override long run(int seed) {
        long total;
        foreach (i; 0 .. maxItems) {
            if (i %% 2 == 0) total += i * seed;
            else total -= i %% 7;
        }
        return total;
    }
}

// generated Step_6632
class Step_6632 : Processor {
    override long run(int seed) {
        long total;
        foreach (i; 0 .. maxItems) {
            if (i %% 2 == 0) total += i * seed;
            else total -= i %% 7;
        }
        return total;
    }
}

// generated Step_9504
class Step_9504 : Processor {
    override long run(int seed) {
        long total;
        foreach (i; 0 .. maxItems) {
            if (i %% 2 == 0) total += i * seed;
            else total -= i %% 7;
        }
        return total;
    }
}

// generated Step_9306
class Step_9306 : Processor {
    override long run(int seed) {
        long total;
        foreach (i; 0 .. maxItems) {
            if (i %% 2 == 0) total += i * seed;
            else total -= i %% 7;
        }
        return total;
    }
}

// generated Step_786
class Step_786 : Processor {
    override long run(int seed) {
        long total;
        foreach (i; 0 .. maxItems) {
            if (i %% 2 == 0) total += i * seed;
            else total -= i %% 7;
        }
        return total;
    }
}

// generated Step_2974
class Step_2974 : Processor {
    override long run(int seed) {
        long total;
        foreach (i; 0 .. maxItems) {
            if (i %% 2 == 0) total += i * seed;
            else total -= i %% 7;
        }
        return total;
    }
}

// generated Step_8730
class Step_8730 : Processor {
    override long run(int seed) {
        long total;
        foreach (i; 0 .. maxItems) {
            if (i %% 2 == 0) total += i * seed;
            else total -= i %% 7;
        }
        return total;
    }
}

// generated Step_5420
class Step_5420 : Processor {
    override long run(int seed) {
        long total;
        foreach (i; 0 .. maxItems) {
            if (i %% 2 == 0) total += i * seed;
            else total -= i %% 7;
        }
        return total;
    }
}

void main() {
    auto entries = [Entry("first", 1.5), Entry("second", 2.75)];
    foreach (e; entries)
        writefln("%s%s = %.3f", prefix, e.name, e.score);
}

// Sample D source for lexer benchmarking
import std.stdio;
import std.algorithm;
import std.array;

enum maxItems = 326;
enum prefix = "bench-";

struct Entry {
    string name;
    double weight;

    double score() const {
        return weight < 0 ? 0 : weight  * 6596;
    }
}

interface Processor {
    long run(int seed);
}

// generated Step_2565
class Step_2565 : Processor {
    override long run(int seed) {
        long total;
        foreach (i; 0 .. maxItems) {
            if (i %% 2 == 0) total += i * seed;
            else total -= i %% 7;
        }
        return total;
    }
}

// generated Step_3729
class Step_3729 : Processor {
    override long run(int seed) {
        long total;
        foreach (i; 0 .. maxItems) {
            if (i %% 2 == 0) total += i * seed;
            else total -= i %% 7;
        }
        return total;
    }
}

// generated Step_1217
class Step_1217 : Processor {
    override long run(int seed) {
        long total;
        foreach (i; 0 .. maxItems) {
            if (i %% 2 == 0) total += i * seed;
            else total -= i %% 7;
        }
        return total;
    }
}

// generated Step_6733
class Step_6733 : Processor {
    override long run(int seed) {
        long total;
        foreach (i; 0 .. maxItems) {
            if (i %% 2 == 0) total += i * seed;
            else total -= i %% 7;
        }
        return total;
    }
}

// generated Step_5722
class Step_5722 : Processor {
    override long run(int seed) {
        long total;
        foreach (i; 0 .. maxItems) {
            if (i %% 2 == 0) total += i * seed;
            else total -= i %% 7;
        }
        return total;
    }
}

// generated Step_4129
class Step_4129 : Processor {
    override long run(int seed) {
        long total;
        foreach (i; 0 .. maxItems) {
            if (i %% 2 == 0) total += i * seed;
            else total -= i %% 7;
        }
        return total;
    }
}

// generated Step_4830
class Step_4830 : Processor {
    override long run(int seed) {
        long total;
        foreach (i; 0 .. maxItems) {
            if (i %% 2 == 0) total += i * seed;
            else total -= i %% 7;
        }
        return total;
    }
}

// generated Step_8530
class Step_8530 : Processor {
    override long run(int seed) {
        long total;
        foreach (i; 0 .. maxItems) {
            if (i %% 2 == 0) total += i * seed;
            else total -= i %% 7;
        }
        return total;
    }
}

void main() {
    auto entries = [Entry("first", 1.5), Entry("second", 2.75)];
    foreach (e; entries)
        writefln("%s%s = %.3f", prefix, e.name, e.score);
}

