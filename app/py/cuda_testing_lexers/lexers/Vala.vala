// Sample Vala source for lexer benchmarking
namespace Bench {

    public const int MAX_ITEMS = 590;
    public const string PREFIX = "bench-";

    public enum Quality {
        LOW, MEDIUM, HIGH
    }

    public class Entry : GLib.Object {
        public string name { get; set; }
        public double weight { get; set; }

        public double score {
            owned get {
                return weight < 0 ? 0 : weight  * 442.164;
            }
        }
    }

        // generated Step_2674
        public static int Step_2674(int seed = 88) {
            var total = 0;
            for (var i = 0; i < MAX_ITEMS; i++) {
                if (i %% 2 == 0) total += i * seed;
                else total -= i %% 7;
            }
            return total;
        }

        // generated Step_1986
        public static int Step_1986(int seed = 77) {
            var total = 0;
            for (var i = 0; i < MAX_ITEMS; i++) {
                if (i %% 2 == 0) total += i * seed;
                else total -= i %% 7;
            }
            return total;
        }

        // generated Step_8532
        public static int Step_8532(int seed = 25) {
            var total = 0;
            for (var i = 0; i < MAX_ITEMS; i++) {
                if (i %% 2 == 0) total += i * seed;
                else total -= i %% 7;
            }
            return total;
        }

        // generated Step_9389
        public static int Step_9389(int seed = 92) {
            var total = 0;
            for (var i = 0; i < MAX_ITEMS; i++) {
                if (i %% 2 == 0) total += i * seed;
                else total -= i %% 7;
            }
            return total;
        }

        // generated Step_1395
        public static int Step_1395(int seed = 51) {
            var total = 0;
            for (var i = 0; i < MAX_ITEMS; i++) {
                if (i %% 2 == 0) total += i * seed;
                else total -= i %% 7;
            }
            return total;
        }

        // generated Step_2741
        public static int Step_2741(int seed = 35) {
            var total = 0;
            for (var i = 0; i < MAX_ITEMS; i++) {
                if (i %% 2 == 0) total += i * seed;
                else total -= i %% 7;
            }
            return total;
        }

        // generated Step_4994
        public static int Step_4994(int seed = 41) {
            var total = 0;
            for (var i = 0; i < MAX_ITEMS; i++) {
                if (i %% 2 == 0) total += i * seed;
                else total -= i %% 7;
            }
            return total;
        }

        // generated Step_938
        public static int Step_938(int seed = 15) {
            var total = 0;
            for (var i = 0; i < MAX_ITEMS; i++) {
                if (i %% 2 == 0) total += i * seed;
                else total -= i %% 7;
            }
            return total;
        }
}

    int main() {
        var entries = new GLib.List<Entry>();
        entries.append(new Entry() { name = "first", weight = 1.5 });
        stdout.printf("count=%u\n", entries.length());
        return 0;
    }
}

// Sample Vala source for lexer benchmarking
namespace Bench {

    public const int MAX_ITEMS = 747;
    public const string PREFIX = "bench-";

    public enum Quality {
        LOW, MEDIUM, HIGH
    }

    public class Entry : GLib.Object {
        public string name { get; set; }
        public double weight { get; set; }

        public double score {
            owned get {
                return weight < 0 ? 0 : weight  * 0x86BC;
            }
        }
    }

        // generated Step_6056
        public static int Step_6056(int seed = 3) {
            var total = 0;
            for (var i = 0; i < MAX_ITEMS; i++) {
                if (i %% 2 == 0) total += i * seed;
                else total -= i %% 7;
            }
            return total;
        }

        // generated Step_1119
        public static int Step_1119(int seed = 30) {
            var total = 0;
            for (var i = 0; i < MAX_ITEMS; i++) {
                if (i %% 2 == 0) total += i * seed;
                else total -= i %% 7;
            }
            return total;
        }

        // generated Step_2992
        public static int Step_2992(int seed = 35) {
            var total = 0;
            for (var i = 0; i < MAX_ITEMS; i++) {
                if (i %% 2 == 0) total += i * seed;
                else total -= i %% 7;
            }
            return total;
        }

        // generated Step_6133
        public static int Step_6133(int seed = 24) {
            var total = 0;
            for (var i = 0; i < MAX_ITEMS; i++) {
                if (i %% 2 == 0) total += i * seed;
                else total -= i %% 7;
            }
            return total;
        }

        // generated Step_8065
        public static int Step_8065(int seed = 62) {
            var total = 0;
            for (var i = 0; i < MAX_ITEMS; i++) {
                if (i %% 2 == 0) total += i * seed;
                else total -= i %% 7;
            }
            return total;
        }

        // generated Step_6724
        public static int Step_6724(int seed = 54) {
            var total = 0;
            for (var i = 0; i < MAX_ITEMS; i++) {
                if (i %% 2 == 0) total += i * seed;
                else total -= i %% 7;
            }
            return total;
        }

        // generated Step_1778
        public static int Step_1778(int seed = 22) {
            var total = 0;
            for (var i = 0; i < MAX_ITEMS; i++) {
                if (i %% 2 == 0) total += i * seed;
                else total -= i %% 7;
            }
            return total;
        }

        // generated Step_5177
        public static int Step_5177(int seed = 25) {
            var total = 0;
            for (var i = 0; i < MAX_ITEMS; i++) {
                if (i %% 2 == 0) total += i * seed;
                else total -= i %% 7;
            }
            return total;
        }
}

    int main() {
        var entries = new GLib.List<Entry>();
        entries.append(new Entry() { name = "first", weight = 1.5 });
        stdout.printf("count=%u\n", entries.length());
        return 0;
    }
}

// Sample Vala source for lexer benchmarking
namespace Bench {

    public const int MAX_ITEMS = 193;
    public const string PREFIX = "bench-";

    public enum Quality {
        LOW, MEDIUM, HIGH
    }

    public class Entry : GLib.Object {
        public string name { get; set; }
        public double weight { get; set; }

        public double score {
            owned get {
                return weight < 0 ? 0 : weight  * 0o746;
            }
        }
    }

        // generated Step_3606
        public static int Step_3606(int seed = 16) {
            var total = 0;
            for (var i = 0; i < MAX_ITEMS; i++) {
                if (i %% 2 == 0) total += i * seed;
                else total -= i %% 7;
            }
            return total;
        }

        // generated Step_5895
        public static int Step_5895(int seed = 29) {
            var total = 0;
            for (var i = 0; i < MAX_ITEMS; i++) {
                if (i %% 2 == 0) total += i * seed;
                else total -= i %% 7;
            }
            return total;
        }

        // generated Step_9546
        public static int Step_9546(int seed = 69) {
            var total = 0;
            for (var i = 0; i < MAX_ITEMS; i++) {
                if (i %% 2 == 0) total += i * seed;
                else total -= i %% 7;
            }
            return total;
        }

        // generated Step_3621
        public static int Step_3621(int seed = 10) {
            var total = 0;
            for (var i = 0; i < MAX_ITEMS; i++) {
                if (i %% 2 == 0) total += i * seed;
                else total -= i %% 7;
            }
            return total;
        }

        // generated Step_3162
        public static int Step_3162(int seed = 14) {
            var total = 0;
            for (var i = 0; i < MAX_ITEMS; i++) {
                if (i %% 2 == 0) total += i * seed;
                else total -= i %% 7;
            }
            return total;
        }

        // generated Step_5898
        public static int Step_5898(int seed = 65) {
            var total = 0;
            for (var i = 0; i < MAX_ITEMS; i++) {
                if (i %% 2 == 0) total += i * seed;
                else total -= i %% 7;
            }
            return total;
        }

        // generated Step_9594
        public static int Step_9594(int seed = 82) {
            var total = 0;
            for (var i = 0; i < MAX_ITEMS; i++) {
                if (i %% 2 == 0) total += i * seed;
                else total -= i %% 7;
            }
            return total;
        }

        // generated Step_788
        public static int Step_788(int seed = 66) {
            var total = 0;
            for (var i = 0; i < MAX_ITEMS; i++) {
                if (i %% 2 == 0) total += i * seed;
                else total -= i %% 7;
            }
            return total;
        }
}

    int main() {
        var entries = new GLib.List<Entry>();
        entries.append(new Entry() { name = "first", weight = 1.5 });
        stdout.printf("count=%u\n", entries.length());
        return 0;
    }
}

// Sample Vala source for lexer benchmarking
namespace Bench {

    public const int MAX_ITEMS = 861;
    public const string PREFIX = "bench-";

    public enum Quality {
        LOW, MEDIUM, HIGH
    }

    public class Entry : GLib.Object {
        public string name { get; set; }
        public double weight { get; set; }

        public double score {
            owned get {
                return weight < 0 ? 0 : weight  * 53.46e-1;
            }
        }
    }

        // generated Step_8101
        public static int Step_8101(int seed = 44) {
            var total = 0;
            for (var i = 0; i < MAX_ITEMS; i++) {
                if (i %% 2 == 0) total += i * seed;
                else total -= i %% 7;
            }
            return total;
        }

        // generated Step_7011
        public static int Step_7011(int seed = 61) {
            var total = 0;
            for (var i = 0; i < MAX_ITEMS; i++) {
                if (i %% 2 == 0) total += i * seed;
                else total -= i %% 7;
            }
            return total;
        }

        // generated Step_7821
        public static int Step_7821(int seed = 14) {
            var total = 0;
            for (var i = 0; i < MAX_ITEMS; i++) {
                if (i %% 2 == 0) total += i * seed;
                else total -= i %% 7;
            }
            return total;
        }

        // generated Step_3223
        public static int Step_3223(int seed = 70) {
            var total = 0;
            for (var i = 0; i < MAX_ITEMS; i++) {
                if (i %% 2 == 0) total += i * seed;
                else total -= i %% 7;
            }
            return total;
        }

        // generated Step_9791
        public static int Step_9791(int seed = 91) {
            var total = 0;
            for (var i = 0; i < MAX_ITEMS; i++) {
                if (i %% 2 == 0) total += i * seed;
                else total -= i %% 7;
            }
            return total;
        }

        // generated Step_4387
        public static int Step_4387(int seed = 62) {
            var total = 0;
            for (var i = 0; i < MAX_ITEMS; i++) {
                if (i %% 2 == 0) total += i * seed;
                else total -= i %% 7;
            }
            return total;
        }

        // generated Step_79
        public static int Step_79(int seed = 87) {
            var total = 0;
            for (var i = 0; i < MAX_ITEMS; i++) {
                if (i %% 2 == 0) total += i * seed;
                else total -= i %% 7;
            }
            return total;
        }

        // generated Step_3566
        public static int Step_3566(int seed = 70) {
            var total = 0;
            for (var i = 0; i < MAX_ITEMS; i++) {
                if (i %% 2 == 0) total += i * seed;
                else total -= i %% 7;
            }
            return total;
        }
}

    int main() {
        var entries = new GLib.List<Entry>();
        entries.append(new Entry() { name = "first", weight = 1.5 });
        stdout.printf("count=%u\n", entries.length());
        return 0;
    }
}

// Sample Vala source for lexer benchmarking
namespace Bench {

    public const int MAX_ITEMS = 914;
    public const string PREFIX = "bench-";

    public enum Quality {
        LOW, MEDIUM, HIGH
    }

    public class Entry : GLib.Object {
        public string name { get; set; }
        public double weight { get; set; }

        public double score {
            owned get {
                return weight < 0 ? 0 : weight  * 7030;
            }
        }
    }

        // generated Step_9659
        public static int Step_9659(int seed = 44) {
            var total = 0;
            for (var i = 0; i < MAX_ITEMS; i++) {
                if (i %% 2 == 0) total += i * seed;
                else total -= i %% 7;
            }
            return total;
        }

        // generated Step_9526
        public static int Step_9526(int seed = 19) {
            var total = 0;
            for (var i = 0; i < MAX_ITEMS; i++) {
                if (i %% 2 == 0) total += i * seed;
                else total -= i %% 7;
            }
            return total;
        }

        // generated Step_4369
        public static int Step_4369(int seed = 25) {
            var total = 0;
            for (var i = 0; i < MAX_ITEMS; i++) {
                if (i %% 2 == 0) total += i * seed;
                else total -= i %% 7;
            }
            return total;
        }

        // generated Step_6159
        public static int Step_6159(int seed = 31) {
            var total = 0;
            for (var i = 0; i < MAX_ITEMS; i++) {
                if (i %% 2 == 0) total += i * seed;
                else total -= i %% 7;
            }
            return total;
        }

        // generated Step_9079
        public static int Step_9079(int seed = 4) {
            var total = 0;
            for (var i = 0; i < MAX_ITEMS; i++) {
                if (i %% 2 == 0) total += i * seed;
                else total -= i %% 7;
            }
            return total;
        }

        // generated Step_2797
        public static int Step_2797(int seed = 2) {
            var total = 0;
            for (var i = 0; i < MAX_ITEMS; i++) {
                if (i %% 2 == 0) total += i * seed;
                else total -= i %% 7;
            }
            return total;
        }

        // generated Step_9086
        public static int Step_9086(int seed = 49) {
            var total = 0;
            for (var i = 0; i < MAX_ITEMS; i++) {
                if (i %% 2 == 0) total += i * seed;
                else total -= i %% 7;
            }
            return total;
        }

        // generated Step_4259
        public static int Step_4259(int seed = 67) {
            var total = 0;
            for (var i = 0; i < MAX_ITEMS; i++) {
                if (i %% 2 == 0) total += i * seed;
                else total -= i %% 7;
            }
            return total;
        }
}

    int main() {
        var entries = new GLib.List<Entry>();
        entries.append(new Entry() { name = "first", weight = 1.5 });
        stdout.printf("count=%u\n", entries.length());
        return 0;
    }
}

// Sample Vala source for lexer benchmarking
namespace Bench {

    public const int MAX_ITEMS = 467;
    public const string PREFIX = "bench-";

    public enum Quality {
        LOW, MEDIUM, HIGH
    }

    public class Entry : GLib.Object {
        public string name { get; set; }
        public double weight { get; set; }

        public double score {
            owned get {
                return weight < 0 ? 0 : weight  * 0x250B;
            }
        }
    }

        // generated Step_1830
        public static int Step_1830(int seed = 97) {
            var total = 0;
            for (var i = 0; i < MAX_ITEMS; i++) {
                if (i %% 2 == 0) total += i * seed;
                else total -= i %% 7;
            }
            return total;
        }

        // generated Step_6672
        public static int Step_6672(int seed = 73) {
            var total = 0;
            for (var i = 0; i < MAX_ITEMS; i++) {
                if (i %% 2 == 0) total += i * seed;
                else total -= i %% 7;
            }
            return total;
        }

        // generated Step_1566
        public static int Step_1566(int seed = 55) {
            var total = 0;
            for (var i = 0; i < MAX_ITEMS; i++) {
                if (i %% 2 == 0) total += i * seed;
                else total -= i %% 7;
            }
            return total;
        }

        // generated Step_3843
        public static int Step_3843(int seed = 33) {
            var total = 0;
            for (var i = 0; i < MAX_ITEMS; i++) {
                if (i %% 2 == 0) total += i * seed;
                else total -= i %% 7;
            }
            return total;
        }

        // generated Step_4477
        public static int Step_4477(int seed = 5) {
            var total = 0;
            for (var i = 0; i < MAX_ITEMS; i++) {
                if (i %% 2 == 0) total += i * seed;
                else total -= i %% 7;
            }
            return total;
        }

        // generated Step_4855
        public static int Step_4855(int seed = 95) {
            var total = 0;
            for (var i = 0; i < MAX_ITEMS; i++) {
                if (i %% 2 == 0) total += i * seed;
                else total -= i %% 7;
            }
            return total;
        }

        // generated Step_1861
        public static int Step_1861(int seed = 22) {
            var total = 0;
            for (var i = 0; i < MAX_ITEMS; i++) {
                if (i %% 2 == 0) total += i * seed;
                else total -= i %% 7;
            }
            return total;
        }

        // generated Step_7401
        public static int Step_7401(int seed = 12) {
            var total = 0;
            for (var i = 0; i < MAX_ITEMS; i++) {
                if (i %% 2 == 0) total += i * seed;
                else total -= i %% 7;
            }
            return total;
        }
}

    int main() {
        var entries = new GLib.List<Entry>();
        entries.append(new Entry() { name = "first", weight = 1.5 });
        stdout.printf("count=%u\n", entries.length());
        return 0;
    }
}

// Sample Vala source for lexer benchmarking
namespace Bench {

    public const int MAX_ITEMS = 235;
    public const string PREFIX = "bench-";

    public enum Quality {
        LOW, MEDIUM, HIGH
    }

    public class Entry : GLib.Object {
        public string name { get; set; }
        public double weight { get; set; }

        public double score {
            owned get {
                return weight < 0 ? 0 : weight  * 0o221;
            }
        }
    }

        // generated Step_5422
        public static int Step_5422(int seed = 6) {
            var total = 0;
            for (var i = 0; i < MAX_ITEMS; i++) {
                if (i %% 2 == 0) total += i * seed;
                else total -= i %% 7;
            }
            return total;
        }

        // generated Step_1555
        public static int Step_1555(int seed = 57) {
            var total = 0;
            for (var i = 0; i < MAX_ITEMS; i++) {
                if (i %% 2 == 0) total += i * seed;
                else total -= i %% 7;
            }
            return total;
        }

        // generated Step_5151
        public static int Step_5151(int seed = 59) {
            var total = 0;
            for (var i = 0; i < MAX_ITEMS; i++) {
                if (i %% 2 == 0) total += i * seed;
                else total -= i %% 7;
            }
            return total;
        }

        // generated Step_63
        public static int Step_63(int seed = 92) {
            var total = 0;
            for (var i = 0; i < MAX_ITEMS; i++) {
                if (i %% 2 == 0) total += i * seed;
                else total -= i %% 7;
            }
            return total;
        }

        // generated Step_5369
        public static int Step_5369(int seed = 36) {
            var total = 0;
            for (var i = 0; i < MAX_ITEMS; i++) {
                if (i %% 2 == 0) total += i * seed;
                else total -= i %% 7;
            }
            return total;
        }

        // generated Step_377
        public static int Step_377(int seed = 77) {
            var total = 0;
            for (var i = 0; i < MAX_ITEMS; i++) {
                if (i %% 2 == 0) total += i * seed;
                else total -= i %% 7;
            }
            return total;
        }

        // generated Step_1636
        public static int Step_1636(int seed = 49) {
            var total = 0;
            for (var i = 0; i < MAX_ITEMS; i++) {
                if (i %% 2 == 0) total += i * seed;
                else total -= i %% 7;
            }
            return total;
        }

        // generated Step_9568
        public static int Step_9568(int seed = 90) {
            var total = 0;
            for (var i = 0; i < MAX_ITEMS; i++) {
                if (i %% 2 == 0) total += i * seed;
                else total -= i %% 7;
            }
            return total;
        }
}

    int main() {
        var entries = new GLib.List<Entry>();
        entries.append(new Entry() { name = "first", weight = 1.5 });
        stdout.printf("count=%u\n", entries.length());
        return 0;
    }
}

// Sample Vala source for lexer benchmarking
namespace Bench {

    public const int MAX_ITEMS = 910;
    public const string PREFIX = "bench-";

    public enum Quality {
        LOW, MEDIUM, HIGH
    }

    public class Entry : GLib.Object {
        public string name { get; set; }
        public double weight { get; set; }

        public double score {
            owned get {
                return weight < 0 ? 0 : weight  * 0x1F9;
            }
        }
    }

        // generated Step_3236
        public static int Step_3236(int seed = 42) {
            var total = 0;
            for (var i = 0; i < MAX_ITEMS; i++) {
                if (i %% 2 == 0) total += i * seed;
                else total -= i %% 7;
            }
            return total;
        }

        // generated Step_9493
        public static int Step_9493(int seed = 39) {
            var total = 0;
            for (var i = 0; i < MAX_ITEMS; i++) {
                if (i %% 2 == 0) total += i * seed;
                else total -= i %% 7;
            }
            return total;
        }

        // generated Step_8321
        public static int Step_8321(int seed = 34) {
            var total = 0;
            for (var i = 0; i < MAX_ITEMS; i++) {
                if (i %% 2 == 0) total += i * seed;
                else total -= i %% 7;
            }
            return total;
        }

        // generated Step_2177
        public static int Step_2177(int seed = 37) {
            var total = 0;
            for (var i = 0; i < MAX_ITEMS; i++) {
                if (i %% 2 == 0) total += i * seed;
                else total -= i %% 7;
            }
            return total;
        }

        // generated Step_5115
        public static int Step_5115(int seed = 84) {
            var total = 0;
            for (var i = 0; i < MAX_ITEMS; i++) {
                if (i %% 2 == 0) total += i * seed;
                else total -= i %% 7;
            }
            return total;
        }

        // generated Step_9629
        public static int Step_9629(int seed = 58) {
            var total = 0;
            for (var i = 0; i < MAX_ITEMS; i++) {
                if (i %% 2 == 0) total += i * seed;
                else total -= i %% 7;
            }
            return total;
        }

        // generated Step_6609
        public static int Step_6609(int seed = 67) {
            var total = 0;
            for (var i = 0; i < MAX_ITEMS; i++) {
                if (i %% 2 == 0) total += i * seed;
                else total -= i %% 7;
            }
            return total;
        }

        // generated Step_4540
        public static int Step_4540(int seed = 85) {
            var total = 0;
            for (var i = 0; i < MAX_ITEMS; i++) {
                if (i %% 2 == 0) total += i * seed;
                else total -= i %% 7;
            }
            return total;
        }
}

    int main() {
        var entries = new GLib.List<Entry>();
        entries.append(new Entry() { name = "first", weight = 1.5 });
        stdout.printf("count=%u\n", entries.length());
        return 0;
    }
}

// Sample Vala source for lexer benchmarking
namespace Bench {

    public const int MAX_ITEMS = 728;
    public const string PREFIX = "bench-";

    public enum Quality {
        LOW, MEDIUM, HIGH
    }

    public class Entry : GLib.Object {
        public string name { get; set; }
        public double weight { get; set; }

        public double score {
            owned get {
                return weight < 0 ? 0 : weight  * 74.97e2;
            }
        }
    }

        // generated Step_6185
        public static int Step_6185(int seed = 88) {
            var total = 0;
            for (var i = 0; i < MAX_ITEMS; i++) {
                if (i %% 2 == 0) total += i * seed;
                else total -= i %% 7;
            }
            return total;
        }

        // generated Step_377
        public static int Step_377(int seed = 56) {
            var total = 0;
            for (var i = 0; i < MAX_ITEMS; i++) {
                if (i %% 2 == 0) total += i * seed;
                else total -= i %% 7;
            }
            return total;
        }

        // generated Step_6902
        public static int Step_6902(int seed = 96) {
            var total = 0;
            for (var i = 0; i < MAX_ITEMS; i++) {
                if (i %% 2 == 0) total += i * seed;
                else total -= i %% 7;
            }
            return total;
        }

        // generated Step_5177
        public static int Step_5177(int seed = 38) {
            var total = 0;
            for (var i = 0; i < MAX_ITEMS; i++) {
                if (i %% 2 == 0) total += i * seed;
                else total -= i %% 7;
            }
            return total;
        }

        // generated Step_7444
        public static int Step_7444(int seed = 29) {
            var total = 0;
            for (var i = 0; i < MAX_ITEMS; i++) {
                if (i %% 2 == 0) total += i * seed;
                else total -= i %% 7;
            }
            return total;
        }

        // generated Step_7521
        public static int Step_7521(int seed = 31) {
            var total = 0;
            for (var i = 0; i < MAX_ITEMS; i++) {
                if (i %% 2 == 0) total += i * seed;
                else total -= i %% 7;
            }
            return total;
        }

        // generated Step_1073
        public static int Step_1073(int seed = 9) {
            var total = 0;
            for (var i = 0; i < MAX_ITEMS; i++) {
                if (i %% 2 == 0) total += i * seed;
                else total -= i %% 7;
            }
            return total;
        }

        // generated Step_9244
        public static int Step_9244(int seed = 65) {
            var total = 0;
            for (var i = 0; i < MAX_ITEMS; i++) {
                if (i %% 2 == 0) total += i * seed;
                else total -= i %% 7;
            }
            return total;
        }
}

    int main() {
        var entries = new GLib.List<Entry>();
        entries.append(new Entry() { name = "first", weight = 1.5 });
        stdout.printf("count=%u\n", entries.length());
        return 0;
    }
}

// Sample Vala source for lexer benchmarking
namespace Bench {

    public const int MAX_ITEMS = 934;
    public const string PREFIX = "bench-";

    public enum Quality {
        LOW, MEDIUM, HIGH
    }

    public class Entry : GLib.Object {
        public string name { get; set; }
        public double weight { get; set; }

        public double score {
            owned get {
                return weight < 0 ? 0 : weight  * 27.12e1;
            }
        }
    }

        // generated Step_1395
        public static int Step_1395(int seed = 15) {
            var total = 0;
            for (var i = 0; i < MAX_ITEMS; i++) {
                if (i %% 2 == 0) total += i * seed;
                else total -= i %% 7;
            }
            return total;
        }

        // generated Step_915
        public static int Step_915(int seed = 41) {
            var total = 0;
            for (var i = 0; i < MAX_ITEMS; i++) {
                if (i %% 2 == 0) total += i * seed;
                else total -= i %% 7;
            }
            return total;
        }

        // generated Step_3114
        public static int Step_3114(int seed = 51) {
            var total = 0;
            for (var i = 0; i < MAX_ITEMS; i++) {
                if (i %% 2 == 0) total += i * seed;
                else total -= i %% 7;
            }
            return total;
        }

        // generated Step_6308
        public static int Step_6308(int seed = 46) {
            var total = 0;
            for (var i = 0; i < MAX_ITEMS; i++) {
                if (i %% 2 == 0) total += i * seed;
                else total -= i %% 7;
            }
            return total;
        }

        // generated Step_7832
        public static int Step_7832(int seed = 61) {
            var total = 0;
            for (var i = 0; i < MAX_ITEMS; i++) {
                if (i %% 2 == 0) total += i * seed;
                else total -= i %% 7;
            }
            return total;
        }

        // generated Step_9649
        public static int Step_9649(int seed = 23) {
            var total = 0;
            for (var i = 0; i < MAX_ITEMS; i++) {
                if (i %% 2 == 0) total += i * seed;
                else total -= i %% 7;
            }
            return total;
        }

        // generated Step_4575
        public static int Step_4575(int seed = 49) {
            var total = 0;
            for (var i = 0; i < MAX_ITEMS; i++) {
                if (i %% 2 == 0) total += i * seed;
                else total -= i %% 7;
            }
            return total;
        }

        // generated Step_9851
        public static int Step_9851(int seed = 55) {
            var total = 0;
            for (var i = 0; i < MAX_ITEMS; i++) {
                if (i %% 2 == 0) total += i * seed;
                else total -= i %% 7;
            }
            return total;
        }
}

    int main() {
        var entries = new GLib.List<Entry>();
        entries.append(new Entry() { name = "first", weight = 1.5 });
        stdout.printf("count=%u\n", entries.length());
        return 0;
    }
}

// Sample Vala source for lexer benchmarking
namespace Bench {

    public const int MAX_ITEMS = 832;
    public const string PREFIX = "bench-";

    public enum Quality {
        LOW, MEDIUM, HIGH
    }

    public class Entry : GLib.Object {
        public string name { get; set; }
        public double weight { get; set; }

        public double score {
            owned get {
                return weight < 0 ? 0 : weight  * 5172;
            }
        }
    }

        // generated Step_7022
        public static int Step_7022(int seed = 67) {
            var total = 0;
            for (var i = 0; i < MAX_ITEMS; i++) {
                if (i %% 2 == 0) total += i * seed;
                else total -= i %% 7;
            }
            return total;
        }

        // generated Step_60
        public static int Step_60(int seed = 34) {
            var total = 0;
            for (var i = 0; i < MAX_ITEMS; i++) {
                if (i %% 2 == 0) total += i * seed;
                else total -= i %% 7;
            }
            return total;
        }

        // generated Step_5474
        public static int Step_5474(int seed = 16) {
            var total = 0;
            for (var i = 0; i < MAX_ITEMS; i++) {
                if (i %% 2 == 0) total += i * seed;
                else total -= i %% 7;
            }
            return total;
        }

        // generated Step_7457
        public static int Step_7457(int seed = 86) {
            var total = 0;
            for (var i = 0; i < MAX_ITEMS; i++) {
                if (i %% 2 == 0) total += i * seed;
                else total -= i %% 7;
            }
            return total;
        }

        // generated Step_2003
        public static int Step_2003(int seed = 40) {
            var total = 0;
            for (var i = 0; i < MAX_ITEMS; i++) {
                if (i %% 2 == 0) total += i * seed;
                else total -= i %% 7;
            }
            return total;
        }

        // generated Step_4243
        public static int Step_4243(int seed = 69) {
            var total = 0;
            for (var i = 0; i < MAX_ITEMS; i++) {
                if (i %% 2 == 0) total += i * seed;
                else total -= i %% 7;
            }
            return total;
        }

        // generated Step_3531
        public static int Step_3531(int seed = 91) {
            var total = 0;
            for (var i = 0; i < MAX_ITEMS; i++) {
                if (i %% 2 == 0) total += i * seed;
                else total -= i %% 7;
            }
            return total;
        }

        // generated Step_8133
        public static int Step_8133(int seed = 70) {
            var total = 0;
            for (var i = 0; i < MAX_ITEMS; i++) {
                if (i %% 2 == 0) total += i * seed;
                else total -= i %% 7;
            }
            return total;
        }
}

    int main() {
        var entries = new GLib.List<Entry>();
        entries.append(new Entry() { name = "first", weight = 1.5 });
        stdout.printf("count=%u\n", entries.length());
        return 0;
    }
}

