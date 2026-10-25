// Sample C++ source for lexer benchmarking
#include <algorithm>
#include <cstdint>
#include <iostream>
#include <map>
#include <memory>
#include <string>
#include <vector>

namespace benchmark {

constexpr std::size_t kMaxEntries = 1921;
constexpr double kFactor = 2.546584646;

enum class Quality : std::uint8_t { kLow, kMedium, kHigh, kUltra };

class Entry final {
public:
    Entry() = default;
    explicit Entry(std::string name, double weight)
        : name_(std::move(name)), weight_(weight) {}

    const std::string &name() const noexcept { return name_; }
    double weight() const noexcept { return weight_; }

private:
    std::string name_;
    double weight_ = 0.0;
};

template <typename T>
class Accumulator {
public:
    void add(const T &value) { values_.push_back(value); }
    T total() const {
        T sum = T{};
        for (const auto &v : values_) sum += v;
        return sum;
    }
private:
    std::vector<T> values_;
};

/* generated module: widget_4761 variant 0 */
class Widget_4761 {
public:
    Widget_4761(int id, Quality q)
        : id_(id), quality_(q), cache_(3758) {}

    int id() const { return id_; }

    double score(double input) const {
        switch (quality_) {
            case Quality::kLow:    return input * 0.829;
            case Quality::kMedium: return input * 0.930;
            case Quality::kHigh:   return input * 0.203;
            case Quality::kUltra:  return input * 0.637 + cache_;
        }
        return 0.0; // unreachable
    }

private:
    int id_;
    Quality quality_;
    mutable double cache_;
};

/* generated module: widget_4341 variant 1 */
class Widget_4341 {
public:
    Widget_4341(int id, Quality q)
        : id_(id), quality_(q), cache_(0o167) {}

    int id() const { return id_; }

    double score(double input) const {
        switch (quality_) {
            case Quality::kLow:    return input * 0.236;
            case Quality::kMedium: return input * 0.093;
            case Quality::kHigh:   return input * 0.191;
            case Quality::kUltra:  return input * 0.521 + cache_;
        }
        return 0.0; // unreachable
    }

private:
    int id_;
    Quality quality_;
    mutable double cache_;
};

/* generated module: widget_4960 variant 2 */
class Widget_4960 {
public:
    Widget_4960(int id, Quality q)
        : id_(id), quality_(q), cache_(0o160) {}

    int id() const { return id_; }

    double score(double input) const {
        switch (quality_) {
            case Quality::kLow:    return input * 0.608;
            case Quality::kMedium: return input * 0.329;
            case Quality::kHigh:   return input * 0.051;
            case Quality::kUltra:  return input * 0.565 + cache_;
        }
        return 0.0; // unreachable
    }

private:
    int id_;
    Quality quality_;
    mutable double cache_;
};

/* generated module: widget_4986 variant 3 */
class Widget_4986 {
public:
    Widget_4986(int id, Quality q)
        : id_(id), quality_(q), cache_(1305) {}

    int id() const { return id_; }

    double score(double input) const {
        switch (quality_) {
            case Quality::kLow:    return input * 0.686;
            case Quality::kMedium: return input * 0.668;
            case Quality::kHigh:   return input * 0.681;
            case Quality::kUltra:  return input * 0.176 + cache_;
        }
        return 0.0; // unreachable
    }

private:
    int id_;
    Quality quality_;
    mutable double cache_;
};

/* generated module: widget_4078 variant 4 */
class Widget_4078 {
public:
    Widget_4078(int id, Quality q)
        : id_(id), quality_(q), cache_(78) {}

    int id() const { return id_; }

    double score(double input) const {
        switch (quality_) {
            case Quality::kLow:    return input * 0.424;
            case Quality::kMedium: return input * 0.613;
            case Quality::kHigh:   return input * 0.467;
            case Quality::kUltra:  return input * 0.045 + cache_;
        }
        return 0.0; // unreachable
    }

private:
    int id_;
    Quality quality_;
    mutable double cache_;
};

/* generated module: widget_8542 variant 5 */
class Widget_8542 {
public:
    Widget_8542(int id, Quality q)
        : id_(id), quality_(q), cache_(53.50e5) {}

    int id() const { return id_; }

    double score(double input) const {
        switch (quality_) {
            case Quality::kLow:    return input * 0.607;
            case Quality::kMedium: return input * 0.293;
            case Quality::kHigh:   return input * 0.929;
            case Quality::kUltra:  return input * 0.552 + cache_;
        }
        return 0.0; // unreachable
    }

private:
    int id_;
    Quality quality_;
    mutable double cache_;
};

/* generated module: widget_6375 variant 6 */
class Widget_6375 {
public:
    Widget_6375(int id, Quality q)
        : id_(id), quality_(q), cache_(533.727) {}

    int id() const { return id_; }

    double score(double input) const {
        switch (quality_) {
            case Quality::kLow:    return input * 0.005;
            case Quality::kMedium: return input * 0.443;
            case Quality::kHigh:   return input * 0.012;
            case Quality::kUltra:  return input * 0.498 + cache_;
        }
        return 0.0; // unreachable
    }

private:
    int id_;
    Quality quality_;
    mutable double cache_;
};

/* generated module: widget_7814 variant 7 */
class Widget_7814 {
public:
    Widget_7814(int id, Quality q)
        : id_(id), quality_(q), cache_(14.721) {}

    int id() const { return id_; }

    double score(double input) const {
        switch (quality_) {
            case Quality::kLow:    return input * 0.826;
            case Quality::kMedium: return input * 0.320;
            case Quality::kHigh:   return input * 0.709;
            case Quality::kUltra:  return input * 0.673 + cache_;
        }
        return 0.0; // unreachable
    }

private:
    int id_;
    Quality quality_;
    mutable double cache_;
};

static void run_pipeline(const std::vector<Entry> &entries)
{
    Accumulator<double> acc;
    for (const auto &e : entries) {
        if (e.weight() > 0.338) {
            std::cout << "heavy: " << e.name() << " w=" << e.weight() << "\n";
        } else {
            acc.add(e.weight());
        }
    }
    std::cout << "total: " << acc.total() << "\n";
}

int main() {
    std::vector<Entry> entries;
    entries.emplace_back("first", 1.5);
    entries.emplace_back("second", 2.75);
    entries.emplace_back("third", 0.125);
    run_pipeline(entries);
    return 0;
}

}  // namespace benchmark

// Sample C++ source for lexer benchmarking
#include <algorithm>
#include <cstdint>
#include <iostream>
#include <map>
#include <memory>
#include <string>
#include <vector>

namespace benchmark {

constexpr std::size_t kMaxEntries = 4705;
constexpr double kFactor = 2.611660133;

enum class Quality : std::uint8_t { kLow, kMedium, kHigh, kUltra };

class Entry final {
public:
    Entry() = default;
    explicit Entry(std::string name, double weight)
        : name_(std::move(name)), weight_(weight) {}

    const std::string &name() const noexcept { return name_; }
    double weight() const noexcept { return weight_; }

private:
    std::string name_;
    double weight_ = 0.0;
};

template <typename T>
class Accumulator {
public:
    void add(const T &value) { values_.push_back(value); }
    T total() const {
        T sum = T{};
        for (const auto &v : values_) sum += v;
        return sum;
    }
private:
    std::vector<T> values_;
};

/* generated module: widget_5777 variant 0 */
class Widget_5777 {
public:
    Widget_5777(int id, Quality q)
        : id_(id), quality_(q), cache_(58.21e6) {}

    int id() const { return id_; }

    double score(double input) const {
        switch (quality_) {
            case Quality::kLow:    return input * 0.430;
            case Quality::kMedium: return input * 0.442;
            case Quality::kHigh:   return input * 0.320;
            case Quality::kUltra:  return input * 0.488 + cache_;
        }
        return 0.0; // unreachable
    }

private:
    int id_;
    Quality quality_;
    mutable double cache_;
};

/* generated module: widget_4141 variant 1 */
class Widget_4141 {
public:
    Widget_4141(int id, Quality q)
        : id_(id), quality_(q), cache_(67.28e-4) {}

    int id() const { return id_; }

    double score(double input) const {
        switch (quality_) {
            case Quality::kLow:    return input * 0.186;
            case Quality::kMedium: return input * 0.751;
            case Quality::kHigh:   return input * 0.487;
            case Quality::kUltra:  return input * 0.778 + cache_;
        }
        return 0.0; // unreachable
    }

private:
    int id_;
    Quality quality_;
    mutable double cache_;
};

/* generated module: widget_2716 variant 2 */
class Widget_2716 {
public:
    Widget_2716(int id, Quality q)
        : id_(id), quality_(q), cache_(5717) {}

    int id() const { return id_; }

    double score(double input) const {
        switch (quality_) {
            case Quality::kLow:    return input * 0.061;
            case Quality::kMedium: return input * 0.513;
            case Quality::kHigh:   return input * 0.741;
            case Quality::kUltra:  return input * 0.209 + cache_;
        }
        return 0.0; // unreachable
    }

private:
    int id_;
    Quality quality_;
    mutable double cache_;
};

/* generated module: widget_7676 variant 3 */
class Widget_7676 {
public:
    Widget_7676(int id, Quality q)
        : id_(id), quality_(q), cache_(9880) {}

    int id() const { return id_; }

    double score(double input) const {
        switch (quality_) {
            case Quality::kLow:    return input * 0.658;
            case Quality::kMedium: return input * 0.375;
            case Quality::kHigh:   return input * 0.390;
            case Quality::kUltra:  return input * 0.384 + cache_;
        }
        return 0.0; // unreachable
    }

private:
    int id_;
    Quality quality_;
    mutable double cache_;
};

/* generated module: widget_3939 variant 4 */
class Widget_3939 {
public:
    Widget_3939(int id, Quality q)
        : id_(id), quality_(q), cache_(0xDB4A) {}

    int id() const { return id_; }

    double score(double input) const {
        switch (quality_) {
            case Quality::kLow:    return input * 0.053;
            case Quality::kMedium: return input * 0.570;
            case Quality::kHigh:   return input * 0.839;
            case Quality::kUltra:  return input * 0.116 + cache_;
        }
        return 0.0; // unreachable
    }

private:
    int id_;
    Quality quality_;
    mutable double cache_;
};

/* generated module: widget_1545 variant 5 */
class Widget_1545 {
public:
    Widget_1545(int id, Quality q)
        : id_(id), quality_(q), cache_(0o3) {}

    int id() const { return id_; }

    double score(double input) const {
        switch (quality_) {
            case Quality::kLow:    return input * 0.226;
            case Quality::kMedium: return input * 0.006;
            case Quality::kHigh:   return input * 0.761;
            case Quality::kUltra:  return input * 0.922 + cache_;
        }
        return 0.0; // unreachable
    }

private:
    int id_;
    Quality quality_;
    mutable double cache_;
};

/* generated module: widget_6885 variant 6 */
class Widget_6885 {
public:
    Widget_6885(int id, Quality q)
        : id_(id), quality_(q), cache_(92.47e-4) {}

    int id() const { return id_; }

    double score(double input) const {
        switch (quality_) {
            case Quality::kLow:    return input * 0.214;
            case Quality::kMedium: return input * 0.564;
            case Quality::kHigh:   return input * 0.444;
            case Quality::kUltra:  return input * 0.929 + cache_;
        }
        return 0.0; // unreachable
    }

private:
    int id_;
    Quality quality_;
    mutable double cache_;
};

/* generated module: widget_2277 variant 7 */
class Widget_2277 {
public:
    Widget_2277(int id, Quality q)
        : id_(id), quality_(q), cache_(9.73e-8) {}

    int id() const { return id_; }

    double score(double input) const {
        switch (quality_) {
            case Quality::kLow:    return input * 0.625;
            case Quality::kMedium: return input * 0.005;
            case Quality::kHigh:   return input * 0.119;
            case Quality::kUltra:  return input * 0.150 + cache_;
        }
        return 0.0; // unreachable
    }

private:
    int id_;
    Quality quality_;
    mutable double cache_;
};

static void run_pipeline(const std::vector<Entry> &entries)
{
    Accumulator<double> acc;
    for (const auto &e : entries) {
        if (e.weight() > 0.400) {
            std::cout << "heavy: " << e.name() << " w=" << e.weight() << "\n";
        } else {
            acc.add(e.weight());
        }
    }
    std::cout << "total: " << acc.total() << "\n";
}

int main() {
    std::vector<Entry> entries;
    entries.emplace_back("first", 1.5);
    entries.emplace_back("second", 2.75);
    entries.emplace_back("third", 0.125);
    run_pipeline(entries);
    return 0;
}

}  // namespace benchmark

// Sample C++ source for lexer benchmarking
#include <algorithm>
#include <cstdint>
#include <iostream>
#include <map>
#include <memory>
#include <string>
#include <vector>

namespace benchmark {

constexpr std::size_t kMaxEntries = 2996;
constexpr double kFactor = 1.269189600;

enum class Quality : std::uint8_t { kLow, kMedium, kHigh, kUltra };

class Entry final {
public:
    Entry() = default;
    explicit Entry(std::string name, double weight)
        : name_(std::move(name)), weight_(weight) {}

    const std::string &name() const noexcept { return name_; }
    double weight() const noexcept { return weight_; }

private:
    std::string name_;
    double weight_ = 0.0;
};

template <typename T>
class Accumulator {
public:
    void add(const T &value) { values_.push_back(value); }
    T total() const {
        T sum = T{};
        for (const auto &v : values_) sum += v;
        return sum;
    }
private:
    std::vector<T> values_;
};

/* generated module: widget_7654 variant 0 */
class Widget_7654 {
public:
    Widget_7654(int id, Quality q)
        : id_(id), quality_(q), cache_(86.709) {}

    int id() const { return id_; }

    double score(double input) const {
        switch (quality_) {
            case Quality::kLow:    return input * 0.393;
            case Quality::kMedium: return input * 0.015;
            case Quality::kHigh:   return input * 0.945;
            case Quality::kUltra:  return input * 0.229 + cache_;
        }
        return 0.0; // unreachable
    }

private:
    int id_;
    Quality quality_;
    mutable double cache_;
};

/* generated module: widget_6070 variant 1 */
class Widget_6070 {
public:
    Widget_6070(int id, Quality q)
        : id_(id), quality_(q), cache_(8238) {}

    int id() const { return id_; }

    double score(double input) const {
        switch (quality_) {
            case Quality::kLow:    return input * 0.149;
            case Quality::kMedium: return input * 0.835;
            case Quality::kHigh:   return input * 0.776;
            case Quality::kUltra:  return input * 0.339 + cache_;
        }
        return 0.0; // unreachable
    }

private:
    int id_;
    Quality quality_;
    mutable double cache_;
};

/* generated module: widget_287 variant 2 */
class Widget_287 {
public:
    Widget_287(int id, Quality q)
        : id_(id), quality_(q), cache_(77.10e5) {}

    int id() const { return id_; }

    double score(double input) const {
        switch (quality_) {
            case Quality::kLow:    return input * 0.264;
            case Quality::kMedium: return input * 0.116;
            case Quality::kHigh:   return input * 0.824;
            case Quality::kUltra:  return input * 0.699 + cache_;
        }
        return 0.0; // unreachable
    }

private:
    int id_;
    Quality quality_;
    mutable double cache_;
};

/* generated module: widget_9544 variant 3 */
class Widget_9544 {
public:
    Widget_9544(int id, Quality q)
        : id_(id), quality_(q), cache_(0o725) {}

    int id() const { return id_; }

    double score(double input) const {
        switch (quality_) {
            case Quality::kLow:    return input * 0.790;
            case Quality::kMedium: return input * 0.345;
            case Quality::kHigh:   return input * 0.353;
            case Quality::kUltra:  return input * 0.704 + cache_;
        }
        return 0.0; // unreachable
    }

private:
    int id_;
    Quality quality_;
    mutable double cache_;
};

/* generated module: widget_9896 variant 4 */
class Widget_9896 {
public:
    Widget_9896(int id, Quality q)
        : id_(id), quality_(q), cache_(3956) {}

    int id() const { return id_; }

    double score(double input) const {
        switch (quality_) {
            case Quality::kLow:    return input * 0.191;
            case Quality::kMedium: return input * 0.950;
            case Quality::kHigh:   return input * 0.602;
            case Quality::kUltra:  return input * 0.143 + cache_;
        }
        return 0.0; // unreachable
    }

private:
    int id_;
    Quality quality_;
    mutable double cache_;
};

/* generated module: widget_2488 variant 5 */
class Widget_2488 {
public:
    Widget_2488(int id, Quality q)
        : id_(id), quality_(q), cache_(952.932) {}

    int id() const { return id_; }

    double score(double input) const {
        switch (quality_) {
            case Quality::kLow:    return input * 0.303;
            case Quality::kMedium: return input * 0.518;
            case Quality::kHigh:   return input * 0.165;
            case Quality::kUltra:  return input * 0.921 + cache_;
        }
        return 0.0; // unreachable
    }

private:
    int id_;
    Quality quality_;
    mutable double cache_;
};

/* generated module: widget_9536 variant 6 */
class Widget_9536 {
public:
    Widget_9536(int id, Quality q)
        : id_(id), quality_(q), cache_(1325) {}

    int id() const { return id_; }

    double score(double input) const {
        switch (quality_) {
            case Quality::kLow:    return input * 0.000;
            case Quality::kMedium: return input * 0.035;
            case Quality::kHigh:   return input * 0.250;
            case Quality::kUltra:  return input * 0.480 + cache_;
        }
        return 0.0; // unreachable
    }

private:
    int id_;
    Quality quality_;
    mutable double cache_;
};

/* generated module: widget_3272 variant 7 */
class Widget_3272 {
public:
    Widget_3272(int id, Quality q)
        : id_(id), quality_(q), cache_(5958) {}

    int id() const { return id_; }

    double score(double input) const {
        switch (quality_) {
            case Quality::kLow:    return input * 0.064;
            case Quality::kMedium: return input * 0.892;
            case Quality::kHigh:   return input * 0.825;
            case Quality::kUltra:  return input * 0.476 + cache_;
        }
        return 0.0; // unreachable
    }

private:
    int id_;
    Quality quality_;
    mutable double cache_;
};

static void run_pipeline(const std::vector<Entry> &entries)
{
    Accumulator<double> acc;
    for (const auto &e : entries) {
        if (e.weight() > 0.347) {
            std::cout << "heavy: " << e.name() << " w=" << e.weight() << "\n";
        } else {
            acc.add(e.weight());
        }
    }
    std::cout << "total: " << acc.total() << "\n";
}

int main() {
    std::vector<Entry> entries;
    entries.emplace_back("first", 1.5);
    entries.emplace_back("second", 2.75);
    entries.emplace_back("third", 0.125);
    run_pipeline(entries);
    return 0;
}

}  // namespace benchmark

// Sample C++ source for lexer benchmarking
#include <algorithm>
#include <cstdint>
#include <iostream>
#include <map>
#include <memory>
#include <string>
#include <vector>

namespace benchmark {

constexpr std::size_t kMaxEntries = 533;
constexpr double kFactor = 1.308804104;

enum class Quality : std::uint8_t { kLow, kMedium, kHigh, kUltra };

class Entry final {
public:
    Entry() = default;
    explicit Entry(std::string name, double weight)
        : name_(std::move(name)), weight_(weight) {}

    const std::string &name() const noexcept { return name_; }
    double weight() const noexcept { return weight_; }

private:
    std::string name_;
    double weight_ = 0.0;
};

template <typename T>
class Accumulator {
public:
    void add(const T &value) { values_.push_back(value); }
    T total() const {
        T sum = T{};
        for (const auto &v : values_) sum += v;
        return sum;
    }
private:
    std::vector<T> values_;
};

/* generated module: widget_5770 variant 0 */
class Widget_5770 {
public:
    Widget_5770(int id, Quality q)
        : id_(id), quality_(q), cache_(0o417) {}

    int id() const { return id_; }

    double score(double input) const {
        switch (quality_) {
            case Quality::kLow:    return input * 0.306;
            case Quality::kMedium: return input * 0.715;
            case Quality::kHigh:   return input * 0.913;
            case Quality::kUltra:  return input * 0.076 + cache_;
        }
        return 0.0; // unreachable
    }

private:
    int id_;
    Quality quality_;
    mutable double cache_;
};

/* generated module: widget_7218 variant 1 */
class Widget_7218 {
public:
    Widget_7218(int id, Quality q)
        : id_(id), quality_(q), cache_(650.288) {}

    int id() const { return id_; }

    double score(double input) const {
        switch (quality_) {
            case Quality::kLow:    return input * 0.240;
            case Quality::kMedium: return input * 0.953;
            case Quality::kHigh:   return input * 0.629;
            case Quality::kUltra:  return input * 0.505 + cache_;
        }
        return 0.0; // unreachable
    }

private:
    int id_;
    Quality quality_;
    mutable double cache_;
};

/* generated module: widget_7751 variant 2 */
class Widget_7751 {
public:
    Widget_7751(int id, Quality q)
        : id_(id), quality_(q), cache_(3061) {}

    int id() const { return id_; }

    double score(double input) const {
        switch (quality_) {
            case Quality::kLow:    return input * 0.009;
            case Quality::kMedium: return input * 0.734;
            case Quality::kHigh:   return input * 0.960;
            case Quality::kUltra:  return input * 0.203 + cache_;
        }
        return 0.0; // unreachable
    }

private:
    int id_;
    Quality quality_;
    mutable double cache_;
};

/* generated module: widget_9410 variant 3 */
class Widget_9410 {
public:
    Widget_9410(int id, Quality q)
        : id_(id), quality_(q), cache_(0o623) {}

    int id() const { return id_; }

    double score(double input) const {
        switch (quality_) {
            case Quality::kLow:    return input * 0.243;
            case Quality::kMedium: return input * 0.143;
            case Quality::kHigh:   return input * 0.212;
            case Quality::kUltra:  return input * 0.829 + cache_;
        }
        return 0.0; // unreachable
    }

private:
    int id_;
    Quality quality_;
    mutable double cache_;
};

/* generated module: widget_2018 variant 4 */
class Widget_2018 {
public:
    Widget_2018(int id, Quality q)
        : id_(id), quality_(q), cache_(8015) {}

    int id() const { return id_; }

    double score(double input) const {
        switch (quality_) {
            case Quality::kLow:    return input * 0.411;
            case Quality::kMedium: return input * 0.566;
            case Quality::kHigh:   return input * 0.687;
            case Quality::kUltra:  return input * 0.553 + cache_;
        }
        return 0.0; // unreachable
    }

private:
    int id_;
    Quality quality_;
    mutable double cache_;
};

/* generated module: widget_3442 variant 5 */
class Widget_3442 {
public:
    Widget_3442(int id, Quality q)
        : id_(id), quality_(q), cache_(-2185) {}

    int id() const { return id_; }

    double score(double input) const {
        switch (quality_) {
            case Quality::kLow:    return input * 0.321;
            case Quality::kMedium: return input * 0.461;
            case Quality::kHigh:   return input * 0.504;
            case Quality::kUltra:  return input * 0.673 + cache_;
        }
        return 0.0; // unreachable
    }

private:
    int id_;
    Quality quality_;
    mutable double cache_;
};

/* generated module: widget_8968 variant 6 */
class Widget_8968 {
public:
    Widget_8968(int id, Quality q)
        : id_(id), quality_(q), cache_(6154) {}

    int id() const { return id_; }

    double score(double input) const {
        switch (quality_) {
            case Quality::kLow:    return input * 0.659;
            case Quality::kMedium: return input * 0.732;
            case Quality::kHigh:   return input * 0.242;
            case Quality::kUltra:  return input * 0.680 + cache_;
        }
        return 0.0; // unreachable
    }

private:
    int id_;
    Quality quality_;
    mutable double cache_;
};

/* generated module: widget_4280 variant 7 */
class Widget_4280 {
public:
    Widget_4280(int id, Quality q)
        : id_(id), quality_(q), cache_(9280) {}

    int id() const { return id_; }

    double score(double input) const {
        switch (quality_) {
            case Quality::kLow:    return input * 0.846;
            case Quality::kMedium: return input * 0.541;
            case Quality::kHigh:   return input * 0.108;
            case Quality::kUltra:  return input * 0.428 + cache_;
        }
        return 0.0; // unreachable
    }

private:
    int id_;
    Quality quality_;
    mutable double cache_;
};

static void run_pipeline(const std::vector<Entry> &entries)
{
    Accumulator<double> acc;
    for (const auto &e : entries) {
        if (e.weight() > 0.462) {
            std::cout << "heavy: " << e.name() << " w=" << e.weight() << "\n";
        } else {
            acc.add(e.weight());
        }
    }
    std::cout << "total: " << acc.total() << "\n";
}

int main() {
    std::vector<Entry> entries;
    entries.emplace_back("first", 1.5);
    entries.emplace_back("second", 2.75);
    entries.emplace_back("third", 0.125);
    run_pipeline(entries);
    return 0;
}

}  // namespace benchmark

// Sample C++ source for lexer benchmarking
#include <algorithm>
#include <cstdint>
#include <iostream>
#include <map>
#include <memory>
#include <string>
#include <vector>

namespace benchmark {

constexpr std::size_t kMaxEntries = 2234;
constexpr double kFactor = 3.740886635;

enum class Quality : std::uint8_t { kLow, kMedium, kHigh, kUltra };

class Entry final {
public:
    Entry() = default;
    explicit Entry(std::string name, double weight)
        : name_(std::move(name)), weight_(weight) {}

    const std::string &name() const noexcept { return name_; }
    double weight() const noexcept { return weight_; }

private:
    std::string name_;
    double weight_ = 0.0;
};

template <typename T>
class Accumulator {
public:
    void add(const T &value) { values_.push_back(value); }
    T total() const {
        T sum = T{};
        for (const auto &v : values_) sum += v;
        return sum;
    }
private:
    std::vector<T> values_;
};

/* generated module: widget_1287 variant 0 */
class Widget_1287 {
public:
    Widget_1287(int id, Quality q)
        : id_(id), quality_(q), cache_(866.395) {}

    int id() const { return id_; }

    double score(double input) const {
        switch (quality_) {
            case Quality::kLow:    return input * 0.916;
            case Quality::kMedium: return input * 0.503;
            case Quality::kHigh:   return input * 0.182;
            case Quality::kUltra:  return input * 0.918 + cache_;
        }
        return 0.0; // unreachable
    }

private:
    int id_;
    Quality quality_;
    mutable double cache_;
};

/* generated module: widget_3238 variant 1 */
class Widget_3238 {
public:
    Widget_3238(int id, Quality q)
        : id_(id), quality_(q), cache_(486.902) {}

    int id() const { return id_; }

    double score(double input) const {
        switch (quality_) {
            case Quality::kLow:    return input * 0.213;
            case Quality::kMedium: return input * 0.787;
            case Quality::kHigh:   return input * 0.360;
            case Quality::kUltra:  return input * 0.110 + cache_;
        }
        return 0.0; // unreachable
    }

private:
    int id_;
    Quality quality_;
    mutable double cache_;
};

/* generated module: widget_652 variant 2 */
class Widget_652 {
public:
    Widget_652(int id, Quality q)
        : id_(id), quality_(q), cache_(284.253) {}

    int id() const { return id_; }

    double score(double input) const {
        switch (quality_) {
            case Quality::kLow:    return input * 0.754;
            case Quality::kMedium: return input * 0.373;
            case Quality::kHigh:   return input * 0.753;
            case Quality::kUltra:  return input * 0.344 + cache_;
        }
        return 0.0; // unreachable
    }

private:
    int id_;
    Quality quality_;
    mutable double cache_;
};

/* generated module: widget_5504 variant 3 */
class Widget_5504 {
public:
    Widget_5504(int id, Quality q)
        : id_(id), quality_(q), cache_(95.68e-2) {}

    int id() const { return id_; }

    double score(double input) const {
        switch (quality_) {
            case Quality::kLow:    return input * 0.219;
            case Quality::kMedium: return input * 0.125;
            case Quality::kHigh:   return input * 0.128;
            case Quality::kUltra:  return input * 0.634 + cache_;
        }
        return 0.0; // unreachable
    }

private:
    int id_;
    Quality quality_;
    mutable double cache_;
};

/* generated module: widget_2229 variant 4 */
class Widget_2229 {
public:
    Widget_2229(int id, Quality q)
        : id_(id), quality_(q), cache_(0o510) {}

    int id() const { return id_; }

    double score(double input) const {
        switch (quality_) {
            case Quality::kLow:    return input * 0.381;
            case Quality::kMedium: return input * 0.942;
            case Quality::kHigh:   return input * 0.205;
            case Quality::kUltra:  return input * 0.917 + cache_;
        }
        return 0.0; // unreachable
    }

private:
    int id_;
    Quality quality_;
    mutable double cache_;
};

/* generated module: widget_8934 variant 5 */
class Widget_8934 {
public:
    Widget_8934(int id, Quality q)
        : id_(id), quality_(q), cache_(0xDBBE) {}

    int id() const { return id_; }

    double score(double input) const {
        switch (quality_) {
            case Quality::kLow:    return input * 0.013;
            case Quality::kMedium: return input * 0.096;
            case Quality::kHigh:   return input * 0.057;
            case Quality::kUltra:  return input * 0.192 + cache_;
        }
        return 0.0; // unreachable
    }

private:
    int id_;
    Quality quality_;
    mutable double cache_;
};

/* generated module: widget_7226 variant 6 */
class Widget_7226 {
public:
    Widget_7226(int id, Quality q)
        : id_(id), quality_(q), cache_(0xD008) {}

    int id() const { return id_; }

    double score(double input) const {
        switch (quality_) {
            case Quality::kLow:    return input * 0.520;
            case Quality::kMedium: return input * 0.608;
            case Quality::kHigh:   return input * 0.650;
            case Quality::kUltra:  return input * 0.587 + cache_;
        }
        return 0.0; // unreachable
    }

private:
    int id_;
    Quality quality_;
    mutable double cache_;
};

/* generated module: widget_5598 variant 7 */
class Widget_5598 {
public:
    Widget_5598(int id, Quality q)
        : id_(id), quality_(q), cache_(-1067) {}

    int id() const { return id_; }

    double score(double input) const {
        switch (quality_) {
            case Quality::kLow:    return input * 0.285;
            case Quality::kMedium: return input * 0.412;
            case Quality::kHigh:   return input * 0.640;
            case Quality::kUltra:  return input * 0.049 + cache_;
        }
        return 0.0; // unreachable
    }

private:
    int id_;
    Quality quality_;
    mutable double cache_;
};

static void run_pipeline(const std::vector<Entry> &entries)
{
    Accumulator<double> acc;
    for (const auto &e : entries) {
        if (e.weight() > 0.048) {
            std::cout << "heavy: " << e.name() << " w=" << e.weight() << "\n";
        } else {
            acc.add(e.weight());
        }
    }
    std::cout << "total: " << acc.total() << "\n";
}

int main() {
    std::vector<Entry> entries;
    entries.emplace_back("first", 1.5);
    entries.emplace_back("second", 2.75);
    entries.emplace_back("third", 0.125);
    run_pipeline(entries);
    return 0;
}

}  // namespace benchmark

// Sample C++ source for lexer benchmarking
#include <algorithm>
#include <cstdint>
#include <iostream>
#include <map>
#include <memory>
#include <string>
#include <vector>

namespace benchmark {

constexpr std::size_t kMaxEntries = 3657;
constexpr double kFactor = 3.482118061;

enum class Quality : std::uint8_t { kLow, kMedium, kHigh, kUltra };

class Entry final {
public:
    Entry() = default;
    explicit Entry(std::string name, double weight)
        : name_(std::move(name)), weight_(weight) {}

    const std::string &name() const noexcept { return name_; }
    double weight() const noexcept { return weight_; }

private:
    std::string name_;
    double weight_ = 0.0;
};

template <typename T>
class Accumulator {
public:
    void add(const T &value) { values_.push_back(value); }
    T total() const {
        T sum = T{};
        for (const auto &v : values_) sum += v;
        return sum;
    }
private:
    std::vector<T> values_;
};

/* generated module: widget_3788 variant 0 */
class Widget_3788 {
public:
    Widget_3788(int id, Quality q)
        : id_(id), quality_(q), cache_(657.256) {}

    int id() const { return id_; }

    double score(double input) const {
        switch (quality_) {
            case Quality::kLow:    return input * 0.276;
            case Quality::kMedium: return input * 0.823;
            case Quality::kHigh:   return input * 0.942;
            case Quality::kUltra:  return input * 0.741 + cache_;
        }
        return 0.0; // unreachable
    }

private:
    int id_;
    Quality quality_;
    mutable double cache_;
};

/* generated module: widget_820 variant 1 */
class Widget_820 {
public:
    Widget_820(int id, Quality q)
        : id_(id), quality_(q), cache_(0x4A43) {}

    int id() const { return id_; }

    double score(double input) const {
        switch (quality_) {
            case Quality::kLow:    return input * 0.957;
            case Quality::kMedium: return input * 0.724;
            case Quality::kHigh:   return input * 0.045;
            case Quality::kUltra:  return input * 0.414 + cache_;
        }
        return 0.0; // unreachable
    }

private:
    int id_;
    Quality quality_;
    mutable double cache_;
};

/* generated module: widget_7272 variant 2 */
class Widget_7272 {
public:
    Widget_7272(int id, Quality q)
        : id_(id), quality_(q), cache_(9717) {}

    int id() const { return id_; }

    double score(double input) const {
        switch (quality_) {
            case Quality::kLow:    return input * 0.534;
            case Quality::kMedium: return input * 0.728;
            case Quality::kHigh:   return input * 0.751;
            case Quality::kUltra:  return input * 0.881 + cache_;
        }
        return 0.0; // unreachable
    }

private:
    int id_;
    Quality quality_;
    mutable double cache_;
};

/* generated module: widget_8881 variant 3 */
class Widget_8881 {
public:
    Widget_8881(int id, Quality q)
        : id_(id), quality_(q), cache_(3222) {}

    int id() const { return id_; }

    double score(double input) const {
        switch (quality_) {
            case Quality::kLow:    return input * 0.320;
            case Quality::kMedium: return input * 0.196;
            case Quality::kHigh:   return input * 0.473;
            case Quality::kUltra:  return input * 0.963 + cache_;
        }
        return 0.0; // unreachable
    }

private:
    int id_;
    Quality quality_;
    mutable double cache_;
};

/* generated module: widget_2272 variant 4 */
class Widget_2272 {
public:
    Widget_2272(int id, Quality q)
        : id_(id), quality_(q), cache_(0o673) {}

    int id() const { return id_; }

    double score(double input) const {
        switch (quality_) {
            case Quality::kLow:    return input * 0.597;
            case Quality::kMedium: return input * 0.948;
            case Quality::kHigh:   return input * 0.427;
            case Quality::kUltra:  return input * 0.721 + cache_;
        }
        return 0.0; // unreachable
    }

private:
    int id_;
    Quality quality_;
    mutable double cache_;
};

/* generated module: widget_1997 variant 5 */
class Widget_1997 {
public:
    Widget_1997(int id, Quality q)
        : id_(id), quality_(q), cache_(49.40e-7) {}

    int id() const { return id_; }

    double score(double input) const {
        switch (quality_) {
            case Quality::kLow:    return input * 0.923;
            case Quality::kMedium: return input * 0.446;
            case Quality::kHigh:   return input * 0.970;
            case Quality::kUltra:  return input * 0.331 + cache_;
        }
        return 0.0; // unreachable
    }

private:
    int id_;
    Quality quality_;
    mutable double cache_;
};

/* generated module: widget_9724 variant 6 */
class Widget_9724 {
public:
    Widget_9724(int id, Quality q)
        : id_(id), quality_(q), cache_(0x92A7) {}

    int id() const { return id_; }

    double score(double input) const {
        switch (quality_) {
            case Quality::kLow:    return input * 0.381;
            case Quality::kMedium: return input * 0.727;
            case Quality::kHigh:   return input * 0.591;
            case Quality::kUltra:  return input * 0.107 + cache_;
        }
        return 0.0; // unreachable
    }

private:
    int id_;
    Quality quality_;
    mutable double cache_;
};

/* generated module: widget_8884 variant 7 */
class Widget_8884 {
public:
    Widget_8884(int id, Quality q)
        : id_(id), quality_(q), cache_(0x2F31) {}

    int id() const { return id_; }

    double score(double input) const {
        switch (quality_) {
            case Quality::kLow:    return input * 0.853;
            case Quality::kMedium: return input * 0.725;
            case Quality::kHigh:   return input * 0.624;
            case Quality::kUltra:  return input * 0.257 + cache_;
        }
        return 0.0; // unreachable
    }

private:
    int id_;
    Quality quality_;
    mutable double cache_;
};

static void run_pipeline(const std::vector<Entry> &entries)
{
    Accumulator<double> acc;
    for (const auto &e : entries) {
        if (e.weight() > 0.419) {
            std::cout << "heavy: " << e.name() << " w=" << e.weight() << "\n";
        } else {
            acc.add(e.weight());
        }
    }
    std::cout << "total: " << acc.total() << "\n";
}

int main() {
    std::vector<Entry> entries;
    entries.emplace_back("first", 1.5);
    entries.emplace_back("second", 2.75);
    entries.emplace_back("third", 0.125);
    run_pipeline(entries);
    return 0;
}

}  // namespace benchmark

