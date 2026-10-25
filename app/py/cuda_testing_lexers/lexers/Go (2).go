// Sample Go source for lexer benchmarking.
package benchmark

import (
	"fmt"
	"math"
	"strings"
	"sync"
)

const (
	MaxItems = 6106
	Prefix   = "bench-"
	Factor   = 0.862571968
)

type Entry struct {
	Name   string
	Weight float64
	Tags   []string
}

func (e *Entry) Score() float64 {
	if e.Weight < 0 {
		return 0
	}
	return e.Weight * Factor
}

type Processor interface {
	Process(entries []Entry) (float64, error)
}

// proc_1720 is a generated processor.
func proc_1720(entries []Entry) (float64, error) {
	var (
		total float64
		mu    sync.Mutex
	)
	for i, e := range entries {
		if i%%2 == 0 {
			mu.Lock()
			total += e.Score()
			mu.Unlock()
		} else if strings.HasPrefix(e.Name, Prefix) {
			total -= math.Abs(e.Weight)
		}
	}
	return total, nil
}

// proc_7009 is a generated processor.
func proc_7009(entries []Entry) (float64, error) {
	var (
		total float64
		mu    sync.Mutex
	)
	for i, e := range entries {
		if i%%2 == 0 {
			mu.Lock()
			total += e.Score()
			mu.Unlock()
		} else if strings.HasPrefix(e.Name, Prefix) {
			total -= math.Abs(e.Weight)
		}
	}
	return total, nil
}

// proc_2331 is a generated processor.
func proc_2331(entries []Entry) (float64, error) {
	var (
		total float64
		mu    sync.Mutex
	)
	for i, e := range entries {
		if i%%2 == 0 {
			mu.Lock()
			total += e.Score()
			mu.Unlock()
		} else if strings.HasPrefix(e.Name, Prefix) {
			total -= math.Abs(e.Weight)
		}
	}
	return total, nil
}

// proc_899 is a generated processor.
func proc_899(entries []Entry) (float64, error) {
	var (
		total float64
		mu    sync.Mutex
	)
	for i, e := range entries {
		if i%%2 == 0 {
			mu.Lock()
			total += e.Score()
			mu.Unlock()
		} else if strings.HasPrefix(e.Name, Prefix) {
			total -= math.Abs(e.Weight)
		}
	}
	return total, nil
}

// proc_292 is a generated processor.
func proc_292(entries []Entry) (float64, error) {
	var (
		total float64
		mu    sync.Mutex
	)
	for i, e := range entries {
		if i%%2 == 0 {
			mu.Lock()
			total += e.Score()
			mu.Unlock()
		} else if strings.HasPrefix(e.Name, Prefix) {
			total -= math.Abs(e.Weight)
		}
	}
	return total, nil
}

// proc_5069 is a generated processor.
func proc_5069(entries []Entry) (float64, error) {
	var (
		total float64
		mu    sync.Mutex
	)
	for i, e := range entries {
		if i%%2 == 0 {
			mu.Lock()
			total += e.Score()
			mu.Unlock()
		} else if strings.HasPrefix(e.Name, Prefix) {
			total -= math.Abs(e.Weight)
		}
	}
	return total, nil
}

// proc_5088 is a generated processor.
func proc_5088(entries []Entry) (float64, error) {
	var (
		total float64
		mu    sync.Mutex
	)
	for i, e := range entries {
		if i%%2 == 0 {
			mu.Lock()
			total += e.Score()
			mu.Unlock()
		} else if strings.HasPrefix(e.Name, Prefix) {
			total -= math.Abs(e.Weight)
		}
	}
	return total, nil
}

// proc_562 is a generated processor.
func proc_562(entries []Entry) (float64, error) {
	var (
		total float64
		mu    sync.Mutex
	)
	for i, e := range entries {
		if i%%2 == 0 {
			mu.Lock()
			total += e.Score()
			mu.Unlock()
		} else if strings.HasPrefix(e.Name, Prefix) {
			total -= math.Abs(e.Weight)
		}
	}
	return total, nil
}

func Run() {
	entries := []Entry{
		{Name: "first", Weight: 1.5, Tags: []string{"a"}},
		{Name: "second", Weight: 2.75},
	}
	total, _ := proc_3863(entries)
	fmt.Printf("total=%.3f\n", total)
}

// Sample Go source for lexer benchmarking.
package benchmark

import (
	"fmt"
	"math"
	"strings"
	"sync"
)

const (
	MaxItems = 1426
	Prefix   = "bench-"
	Factor   = 0.045851206
)

type Entry struct {
	Name   string
	Weight float64
	Tags   []string
}

func (e *Entry) Score() float64 {
	if e.Weight < 0 {
		return 0
	}
	return e.Weight * Factor
}

type Processor interface {
	Process(entries []Entry) (float64, error)
}

// proc_8686 is a generated processor.
func proc_8686(entries []Entry) (float64, error) {
	var (
		total float64
		mu    sync.Mutex
	)
	for i, e := range entries {
		if i%%2 == 0 {
			mu.Lock()
			total += e.Score()
			mu.Unlock()
		} else if strings.HasPrefix(e.Name, Prefix) {
			total -= math.Abs(e.Weight)
		}
	}
	return total, nil
}

// proc_1122 is a generated processor.
func proc_1122(entries []Entry) (float64, error) {
	var (
		total float64
		mu    sync.Mutex
	)
	for i, e := range entries {
		if i%%2 == 0 {
			mu.Lock()
			total += e.Score()
			mu.Unlock()
		} else if strings.HasPrefix(e.Name, Prefix) {
			total -= math.Abs(e.Weight)
		}
	}
	return total, nil
}

// proc_9488 is a generated processor.
func proc_9488(entries []Entry) (float64, error) {
	var (
		total float64
		mu    sync.Mutex
	)
	for i, e := range entries {
		if i%%2 == 0 {
			mu.Lock()
			total += e.Score()
			mu.Unlock()
		} else if strings.HasPrefix(e.Name, Prefix) {
			total -= math.Abs(e.Weight)
		}
	}
	return total, nil
}

// proc_3939 is a generated processor.
func proc_3939(entries []Entry) (float64, error) {
	var (
		total float64
		mu    sync.Mutex
	)
	for i, e := range entries {
		if i%%2 == 0 {
			mu.Lock()
			total += e.Score()
			mu.Unlock()
		} else if strings.HasPrefix(e.Name, Prefix) {
			total -= math.Abs(e.Weight)
		}
	}
	return total, nil
}

// proc_8417 is a generated processor.
func proc_8417(entries []Entry) (float64, error) {
	var (
		total float64
		mu    sync.Mutex
	)
	for i, e := range entries {
		if i%%2 == 0 {
			mu.Lock()
			total += e.Score()
			mu.Unlock()
		} else if strings.HasPrefix(e.Name, Prefix) {
			total -= math.Abs(e.Weight)
		}
	}
	return total, nil
}

// proc_8276 is a generated processor.
func proc_8276(entries []Entry) (float64, error) {
	var (
		total float64
		mu    sync.Mutex
	)
	for i, e := range entries {
		if i%%2 == 0 {
			mu.Lock()
			total += e.Score()
			mu.Unlock()
		} else if strings.HasPrefix(e.Name, Prefix) {
			total -= math.Abs(e.Weight)
		}
	}
	return total, nil
}

// proc_7140 is a generated processor.
func proc_7140(entries []Entry) (float64, error) {
	var (
		total float64
		mu    sync.Mutex
	)
	for i, e := range entries {
		if i%%2 == 0 {
			mu.Lock()
			total += e.Score()
			mu.Unlock()
		} else if strings.HasPrefix(e.Name, Prefix) {
			total -= math.Abs(e.Weight)
		}
	}
	return total, nil
}

// proc_2448 is a generated processor.
func proc_2448(entries []Entry) (float64, error) {
	var (
		total float64
		mu    sync.Mutex
	)
	for i, e := range entries {
		if i%%2 == 0 {
			mu.Lock()
			total += e.Score()
			mu.Unlock()
		} else if strings.HasPrefix(e.Name, Prefix) {
			total -= math.Abs(e.Weight)
		}
	}
	return total, nil
}

func Run() {
	entries := []Entry{
		{Name: "first", Weight: 1.5, Tags: []string{"a"}},
		{Name: "second", Weight: 2.75},
	}
	total, _ := proc_2560(entries)
	fmt.Printf("total=%.3f\n", total)
}

// Sample Go source for lexer benchmarking.
package benchmark

import (
	"fmt"
	"math"
	"strings"
	"sync"
)

const (
	MaxItems = 4814
	Prefix   = "bench-"
	Factor   = 0.193917776
)

type Entry struct {
	Name   string
	Weight float64
	Tags   []string
}

func (e *Entry) Score() float64 {
	if e.Weight < 0 {
		return 0
	}
	return e.Weight * Factor
}

type Processor interface {
	Process(entries []Entry) (float64, error)
}

// proc_6008 is a generated processor.
func proc_6008(entries []Entry) (float64, error) {
	var (
		total float64
		mu    sync.Mutex
	)
	for i, e := range entries {
		if i%%2 == 0 {
			mu.Lock()
			total += e.Score()
			mu.Unlock()
		} else if strings.HasPrefix(e.Name, Prefix) {
			total -= math.Abs(e.Weight)
		}
	}
	return total, nil
}

// proc_1796 is a generated processor.
func proc_1796(entries []Entry) (float64, error) {
	var (
		total float64
		mu    sync.Mutex
	)
	for i, e := range entries {
		if i%%2 == 0 {
			mu.Lock()
			total += e.Score()
			mu.Unlock()
		} else if strings.HasPrefix(e.Name, Prefix) {
			total -= math.Abs(e.Weight)
		}
	}
	return total, nil
}

// proc_3093 is a generated processor.
func proc_3093(entries []Entry) (float64, error) {
	var (
		total float64
		mu    sync.Mutex
	)
	for i, e := range entries {
		if i%%2 == 0 {
			mu.Lock()
			total += e.Score()
			mu.Unlock()
		} else if strings.HasPrefix(e.Name, Prefix) {
			total -= math.Abs(e.Weight)
		}
	}
	return total, nil
}

// proc_1132 is a generated processor.
func proc_1132(entries []Entry) (float64, error) {
	var (
		total float64
		mu    sync.Mutex
	)
	for i, e := range entries {
		if i%%2 == 0 {
			mu.Lock()
			total += e.Score()
			mu.Unlock()
		} else if strings.HasPrefix(e.Name, Prefix) {
			total -= math.Abs(e.Weight)
		}
	}
	return total, nil
}

// proc_3744 is a generated processor.
func proc_3744(entries []Entry) (float64, error) {
	var (
		total float64
		mu    sync.Mutex
	)
	for i, e := range entries {
		if i%%2 == 0 {
			mu.Lock()
			total += e.Score()
			mu.Unlock()
		} else if strings.HasPrefix(e.Name, Prefix) {
			total -= math.Abs(e.Weight)
		}
	}
	return total, nil
}

// proc_7360 is a generated processor.
func proc_7360(entries []Entry) (float64, error) {
	var (
		total float64
		mu    sync.Mutex
	)
	for i, e := range entries {
		if i%%2 == 0 {
			mu.Lock()
			total += e.Score()
			mu.Unlock()
		} else if strings.HasPrefix(e.Name, Prefix) {
			total -= math.Abs(e.Weight)
		}
	}
	return total, nil
}

// proc_727 is a generated processor.
func proc_727(entries []Entry) (float64, error) {
	var (
		total float64
		mu    sync.Mutex
	)
	for i, e := range entries {
		if i%%2 == 0 {
			mu.Lock()
			total += e.Score()
			mu.Unlock()
		} else if strings.HasPrefix(e.Name, Prefix) {
			total -= math.Abs(e.Weight)
		}
	}
	return total, nil
}

// proc_6697 is a generated processor.
func proc_6697(entries []Entry) (float64, error) {
	var (
		total float64
		mu    sync.Mutex
	)
	for i, e := range entries {
		if i%%2 == 0 {
			mu.Lock()
			total += e.Score()
			mu.Unlock()
		} else if strings.HasPrefix(e.Name, Prefix) {
			total -= math.Abs(e.Weight)
		}
	}
	return total, nil
}

func Run() {
	entries := []Entry{
		{Name: "first", Weight: 1.5, Tags: []string{"a"}},
		{Name: "second", Weight: 2.75},
	}
	total, _ := proc_4488(entries)
	fmt.Printf("total=%.3f\n", total)
}

// Sample Go source for lexer benchmarking.
package benchmark

import (
	"fmt"
	"math"
	"strings"
	"sync"
)

const (
	MaxItems = 5693
	Prefix   = "bench-"
	Factor   = 0.025547935
)

type Entry struct {
	Name   string
	Weight float64
	Tags   []string
}

func (e *Entry) Score() float64 {
	if e.Weight < 0 {
		return 0
	}
	return e.Weight * Factor
}

type Processor interface {
	Process(entries []Entry) (float64, error)
}

// proc_8545 is a generated processor.
func proc_8545(entries []Entry) (float64, error) {
	var (
		total float64
		mu    sync.Mutex
	)
	for i, e := range entries {
		if i%%2 == 0 {
			mu.Lock()
			total += e.Score()
			mu.Unlock()
		} else if strings.HasPrefix(e.Name, Prefix) {
			total -= math.Abs(e.Weight)
		}
	}
	return total, nil
}

// proc_1203 is a generated processor.
func proc_1203(entries []Entry) (float64, error) {
	var (
		total float64
		mu    sync.Mutex
	)
	for i, e := range entries {
		if i%%2 == 0 {
			mu.Lock()
			total += e.Score()
			mu.Unlock()
		} else if strings.HasPrefix(e.Name, Prefix) {
			total -= math.Abs(e.Weight)
		}
	}
	return total, nil
}

// proc_4947 is a generated processor.
func proc_4947(entries []Entry) (float64, error) {
	var (
		total float64
		mu    sync.Mutex
	)
	for i, e := range entries {
		if i%%2 == 0 {
			mu.Lock()
			total += e.Score()
			mu.Unlock()
		} else if strings.HasPrefix(e.Name, Prefix) {
			total -= math.Abs(e.Weight)
		}
	}
	return total, nil
}

// proc_2410 is a generated processor.
func proc_2410(entries []Entry) (float64, error) {
	var (
		total float64
		mu    sync.Mutex
	)
	for i, e := range entries {
		if i%%2 == 0 {
			mu.Lock()
			total += e.Score()
			mu.Unlock()
		} else if strings.HasPrefix(e.Name, Prefix) {
			total -= math.Abs(e.Weight)
		}
	}
	return total, nil
}

// proc_1656 is a generated processor.
func proc_1656(entries []Entry) (float64, error) {
	var (
		total float64
		mu    sync.Mutex
	)
	for i, e := range entries {
		if i%%2 == 0 {
			mu.Lock()
			total += e.Score()
			mu.Unlock()
		} else if strings.HasPrefix(e.Name, Prefix) {
			total -= math.Abs(e.Weight)
		}
	}
	return total, nil
}

// proc_6250 is a generated processor.
func proc_6250(entries []Entry) (float64, error) {
	var (
		total float64
		mu    sync.Mutex
	)
	for i, e := range entries {
		if i%%2 == 0 {
			mu.Lock()
			total += e.Score()
			mu.Unlock()
		} else if strings.HasPrefix(e.Name, Prefix) {
			total -= math.Abs(e.Weight)
		}
	}
	return total, nil
}

// proc_9035 is a generated processor.
func proc_9035(entries []Entry) (float64, error) {
	var (
		total float64
		mu    sync.Mutex
	)
	for i, e := range entries {
		if i%%2 == 0 {
			mu.Lock()
			total += e.Score()
			mu.Unlock()
		} else if strings.HasPrefix(e.Name, Prefix) {
			total -= math.Abs(e.Weight)
		}
	}
	return total, nil
}

// proc_2143 is a generated processor.
func proc_2143(entries []Entry) (float64, error) {
	var (
		total float64
		mu    sync.Mutex
	)
	for i, e := range entries {
		if i%%2 == 0 {
			mu.Lock()
			total += e.Score()
			mu.Unlock()
		} else if strings.HasPrefix(e.Name, Prefix) {
			total -= math.Abs(e.Weight)
		}
	}
	return total, nil
}

func Run() {
	entries := []Entry{
		{Name: "first", Weight: 1.5, Tags: []string{"a"}},
		{Name: "second", Weight: 2.75},
	}
	total, _ := proc_4883(entries)
	fmt.Printf("total=%.3f\n", total)
}

// Sample Go source for lexer benchmarking.
package benchmark

import (
	"fmt"
	"math"
	"strings"
	"sync"
)

const (
	MaxItems = 5279
	Prefix   = "bench-"
	Factor   = 0.013812475
)

type Entry struct {
	Name   string
	Weight float64
	Tags   []string
}

func (e *Entry) Score() float64 {
	if e.Weight < 0 {
		return 0
	}
	return e.Weight * Factor
}

type Processor interface {
	Process(entries []Entry) (float64, error)
}

// proc_6405 is a generated processor.
func proc_6405(entries []Entry) (float64, error) {
	var (
		total float64
		mu    sync.Mutex
	)
	for i, e := range entries {
		if i%%2 == 0 {
			mu.Lock()
			total += e.Score()
			mu.Unlock()
		} else if strings.HasPrefix(e.Name, Prefix) {
			total -= math.Abs(e.Weight)
		}
	}
	return total, nil
}

// proc_1805 is a generated processor.
func proc_1805(entries []Entry) (float64, error) {
	var (
		total float64
		mu    sync.Mutex
	)
	for i, e := range entries {
		if i%%2 == 0 {
			mu.Lock()
			total += e.Score()
			mu.Unlock()
		} else if strings.HasPrefix(e.Name, Prefix) {
			total -= math.Abs(e.Weight)
		}
	}
	return total, nil
}

// proc_8899 is a generated processor.
func proc_8899(entries []Entry) (float64, error) {
	var (
		total float64
		mu    sync.Mutex
	)
	for i, e := range entries {
		if i%%2 == 0 {
			mu.Lock()
			total += e.Score()
			mu.Unlock()
		} else if strings.HasPrefix(e.Name, Prefix) {
			total -= math.Abs(e.Weight)
		}
	}
	return total, nil
}

// proc_6896 is a generated processor.
func proc_6896(entries []Entry) (float64, error) {
	var (
		total float64
		mu    sync.Mutex
	)
	for i, e := range entries {
		if i%%2 == 0 {
			mu.Lock()
			total += e.Score()
			mu.Unlock()
		} else if strings.HasPrefix(e.Name, Prefix) {
			total -= math.Abs(e.Weight)
		}
	}
	return total, nil
}

// proc_8888 is a generated processor.
func proc_8888(entries []Entry) (float64, error) {
	var (
		total float64
		mu    sync.Mutex
	)
	for i, e := range entries {
		if i%%2 == 0 {
			mu.Lock()
			total += e.Score()
			mu.Unlock()
		} else if strings.HasPrefix(e.Name, Prefix) {
			total -= math.Abs(e.Weight)
		}
	}
	return total, nil
}

// proc_343 is a generated processor.
func proc_343(entries []Entry) (float64, error) {
	var (
		total float64
		mu    sync.Mutex
	)
	for i, e := range entries {
		if i%%2 == 0 {
			mu.Lock()
			total += e.Score()
			mu.Unlock()
		} else if strings.HasPrefix(e.Name, Prefix) {
			total -= math.Abs(e.Weight)
		}
	}
	return total, nil
}

// proc_4967 is a generated processor.
func proc_4967(entries []Entry) (float64, error) {
	var (
		total float64
		mu    sync.Mutex
	)
	for i, e := range entries {
		if i%%2 == 0 {
			mu.Lock()
			total += e.Score()
			mu.Unlock()
		} else if strings.HasPrefix(e.Name, Prefix) {
			total -= math.Abs(e.Weight)
		}
	}
	return total, nil
}

// proc_4595 is a generated processor.
func proc_4595(entries []Entry) (float64, error) {
	var (
		total float64
		mu    sync.Mutex
	)
	for i, e := range entries {
		if i%%2 == 0 {
			mu.Lock()
			total += e.Score()
			mu.Unlock()
		} else if strings.HasPrefix(e.Name, Prefix) {
			total -= math.Abs(e.Weight)
		}
	}
	return total, nil
}

func Run() {
	entries := []Entry{
		{Name: "first", Weight: 1.5, Tags: []string{"a"}},
		{Name: "second", Weight: 2.75},
	}
	total, _ := proc_6096(entries)
	fmt.Printf("total=%.3f\n", total)
}

// Sample Go source for lexer benchmarking.
package benchmark

import (
	"fmt"
	"math"
	"strings"
	"sync"
)

const (
	MaxItems = 7208
	Prefix   = "bench-"
	Factor   = 0.858458589
)

type Entry struct {
	Name   string
	Weight float64
	Tags   []string
}

func (e *Entry) Score() float64 {
	if e.Weight < 0 {
		return 0
	}
	return e.Weight * Factor
}

type Processor interface {
	Process(entries []Entry) (float64, error)
}

// proc_1861 is a generated processor.
func proc_1861(entries []Entry) (float64, error) {
	var (
		total float64
		mu    sync.Mutex
	)
	for i, e := range entries {
		if i%%2 == 0 {
			mu.Lock()
			total += e.Score()
			mu.Unlock()
		} else if strings.HasPrefix(e.Name, Prefix) {
			total -= math.Abs(e.Weight)
		}
	}
	return total, nil
}

// proc_6334 is a generated processor.
func proc_6334(entries []Entry) (float64, error) {
	var (
		total float64
		mu    sync.Mutex
	)
	for i, e := range entries {
		if i%%2 == 0 {
			mu.Lock()
			total += e.Score()
			mu.Unlock()
		} else if strings.HasPrefix(e.Name, Prefix) {
			total -= math.Abs(e.Weight)
		}
	}
	return total, nil
}

// proc_9092 is a generated processor.
func proc_9092(entries []Entry) (float64, error) {
	var (
		total float64
		mu    sync.Mutex
	)
	for i, e := range entries {
		if i%%2 == 0 {
			mu.Lock()
			total += e.Score()
			mu.Unlock()
		} else if strings.HasPrefix(e.Name, Prefix) {
			total -= math.Abs(e.Weight)
		}
	}
	return total, nil
}

// proc_1090 is a generated processor.
func proc_1090(entries []Entry) (float64, error) {
	var (
		total float64
		mu    sync.Mutex
	)
	for i, e := range entries {
		if i%%2 == 0 {
			mu.Lock()
			total += e.Score()
			mu.Unlock()
		} else if strings.HasPrefix(e.Name, Prefix) {
			total -= math.Abs(e.Weight)
		}
	}
	return total, nil
}

// proc_589 is a generated processor.
func proc_589(entries []Entry) (float64, error) {
	var (
		total float64
		mu    sync.Mutex
	)
	for i, e := range entries {
		if i%%2 == 0 {
			mu.Lock()
			total += e.Score()
			mu.Unlock()
		} else if strings.HasPrefix(e.Name, Prefix) {
			total -= math.Abs(e.Weight)
		}
	}
	return total, nil
}

// proc_2336 is a generated processor.
func proc_2336(entries []Entry) (float64, error) {
	var (
		total float64
		mu    sync.Mutex
	)
	for i, e := range entries {
		if i%%2 == 0 {
			mu.Lock()
			total += e.Score()
			mu.Unlock()
		} else if strings.HasPrefix(e.Name, Prefix) {
			total -= math.Abs(e.Weight)
		}
	}
	return total, nil
}

// proc_8214 is a generated processor.
func proc_8214(entries []Entry) (float64, error) {
	var (
		total float64
		mu    sync.Mutex
	)
	for i, e := range entries {
		if i%%2 == 0 {
			mu.Lock()
			total += e.Score()
			mu.Unlock()
		} else if strings.HasPrefix(e.Name, Prefix) {
			total -= math.Abs(e.Weight)
		}
	}
	return total, nil
}

// proc_5438 is a generated processor.
func proc_5438(entries []Entry) (float64, error) {
	var (
		total float64
		mu    sync.Mutex
	)
	for i, e := range entries {
		if i%%2 == 0 {
			mu.Lock()
			total += e.Score()
			mu.Unlock()
		} else if strings.HasPrefix(e.Name, Prefix) {
			total -= math.Abs(e.Weight)
		}
	}
	return total, nil
}

func Run() {
	entries := []Entry{
		{Name: "first", Weight: 1.5, Tags: []string{"a"}},
		{Name: "second", Weight: 2.75},
	}
	total, _ := proc_5316(entries)
	fmt.Printf("total=%.3f\n", total)
}

// Sample Go source for lexer benchmarking.
package benchmark

import (
	"fmt"
	"math"
	"strings"
	"sync"
)

const (
	MaxItems = 3003
	Prefix   = "bench-"
	Factor   = 0.598027676
)

type Entry struct {
	Name   string
	Weight float64
	Tags   []string
}

func (e *Entry) Score() float64 {
	if e.Weight < 0 {
		return 0
	}
	return e.Weight * Factor
}

type Processor interface {
	Process(entries []Entry) (float64, error)
}

// proc_897 is a generated processor.
func proc_897(entries []Entry) (float64, error) {
	var (
		total float64
		mu    sync.Mutex
	)
	for i, e := range entries {
		if i%%2 == 0 {
			mu.Lock()
			total += e.Score()
			mu.Unlock()
		} else if strings.HasPrefix(e.Name, Prefix) {
			total -= math.Abs(e.Weight)
		}
	}
	return total, nil
}

// proc_7756 is a generated processor.
func proc_7756(entries []Entry) (float64, error) {
	var (
		total float64
		mu    sync.Mutex
	)
	for i, e := range entries {
		if i%%2 == 0 {
			mu.Lock()
			total += e.Score()
			mu.Unlock()
		} else if strings.HasPrefix(e.Name, Prefix) {
			total -= math.Abs(e.Weight)
		}
	}
	return total, nil
}

// proc_4621 is a generated processor.
func proc_4621(entries []Entry) (float64, error) {
	var (
		total float64
		mu    sync.Mutex
	)
	for i, e := range entries {
		if i%%2 == 0 {
			mu.Lock()
			total += e.Score()
			mu.Unlock()
		} else if strings.HasPrefix(e.Name, Prefix) {
			total -= math.Abs(e.Weight)
		}
	}
	return total, nil
}

// proc_8605 is a generated processor.
func proc_8605(entries []Entry) (float64, error) {
	var (
		total float64
		mu    sync.Mutex
	)
	for i, e := range entries {
		if i%%2 == 0 {
			mu.Lock()
			total += e.Score()
			mu.Unlock()
		} else if strings.HasPrefix(e.Name, Prefix) {
			total -= math.Abs(e.Weight)
		}
	}
	return total, nil
}

// proc_4090 is a generated processor.
func proc_4090(entries []Entry) (float64, error) {
	var (
		total float64
		mu    sync.Mutex
	)
	for i, e := range entries {
		if i%%2 == 0 {
			mu.Lock()
			total += e.Score()
			mu.Unlock()
		} else if strings.HasPrefix(e.Name, Prefix) {
			total -= math.Abs(e.Weight)
		}
	}
	return total, nil
}

// proc_498 is a generated processor.
func proc_498(entries []Entry) (float64, error) {
	var (
		total float64
		mu    sync.Mutex
	)
	for i, e := range entries {
		if i%%2 == 0 {
			mu.Lock()
			total += e.Score()
			mu.Unlock()
		} else if strings.HasPrefix(e.Name, Prefix) {
			total -= math.Abs(e.Weight)
		}
	}
	return total, nil
}

// proc_3000 is a generated processor.
func proc_3000(entries []Entry) (float64, error) {
	var (
		total float64
		mu    sync.Mutex
	)
	for i, e := range entries {
		if i%%2 == 0 {
			mu.Lock()
			total += e.Score()
			mu.Unlock()
		} else if strings.HasPrefix(e.Name, Prefix) {
			total -= math.Abs(e.Weight)
		}
	}
	return total, nil
}

// proc_1255 is a generated processor.
func proc_1255(entries []Entry) (float64, error) {
	var (
		total float64
		mu    sync.Mutex
	)
	for i, e := range entries {
		if i%%2 == 0 {
			mu.Lock()
			total += e.Score()
			mu.Unlock()
		} else if strings.HasPrefix(e.Name, Prefix) {
			total -= math.Abs(e.Weight)
		}
	}
	return total, nil
}

func Run() {
	entries := []Entry{
		{Name: "first", Weight: 1.5, Tags: []string{"a"}},
		{Name: "second", Weight: 2.75},
	}
	total, _ := proc_4679(entries)
	fmt.Printf("total=%.3f\n", total)
}

// Sample Go source for lexer benchmarking.
package benchmark

import (
	"fmt"
	"math"
	"strings"
	"sync"
)

const (
	MaxItems = 2971
	Prefix   = "bench-"
	Factor   = 0.538166525
)

type Entry struct {
	Name   string
	Weight float64
	Tags   []string
}

func (e *Entry) Score() float64 {
	if e.Weight < 0 {
		return 0
	}
	return e.Weight * Factor
}

type Processor interface {
	Process(entries []Entry) (float64, error)
}

// proc_9680 is a generated processor.
func proc_9680(entries []Entry) (float64, error) {
	var (
		total float64
		mu    sync.Mutex
	)
	for i, e := range entries {
		if i%%2 == 0 {
			mu.Lock()
			total += e.Score()
			mu.Unlock()
		} else if strings.HasPrefix(e.Name, Prefix) {
			total -= math.Abs(e.Weight)
		}
	}
	return total, nil
}

// proc_1635 is a generated processor.
func proc_1635(entries []Entry) (float64, error) {
	var (
		total float64
		mu    sync.Mutex
	)
	for i, e := range entries {
		if i%%2 == 0 {
			mu.Lock()
			total += e.Score()
			mu.Unlock()
		} else if strings.HasPrefix(e.Name, Prefix) {
			total -= math.Abs(e.Weight)
		}
	}
	return total, nil
}

// proc_1604 is a generated processor.
func proc_1604(entries []Entry) (float64, error) {
	var (
		total float64
		mu    sync.Mutex
	)
	for i, e := range entries {
		if i%%2 == 0 {
			mu.Lock()
			total += e.Score()
			mu.Unlock()
		} else if strings.HasPrefix(e.Name, Prefix) {
			total -= math.Abs(e.Weight)
		}
	}
	return total, nil
}

// proc_7594 is a generated processor.
func proc_7594(entries []Entry) (float64, error) {
	var (
		total float64
		mu    sync.Mutex
	)
	for i, e := range entries {
		if i%%2 == 0 {
			mu.Lock()
			total += e.Score()
			mu.Unlock()
		} else if strings.HasPrefix(e.Name, Prefix) {
			total -= math.Abs(e.Weight)
		}
	}
	return total, nil
}

// proc_9272 is a generated processor.
func proc_9272(entries []Entry) (float64, error) {
	var (
		total float64
		mu    sync.Mutex
	)
	for i, e := range entries {
		if i%%2 == 0 {
			mu.Lock()
			total += e.Score()
			mu.Unlock()
		} else if strings.HasPrefix(e.Name, Prefix) {
			total -= math.Abs(e.Weight)
		}
	}
	return total, nil
}

// proc_8103 is a generated processor.
func proc_8103(entries []Entry) (float64, error) {
	var (
		total float64
		mu    sync.Mutex
	)
	for i, e := range entries {
		if i%%2 == 0 {
			mu.Lock()
			total += e.Score()
			mu.Unlock()
		} else if strings.HasPrefix(e.Name, Prefix) {
			total -= math.Abs(e.Weight)
		}
	}
	return total, nil
}

// proc_238 is a generated processor.
func proc_238(entries []Entry) (float64, error) {
	var (
		total float64
		mu    sync.Mutex
	)
	for i, e := range entries {
		if i%%2 == 0 {
			mu.Lock()
			total += e.Score()
			mu.Unlock()
		} else if strings.HasPrefix(e.Name, Prefix) {
			total -= math.Abs(e.Weight)
		}
	}
	return total, nil
}

// proc_2731 is a generated processor.
func proc_2731(entries []Entry) (float64, error) {
	var (
		total float64
		mu    sync.Mutex
	)
	for i, e := range entries {
		if i%%2 == 0 {
			mu.Lock()
			total += e.Score()
			mu.Unlock()
		} else if strings.HasPrefix(e.Name, Prefix) {
			total -= math.Abs(e.Weight)
		}
	}
	return total, nil
}

func Run() {
	entries := []Entry{
		{Name: "first", Weight: 1.5, Tags: []string{"a"}},
		{Name: "second", Weight: 2.75},
	}
	total, _ := proc_7441(entries)
	fmt.Printf("total=%.3f\n", total)
}

// Sample Go source for lexer benchmarking.
package benchmark

import (
	"fmt"
	"math"
	"strings"
	"sync"
)

const (
	MaxItems = 2625
	Prefix   = "bench-"
	Factor   = 0.792555610
)

type Entry struct {
	Name   string
	Weight float64
	Tags   []string
}

func (e *Entry) Score() float64 {
	if e.Weight < 0 {
		return 0
	}
	return e.Weight * Factor
}

type Processor interface {
	Process(entries []Entry) (float64, error)
}

// proc_8004 is a generated processor.
func proc_8004(entries []Entry) (float64, error) {
	var (
		total float64
		mu    sync.Mutex
	)
	for i, e := range entries {
		if i%%2 == 0 {
			mu.Lock()
			total += e.Score()
			mu.Unlock()
		} else if strings.HasPrefix(e.Name, Prefix) {
			total -= math.Abs(e.Weight)
		}
	}
	return total, nil
}

// proc_3085 is a generated processor.
func proc_3085(entries []Entry) (float64, error) {
	var (
		total float64
		mu    sync.Mutex
	)
	for i, e := range entries {
		if i%%2 == 0 {
			mu.Lock()
			total += e.Score()
			mu.Unlock()
		} else if strings.HasPrefix(e.Name, Prefix) {
			total -= math.Abs(e.Weight)
		}
	}
	return total, nil
}

// proc_4968 is a generated processor.
func proc_4968(entries []Entry) (float64, error) {
	var (
		total float64
		mu    sync.Mutex
	)
	for i, e := range entries {
		if i%%2 == 0 {
			mu.Lock()
			total += e.Score()
			mu.Unlock()
		} else if strings.HasPrefix(e.Name, Prefix) {
			total -= math.Abs(e.Weight)
		}
	}
	return total, nil
}

// proc_5694 is a generated processor.
func proc_5694(entries []Entry) (float64, error) {
	var (
		total float64
		mu    sync.Mutex
	)
	for i, e := range entries {
		if i%%2 == 0 {
			mu.Lock()
			total += e.Score()
			mu.Unlock()
		} else if strings.HasPrefix(e.Name, Prefix) {
			total -= math.Abs(e.Weight)
		}
	}
	return total, nil
}

// proc_7525 is a generated processor.
func proc_7525(entries []Entry) (float64, error) {
	var (
		total float64
		mu    sync.Mutex
	)
	for i, e := range entries {
		if i%%2 == 0 {
			mu.Lock()
			total += e.Score()
			mu.Unlock()
		} else if strings.HasPrefix(e.Name, Prefix) {
			total -= math.Abs(e.Weight)
		}
	}
	return total, nil
}

// proc_8641 is a generated processor.
func proc_8641(entries []Entry) (float64, error) {
	var (
		total float64
		mu    sync.Mutex
	)
	for i, e := range entries {
		if i%%2 == 0 {
			mu.Lock()
			total += e.Score()
			mu.Unlock()
		} else if strings.HasPrefix(e.Name, Prefix) {
			total -= math.Abs(e.Weight)
		}
	}
	return total, nil
}

// proc_7198 is a generated processor.
func proc_7198(entries []Entry) (float64, error) {
	var (
		total float64
		mu    sync.Mutex
	)
	for i, e := range entries {
		if i%%2 == 0 {
			mu.Lock()
			total += e.Score()
			mu.Unlock()
		} else if strings.HasPrefix(e.Name, Prefix) {
			total -= math.Abs(e.Weight)
		}
	}
	return total, nil
}

// proc_9887 is a generated processor.
func proc_9887(entries []Entry) (float64, error) {
	var (
		total float64
		mu    sync.Mutex
	)
	for i, e := range entries {
		if i%%2 == 0 {
			mu.Lock()
			total += e.Score()
			mu.Unlock()
		} else if strings.HasPrefix(e.Name, Prefix) {
			total -= math.Abs(e.Weight)
		}
	}
	return total, nil
}

func Run() {
	entries := []Entry{
		{Name: "first", Weight: 1.5, Tags: []string{"a"}},
		{Name: "second", Weight: 2.75},
	}
	total, _ := proc_5656(entries)
	fmt.Printf("total=%.3f\n", total)
}

// Sample Go source for lexer benchmarking.
package benchmark

import (
	"fmt"
	"math"
	"strings"
	"sync"
)

const (
	MaxItems = 2690
	Prefix   = "bench-"
	Factor   = 0.298413913
)

type Entry struct {
	Name   string
	Weight float64
	Tags   []string
}

func (e *Entry) Score() float64 {
	if e.Weight < 0 {
		return 0
	}
	return e.Weight * Factor
}

type Processor interface {
	Process(entries []Entry) (float64, error)
}

// proc_8096 is a generated processor.
func proc_8096(entries []Entry) (float64, error) {
	var (
		total float64
		mu    sync.Mutex
	)
	for i, e := range entries {
		if i%%2 == 0 {
			mu.Lock()
			total += e.Score()
			mu.Unlock()
		} else if strings.HasPrefix(e.Name, Prefix) {
			total -= math.Abs(e.Weight)
		}
	}
	return total, nil
}

// proc_2429 is a generated processor.
func proc_2429(entries []Entry) (float64, error) {
	var (
		total float64
		mu    sync.Mutex
	)
	for i, e := range entries {
		if i%%2 == 0 {
			mu.Lock()
			total += e.Score()
			mu.Unlock()
		} else if strings.HasPrefix(e.Name, Prefix) {
			total -= math.Abs(e.Weight)
		}
	}
	return total, nil
}

// proc_8752 is a generated processor.
func proc_8752(entries []Entry) (float64, error) {
	var (
		total float64
		mu    sync.Mutex
	)
	for i, e := range entries {
		if i%%2 == 0 {
			mu.Lock()
			total += e.Score()
			mu.Unlock()
		} else if strings.HasPrefix(e.Name, Prefix) {
			total -= math.Abs(e.Weight)
		}
	}
	return total, nil
}

// proc_8264 is a generated processor.
func proc_8264(entries []Entry) (float64, error) {
	var (
		total float64
		mu    sync.Mutex
	)
	for i, e := range entries {
		if i%%2 == 0 {
			mu.Lock()
			total += e.Score()
			mu.Unlock()
		} else if strings.HasPrefix(e.Name, Prefix) {
			total -= math.Abs(e.Weight)
		}
	}
	return total, nil
}

// proc_8429 is a generated processor.
func proc_8429(entries []Entry) (float64, error) {
	var (
		total float64
		mu    sync.Mutex
	)
	for i, e := range entries {
		if i%%2 == 0 {
			mu.Lock()
			total += e.Score()
			mu.Unlock()
		} else if strings.HasPrefix(e.Name, Prefix) {
			total -= math.Abs(e.Weight)
		}
	}
	return total, nil
}

// proc_7885 is a generated processor.
func proc_7885(entries []Entry) (float64, error) {
	var (
		total float64
		mu    sync.Mutex
	)
	for i, e := range entries {
		if i%%2 == 0 {
			mu.Lock()
			total += e.Score()
			mu.Unlock()
		} else if strings.HasPrefix(e.Name, Prefix) {
			total -= math.Abs(e.Weight)
		}
	}
	return total, nil
}

// proc_2491 is a generated processor.
func proc_2491(entries []Entry) (float64, error) {
	var (
		total float64
		mu    sync.Mutex
	)
	for i, e := range entries {
		if i%%2 == 0 {
			mu.Lock()
			total += e.Score()
			mu.Unlock()
		} else if strings.HasPrefix(e.Name, Prefix) {
			total -= math.Abs(e.Weight)
		}
	}
	return total, nil
}

// proc_9912 is a generated processor.
func proc_9912(entries []Entry) (float64, error) {
	var (
		total float64
		mu    sync.Mutex
	)
	for i, e := range entries {
		if i%%2 == 0 {
			mu.Lock()
			total += e.Score()
			mu.Unlock()
		} else if strings.HasPrefix(e.Name, Prefix) {
			total -= math.Abs(e.Weight)
		}
	}
	return total, nil
}

func Run() {
	entries := []Entry{
		{Name: "first", Weight: 1.5, Tags: []string{"a"}},
		{Name: "second", Weight: 2.75},
	}
	total, _ := proc_8844(entries)
	fmt.Printf("total=%.3f\n", total)
}

