// Single-line comment
/*
 * Multi-line comment
 * Can span multiple lines.
 */

// 1. Package Declaration and Imports
// Every Go program is made up of packages.
// Programs start running in package main.
package main
// cmt
// cmt2
    0; 123456789; -0; -123456789; 1777_000_000;
    00; 01234567; -01234567; 0_0; 012_45;
    08; 09; //invalid octals
    0o660; 0O061; -0o02; 0o_660; 0O0_6_1;
    0x0; 0x0123456789ABCDEFabcdef; -0x0123456789ABCDEFabcdef;
    0x_0; 0x012_3456_7_8_9ABCDEFabcd_ef;
    0b1011; 0B00001; -0b1; 0b_1; 0B1_0;
    000.000; 123.456; .0; 1.;
    0_1.0_1; 1_23.4_6;
    -000.000; -123.456; -.0; -1. -1.. ;
    0e+0; 0E+0; 0.0e+0; 0.0E+0; 123.456e+789;
    1_2e+0; 1E+0_1; 0.1_2e2; 1_23.4_56e+78_9;
    0e-0; 0E-0; 0.0e-0; 0.0E-0; 123.456e-789;
    0.e+0; .0e+0; 0.e-0; .0e-0;
    0x1p-2; 0X1P+2; 0x1p2;
    0x_1p-2; 0X1_1P+2; 0x_1p2_1;
    0x1.0P-1021; 0X1.0p-1021;
    0x_1_1.0_7P-1_021;
    0x2.p10; 0x1.Fp+0; 0X.8p-0;
    000i; 100i; -100i; 1_1i;
    123.456i; -123.456i; 1_23.45_6i;
    1e+2i; 1e-2i; 1.2e+3i; 1.2e-3i; 1E+2i; 1E-2i; 1.2E+3i; 1.2E-3i;
    1_1e+2_1i; 1.2_1E-3_5i;
    0o6i; 0O35i; 0o_6i; 0O3_5i;
    0x0i; 0x0123456789ABCDEFabcdefi; 0x_012_CD_Efi;
    0b1011i; 0B00001i; 0b_1011i; 0B000_01i;
    0x1p-2i; 0x1.0P-1021i; 0x1.Fp+0i;
    0x_1p-2i; 0x1_4.0_5P-102_1i;
                
template = "{{block "name" pipeline}} T1 {{end}}"

func NewChain(prefixLen int) *Chain {
  Label:
	return `mul
    string`
}

func (c *Chain) Generate(n int) string {
	var words []string
	for i := 0; i < n; i++ {
	}
	return "Text\"end" + 'Test'
}

type MyContainer /*..*/ struct {
  name1 func(arg string)
  name2 func(arg interface{})
  name3 package.MyType `tag1:""`
  name4 interface{} `tag1:""`
  name5 map[string]int `t`
}

// Import statements bring in functionality from other packages.
// The "fmt" package provides formatted I/O (like printing to console).
import "fmt"
import "math" // For math operations
import "time" // For time-related functions

// You can also group imports:
import (
	"errors" // For error handling
	"strconv" // For string conversions
)

// 2. Variables and Data Types
// Go is statically typed, meaning variable types are checked at compile time.
// Variables are declared using 'var' keyword. Zero-values are assigned by default.
var a int // Declares an integer 'a', defaults to 0
var b string // Declares a string 'b', defaults to "" (empty string)
var c bool // Declares a boolean 'c', defaults to false

// You can declare and initialize on one line:
var x int = 10
var y string = "Hello, Go!"

// Type inference (short variable declaration) using `:=`
// This can only be used inside functions.
func main() { // main is the entry point for executable programs
	name := "Alice" // Type (string) is inferred
	age := 30       // Type (int) is inferred
	isStudent := true

	fmt.Println("Hello,", name, "age", age, "is student:", isStudent)

	// Basic Types:
	// Integers: int, int8, int16, int32, int64, uint, uint8, ... uintptr
	var myInt int = 42
	var myUint uint = 100

	// Floating-point: float32, float64
	var pi float64 = 3.14159

	// Booleans: bool
	var isOpen bool = true

	// Strings: string
	var message string = "Go is awesome!"
	var rawString string = `This is a "raw" string.
It can span multiple lines and doesn't interpret escape sequences.`

	// Rune: Represents a Unicode code point (alias for int32)
	var myRune rune = '🚀'
	fmt.Printf("My rune: %c (Unicode: %U)\n", myRune, myRune)

	// Constants (declared with `const`)
	const SpeedOfLight = 299792458 // Type is inferred
	const Pi float32 = 3.14        // Explicit type

	// Iota: Used for creating sequences of related constants
	const (
		Red   = iota // 0
		Green        // 1
		Blue         // 2
	)
	fmt.Println("Colors (iota):", Red, Green, Blue)

	// 3. Operators
	// Arithmetic: +, -, *, /, %, ++, -- (postfix only, not expressions)
	sum := 10 + 5
	diff := 10 - 5
	prod := 10 * 5
	quot := 10 / 3 // Integer division
	rem := 10 % 3
	sum++ // sum is now 16

	// Relational: ==, !=, <, >, <=, >=
	isEqual := (10 == 10)
	isNotEqual := (10 != 5)

	// Logical: && (and), || (or), ! (not)
	logicAnd := (true && false)
	logicOr := (true || false)
	logicNot := !true

	// Bitwise: &, |, ^, <<, >>, &^ (bit clear)
	bitAnd := 5 & 3 // 0101 & 0011 = 0001 (1)

	// Assignment: =, +=, -=, *=, /=, %=, etc.
	var val int = 5
	val += 2 // val is now 7

	// 4. Control Structures

	// If-ElseIf-Else (No parentheses around the condition)
	if age >= 18 {
		fmt.Println("You are an adult.")
	} else if age > 0 {
		fmt.Println("You are a minor.")
	} else {
		fmt.Println("Invalid age.")
	}

	// 'if' with a short statement (variable scope limited to if/else)
	if val := getStatus(); val == "OK" {
		fmt.Println("Status is OK.")
	} else {
		fmt.Println("Status is not OK:", val)
	}
	// fmt.Println(val) // Error: val is not defined here

	// For Loop (Go's only loop construct)
	// Classic C-style for loop:
	for i := 0; i < 5; i++ {
		fmt.Println("For loop iteration:", i)
	}

	// While loop equivalent:
	j := 0
	for j < 3 {
		fmt.Println("While loop iteration:", j)
		j++
	}

	// Infinite loop:
	// for {
	// 	fmt.Println("Infinite loop!")
	// }

	// Range loop (for iterating over arrays, slices, maps, strings, channels)
	numbers := []int{10, 20, 30, 40}
	for index, value := range numbers {
		fmt.Printf("Index: %d, Value: %d\n", index, value)
	}
	// To iterate only over values:
	for _, value := range numbers { // Use '_' for unused variables
		fmt.Println("Value:", value)
	}

	// Switch Statement
	day := "Wednesday"
	switch day {
	case "Monday", "Tuesday": // Multiple cases
		fmt.Println("Start of the week.")
	case "Wednesday":
		fmt.Println("Midweek.")
		// No implicit fallthrough! 'break' is not needed unless you want to exit early
	case "Thursday", "Friday":
		fmt.Println("End of the week approaching.")
	default: // Optional default case
		fmt.Println("Weekend or invalid day.")
	}

	// Switch with a short statement
	switch hour := time.Now().Hour(); { // No expression after switch, acts like if-else if
	case hour < 12:
		fmt.Println("Good morning!")
	case hour < 18:
		fmt.Println("Good afternoon!")
	default:
		fmt.Println("Good evening!")
	}

	// 5. Functions
	// Function definition
	greet("Programmer")
	addResult := add(15, 7)
	fmt.Println("Addition result:", addResult)

	// Function with multiple return values
	val1, val2 := swap(100, 200)
	fmt.Println("Swapped values:", val1, val2)

	// Anonymous function (closure)
	multiplier := func(a, b int) int {
		return a * b
	}
	fmt.Println("Multiplier result:", multiplier(5, 3))

	// Variadic functions (accepts a variable number of arguments)
	sumAll(1, 2, 3, 4, 5)

	// Defer statement: Executes a function call just before the surrounding function returns
	defer fmt.Println("This prints last (due to defer).")
	fmt.Println("This prints before defer.")

	// 6. Arrays, Slices, and Maps

	// Arrays (fixed size)
	var staticArray [3]string // Array of 3 strings, default to ""
	staticArray[0] = "Go"
	staticArray[1] = "Lang"
	staticArray[2] = "Array"
	fmt.Println("Static Array:", staticArray)
	fmt.Println("Array length:", len(staticArray))

	// Slices (dynamic, more common than arrays)
	mySlice := []int{1, 2, 3, 4, 5} // Declares and initializes a slice
	fmt.Println("Slice:", mySlice)
	mySlice = append(mySlice, 6) // Append elements
	fmt.Println("Slice after append:", mySlice)

	// Make a slice with specified length and capacity
	dynamicSlice := make([]string, 2, 5) // len=2, cap=5
	dynamicSlice[0] = "Red"
	dynamicSlice[1] = "Green"
	fmt.Println("Dynamic Slice:", dynamicSlice, "len:", len(dynamicSlice), "cap:", cap(dynamicSlice))

	// Slicing (creating a new slice from parts of an existing one)
	subSlice := mySlice[1:4] // Elements from index 1 (inclusive) to 4 (exclusive)
	fmt.Println("Sub-slice:", subSlice)

	// Maps (key-value pairs, hash maps/dictionaries)
	// Declare a map
	var ages map[string]int
	ages = make(map[string]int) // Initialize the map

	// Or use a literal
	capitals := map[string]string{
		"USA":    "Washington D.C.",
		"France": "Paris",
	}
	capitals["Germany"] = "Berlin" // Add/update an entry
	fmt.Println("Capitals:", capitals)

	// Accessing map values and checking for existence
	if capital, ok := capitals["USA"]; ok {
		fmt.Println("Capital of USA:", capital)
	} else {
		fmt.Println("USA not found.")
	}
	delete(capitals, "France") // Delete an entry

	// 7. Structs (Custom Data Types, similar to classes/objects without methods initially)
	type Person struct {
		FirstName string
		LastName  string
		Age       int
	}

	// Create a struct instance
	p1 := Person{"John", "Doe", 30} // Positional way
	p2 := Person{
		FirstName: "Jane",
		LastName:  "Smith",
		Age:       25,
	} // Named fields way
	fmt.Println("Person 1:", p1.FirstName, p1.LastName, p1.Age)
	fmt.Println("Person 2:", p2) // Prints the struct in default format

	// Structs with embedded fields (composition over inheritance)
	type Employee struct {
		Person    // Embedded Person struct (promotes Person's fields directly)
		EmployeeID string
	}
	emp := Employee{
		Person: Person{
			FirstName: "Alice",
			LastName:  "Johnson",
			Age:       35,
		},
		EmployeeID: "EMP001",
	}
	fmt.Println("Employee Name:", emp.FirstName, "ID:", emp.EmployeeID)

	// 8. Methods (Functions associated with a type)
	// Defined below main for Person type
	p1.Introduce()

	// Pointer receivers for methods (to modify the original struct)
	p1.Birthday()
	fmt.Println("John's new age after Birthday():", p1.Age)

	// 9. Interfaces (Implicitly implemented)
	// Define an interface with a method signature
	type Greeter interface {
		Introduce() // Any type with an Introduce() method implements Greeter
	}

	// A variable of interface type can hold any value that implements the interface.
	var g Greeter
	g = p1 // p1 (Person) implements Greeter
	g.Introduce()

	type Dog struct {
		Name string
	}

	func (d Dog) Introduce() {
		fmt.Printf("Woof, my name is %s!\n", d.Name)
	}
	d := Dog{"Buddy"}
	g = d // Dog also implements Greeter
	g.Introduce()

	// Type assertion (checking the underlying type of an interface value)
	if personVal, ok := g.(Person); ok { // Assert if g is a Person
		fmt.Println("Interface holds a Person:", personVal.FirstName)
	} else if dogVal, ok := g.(Dog); ok {
		fmt.Println("Interface holds a Dog:", dogVal.Name)
	}

	// 10. Concurrency (Goroutines and Channels) - Go's superpowers!
	// Goroutine: A lightweight thread of execution
	go sayHelloConcurrent("world") // Starts a new goroutine
	sayHelloConcurrent("Go!")     // Runs in the main goroutine
	time.Sleep(100 * time.Millisecond) // Give goroutines time to finish

	// Channels: Typed conduits through which you can send and receive values
	messages := make(chan string) // Create a channel of strings

	go func() {
		messages <- "ping" // Send "ping" to the channel
	}()

	msg := <-messages // Receive from the channel
	fmt.Println(msg)

	// Buffered channels
	bufferedChannel := make(chan int, 2) // Buffer size of 2
	bufferedChannel <- 1
	bufferedChannel <- 2
	// bufferedChannel <- 3 // This would block until a value is received

	fmt.Println("Received from buffered:", <-bufferedChannel)
	fmt.Println("Received from buffered:", <-bufferedChannel)

	// Select statement (for managing multiple channel operations)
	c1 := make(chan string)
	c2 := make(chan string)

	go func() {
		time.Sleep(1 * time.Second)
		c1 <- "one"
	}()
	go func() {
		time.Sleep(2 * time.Second)
		c2 <- "two"
	}()

	for i := 0; i < 2; i++ {
		select {
		case msg1 := <-c1:
			fmt.Println("Received:", msg1)
		case msg2 := <-c2:
			fmt.Println("Received:", msg2)
		case <-time.After(500 * time.Millisecond): // Timeout
			fmt.Println("Timeout!")
		}
	}

	// 11. Error Handling (Explicit, not exceptions)
	// Go functions often return a value and an error.
	value, err := divide(10, 2)
	if err != nil {
		fmt.Println("Error dividing:", err)
	} else {
		fmt.Println("Division result:", value)
	}

	value, err = divide(10, 0) // This will produce an error
	if err != nil {
		fmt.Println("Error dividing:", err)
	} else {
		fmt.Println("Division result:", value)
	}

	// Custom error type
	customErr := &MyCustomError{"Something specific went wrong."}
	fmt.Println("Custom error:", customErr.Error())
}

// Function with a single return value
func getStatus() string {
	return "OK"
}

// Function definition
func greet(name string) {
	fmt.Printf("Hello, %s from a Go function!\n", name)
}

// Function with return type
func add(a int, b int) int { // or add(a, b int) int
	return a + b
}

// Function with named return values (and implicit return)
func swap(x, y int) (a, b int) {
	a = y
	b = x
	return // Returns a and b
}

// Variadic function
func sumAll(numbers ...int) { // '...' indicates variadic
	total := 0
	for _, num := range numbers {
		total += num
	}
	fmt.Println("Sum of all numbers:", total)
}

// Method for the Person struct
func (p Person) Introduce() { // 'p Person' is the receiver
	fmt.Printf("Hi, my name is %s %s and I am %d years old.\n", p.FirstName, p.LastName, p.Age)
}

// Method with a pointer receiver (modifies the original struct)
func (p *Person) Birthday() { // '*p Person' indicates a pointer receiver
	p.Age++
	fmt.Println("Happy Birthday! New age:", p.Age)
}

// Goroutine example function
func sayHelloConcurrent(s string) {
	for i := 0; i < 3; i++ {
		time.Sleep(50 * time.Millisecond)
		fmt.Println(s)
	}
}

// Function returning a value and an error
func divide(numerator, denominator float64) (float64, error) {
	if denominator == 0 {
		return 0, errors.New("cannot divide by zero") // Create a new error
	}
	return numerator / denominator, nil // nil indicates no error
}

// Custom error type (implementing the error interface)
type MyCustomError struct {
	Message string
}

func (e *MyCustomError) Error() string {
	return "MyCustomError: " + e.Message
}