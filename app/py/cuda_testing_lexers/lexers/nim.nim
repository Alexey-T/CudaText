# Single-line comment
# Multiple # for clarity for multi-line comments within blocks or if-else.

# Multi-line comment block
# This is a Nim comment.
# It uses the '#' character.

# You can also use discarded string literals for multi-line comments:
discard """
This is a multi-line string literal,
often used as a block comment.
Since it's not assigned to anything, it's discarded.
"""

#[ multiline
  #[ nested
  ]#
comment
end ]#
test = """
test""" + re"test"
#comment
{.pragma.}
0XAB_DE + 0o12_3 + 0B01_01 + 1_2.3_4  + 1.2 + 1e+3_4

template all(container, cond: expr): expr {.immediate.} =
  block:
    var result = true
    for it in items(container):
      if not cond(it):
        result = false
        break
    result

proc proc1(a, b: string): bool =
  a[0] == b[0] 

func func1: bool =
  dddd 
  
if all("mystring", {'a'..'z'}.contains) and any("myohmy", 'y'.`==`): 
  echo "works"
else: 
  echo "does not work"


# 1. Modules and Program Structure
# Every .nim file is a module.
# The entry point for an executable is typically the module that doesn't define a 'proc main()',
# or if it does, it's explicitly called. Often, it's just the top-level code in the module.

# Importing other modules
import std/stdio # For I/O operations (e.g., echo)
import std/strutils # For string utilities (e.g., parseInt)
import std/sequtils # For sequence utilities (e.g., filter, map)
import std/tables # For hash maps
import std/random # For random numbers

# 2. Variables and Data Types
# Nim is statically typed, but with strong type inference.
# Indentation is significant (like Python).

# Variable declaration: `var` for mutable variables
var myInt: int             # Explicit type declaration, defaults to 0
var myFloat = 3.14         # Type inferred as float (float64)
var myString = "Hello, Nim!" # Type inferred as string
var myBool: bool = true    # Explicit type with initial value

# Immutable variable declaration: `let` (similar to 'const' in JS/Go, 'final' in Java)
let pi = 3.14159           # Type inferred as float
let appName: string = "My Nim App"

# Compile-time constants: `const`
const MaxUsers = 100
const Greeting = "Welcome!"

# Type aliases: `type`
type
  Age = int
  PersonName = string
  MyTuple = tuple[name: string, age: int] # Custom tuple type

var userAge: Age = 30
var userName: PersonName = "Alice"

# Primitive Data Types:
# Integers: int, int8, int16, int32, int64, uint, uint8, ...
# Floats: float, float32, float64
# Boolean: bool (true or false)
# Character: char (single Unicode scalar value)
# String: string (UTF-8 by default)

# Collections:
# Arrays (fixed-size): `array[N, T]`
var staticArray: array[3, string] # An array of 3 strings
staticArray[0] = "first"
staticArray[1] = "second"
staticArray[2] = "third"
echo "Static Array: ", staticArray # Uses `$` operator for string conversion

# Sequences (dynamic arrays): `seq[T]`
var mySequence: seq[int] = @[1, 2, 3] # Initialized with @[] syntax
mySequence.add(4)
mySequence.add(5)
echo "Sequence: ", mySequence
echo "Sequence length: ", len(mySequence)

# Tuples: Ordered, fixed-size collection of potentially different types
var myCoord = (x: 10, y: 20) # Anonymous tuple
echo "Tuple x: ", myCoord.x
echo "Tuple y: ", myCoord.y

var namedTuple: MyTuple = (name: "Bob", age: 25)
echo "Named Tuple: ", namedTuple.name, " is ", namedTuple.age, " years old."

# Sets: Unordered collection of unique items (based on ordinal types like enums, chars, ints)
type MySet = set[char]
var vowelSet: MySet = {'a', 'e', 'i', 'o', 'u'}
echo "Is 'a' in vowelSet? ", 'a' in vowelSet

# Enums: Enumerated types
type Direction = enum
  North, South, East, West

var currentDirection: Direction = Direction.North
echo "Current direction: ", currentDirection

# Pointers and References (Explicit control, unlike GC'd languages, but GC is default)
type MyObject = ref object # 'ref' indicates a garbage-collected reference type
  value: int

var objPtr: MyObject = MyObject(value: 42) # Allocate on GC heap
echo "Object value: ", objPtr.value

# 3. Operators
# Arithmetic: +, -, *, /, div (integer division), mod (remainder), shl, shr
var sum = 10 + 5
var quotient = 10 div 3 # 3
var remainder = 10 mod 3 # 1

# Comparison: ==, !=, <, >, <=, >=
var isEqual = (sum == 15)

# Logical: and, or, not
var logicAnd = (true and false)
var logicOr = (true or false)
var logicNot = (not true)

# Assignment: =, +=, -=, *=, /=, &=, |=, etc.
var z = 10
z += 5 # z is 15

# Custom operators (Nim allows defining custom operators)
proc `+`(a, b: string): string = a & b # Concatenate strings with '+'
echo "Custom operator: ", "Hello " + "Nim"

# 4. Control Structures

# If-Elif-Else (Indentation-based blocks)
if temperature > 30:
  echo "It's hot!"
elif temperature > 20:
  echo "It's warm."
else:
  echo "It's cool."

# Case Statement (similar to switch, type-safe)
case currentDirection
of North: echo "Going North"
of South: echo "Going South"
of East: echo "Going East"
of West: echo "Going West"
# No 'break' needed, Nim doesn't have fall-through.
# 'of else' can be used for a default case.

# For Loop (iterates over iterables)
for i in 0..<5: # Range 0 to 4 (exclusive of 5)
  echo "For loop iteration: ", i

for fruit in staticArray:
  echo "Fruit: ", fruit

for index, value in mySequence: # Iterate with index and value
  echo "Seq[", index, "]: ", value

# While Loop
var counter = 0
while counter < 3:
  echo "While loop iteration: ", counter
  inc(counter) # Increment function

# Break and Continue
for i in 0..9:
  if i mod 2 != 0:
    continue # Skips odd numbers
  if i > 6:
    break    # Exits the loop entirely
  echo "Break/Continue example: ", i

# 5. Procedures (Functions)
# Function definition: `proc`
proc greet(name: string) = # No return type indicated for void procedure
  echo "Hello, ", name, " from a Nim proc!"

# Function with return type
proc add(a, b: int): int = # Return type after colon
  result = a + b # 'result' is an implicit variable for the return value
  # or simply: return a + b

# Calling procedures
greet("Nim Programmer")
var sumResult = add(10, 20)
echo "Add result: ", sumResult

# Named arguments
proc describe(name: string, age: int, city: string) =
  echo name, " is ", age, " years old and lives in ", city, "."

describe(name: "Charlie", age: 40, city: "Nimbleton")
# Order doesn't matter when using named arguments

# Generic Procedures (Templates)
proc max[T](a, b: T): T = # T is a type parameter
  if a > b: result = a else: result = b

echo "Max of ints: ", max(10, 20)
echo "Max of floats: ", max(3.14, 2.71)
echo "Max of strings: ", max("apple", "banana")

# Method Syntax (Calling procedures with dot notation)
# If the first parameter is special, it can be called using dot notation.
proc `$`(p: Person): string = # The `$` proc converts any type to string
  result = p.name & " (" & $p.age & " years old)"

proc introduce(p: Person) = # 'p' is the receiver
  echo "Hi, I'm ", p.name, "."

# 6. Object-Oriented and Data Structures

# Object (value type, usually on stack/local memory)
type Person = object
  name: string
  age: int

# Instantiation and access
var person1: Person
person1.name = "John"
person1.age = 30
echo "Person 1: ", person1.name, ", age: ", person1.age

var person2 = Person(name: "Jane", age: 25) # Constructor-like syntax
echo "Person 2: ", person2

person1.introduce() # Calling method using dot notation

# Object variants (for "inheritance-like" behavior or discriminated unions)
type AnimalKind = enum
  Mammal, Bird, Fish

type Animal = object
  case kind: AnimalKind # Discriminant field
  of Mammal:
    mammalSpecies: string
  of Bird:
    featherColor: string
  of Fish:
    finCount: int

var myMammal: Animal
myMammal.kind = Mammal
myMammal.mammalSpecies = "Dog"
echo "My animal is a ", myMammal.mammalSpecies

# Table (Hash Map / Dictionary)
var scores = initTable[string, int]() # Key: string, Value: int
scores["Alice"] = 95
scores["Bob"] = 88
echo "Bob's score: ", scores["Bob"]
echo "Table size: ", len(scores)

for key, value in scores.pairs: # Iterate over key-value pairs
  echo "Student: ", key, ", Score: ", value

# 7. Macros and Metaprogramming (Nim's powerful feature)
# Macros operate on the Abstract Syntax Tree (AST) at compile-time.
# They are like functions that take code (AST) and return code (modified AST).

macro myMacro(args: varargs[untyped]): untyped =
  # Manipulate the AST here
  # E.g., `quote` for creating AST nodes
  # `newLit` for creating literals
  # `bind` for binding symbols
  # `dumpTree` for inspecting AST
  echo "Macro 'myMacro' invoked with: ", args.repr # Print representation of args
  # This macro does nothing, but it demonstrates the entry point.
  # A real macro would return new AST nodes.
  newLit(nil) # Return an empty node for now

# myMacro(1, "hello", true) # Invoking the macro (not a proc call)

# 8. Error Handling (Exceptions)
# Nim uses 'try-except-finally' similar to Python/Java.

proc mightFail(value: int): string =
  if value == 0:
    raise newException(ValueError, "Value cannot be zero!") # Raise an exception
  result = $value

try:
  echo "Result: ", mightFail(5)
  echo "Result: ", mightFail(0) # This will raise an exception
  echo "This line will not be executed."
except ValueError: # Catch a specific exception type
  echo "Caught a ValueError!"
except: # Catch any other exception
  echo "Caught an unknown exception!"
finally:
  echo "This finally block always executes."

# 9. Concurrency (async/await and channels)
# Nim provides powerful concurrency primitives.
import std/asyncdispatch # For async/await

proc simulateWork(): Future[int] {.async.} =
  echo "Starting async work..."
  # Simulates an async operation
  await sleepAsync(1000) # Sleep for 1 second asynchronously
  echo "Async work finished."
  return 42

proc mainAsync() {.async.} =
  let result = await simulateWork() # Await the result of the async proc
  echo "Received async result: ", result

# To run an async main, you typically use `waitFor` or `runForever`
# waitFor mainAsync() # In a real app, this would be the last line
# (Not run directly in this example for simplicity, as it requires a specific execution context.)

# 10. Iterators
# Custom iterators for creating sequences of values
iterator countUp(n: int): int =
  for i in 1..n:
    yield i

for num in countUp(3):
  echo "Count up: ", num

# 11. Concepts (Newer feature for generic type constraints)
# Allows defining requirements for generic type parameters.
concept HasLength[T]
  len(T) is int # T must have a `len` procedure that returns an int

proc process[T: HasLength](data: T) =
  echo "Processing data with length: ", len(data)

process(@[1, 2, 3]) # Works with sequences
# process(123) # Error: int does not have a len proc

# 12. Mixins (Code injection at compile time)
# Less common than macros, but allows injecting fragments of code.
mixin myMixinVar: int = 5

# echo myMixinVar # This would be injected wherever 'mixin myMixinVar' is used.

# Nim programs typically end without an explicit `main` function call if
# the top-level code is meant to be the entry point.
# Any code outside of a proc will be executed when the module is loaded.
echo "Nim syntax example finished."