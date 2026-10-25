// Single-line comment
/*
 * Multi-line comment
 * Can span multiple lines.
 */
/// Documentation comment (used for Ddoc, D's documentation generator)
/**
 * Another style of documentation comment.
 */

// 1. Basic Structure and Modules
// Modules are D's way of organizing code, similar to namespaces or packages.
// A file's name typically matches its module name (e.g., mymodule.d defines module mymodule;).
module mymodule;

// Importing other modules
import std.stdio;     // For input/output operations (e.g., writeln)
import std.conv;      // For type conversions (e.g., to!)
import std.string;    // For string manipulation
import std.array;     // For array operations

// The main function is the entry point of a D application.
// Similar to C#/C++/Java's main.
int main(string[] args)
{
    // Print to console (equivalent to Console.WriteLine in C#, printf in C)
    writeln("Hello, D!");

    // 2. Variables and Data Types
    // D is statically typed, but 'auto' can be used for type inference.

    // Integer types
    byte myByte = 10;           // 8-bit signed
    short myShort = 100;        // 16-bit signed
    int myInt = 1_000_000;      // 32-bit signed (underscores for readability)
    long myLong = 10_000_000_000L; // 64-bit signed (L suffix for long literals)
    ubyte myUbyte = 200;        // 8-bit unsigned
    uint myUint = 2_000_000_000U; // 32-bit unsigned (U suffix)

    // Floating-point types
    float myFloat = 3.14f;      // Single-precision (32-bit)
    double myDouble = 3.14159;  // Double-precision (64-bit)
    real myReal = 1.618L;       // Extended-precision (usually 80-bit)

    // Boolean
    bool myBool = true;         // true or false

    // Characters and Strings
    // D has built-in support for UTF-8 (char), UTF-16 (wchar), UTF-32 (dchar).
    char myChar = 'D';          // UTF-8 code unit (byte)
    wchar myWchar = 'Ħ';        // UTF-16 code unit (word)
    dchar myDchar = '🚀';       // UTF-32 code unit (double word)

    string myString = "Hello, D String!"; // string is alias for immutable(char)[]
    immutable(char)[] anotherString = "Another one.";
    char[] mutableString = "Mutable".dup; // Mutable string (char[])

    // Arrays
    int[] dynamicArray = [1, 2, 3];         // Dynamic array (can grow/shrink)
    int[5] staticArray = [4, 5, 6, 7, 8];   // Static array (fixed size)
    int[] emptyArray;                       // Declares an empty dynamic array (null reference)
    emptyArray ~= 9;                        // Append to dynamic array
    writeln("Dynamic array: ", dynamicArray);
    writeln("Static array: ", staticArray);

    // Associative Arrays (Hash Maps/Dictionaries)
    string[int] ages; // Key: int, Value: string
    ages[1] = "Alice";
    ages[2] = "Bob";
    writeln("Age of 1: ", ages[1]);
    if (1 in ages) { // Check if key exists
        writeln("1 is in ages.");
    }

    // Pointers (less common in modern D, but available)
    int* ptr = &myInt;
    writeln("Value via pointer: ", *ptr);

    // Type inference with 'auto'
    auto inferredValue = "Inferred string";
    writeln("Inferred value: ", inferredValue);

    // Constants
    const int MAX_USERS = 100; // Compile-time constant
    immutable string GREET_MSG = "Welcome!"; // Immutable value (runtime or compile-time)

    // 3. Operators
    // Arithmetic: +, -, *, /, %, ^^ (power)
    int power = 2 ^^ 3; // 8
    writeln("2^3 = ", power);

    // Assignment: =, +=, -=, *=, /=, %=, &=, |=, ^=, <<=, >>=, >>>=
    int a = 10;
    a += 5; // a is now 15

    // Relational: ==, !=, <, >, <=, >=
    bool areEqual = (a == 15);

    // Logical: && (and), || (or), ! (not)
    bool result = (true && !false);

    // Bitwise: &, |, ^, ~, <<, >>, >>>
    int bitwiseAnd = 5 & 3; // 101 & 011 = 001 (1)

    // Unary: ++, --
    int b = 5;
    b++; // b is 6

    // Ternary Conditional: condition ? true_value : false_value
    string status = (a > 10) ? "Greater" : "Not Greater";

    // `is` operator (type comparison)
    if (a is int) { // Checks if 'a' is an 'int' type (or compatible reference)
        writeln("a is an int.");
    }

    // `in` operator (for associative arrays)
    if (1 in ages) {
        writeln("Key 1 exists in 'ages'.");
    }

    // 4. Control Structures

    // If-ElseIf-Else
    if (myInt > 1_000_000)
    {
        writeln("myInt is large.");
    }
    else if (myInt == 1_000_000)
    {
        writeln("myInt is exactly one million.");
    }
    else
    {
        writeln("myInt is small.");
    }

    // Switch Statement
    string day = "Tuesday";
    switch (day)
    {
        case "Monday":
            writeln("It's Monday.");
            break; // Fall-through is explicit, not implicit
        case "Tuesday":
            writeln("It's Tuesday.");
            goto case; // Explicit fall-through
        case "Wednesday":
            writeln("Midweek.");
            break;
        default:
            writeln("Some other day.");
            break;
    }

    // For Loop
    for (int i = 0; i < 5; ++i)
    {
        writeln("For loop iteration: ", i);
    }

    // Foreach Loop (for iterating over collections)
    string[] names = ["Alice", "Bob", "Charlie"];
    foreach (string name; names) // Only value
    {
        writeln("Name: ", name);
    }
    foreach (int index, string value; names) // Index and value
    {
        writeln("Index ", index, ": ", value);
    }

    // While Loop
    int counter = 0;
    while (counter < 3)
    {
        writeln("While loop iteration: ", counter);
        counter++;
    }

    // Do-While Loop (executes at least once)
    int doCounter = 0;
    do
    {
        writeln("Do-While loop iteration: ", doCounter);
        doCounter++;
    } while (doCounter < 2);

    // Label and Goto (use sparingly)
    // myLabel:
    // writeln("Reached label");
    // goto myLabel;

    // `break` and `continue` keywords
    for (int i = 0; i < 10; ++i) {
        if (i % 2 != 0) continue; // Skip odd numbers
        if (i >= 6) break;       // Exit loop after 6
        writeln("Break/Continue example: ", i);
    }

    // 5. Functions
    // Function definition
    greet("Programmer");
    int sumResult = add(10, 20);
    writeln("Sum: ", sumResult);

    // Function with default arguments
    printMessage("Hello");
    printMessage("Goodbye", true);

    // Variadic functions (fixed type arguments, C-style varargs not recommended)
    // Use tuple arguments or dynamic arrays for varying arguments in modern D.
    // D's `std.format` handles printf-style formatting.

    // Lambda functions (anonymous functions/delegates)
    auto square = (int x) => x * x;
    writeln("Square of 5: ", square(5));

    // Delegates (function pointers)
    int delegate(int, int) op = &add; // Delegate to the 'add' function
    writeln("Delegate add: ", op(7, 3));

    // Nested functions
    string outerVar = "Outer";
    void nestedFunction() {
        writeln("From nested: ", outerVar);
    }
    nestedFunction();

    // 6. Structs and Classes (Object-Oriented Programming)

    // Structs (value types, no inheritance)
    MyStruct s1; // Default initialized
    s1.x = 10;
    s1.y = 20;
    writeln("Struct s1: (", s1.x, ", ", s1.y, ")");
    MyStruct s2 = MyStruct(30, 40); // Constructor
    writeln("Struct s2: (", s2.x, ", ", s2.y, ")");

    // Classes (reference types, support inheritance, polymorphism)
    // Creating an object (instance of a class)
    MyClass obj1 = new MyClass(); // Constructor call
    obj1.name = "Class Instance 1";
    obj1.printName();

    MyClass obj2 = new MyClass("Class Instance 2");
    obj2.printName();

    // Inheritance
    DerivedClass dObj = new DerivedClass("Derived Object");
    dObj.printName(); // Inherited method
    dObj.printDerived(); // Derived method

    // Interfaces
    IPrintable p = new MyClass("Interface Object");
    p.printName(); // Calling method through interface

    // Enums
    Color myColor = Color.Blue;
    writeln("My color: ", myColor);

    // Unions (memory overlay)
    MyUnion u;
    u.i = 123;
    writeln("Union as int: ", u.i);
    u.f = 4.5f;
    writeln("Union as float: ", u.f); // i's bits are now interpreted as float

    // 7. Templates (Generics)
    // Function template
    writeln("Max of int: ", max(10, 20));
    writeln("Max of double: ", max(3.14, 2.71));

    // Class template
    Container!int intContainer; // Container of ints
    intContainer.value = 100;
    writeln("Int container value: ", intContainer.value);

    Container!string stringContainer; // Container of strings
    stringContainer.value = "Template String";
    writeln("String container value: ", stringContainer.value);

    // 8. Mixins (Code injection at compile time)
    string mixinCode = "int mixedInVar = 10;";
    mixin(mixinCode); // Injects 'int mixedInVar = 10;' here
    writeln("Mixed-in variable: ", mixedInVar);

    // 9. Contract Programming (Design by Contract - DbC)
    // Invariants, Preconditions, Postconditions
    // (See `PositiveNumber` struct below)
    PositiveNumber posNum = PositiveNumber(5);
    // PositiveNumber negNum = PositiveNumber(-1); // This would assert at runtime

    // 10. Concurrency (Fibers, Threads, Futures, Async/Await)
    // (A full example is too large for syntax, but keywords exist)
    // import core.thread;
    // import std.parallelism;
    // auto myThread = new Thread({ writeln("Hello from thread!"); });
    // myThread.start();
    // myThread.join();

    // 11. Unit Testing (Built-in)
    // 'unittest' blocks are compiled only when testing
    // unittest {
    //    assert(add(1, 1) == 2);
    //    assert(max(5, 10) == 10);
    // }

    // 12. D's "universal" types: `void`, `auto`, `typeof`
    void noReturnFunction() {
        writeln("This function returns nothing.");
    }
    // typeof(myInt) x; // Declares x with the type of myInt

    // 13. Compile-Time Function Execution (CTFE)
    // Functions marked as `pure` and using only pure constructs can run at compile time.
    enum int compiledValue = add(10, 20); // add() runs at compile time
    writeln("Compiled value: ", compiledValue);

    return 0; // Standard exit code for success
}

// Function definitions
void greet(string name)
{
    writeln("Hello, ", name, " from a D function!");
}

int add(int a, int b)
{
    // Function contracts (DbC)
    in { // Preconditions
        assert(a >= 0 && b >= 0, "Arguments must be non-negative.");
    }
    out (result) { // Postconditions
        assert(result >= 0, "Result must be non-negative.");
        assert(result == a + b, "Result must be sum of a and b.");
    }
    body {
        return a + b;
    }
}

// Function with default arguments
void printMessage(string msg, bool addExclamation = false)
{
    if (addExclamation)
    {
        writeln(msg, "!");
    }
    else
    {
        writeln(msg);
    }
}

// Struct definition
struct MyStruct
{
    int x;
    int y;

    // Constructor for structs
    this(int x, int y)
    {
        this.x = x;
        this.y = y;
    }

    // Invariant (for structs and classes) - checked at entry/exit of public methods
    invariant() {
        assert(x >= 0 && y >= 0, "Coordinates must be non-negative.");
    }
}

// Class definition
class MyClass : IPrintable // Implements IPrintable interface
{
    string name;

    // Constructor
    this()
    {
        this.name = "Default Name";
    }

    this(string name)
    {
        this.name = name;
    }

    // Method
    void printName()
    {
        writeln("My name is: ", name);
    }

    // Invariant for class
    invariant() {
        assert(name !is null && name.length > 0, "Name cannot be null or empty.");
    }
}

// Inheritance
class DerivedClass : MyClass // DerivedClass inherits from MyClass
{
    this(string name)
    {
        super(name); // Call base class constructor
    }

    void printDerived()
    {
        writeln("This is a derived class method.");
    }

    // You can override methods (implicitly or explicitly with 'override' keyword)
    override void printName() {
        writeln("Overridden name: ", name);
    }
}

// Interface definition
interface IPrintable
{
    void printName();
}

// Enum definition
enum Color
{
    Red,
    Green,
    Blue
}

// Union definition
union MyUnion
{
    int i;
    float f;
}

// Function Template
T max(T)(T a, T b) if (isNumeric!T) // 'if' is a template constraint
{
    return (a > b) ? a : b;
}

// Class Template
class Container(T)
{
    T value;
}

// Example of Design by Contract with a struct
struct PositiveNumber
{
    int value;

    this(int val)
    {
        in { // Precondition
            assert(val >= 0, "PositiveNumber must be non-negative.");
        }
        body {
            value = val;
        }
    }

    // Invariant (checked after creation and before/after public method calls)
    invariant() {
        assert(value >= 0, "Invariant: PositiveNumber value must remain non-negative.");
    }
}