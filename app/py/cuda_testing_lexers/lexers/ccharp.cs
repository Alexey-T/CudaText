// Single-line comment
/*
 * Multi-line comment
 * Can span multiple lines.
 */
 
//Comment
   ///Comment doc
   ///doc
#  pragma Test
test = @"begin
end\" + "str\"d\"" + 'd' + 0xF0 + 1.2e-3;
from nn select dd; //Linq
enum Days {P1, P2, P3};
 
private static List<T1<aa, bb, cc>, T2> PreFormatNames(
  int new_acct_key, 
  string new_acct_name, 
  List<string> _trivialWordList, 
  bool IsFilterTrivialWord_On)
{
}

public class Stack: IEdit, IEdit2
{
  public delegate int[] DelDemo();
  private Node first = null;
  public bool Empty {
    //property
    get {
      return (first == null);
    }
  }
  public object IEdit.Pop() {
    if (first == null)
      throw new Exception("Err");
    else {
      return temp;
    }
  }
  
  static void RegisterFunction(Type reg1; Type2 reg2)
  {
    CategoryRegistration(registerType);
  }

  [ComUnregisterFunction()]
  [ComVisible(false)] 
  static void UnregisterFunction(Type reg1; Type[] regs; Type2 reg2)
  {
    CategoryUnregistration(registerType);
  }
}

struct MyData { }
bool operator + (v, vv) {}
bool this[n] {}
namespace N {}



// 1. Basic Structure and Namespaces
// 'using' directives allow you to use types from a namespace without fully qualifying them.
using System; // Provides fundamental classes and base types
using System.Collections.Generic; // For generic collections like List<T>, Dictionary<TKey, TValue>

// Namespaces organize code and prevent naming collisions.
namespace CSharpSyntaxExample
{
    // A class is the blueprint for creating objects.
    public class Program
    {
        // The Main method is the entry point of a C# console application.
        // C# 9.0 and later also support "top-level statements" which remove
        // the need for a Program class and Main method in a single file.
        static void Main(string[] args)
        {
            Console.WriteLine("Hello, C#!"); // Output to the console

            // 2. Variables and Data Types
            // C# is strongly-typed: you declare the type of a variable.
            int myInteger = 10;                     // Integer (32-bit signed)
            double myDouble = 3.14;                 // Double-precision floating-point
            float myFloat = 1.23f;                  // Single-precision floating-point (note 'f' suffix)
            decimal myDecimal = 99.99m;             // High-precision decimal (note 'm' suffix)
            bool myBoolean = true;                  // Boolean (true/false)
            char myChar = 'A';                      // Single character
            string myString = "This is a string.";  // String of characters

            // Implicitly typed local variables (C# 3.0+)
            var inferredInt = 20; // Type is inferred as int
            var inferredString = "Hello"; // Type is inferred as string

            // Constants (value cannot change after declaration)
            const int MaxValue = 100;

            // Nullable types (for value types)
            int? nullableInt = null; // A nullable integer

            // 3. Operators
            // Arithmetic
            int sum = myInteger + 5;
            int difference = myInteger - 3;
            int product = myInteger * 2;
            double quotient = myDouble / 2.0;
            int remainder = myInteger % 3;

            // Assignment
            int x = 5;
            x += 2; // x is now 7 (x = x + 2)

            // Relational
            bool isEqual = (10 == 10);
            bool isNotEqual = (10 != 5);
            bool isLess = (5 < 10);
            bool isGreater = (10 > 5);
            bool isLessEqual = (10 <= 10);
            bool isGreaterEqual = (10 >= 5);

            // Logical
            bool andOp = (true && false); // false
            bool orOp = (true || false);  // true
            bool notOp = (!true);         // false

            // String Concatenation
            string greeting = "Hello, " + myString; // "Hello, This is a string."
            // String Interpolation (C# 6.0+) - preferred
            string interpolatedGreeting = $"Hello, {myString}";

            // 4. Control Structures

            // If-ElseIf-Else
            if (myInteger > 10)
            {
                Console.WriteLine("Integer is greater than 10");
            }
            else if (myInteger == 10)
            {
                Console.WriteLine("Integer is exactly 10");
            }
            else
            {
                Console.WriteLine("Integer is less than 10");
            }

            // Switch Statement
            char grade = 'B';
            switch (grade)
            {
                case 'A':
                    Console.WriteLine("Excellent!");
                    break;
                case 'B':
                case 'C': // Fall-through for multiple cases
                    Console.WriteLine("Good job!");
                    break;
                case 'D':
                    Console.WriteLine("You passed.");
                    break;
                case 'F':
                    Console.WriteLine("You failed.");
                    break;
                default:
                    Console.WriteLine("Invalid grade.");
                    break;
            }

            // For Loop
            for (int i = 0; i < 5; i++)
            {
                Console.WriteLine($"For loop iteration: {i}");
            }

            // While Loop
            int count = 0;
            while (count < 3)
            {
                Console.WriteLine($"While loop iteration: {count}");
                count++;
            }

            // Do-While Loop (executes at least once)
            int doWhileCount = 0;
            do
            {
                Console.WriteLine($"Do-While loop iteration: {doWhileCount}");
                doWhileCount++;
            } while (doWhileCount < 2);

            // Foreach Loop (for collections)
            string[] fruits = { "Apple", "Banana", "Cherry" };
            foreach (string fruit in fruits)
            {
                Console.WriteLine($"Fruit: {fruit}");
            }

            // 5. Arrays
            int[] numbers = new int[5]; // Declares an array of 5 integers
            numbers[0] = 10;
            numbers[1] = 20;
            // ...

            int[] initializedNumbers = { 1, 2, 3, 4, 5 }; // Declares and initializes

            // Multi-dimensional arrays
            int[,] matrix = new int[2, 3]; // 2 rows, 3 columns

            // Jagged arrays (array of arrays)
            int[][] jaggedArray = new int[2][];
            jaggedArray[0] = new int[] { 1, 2 };
            jaggedArray[1] = new int[] { 3, 4, 5 };

            // 6. Methods (Functions)
            // Call a static method
            SayHello("Programmer");

            // Call an instance method (requires an object)
            Calculator calc = new Calculator();
            int result = calc.Add(15, 7);
            Console.WriteLine($"Addition result: {result}");

            // Method with multiple return values using tuples (C# 7.0+)
            (int xCoord, int yCoord) coords = GetCoordinates();
            Console.WriteLine($"Coordinates: {coords.xCoord}, {coords.yCoord}");

            // Out parameters
            int parsedValue;
            bool success = int.TryParse("123", out parsedValue);
            if (success)
            {
                Console.WriteLine($"Parsed value: {parsedValue}");
            }

            // Optional parameters
            PrintInfo("John Doe");
            PrintInfo("Jane Smith", 30);

            // 7. Classes and Objects (Object-Oriented Programming)
            // Creating an object (instance of a class)
            Person person1 = new Person();
            person1.Name = "Alice";
            person1.Age = 25;
            person1.Introduce();

            Person person2 = new Person("Bob", 30); // Using a constructor
            person2.Introduce();

            // Properties
            Console.WriteLine($"Person 1's Name: {person1.Name}");

            // Static members (belong to the class, not an instance)
            Console.WriteLine($"Total people created: {Person.PopulationCount}");

            // Inheritance
            Dog myDog = new Dog("Buddy", "Golden Retriever");
            myDog.Eat();
            myDog.Bark();

            // Interfaces
            IDrawable circle = new Circle();
            circle.Draw();

            // Enums
            DaysOfWeek today = DaysOfWeek.Wednesday;
            Console.WriteLine($"Today is: {today}");

            // Structs (value types, lighter than classes for small data)
            Point p1;
            p1.X = 10;
            p1.Y = 20;
            Console.WriteLine($"Point: ({p1.X}, {p1.Y})");

            // 8. Error Handling (Exceptions)
            try
            {
                // Code that might throw an exception
                int zero = 0;
                int divisionResult = 10 / zero;
            }
            catch (DivideByZeroException ex)
            {
                Console.WriteLine($"Caught an error: {ex.Message}");
            }
            catch (Exception ex) // Generic catch-all
            {
                Console.WriteLine($"An unexpected error occurred: {ex.Message}");
            }
            finally
            {
                Console.WriteLine("This block always executes, regardless of error.");
            }

            // Throwing exceptions
            // throw new ArgumentOutOfRangeException("Value is out of range.");

            // 9. Collections (beyond arrays)
            // List<T> (dynamic array)
            List<string> shoppingList = new List<string>();
            shoppingList.Add("Milk");
            shoppingList.Add("Eggs");
            shoppingList.Remove("Milk");
            Console.WriteLine($"Items in shopping list: {shoppingList.Count}");

            // Dictionary<TKey, TValue> (key-value pairs)
            Dictionary<string, int> ages = new Dictionary<string, int>();
            ages.Add("Alice", 25);
            ages["Bob"] = 30;
            Console.WriteLine($"Bob's age: {ages["Bob"]}");

            // 10. LINQ (Language Integrated Query) - C# 3.0+
            // Allows querying data from various sources using a SQL-like syntax.
            List<int> numbersList = new List<int> { 1, 5, 2, 8, 3, 9, 4, 7, 6 };
            var evenNumbers = from num in numbersList
                              where num % 2 == 0
                              orderby num ascending
                              select num;
            Console.WriteLine("Even numbers:");
            foreach (var num in evenNumbers)
            {
                Console.WriteLine(num);
            }

            // Lambda expressions (often used with LINQ)
            var oddNumbers = numbersList.Where(n => n % 2 != 0).OrderByDescending(n => n);
            Console.WriteLine("Odd numbers (descending):");
            foreach (var num in oddNumbers)
            {
                Console.WriteLine(num);
            }

            // 11. Asynchronous Programming (async/await) - C# 5.0+
            // For non-blocking I/O operations, UI responsiveness, etc.
            // (Example: Simulate a long-running operation)
            Console.WriteLine("Starting async operation...");
            var task = DoSomethingAsync();
            Console.WriteLine("Async operation started, doing other work...");
            task.Wait(); // Wait for the async operation to complete (blocking here for example)
            Console.WriteLine("Async operation completed.");

            // 12. Events and Delegates
            // (Often used for UI programming or custom event systems)
            Publisher publisher = new Publisher();
            Subscriber subscriber1 = new Subscriber("Subscriber 1");
            Subscriber subscriber2 = new Subscriber("Subscriber 2");

            // Subscribe to the event
            publisher.MyEvent += subscriber1.HandleEvent;
            publisher.MyEvent += subscriber2.HandleEvent;

            publisher.RaiseEvent("Hello Event Listeners!");
            publisher.MyEvent -= subscriber1.HandleEvent; // Unsubscribe
            publisher.RaiseEvent("Another message!");

            // 13. Attributes
            // Metadata that can be applied to code elements.
            // [Obsolete("This method is deprecated. Use NewMethod instead.")]
            // public void OldMethod() { }
        }

        // Static method (belongs to the class, no object needed to call)
        public static void SayHello(string name)
        {
            Console.WriteLine($"Hello, {name} from a static method!");
        }

        // Method with multiple return values using a tuple
        public static (int x, int y) GetCoordinates()
        {
            return (10, 20);
        }

        // Method with optional parameters
        public static void PrintInfo(string name, int age = -1) // -1 as a sentinel for "not provided"
        {
            if (age == -1)
            {
                Console.WriteLine($"Name: {name}");
            }
            else
            {
                Console.WriteLine($"Name: {name}, Age: {age}");
            }
        }

        // Async method
        public static async Task DoSomethingAsync()
        {
            Console.WriteLine("Doing something asynchronous...");
            await Task.Delay(2000); // Simulate an I/O-bound operation (e.g., network call, file access)
            Console.WriteLine("Asynchronous operation finished!");
        }
    }

    // Class definition
    public class Person
    {
        // Fields (private by convention)
        private string _name;
        private int _age;

        // Static field (shared by all instances of the class)
        public static int PopulationCount = 0;

        // Properties (encapsulate fields, providing getter/setter access)
        public string Name
        {
            get { return _name; }
            set { _name = value; } // 'value' is an implicit parameter
        }

        // Auto-implemented property (shorthand)
        public int Age { get; set; }

        // Constructor (special method called when an object is created)
        public Person()
        {
            Name = "Unknown";
            Age = 0;
            PopulationCount++;
        }

        public Person(string name, int age)
        {
            Name = name; // Uses the property's setter
            Age = age;   // Uses the property's setter
            PopulationCount++;
        }

        // Method (behavior of the object)
        public void Introduce()
        {
            Console.WriteLine($"Hi, my name is {Name} and I am {Age} years old.");
        }
    }

    // Inheritance: Dog is a type of Animal
    public class Animal
    {
        public string Name { get; set; }

        public Animal(string name)
        {
            Name = name;
        }

        public void Eat()
        {
            Console.WriteLine($"{Name} is eating.");
        }
    }

    public class Dog : Animal // Dog inherits from Animal
    {
        public string Breed { get; set; }

        public Dog(string name, string breed) : base(name) // Call base class constructor
        {
            Breed = breed;
        }

        public void Bark()
        {
            Console.WriteLine($"{Name} (a {Breed}) is barking!");
        }
    }

    // Interface: Defines a contract of methods that a class must implement.
    public interface IDrawable
    {
        void Draw();
    }

    public class Circle : IDrawable
    {
        public void Draw()
        {
            Console.WriteLine("Drawing a circle.");
        }
    }

    // Enum (Enumeration): A set of named constant integers.
    public enum DaysOfWeek
    {
        Sunday,     // 0 by default
        Monday,     // 1
        Tuesday,    // 2
        Wednesday,  // 3
        Thursday,   // 4
        Friday,     // 5
        Saturday    // 6
    }

    // Struct: A value type, generally used for small data structures.
    public struct Point
    {
        public int X;
        public int Y;
    }

    // Delegates (Type-safe function pointers)
    // Define a delegate type
    public delegate void MyEventHandler(string message);

    // Event publisher
    public class Publisher
    {
        // Declare an event based on the delegate
        public event MyEventHandler MyEvent;

        public void RaiseEvent(string message)
        {
            // Check if there are any subscribers before raising the event
            MyEvent?.Invoke(message); // The '?' is null-conditional operator (C# 6.0+)
        }
    }

    // Event subscriber
    public class Subscriber
    {
        public string Name { get; set; }
        public Subscriber(string name)
        {
            Name = name;
        }

        // Event handler method (must match delegate signature)
        public void HandleEvent(string message)
        {
            Console.WriteLine($"{Name} received event: {message}");
        }
    }

    // Another simple class for method demonstration
    public class Calculator
    {
        public int Add(int a, int b)
        {
            return a + b;
        }
    }
}