// Single-line comment
/*
 * Multi-line comment.
 * Can span multiple lines.
 */
/**
 * Javadoc comment. Used to generate API documentation.
 * @param args Command-line arguments.
 * @return An integer exit code.
 */

// 1. Package Declaration and Imports
// 'package' declares the package this source file belongs to.
// (Optional, if omitted, it's the default package).
package com.example.javasyntax;

// 'import' statements bring in classes from other packages.
// java.lang package is implicitly imported.
import java.util.ArrayList;   // For dynamic arrays
import java.util.HashMap;     // For key-value pairs (dictionaries/hash maps)
import java.util.List;        // For list interface
import java.util.Map;         // For map interface
import java.util.Scanner;     // For user input

// 2. Class Definition - The fundamental building block in Java
// Every Java program must have at least one class.
public class MainSyntaxExample {

    // 3. Main Method - The entry point for execution of a Java application
    // 'public': accessible from anywhere.
    // 'static': belongs to the class itself, not to an instance of the class.
    // 'void': returns no value.
    // 'String[] args': command-line arguments passed as an array of strings.
    public static void main(String[] args) {

        System.out.println("Hello, Java!"); // Output to console

        // 4. Variables and Data Types
        // Java is strongly (statically) typed. All variables must have a declared type.

        // Primitive Data Types (store direct values)
        byte myByte = 10;           // 8-bit signed integer (-128 to 127)
        short myShort = 100;        // 16-bit signed integer
        int myInt = 1_000_000;      // 32-bit signed integer (underscores for readability)
        long myLong = 10_000_000_000L; // 64-bit signed integer (L suffix for long literals)
        float myFloat = 3.14f;      // 32-bit floating-point (f suffix for float literals)
        double myDouble = 3.14159;  // 64-bit floating-point (default for decimals)
        boolean myBoolean = true;   // true or false
        char myChar = 'A';          // 16-bit Unicode character

        // Reference Data Types (store references to objects)
        String myString = "This is a Java string."; // String is a class, not a primitive
        // null is a special literal indicating no object reference
        String anotherString = null;

        // Constants: declared with the 'final' keyword. Value cannot be changed after initialization.
        final double PI = 3.1415926535;
        final int MAX_RETRIES = 3;

        // Type Casting (explicit conversion)
        int castInt = (int) myDouble; // double to int (truncates decimal part)
        double castDouble = myInt;    // int to double (implicit, widening conversion)

        // 5. Operators
        // Arithmetic: +, -, *, /, % (modulus)
        int sum = myInt + 5;
        int difference = myInt - 3;
        int product = myInt * 2;
        double quotient = (double) myInt / 3; // Cast to double for floating-point division
        int remainder = myInt % 3;

        // Assignment: =, +=, -=, *=, /=, %=, etc.
        int x = 5;
        x += 2; // x is now 7 (equivalent to x = x + 2)

        // Relational: ==, !=, <, >, <=, >=
        boolean isEqual = (10 == 10);
        boolean isNotEqual = (10 != 5);
        boolean isLess = (5 < 10);

        // Logical: && (AND), || (OR), ! (NOT)
        boolean andResult = (true && false); // false
        boolean orResult = (true || false);  // true
        boolean notResult = (!true);         // false

        // Increment/Decrement: ++, -- (prefix or postfix)
        int i = 0;
        i++; // i is 1
        ++i; // i is 2

        // String Concatenation: uses '+' operator
        String greeting = "Hello, " + "World!";
        String combined = myString + " " + myInt;

        // Ternary Conditional: condition ? valueIfTrue : valueIfFalse
        String status = (age >= 18) ? "Adult" : "Minor";
        System.out.println("Status: " + status);

        // 6. Control Structures

        // If-ElseIf-Else
        int age = 20;
        if (age >= 18) {
            System.out.println("You are an adult.");
        } else if (age > 0) {
            System.out.println("You are a minor.");
        } else {
            System.out.println("Invalid age.");
        }

        // Switch Statement (supports int, char, byte, short, String, enum)
        String day = "Tuesday";
        switch (day) {
            case "Monday":
                System.out.println("It's Monday.");
                break; // 'break' is necessary to exit the switch block
            case "Tuesday":
            case "Wednesday": // Fall-through for multiple cases if no 'break'
                System.out.println("It's Tuesday or Wednesday.");
                break;
            default:
                System.out.println("Some other day.");
                break;
        }

        // For Loop
        for (int k = 0; k < 5; k++) {
            System.out.println("For loop iteration: " + k);
        }

        // Enhanced For Loop (For-Each loop) - for arrays and Collections
        String[] fruits = {"Apple", "Banana", "Cherry"};
        for (String fruit : fruits) {
            System.out.println("Fruit: " + fruit);
        }

        // While Loop
        int counter = 0;
        while (counter < 3) {
            System.out.println("While loop iteration: " + counter);
            counter++;
        }

        // Do-While Loop (executes at least once)
        int doCounter = 0;
        do {
            System.out.println("Do-While loop iteration: " + doCounter);
            doCounter++;
        } while (doCounter < 2);

        // break and continue keywords
        for (int n = 0; n < 10; n++) {
            if (n == 3) {
                continue; // Skips the rest of the current iteration
            }
            if (n == 7) {
                break;    // Exits the loop entirely
            }
            System.out.println("Break/Continue example: " + n);
        }

        // 7. Methods (Functions)
        // Call a static method (belongs to the class)
        printMessage("Hello from a static method!");

        // Call an instance method (requires an object)
        Calculator myCalc = new Calculator(); // Create an object
        int addResult = myCalc.add(10, 20);
        System.out.println("Add result: " + addResult);

        // Method Overloading (same method name, different parameters)
        myCalc.display("Overloaded method called.");
        myCalc.display(123);

        // 8. Arrays
        // Declaration and initialization
        int[] numbers = new int[5]; // Declares an array of 5 integers (default to 0)
        numbers[0] = 10;
        numbers[1] = 20;
        // Accessing elements: numbers[index]

        int[] initializedArray = {1, 2, 3, 4, 5}; // Initialized with values

        // Multi-dimensional arrays
        int[][] matrix = new int[2][3]; // 2 rows, 3 columns
        matrix[0][0] = 1;

        // 9. Object-Oriented Programming (OOP) Concepts

        // Creating objects (instantiation of a class)
        // 'new' keyword is used to create an instance of a class.
        // 'Person()' calls the constructor.
        Person person1 = new Person("Alice", 25); // Calls constructor with args
        Person person2 = new Person(); // Calls default constructor

        // Accessing object's fields (attributes)
        System.out.println("Person 1 Name: " + person1.name);
        // person1.age = 26; // Direct access if 'age' were public, but it's private.

        // Calling object's methods (behaviors)
        person1.introduce();
        person2.introduce(); // Person 2 has default values

        person1.setAge(26); // Using a 'setter' method (encapsulation)
        System.out.println("Alice's new age: " + person1.getAge()); // Using a 'getter' method

        // Inheritance
        Dog myDog = new Dog("Buddy", "Golden Retriever");
        myDog.eat();    // Inherited from Animal
        myDog.bark();   // Specific to Dog

        // Polymorphism (Method Overriding)
        Animal genericAnimal = new Dog("Lucy", "Labrador"); // Animal reference, Dog object
        genericAnimal.makeSound(); // Calls Dog's overridden makeSound()
        genericAnimal = new Cat("Whiskers"); // Now references a Cat object
        genericAnimal.makeSound(); // Calls Cat's overridden makeSound()

        // Abstraction (Abstract Class)
        // You cannot instantiate an abstract class directly: new Animal("Generic") would be an error.
        // But you can use it as a reference type: Animal genericAnimal above.

        // Abstraction (Interface)
        // An interface cannot be instantiated.
        // MyInterface obj = new MyInterface(); // Error
        MyClass implementsInterface = new MyClass();
        implementsInterface.doSomething();
        implementsInterface.doAnotherThing(); // Default method from interface

        // 10. Collections Framework (Basic Usage)
        // ArrayList (dynamic array/resizable array)
        List<String> shoppingList = new ArrayList<>(); // Diamond operator <> infers type (Java 7+)
        shoppingList.add("Milk");
        shoppingList.add("Eggs");
        shoppingList.add("Bread");
        System.out.println("Shopping list: " + shoppingList);
        shoppingList.remove("Eggs");
        System.out.println("List size: " + shoppingList.size());
        System.out.println("First item: " + shoppingList.get(0));

        // HashMap (key-value pairs)
        Map<String, Integer> studentScores = new HashMap<>();
        studentScores.put("Alice", 95);
        studentScores.put("Bob", 88);
        System.out.println("Bob's score: " + studentScores.get("Bob"));
        if (studentScores.containsKey("Alice")) {
            System.out.println("Alice is in the map.");
        }
        for (String studentName : studentScores.keySet()) {
            System.out.println(studentName + " scored " + studentScores.get(studentName));
        }

        // 11. Error Handling (Exceptions)
        // try-catch block
        try {
            int result = 10 / 0; // This will cause an ArithmeticException
            System.out.println("Result: " + result); // This line will not be executed
        } catch (ArithmeticException e) {
            System.err.println("Caught an arithmetic error: " + e.getMessage());
            // e.printStackTrace(); // Prints the stack trace
        } catch (Exception e) { // Catch-all for other exceptions
            System.err.println("Caught a general error: " + e.getMessage());
        } finally {
            System.out.println("This 'finally' block always executes.");
        }

        // Throwing exceptions
        try {
            validateAge(-5);
        } catch (IllegalArgumentException e) {
            System.err.println("Error validating age: " + e.getMessage());
        }

        // 12. Input from Console (using Scanner)
        Scanner scanner = new Scanner(System.in);
        System.out.print("Enter your name: ");
        String userName = scanner.nextLine();
        System.out.println("Hello, " + userName + "!");
        scanner.close(); // Close the scanner to release resources

        // 13. Enums (Enumerations)
        Day currentDay = Day.MONDAY;
        System.out.println("Current day: " + currentDay);
        if (currentDay == Day.MONDAY) {
            System.out.println("It's the start of the week.");
        }
    } // End of main method

    // Static method (belongs to the class, no object needed to call)
    public static void printMessage(String message) {
        System.out.println(message);
    }

    // Method that throws an exception
    public static void validateAge(int age) throws IllegalArgumentException {
        if (age < 0) {
            throw new IllegalArgumentException("Age cannot be negative.");
        }
        System.out.println("Valid age: " + age);
    }

} // End of MainSyntaxExample class


// --- Additional Classes to Demonstrate OOP Concepts ---

// Class (Blueprint for objects)
class Person {
    // Fields (Attributes)
    String name;
    private int age; // 'private' access modifier: only accessible within this class

    // Constructor (Special method called when an object is created)
    // Default constructor
    public Person() {
        this.name = "Unknown";
        this.age = 0;
        System.out.println("Person created with default values.");
    }

    // Parameterized constructor (overloaded constructor)
    public Person(String name, int age) {
        this.name = name; // 'this' refers to the current object's field
        this.age = age;
        System.out.println("Person created: " + name);
    }

    // Methods (Behaviors)
    public void introduce() {
        System.out.println("Hi, my name is " + name + " and I am " + age + " years old.");
    }

    // Getter (Accessor method)
    public int getAge() {
        return age;
    }

    // Setter (Mutator method)
    public void setAge(int age) {
        if (age >= 0) { // Basic validation
            this.age = age;
        } else {
            System.err.println("Age cannot be negative!");
        }
    }
}

// Inheritance: Animal is the superclass (parent), Dog and Cat are subclasses (children)
abstract class Animal { // 'abstract' class: cannot be instantiated directly
    String name;

    public Animal(String name) {
        this.name = name;
    }

    public void eat() { // Concrete method
        System.out.println(name + " is eating.");
    }

    public abstract void makeSound(); // Abstract method: must be implemented by subclasses
}

class Dog extends Animal { // 'extends' keyword for inheritance
    String breed;

    public Dog(String name, String breed) {
        super(name); // Call the superclass's constructor
        this.breed = breed;
    }

    public void bark() {
        System.out.println(name + " (" + breed + ") says Woof!");
    }

    // Method Overriding: providing a specific implementation for an inherited method
    @Override // Annotation to indicate method overriding (good practice)
    public void makeSound() {
        System.out.println(name + " barks loudly!");
    }
}

class Cat extends Animal {
    public Cat(String name) {
        super(name);
    }

    @Override
    public void makeSound() {
        System.out.println(name + " meows softly.");
    }
}

// Interface: Defines a contract of methods that implementing classes must provide
// Interfaces can have abstract methods (implicitly public abstract), default methods (Java 8+),
// static methods (Java 8+), and constant fields (implicitly public static final).
interface MyInterface {
    // Abstract method (implicitly public abstract)
    void doSomething();

    // Default method (concrete implementation in interface)
    default void doAnotherThing() {
        System.out.println("Doing another thing from default interface method.");
    }

    // Static method (Java 8+)
    static void commonUtility() {
        System.out.println("This is a static method in an interface.");
    }
}

// Class implementing an interface
class MyClass implements MyInterface { // 'implements' keyword for interfaces
    @Override
    public void doSomething() {
        System.out.println("MyClass is doing something!");
    }
    // No need to implement doAnotherThing() because it's a default method
}


// Enum (Enumeration): A special data type that enables a variable to be a set of predefined constants.
enum Day {
    SUNDAY,
    MONDAY,
    TUESDAY,
    WEDNESDAY,
    THURSDAY,
    FRIDAY,
    SATURDAY
}

// Simple class for method demonstration
class Calculator {
    public int add(int a, int b) {
        return a + b;
    }

    // Method Overloading
    public void display(String message) {
        System.out.println("String message: " + message);
    }

    public void display(int number) {
        System.out.println("Number: " + number);
    }
}