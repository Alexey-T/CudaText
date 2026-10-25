#include <iostream>   // Input/Output operations (cin, cout)
#include <string>     // C++ string class
#include <vector>     // Dynamic array (std::vector)
#include <algorithm>  // Standard algorithms (sort, find, for_each)
#include <map>        // Associative array (std::map)
#include <memory>     // Smart pointers (std::unique_ptr, std::shared_ptr)
#include <stdexcept>  // Standard exception classes (e.g., std::runtime_error)
#include <fstream>    // File stream operations (ifstream, ofstream)
#include <limits>     // Numeric limits (e.g., for clearing input buffer)
#include <iomanip>    // Output formatting (setprecision, fixed)
#include <random>     // Random number generation
#include <chrono>     // Time utilities

// Single-line comment
/*
    Multi-line comment block.
    This program demonstrates various C++ syntax elements and standard library features.
    It's designed to be compiled with a modern C++ compiler (C++11/14/17/20 standard).
*/

// ##########################################################################
// #                            Preprocessor Directives                     #
// ##########################################################################

#define PI_MACRO 3.14159 // Old-style macro, prefer 'const' or 'constexpr'
const double E_CONSTANT = 2.71828; // Compile-time constant

// ##########################################################################
// #                            Function Declarations                       #
// ##########################################################################
// Functions in C++ can be declared before their definition.

void print_separator(const std::string& title);
int add_numbers(int a, int b);
void swap_values(int& x, int& y); // Pass by reference using &
void demonstrate_stl_vector();
void demonstrate_stl_map();
void demonstrate_custom_class();
void demonstrate_inheritance();
void demonstrate_polymorphism();
void demonstrate_templates();
void demonstrate_exception_handling();
void demonstrate_file_io_cpp_streams();
void demonstrate_smart_pointers();
void demonstrate_random_numbers();
void demonstrate_chrono();

// ##########################################################################
// #                            Main Function                               #
// ##########################################################################
// The entry point of every C++ program.

int main(int argc, char* argv[]) {
    print_separator("1. Basic Structure, Variables & I/O");

    // 1.1. Basic Data Types and Variable Declaration
    int integer_var = 100;
    float float_var = 2.71f;
    double double_var = 123.456789123;
    char char_var = 'Z';
    bool boolean_var = false;

    // Output using std::cout (stream insertion operator <<)
    std::cout << "Integer: " << integer_var << std::endl; // std::endl flushes buffer and adds newline
    std::cout << std::fixed << std::setprecision(2); // Set precision for floats/doubles
    std::cout << "Float: " << float_var << std::endl;
    std::cout << std::setprecision(9);
    std::cout << "Double: " << double_var << std::endl;
    std::cout << "Char: " << char_var << std::endl;
    std::cout << "Boolean: " << (boolean_var ? "true" : "false") << std::endl; // Ternary operator

    // C++ string (std::string) - dynamic, safe, and powerful
    std::string cpp_string = "Hello, C++ World!";
    std::cout << "C++ String: " << cpp_string << std::endl;

    // 1.2. Constants
    const int MAX_USERS = 100;
    std::cout << "Max Users (const): " << MAX_USERS << std::endl;
    std::cout << "PI (macro): " << PI_MACRO << std::endl;
    std::cout << "E (const double): " << E_CONSTANT << std::endl;

    // 1.3. User Input (std::cin, stream extraction operator >>)
    print_separator("2. User Input");
    int user_int;
    std::string user_name;

    std::cout << "Enter an integer: ";
    std::cin >> user_int;
    if (std::cin.fail()) { // Check for input errors
        std::cout << "Invalid integer input!" << std::endl;
        std::cin.clear(); // Clear error flags
        std::cin.ignore(std::numeric_limits<std::streamsize>::max(), '\n'); // Discard bad input
    } else {
        std::cout << "You entered: " << user_int << std::endl;
    }

    // Important: Clear the input buffer after numeric input and before getline
    std::cin.ignore(std::numeric_limits<std::streamsize>::max(), '\n');

    std::cout << "Enter your full name: ";
    std::getline(std::cin, user_name); // Read entire line including spaces
    std::cout << "Hello, " << user_name << "!" << std::endl;

    // ##########################################################################
    // #                            3. Operators                                #
    // ##########################################################################
    print_separator("3. Operators");
    int a = 10, b = 3;

    // Arithmetic Operators
    std::cout << "a + b = " << a + b << std::endl;
    std::cout << "a - b = " << a - b << std::endl;
    std::cout << "a * b = " << a * b << std::endl;
    std::cout << "a / b (integer div) = " << a / b << std::endl;
    std::cout << "a % b (modulo) = " << a % b << std::endl;
    std::cout << "a / b (float div) = " << static_cast<double>(a) / b << std::endl; // static_cast for type conversion

    // Increment/Decrement Operators
    std::cout << "a++ = " << a++ << " (a is now " << a << ")" << std::endl; // Post-increment
    std::cout << "++b = " << ++b << " (b is now " << b << ")" << std::endl; // Pre-increment

    // Relational and Logical Operators (return bool true/false)
    std::cout << "a > b: " << (a > b ? "true" : "false") << std::endl;
    std::cout << "a == b: " << (a == b ? "true" : "false") << std::endl;
    std::cout << "(a > 5 && b < 5): " << ((a > 5 && b < 5) ? "true" : "false") << std::endl;

    // Assignment Operators
    int c_assign = 20;
    c_assign += 5; // c_assign = c_assign + 5
    std::cout << "c_assign after c_assign += 5: " << c_assign << std::endl;

    // ##########################################################################
    // #                            4. Control Flow                             #
    // ##########################################################################
    print_separator("4. Control Flow");

    // 4.1. If/Else If/Else
    int score = 75;
    if (score >= 90) {
        std::cout << "Score: Excellent\n";
    } else if (score >= 70) {
        std::cout << "Score: Good\n";
    } else {
        std::cout << "Score: Needs Improvement\n";
    }

    // 4.2. Switch/Case
    char grade_char = 'B';
    switch (grade_char) {
        case 'A':
            std::cout << "Grade is A.\n";
            break;
        case 'B':
            std::cout << "Grade is B.\n";
            break;
        case 'C':
            std::cout << "Grade is C.\n";
            break;
        default:
            std::cout << "Grade is not A, B, or C.\n";
    }

    // 4.3. For Loop
    std::cout << "For loop (0 to 4): ";
    for (int i = 0; i < 5; ++i) {
        std::cout << i << " ";
    }
    std::cout << std::endl;

    // Range-based for loop (C++11 and later) - ideal for containers
    std::vector<int> numbers = {10, 20, 30};
    std::cout << "Range-based for loop on vector: ";
    for (int num : numbers) {
        std::cout << num << " ";
    }
    std::cout << std::endl;

    // 4.4. While Loop
    int counter = 0;
    std::cout << "While loop (0 to 2): ";
    while (counter < 3) {
        std::cout << counter << " ";
        counter++;
    }
    std::cout << std::endl;

    // 4.5. Do/While Loop
    int val_do = 5;
    std::cout << "Do/While loop (at least once): ";
    do {
        std::cout << val_do << " ";
        val_do++;
    } while (val_do < 5); // Condition is false, but runs once
    std::cout << std::endl;

    // 4.6. Break and Continue
    std::cout << "Break/Continue demo: ";
    for (int i = 0; i < 10; ++i) {
        if (i == 3) {
            continue; // Skip current iteration
        }
        if (i == 7) {
            break; // Exit loop entirely
        }
        std::cout << i << " ";
    }
    std::cout << std::endl;

    // ##########################################################################
    // #                            5. Functions                                #
    // ##########################################################################
    print_separator("5. Functions");

    int sum_result = add_numbers(50, 70); // Call function by value
    std::cout << "Sum of 50 and 70: " << sum_result << std::endl;

    int val_x = 10, val_y = 20;
    std::cout << "Before swap: val_x = " << val_x << ", val_y = " << val_y << std::endl;
    swap_values(val_x, val_y); // Pass by reference
    std::cout << "After swap: val_x = " << val_x << ", val_y = " << val_y << std::endl;

    // ##########################################################################
    // #                            6. Standard Template Library (STL)          #
    // ##########################################################################
    print_separator("6. STL: std::vector");
    demonstrate_stl_vector();

    print_separator("6. STL: std::map");
    demonstrate_stl_map();

    // ##########################################################################
    // #                            7. Classes and Objects                      #
    // ##########################################################################
    print_separator("7. Custom Class");
    demonstrate_custom_class();

    print_separator("7. Inheritance");
    demonstrate_inheritance();

    print_separator("7. Polymorphism");
    demonstrate_polymorphism();

    // ##########################################################################
    // #                            8. Templates                                #
    // ##########################################################################
    print_separator("8. Templates");
    demonstrate_templates();

    // ##########################################################################
    // #                            9. Exception Handling                       #
    // ##########################################################################
    print_separator("9. Exception Handling");
    demonstrate_exception_handling();

    // ##########################################################################
    // #                            10. File I/O (C++ Streams)                  #
    // ##########################################################################
    print_separator("10. File I/O (C++ Streams)");
    demonstrate_file_io_cpp_streams();

    // ##########################################################################
    // #                            11. Smart Pointers (C++11+)                 #
    // ##########################################################################
    print_separator("11. Smart Pointers");
    demonstrate_smart_pointers();

    // ##########################################################################
    // #                            12. Random Number Generation (C++11+)       #
    // ##########################################################################
    print_separator("12. Random Number Generation");
    demonstrate_random_numbers();

    // ##########################################################################
    // #                            13. Time Utilities (C++11+ chrono)          #
    // ##########################################################################
    print_separator("13. Time Utilities (std::chrono)");
    demonstrate_chrono();

    // ##########################################################################
    // #                            14. Command Line Arguments                  #
    // ##########################################################################
    print_separator("14. Command Line Arguments");
    std::cout << "Number of arguments: " << argc << std::endl;
    std::cout << "Arguments:" << std::endl;
    for (int i = 0; i < argc; ++i) {
        std::cout << "  argv[" << i << "]: " << argv[i] << std::endl;
    }
    if (argc > 1) {
        // C-style string to int conversion for argv
        int arg_as_int = std::atoi(argv[1]);
        std::cout << "First argument as integer: " << arg_as_int << std::endl;
    }
    std::cout << "To test this, compile and run like: ./program_name 123 \"hello world\"" << std::endl;


    print_separator("End of Demonstration");
    return 0; // Indicate successful execution
}

// ##########################################################################
// #                            Function Definitions                        #
// ##########################################################################

void print_separator(const std::string& title) {
    std::cout << "\n" << title << "\n";
    std::cout << "----------------------------------------\n";
}

int add_numbers(int a, int b) {
    return a + b;
}

// Function to swap two integers using references
void swap_values(int& x, int& y) {
    int temp = x;
    x = y;
    y = temp;
}

void demonstrate_stl_vector() {
    std::vector<int> numbers = {10, 50, 30, 20, 40};
    std::cout << "Original vector: ";
    for (int num : numbers) {
        std::cout << num << " ";
    }
    std::cout << std::endl;

    numbers.push_back(60); // Add element to end
    std::cout << "After push_back(60): ";
    for (int num : numbers) {
        std::cout << num << " ";
    }
    std::cout << std::endl;

    // Sort the vector using std::sort algorithm
    std::sort(numbers.begin(), numbers.end());
    std::cout << "Sorted vector: ";
    for (int num : numbers) {
        std::cout << num << " ";
    }
    std::cout << std::endl;

    // Access elements
    std::cout << "Element at index 2: " << numbers[2] << std::endl;
    std::cout << "Vector size: " << numbers.size() << std::endl;
}

void demonstrate_stl_map() {
    std::map<std::string, int> ages; // Map from string (key) to int (value)
    ages["Alice"] = 30;
    ages["Bob"] = 25;
    ages["Charlie"] = 35;

    std::cout << "Bob's age: " << ages["Bob"] << std::endl;

    // Iterate over map
    std::cout << "Map contents:" << std::endl;
    for (const auto& pair : ages) { // auto infers std::pair<const std::string, int>
        std::cout << "  " << pair.first << ": " << pair.second << std::endl;
    }

    // Check if key exists
    if (ages.count("David")) { // count returns 0 or 1 for std::map
        std::cout << "David is in the map." << std::endl;
    } else {
        std::cout << "David is NOT in the map." << std::endl;
    }

    ages.erase("Bob"); // Remove an element
    std::cout << "After erasing Bob, map size: " << ages.size() << std::endl;
}

// 7.1. Custom Class Definition
class Dog {
public: // Public members can be accessed from outside the class
    std::string name;
    int age;

    // Constructor: Called when an object is created
    Dog(const std::string& dogName, int dogAge) : name(dogName), age(dogAge) {
        std::cout << "Dog object created: " << name << std::endl;
    }

    // Member function (method)
    void bark() const { // 'const' means this method doesn't modify the object's data
        std::cout << name << " says Woof!" << std::endl;
    }

    // Destructor: Called when an object is destroyed
    ~Dog() {
        std::cout << "Dog object destroyed: " << name << std::endl;
    }
};

void demonstrate_custom_class() {
    Dog myDog("Buddy", 5); // Create a Dog object (calls constructor)
    myDog.bark();         // Call a member function
    std::cout << myDog.name << " is " << myDog.age << " years old." << std::endl;
    // myDog goes out of scope here, destructor is called automatically.
}

// 7.2. Inheritance
class Animal {
public:
    std::string species;
    Animal(const std::string& s) : species(s) {
        std::cout << "Animal constructor: " << species << std::endl;
    }
    void eat() const {
        std::cout << species << " is eating." << std::endl;
    }
    virtual ~Animal() { std::cout << "Animal destructor: " << species << std::endl; } // Virtual destructor for proper cleanup
};

// Derived class inherits from Animal
class Cat : public Animal {
public:
    Cat(const std::string& name) : Animal("Cat") { // Call base class constructor
        this->name = name; // 'this->' refers to current object's members
        std::cout << "Cat constructor: " << name << std::endl;
    }
    std::string name;
    void meow() const {
        std::cout << name << " says Meow!" << std::endl;
    }
    ~Cat() override { std::cout << "Cat destructor: " << name << std::endl; } // 'override' (C++11) ensures overriding a base virtual method
};

void demonstrate_inheritance() {
    Cat myCat("Whiskers");
    myCat.eat();  // Inherited method
    myCat.meow(); // Cat's specific method
}

// 7.3. Polymorphism
class Shape {
public:
    virtual void draw() const = 0; // Pure virtual function (makes Shape an abstract class)
    virtual ~Shape() {} // Virtual destructor is crucial for polymorphic deletion
};

class Circle : public Shape {
public:
    void draw() const override {
        std::cout << "Drawing a Circle." << std::endl;
    }
};

class Rectangle : public Shape {
public:
    void draw() const override {
        std::cout << "Drawing a Rectangle." << std::endl;
    }
};

void demonstrate_polymorphism() {
    // You cannot create an object of an abstract class directly: Shape myShape; // Error!
    std::vector<std::unique_ptr<Shape>> shapes; // Use smart pointers for managing objects on heap
    shapes.push_back(std::make_unique<Circle>());
    shapes.push_back(std::make_unique<Rectangle>());

    for (const auto& shape_ptr : shapes) {
        shape_ptr->draw(); // Calls the appropriate draw() method based on the actual object type
    }
    // unique_ptr handles deletion automatically when 'shapes' vector goes out of scope.
}

// 8. Templates
template <typename T> // Function template
T maximum(T a, T b) {
    return (a > b) ? a : b; // Ternary operator
}

template <typename T> // Class template
class MyContainer {
private:
    T data;
public:
    MyContainer(T val) : data(val) {}
    void print_data() const {
        std::cout << "Container data: " << data << std::endl;
    }
};

void demonstrate_templates() {
    std::cout << "Maximum of 5 and 10: " << maximum(5, 10) << std::endl;
    std::cout << "Maximum of 3.5 and 2.1: " << maximum(3.5, 2.1) << std::endl;
    std::cout << "Maximum of 'apple' and 'banana': " << maximum(std::string("apple"), std::string("banana")) << std::endl;

    MyContainer<int> intContainer(123);
    intContainer.print_data();

    MyContainer<std::string> stringContainer("Generic String");
    stringContainer.print_data();
}

// 9. Exception Handling
double divide(double numerator, double denominator) {
    if (denominator == 0) {
        throw std::runtime_error("Division by zero error!"); // Throw an exception
    }
    return numerator / denominator;
}

void demonstrate_exception_handling() {
    try {
        double result1 = divide(10.0, 2.0);
        std::cout << "10.0 / 2.0 = " << result1 << std::endl;

        double result2 = divide(10.0, 0.0); // This will throw an exception
        std::cout << "This line will not be executed." << std::endl;
    } catch (const std::runtime_error& e) { // Catch the specific exception type
        std::cerr << "Caught exception: " << e.what() << std::endl; // std::cerr for error output
    } catch (const std::exception& e) { // Catch base exception class (more general)
        std::cerr << "Caught a generic exception: " << e.what() << std::endl;
    }
    std::cout << "Program continues after exception handling." << std::endl;
}

// 10. File I/O with C++ streams
void demonstrate_file_io_cpp_streams() {
    const std::string filename = "cpp_example.txt";

    // Writing to a file (ofstream)
    std::ofstream output_file(filename); // Creates/overwrites file
    if (output_file.is_open()) {
        output_file << "Hello from C++ File I/O!" << std::endl;
        output_file << "This is the second line." << std::endl;
        output_file.close(); // Close the file
        std::cout << "Content written to " << filename << std::endl;
    } else {
        std::cerr << "Error: Could not open " << filename << " for writing." << std::endl;
    }

    // Reading from a file (ifstream)
    std::ifstream input_file(filename);
    if (input_file.is_open()) {
        std::string line;
        std::cout << "Content of " << filename << ":" << std::endl;
        while (std::getline(input_file, line)) { // Read line by line
            std::cout << line << std::endl;
        }
        input_file.close();
    } else {
        std::cerr << "Error: Could not open " << filename << " for reading." << std::endl;
    }

    // Appending to a file (ofstream with std::ios::app)
    std::ofstream append_file(filename, std::ios::app); // Open in append mode
    if (append_file.is_open()) {
        append_file << "This line was appended by C++." << std::endl;
        append_file.close();
        std::cout << "Content appended to " << filename << std::endl;
    } else {
        std::cerr << "Error: Could not open " << filename << " for appending." << std::endl;
    }

    // Clean up: delete the created file (using C-style remove for simplicity)
    if (std::remove(filename.c_str()) == 0) { // C-style remove function, needs C string
        std::cout << "Cleaned up: " << filename << " deleted." << std::endl;
    } else {
        std::cerr << "Error deleting file: " << filename << std::endl;
    }
}

// 11. Smart Pointers (unique_ptr, shared_ptr)
class Resource {
public:
    Resource(const std::string& name) : name_(name) {
        std::cout << "Resource " << name_ << " acquired." << std::endl;
    }
    ~Resource() {
        std::cout << "Resource " << name_ << " released." << std::endl;
    }
    void do_something() const {
        std::cout << "Resource " << name_ << " is doing something." << std::endl;
    }
private:
    std::string name_;
};

void demonstrate_unique_ptr() {
    std::cout << "Entering unique_ptr scope:" << std::endl;
    // unique_ptr: Exclusive ownership. Resource is automatically deleted when unique_ptr goes out of scope.
    std::unique_ptr<Resource> res1 = std::make_unique<Resource>("UniqueResource");
    res1->do_something(); // Access member functions using ->
    std::cout << "Exiting unique_ptr scope." << std::endl;
} // res1 goes out of scope here, destructor of Resource is called.

void demonstrate_shared_ptr() {
    std::cout << "Entering shared_ptr scope:" << std::endl;
    // shared_ptr: Shared ownership. Resource is deleted when last shared_ptr pointing to it is destroyed.
    std::shared_ptr<Resource> res_owner = std::make_shared<Resource>("SharedResource");
    res_owner->do_something();

    std::cout << "Creating a copy of shared_ptr..." << std::endl;
    std::shared_ptr<Resource> res_copy = res_owner; // Copying increments reference count
    res_copy->do_something();

    std::cout << "Exiting inner shared_ptr scope (res_copy)." << std::endl;
} // res_copy goes out of scope, reference count decrements. Resource is NOT deleted yet.
// res_owner goes out of scope after this function call, reference count becomes 0, then Resource is deleted.

void demonstrate_smart_pointers() {
    demonstrate_unique_ptr();
    std::cout << "\n";
    demonstrate_shared_ptr();
}

// 12. Random Number Generation (C++11)
void demonstrate_random_numbers() {
    // 1. Create a random number engine (e.g., Mersenne Twister)
    std::random_device rd; // Provides a seed for the engine
    std::mt19937 gen(rd()); // Mersenne Twister engine seeded by random_device

    // 2. Create a distribution
    std::uniform_int_distribution<> distrib_int(1, 10); // Uniform integer distribution between 1 and 10 (inclusive)
    std::uniform_real_distribution<> distrib_real(0.0, 1.0); // Uniform real distribution between 0.0 and 1.0

    std::cout << "5 random integers between 1 and 10: ";
    for (int i = 0; i < 5; ++i) {
        std::cout << distrib_int(gen) << " "; // Generate number using engine and distribution
    }
    std::cout << std::endl;

    std::cout << "3 random real numbers between 0.0 and 1.0: ";
    for (int i = 0; i < 3; ++i) {
        std::cout << distrib_real(gen) << std::fixed << std::setprecision(4) << " ";
    }
    std::cout << std::endl;
}

// 13. Time Utilities (std::chrono)
void demonstrate_chrono() {
    // Get current time point
    auto now = std::chrono::system_clock::now();
    std::time_t current_time = std::chrono::system_clock::to_time_t(now);
    std::cout << "Current time: " << std::ctime(&current_time); // ctime includes newline

    // Measure elapsed time
    auto start = std::chrono::high_resolution_clock::now();
    volatile double sum = 0; // volatile to prevent optimization
    for (long long i = 0; i < 10000000; ++i) {
        sum += sqrt(i);
    }
    auto end = std::chrono::high_resolution_clock::now();

    std::chrono::duration<double> elapsed_seconds = end - start;
    std::cout << "Elapsed time for loop: " << elapsed_seconds.count() << " seconds." << std::endl;

    // Create a duration
    auto one_second = std::chrono::seconds(1);
    std::cout << "One second is " << one_second.count() << " seconds." << std::endl;
}