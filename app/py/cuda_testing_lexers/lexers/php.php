<!doctype test="123">
<!-- comment -->

<!-- sdfsdfsdf
sdfsdfsdfs
sdfsdfsdf -->

<meta1/>
<meta2>text</meta3>
<?php echo 'test', 20

// sdfdsfsd
/* ddsdsfdsf */
?> 

<html> 
  <head>
    <title test="15" #$@ #aa22cc >Text</title>
  </head>

  <script type="text/javascript">
    var n=10; 
  </script>
  <script type="text/javascript" src="<?=SITE_TEMPLATE_PATH?>/js/jquery.js">
    var n=10;
  </script>
      
  <body src="www.com" style='aaa; aaa; aaa;'> 
    <h1 color=#dde color=yellow color=violet >Text &nbsp;</h1> 
  </body> 
</html>


<?php
// PHP code is typically embedded within   ...   tags.
// Anything outside these tags is treated as plain HTML.

// Single-line comment
# Another style of single-line comment
/*
 * Multi-line comment.
 * Can span multiple lines.
 */

// 1. Basic Output
 
// echo "Hello, PHP!\n"; // echo is a language construct, slightly faster than print
/* echo "Hello, PHP!\n"; // echo is a language construct, slightly faster than print
echo "Hello, PHP!\n"; // echo is a language construct, slightly faster than print
echo "Hello, PHP!\n"; // echo is a language construct, slightly faster than print */


echo "Hello, PHP!\n"; // echo is a language construct, slightly faster than print
print "This is another way to print.\n"; // print is a function

// You can also concatenate strings using the dot operator
echo "Hello " . "World!\n";

// HTML output
echo "<h1>PHP Basics</h1>";
?>
<p>This is plain HTML.</p>
<?php

// 2. Variables and Data Types
// All variables in PHP start with a dollar sign ($).
// PHP is dynamically typed, meaning you don't declare the variable type explicitly.

// Scalar Types:
$name = "Alice";    // String
$age = 30;          // Integer
$price = 19.99;     // Float (or double)
$is_active = true;  // Boolean (true or false, case-insensitive)
$null_var = null;   // Null (represents no value)

echo "Name: " . $name . ", Age: " . $age . ", Price: " . $price . "\n";
echo "Is Active: " . ($is_active ? "Yes" : "No") . "\n"; // Ternary for boolean output

// Compound Types:
// Arrays (ordered map)
// Numeric array (indexed starting from 0 by default)
$fruits = ["Apple", "Banana", "Cherry"]; // Short array syntax (PHP 5.4+)
// $fruits = array("Apple", "Banana", "Cherry"); // Old syntax

echo "First fruit: " . $fruits[0] . "\n";
echo "Last fruit: " . $fruits[2] . "\n";

// Associative array (key-value pairs)
$person = [
    "first_name" => "John",
    "last_name" => "Doe",
    "age" => 25
];
echo "Person's name: " . $person["first_name"] . " " . $person["last_name"] . "\n";

// Multi-dimensional array
$matrix = [
    [1, 2, 3],
    [4, 5, 6]
];
echo "Matrix element: " . $matrix[0][1] . "\n"; // Accessing element at row 0, column 1 (value 2)

// Objects (instances of classes) - covered more in OOP section
class Car {
    public $make;
    public $model;
}
$my_car = new Car();
$my_car->make = "Toyota";
$my_car->model = "Camry";
echo "My car: " . $my_car->make . " " . $my_car->model . "\n";

// Special Type: Resource (e.g., file handles, database connections)
// This is typically implicitly managed, but exists.

// Type juggling (PHP's dynamic typing can automatically convert types)
$num_string = "10";
$total = $num_string + 5; // $num_string is converted to an integer 10
echo "Type juggling example: " . $total . "\n";

// 3. Operators
// Arithmetic: +, -, *, /, %, **, ++, --
$res = 10 + 5;
$res++; // $res is now 16

// Assignment: =, +=, -=, *=, /=, .= (for string concat)
$x = 10;
$x += 5; // $x is 15
$message = "Hello";
$message .= " World"; // $message is "Hello World"

// Comparison: == (loose equality), === (strict equality), !=, !==, <, >, <=, >=, <=> (spaceship PHP 7+)
echo "5 == '5': " . (5 == '5' ? "true" : "false") . "\n";   // true
echo "5 === '5': " . (5 === '5' ? "true" : "false") . "\n"; // false (different types)

// Logical: && (AND), || (OR), ! (NOT), xor (exclusive OR)
if ($age > 18 && $is_active) {
    echo "Access granted.\n";
}

// Null Coalescing Operator (PHP 7+)
// Checks if a variable is set and not null, otherwise uses a default value
$username = $_GET['user'] ?? 'Guest'; // If $_GET['user'] is not set or is null, $username becomes 'Guest'
echo "Username: " . $username . "\n";

// 4. Control Structures

// If-Elseif-Else
$score = 85;
if ($score >= 90) {
    echo "Grade: A\n";
} elseif ($score >= 80) {
    echo "Grade: B\n";
} else {
    echo "Grade: C or below\n";
}

// Switch Statement
$day = "Wednesday";
switch ($day) {
    case "Monday":
    case "Tuesday":
        echo "Start of week.\n";
        break; // break is essential to prevent fall-through
    case "Wednesday":
        echo "Midweek.\n";
        break;
    default:
        echo "Weekend or invalid day.\n";
        break;
}

// For Loop
for ($i = 0; $i < 3; $i++) {
    echo "For loop iteration: " . $i . "\n";
}

// Foreach Loop (common for arrays)
foreach ($fruits as $fruit) { // Iterates over values
    echo "I like " . $fruit . "\n";
}
foreach ($person as $key => $value) { // Iterates over key-value pairs
    echo $key . ": " . $value . "\n";
}

// While Loop
$counter = 0;
while ($counter < 2) {
    echo "While loop iteration: " . $counter . "\n";
    $counter++;
}

// Do-While Loop (executes at least once)
$do_counter = 0;
do {
    echo "Do-While loop iteration: " . $do_counter . "\n";
    $do_counter++;
} while ($do_counter < 1);

// Break and Continue
for ($j = 0; $j < 5; $j++) {
    if ($j == 2) {
        continue; // Skips current iteration
    }
    if ($j == 4) {
        break;    // Exits loop entirely
    }
    echo "Loop control: " . $j . "\n";
}

// 5. Functions
// Function definition
function greet($name) {
    echo "Hello, " . $name . " from a PHP function!\n";
}
greet("Developer");

// Function with return value
function add_numbers($num1, $num2) {
    return $num1 + $num2;
}
$sum_result = add_numbers(5, 7);
echo "Sum: " . $sum_result . "\n";

// Function with default parameter value
function get_info($item, $quantity = 1) {
    echo "You requested " . $quantity . " of " . $item . ".\n";
}
get_info("Milk");         // Uses default quantity 1
get_info("Bread", 2);     // Overrides default quantity

// Scalar Type Declarations and Return Type Declarations (PHP 7+)
function multiply(float $a, float $b): float {
    return $a * $b;
}
echo "Multiplication: " . multiply(2.5, 4) . "\n";

// Variadic functions (accepts variable number of arguments)
function sum_all(...$numbers) {
    $total = 0;
    foreach ($numbers as $num) {
        $total += $num;
    }
    return $total;
}
echo "Sum all: " . sum_all(1, 2, 3, 4, 5) . "\n";

// Anonymous Functions (Closures)
$greeter = function($name_arg) {
    echo "Anonymous hello, " . $name_arg . "!\n";
};
$greeter("Guest");

// Anonymous Function with `use` (to import variables from parent scope)
$factor = 10;
$multiplier = function($num) use ($factor) {
    return $num * $factor;
};
echo "Multiplier result: " . $multiplier(5) . "\n";

// Arrow Functions (PHP 7.4+) - concise one-liner closures
$adder = fn($a, $b) => $a + $b;
echo "Arrow function sum: " . $adder(8, 2) . "\n";


// 6. Object-Oriented Programming (OOP)

// Class definition
class Product {
    // Properties (variables within a class)
    // Access modifiers: public, protected, private
    public $name;
    protected $price;
    private $sku; // Only accessible from within this class

    // Constants
    const CATEGORY = "General";

    // Static property
    public static $instanceCount = 0;

    // Constructor (called when object is created)
    public function __construct($name, $price, $sku) {
        $this->name = $name; // $this refers to the current object
        $this->price = $price;
        $this->sku = $sku;
        self::$instanceCount++; // Access static property using self::
    }

    // Methods (functions within a class)
    public function getDetails() {
        return "Product: " . $this->name . ", Price: $" . $this->price . ", SKU: " . $this->sku;
    }

    // Public getter for protected property
    public function getPrice() {
        return $this->price;
    }

    // Static method
    public static function getCategory() {
        return self::CATEGORY; // Access constant using self::
    }
}

// Create objects (instantiate a class)
$product1 = new Product("Laptop", 1200.50, "LAP001");
$product2 = new Product("Mouse", 25.00, "MOU002");

echo $product1->getDetails() . "\n";
echo $product2->getDetails() . "\n";
echo "Product Category: " . Product::CATEGORY . "\n"; // Accessing class constant
echo "Total products created: " . Product::$instanceCount . "\n"; // Accessing static property
echo "Static category method: " . Product::getCategory() . "\n"; // Calling static method

// Inheritance
class DigitalProduct extends Product {
    public $downloadLink;

    public function __construct($name, $price, $sku, $link) {
        parent::__construct($name, $price, $sku); // Call parent constructor
        $this->downloadLink = $link;
    }

    // Method overriding
    public function getDetails() {
        // Access protected property from parent using parent::
        return parent::getDetails() . ", Download: " . $this->downloadLink;
    }

    // Public method to access protected property from parent
    public function getProtectedPrice() {
        return $this->price; // Accessible because DigitalProduct is a child class
    }
}

$ebook = new DigitalProduct("PHP Guide", 19.99, "EBOOK001", "http://example.com/ebook");
echo $ebook->getDetails() . "\n";
echo "Ebook price: " . $ebook->getProtectedPrice() . "\n";

// Interfaces
interface Logger {
    public function logMessage(string $message): void;
}

class DatabaseLogger implements Logger {
    public function logMessage(string $message): void {
        echo "Logging to database: " . $message . "\n";
    }
}

$dbLogger = new DatabaseLogger();
$dbLogger->logMessage("User logged in.");

// Abstract Classes
abstract class Shape {
    protected $color;

    public function __construct(string $color) {
        $this->color = $color;
    }

    abstract public function calculateArea(): float; // Abstract method, must be implemented by children
    public function getColor(): string {
        return $this->color;
    }
}

class Circle extends Shape {
    public $radius;

    public function __construct(string $color, float $radius) {
        parent::__construct($color);
        $this->radius = $radius;
    }

    public function calculateArea(): float {
        return M_PI * $this->radius * $this->radius;
    }
}

$myCircle = new Circle("blue", 5.0);
echo "Circle area: " . $myCircle->calculateArea() . ", Color: " . $myCircle->getColor() . "\n";

// Traits (PHP 5.4+) - for reusing code across different class hierarchies
trait Notifier {
    public function sendNotification(string $msg) {
        echo "Notification sent: " . $msg . "\n";
    }
}

class User {
    use Notifier; // Use the trait
    public $username;
    public function __construct($username) { $this->username = $username; }
}

$user = new User("JaneDoe");
$user->sendNotification("Welcome, " . $user->username . "!");

// 7. Namespaces (PHP 5.3+) - for organizing code and preventing name collisions
namespace My\App;
class DataProcessor {
    public function process(string $data) {
        echo "Processing '" . $data . "' in My\\App namespace.\n";
    }
}
// To use this outside its namespace: new My\App\DataProcessor();
// Or using 'use' statement: use My\App\DataProcessor; $processor = new DataProcessor();

// Back to global namespace
namespace {
    $processor = new My\App\DataProcessor();
    $processor->process("some raw data");
}


// 8. Error Handling
// Basic error reporting settings (usually in php.ini)
ini_set('display_errors', 1);
error_reporting(E_ALL);

// Exceptions (PHP 5+)
function divide(int $numerator, int $denominator): float {
    if ($denominator === 0) {
        throw new Exception("Division by zero is not allowed."); // Throw an exception
    }
    return $numerator / $denominator;
}

try {
    echo "Division result: " . divide(10, 2) . "\n";
    echo "Division result: " . divide(10, 0) . "\n"; // This will throw and skip the next line
    echo "This line will not be executed after error.\n";
} catch (Exception $e) { // Catch the exception
    echo "Caught an exception: " . $e->getMessage() . " on line " . $e->getLine() . "\n";
} finally {
    echo "This 'finally' block always executes.\n";
}

// 9. Superglobals
// Pre-defined global arrays accessible from anywhere.
// $_GET: For URL query parameters (e.g., ?id=1)
// $_POST: For data sent via HTTP POST method (e.g., form submissions)
// $_SERVER: Server and execution environment information (e.g., $_SERVER['REQUEST_METHOD'])
// $_SESSION: Session variables
// $_COOKIE: HTTP Cookies
// $_FILES: Uploaded files
// $_REQUEST: Contains contents of $_GET, $_POST, and $_COOKIE (use with caution due to mixed sources)
// $_ENV: Environment variables

// Example (would typically be used in web context):
// if (isset($_GET['param'])) {
//     echo "URL parameter 'param' is: " . htmlspecialchars($_GET['param']) . "\n";
// }

// 10. Including and Requiring Files
// include 'header.php'; // Includes a file. If file not found, warning but script continues.
// require 'config.php'; // Includes a file. If file not found, fatal error and script stops.
// include_once 'functions.php'; // Includes only if not already included.
// require_once 'classes.php'; // Requires only if not already required.

echo "PHP syntax example finished.\n";

?>