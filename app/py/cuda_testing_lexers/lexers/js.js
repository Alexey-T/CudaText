// Single-line comment

/*
 * Multi-line comment.
 * Can span multiple lines.
 */

// JavaScript code is typically embedded in <script> tags in HTML,
// or linked as external .js files.

#!/bin/bash                         
//Comment
/** @param {string} some[].name - Name.
*/
n = 12_34_5n+ 1.2+ 1.+ 1.2e+3+ 1.e-3+ 
  +0xFF_AAn+ 0b11_00n+ 0o77_11n
AB + AB$_AB+ _AB + $$AB_AB$ + NOTconst + NOT_CONST() 
   
<?php echo 'test' ?>
var test = NaN + Object + $ + offset;
var s_regex = /^<\/?\w.*?>/gi;
    s_not_regex = 1 / 2 + (0xFF+10) / 3 + false-true;
    s_template = css` br { background: #a00; }`;
    s_mulline = " \
      <div class='"+some+"'> \
      <td width='200'> \
      "+some+'"...';
    
class MyClass {
}

function funcMenu() {
  addMenuItem('Txt', 'Ctrl+\'');
  addMenuItem('Txt3', 'Ctrl+Alt+;');
}

anon_func1: function(inc, offset) {
  return -1
}
anon_func2 = (v, x) => { return x };
    
function funcWork() {
  $('#name').html('');
  for (id in ids) {
    if (ids[id].q>0) {
      $.cookie('some', $.toJSON(someth), {path: '/'});
    } else {
      delete ids[id];
    }
  }
}

function css_in_js() {
  return (
    <JssProvider jss={jss} generateClassName={generateClassName}>
      ...
    </JssProvider>
  );
  return <div className={this.props.classes.root} />;
}


 
// 1. Variables and Data Types
// JavaScript is dynamically (loosely) typed: you don't declare the type explicitly.
// It uses 'var', 'let', and 'const' for variable declaration.
 

#! dsdsd                         
 
/**  
* ddsfsdf
* sdfsdfdsfsdfsd
*/

// var: Function-scoped, can be re-declared and re-assigned (older style, generally avoided)
var oldSchoolVar = "I'm an old var.";
oldSchoolVar = "I can be re-assigned.";
var oldSchoolVar = "I can be re-declared."; // This is valid but problematic

// let: Block-scoped, can be re-assigned, but NOT re-declared (modern choice for mutable vars)
let myMutableVariable = 10;
myMutableVariable = 20; // Valid
// let myMutableVariable = 30; // Error: Cannot redeclare block-scoped variable

// const: Block-scoped, must be initialized, and CANNOT be re-assigned (modern choice for constants)
const MY_CONSTANT = 100;
// MY_CONSTANT = 200; // Error: Assignment to constant variable.

// Primitive Data Types:
let myString = "Hello, JavaScript!"; // String
let myNumber = 42;                  // Number (integers and floats are the same type)
let myFloat = 3.14;                 // Number
let myBoolean = true;               // Boolean (true or false)
let myNull = null;                  // Null (represents intentional absence of any object value)
let myUndefined;                    // Undefined (variable declared but not assigned a value)
let mySymbol = Symbol('unique');    // Symbol (ES6, unique identifiers)
let myBigInt = 1234567890123456789012345678901234567890n; // BigInt (ES2020, for very large integers)

// Non-Primitive Data Type: Object (all other data structures are objects)
let myObject = {                    // Object Literal
    key1: "value1",
    key2: 123,
    "another-key": true             // Keys can be strings, especially if they have hyphens
};

let myArray = [1, 2, 3, "four", true]; // Array (a special type of object)

// Check type with `typeof` operator
console.log(typeof myString);   // "string"
console.log(typeof myNumber);   // "number"
console.log(typeof myBoolean);  // "boolean"
console.log(typeof myNull);     // "object" (historical bug, still persists)
console.log(typeof myUndefined); // "undefined"
console.log(typeof myObject);   // "object"
console.log(typeof myArray);    // "object" (arrays are objects)

// 2. Operators
// Arithmetic: +, -, *, /, %, ++, --
let result = 10 + 5;
let remainder = 10 % 3;
result++; // result is now 16

// Assignment: =, +=, -=, *=, /=, %=
let a = 10;
a += 5; // a is now 15

// Comparison: == (loose equality), === (strict equality), !=, !==, <, >, <=, >=
console.log(5 == '5');   // true (coerces types)
console.log(5 === '5');  // false (checks value AND type)
console.log(5 !== '5');  // true

// Logical: && (AND), || (OR), ! (NOT)
let logicalAnd = (true && false); // false
let logicalOr = (true || false);   // true
let logicalNot = (!true);          // false

// Ternary Conditional: condition ? valueIfTrue : valueIfFalse
let message = (myNumber > 10) ? "Number is large" : "Number is small";
console.log(message);

// 3. Control Structures

// If-ElseIf-Else
let temperature = 25;
if (temperature > 30) {
    console.log("It's hot!");
} else if (temperature > 20) {
    console.log("It's warm.");
} else {
    console.log("It's cool.");
}

// Switch Statement
let dayOfWeek = "Wednesday";
switch (dayOfWeek) {
    case "Monday":
        console.log("Start of the week.");
        break; // 'break' is essential to prevent fall-through
    case "Wednesday":
        console.log("Midweek.");
        break;
    default:
        console.log("Some other day.");
        // No break needed for default if it's the last case
}

// For Loop
for (let i = 0; i < 5; i++) {
    console.log("For loop iteration:", i);
}

// While Loop
let count = 0;
while (count < 3) {
    console.log("While loop iteration:", count);
    count++;
}

// Do-While Loop (executes at least once)
let doCount = 0;
do {
    console.log("Do-While loop iteration:", doCount);
    doCount++;
} while (doCount < 2);

// For...in Loop (for iterating over object properties - use with caution for arrays)
for (let key in myObject) {
    console.log(`Object property: ${key} = ${myObject[key]}`);
}

// For...of Loop (for iterating over iterable collections like Arrays, Strings, Maps, Sets)
let colors = ["red", "green", "blue"];
for (let color of colors) {
    console.log("Color:", color);
}

// Break and Continue
for (let i = 0; i < 10; i++) {
    if (i % 2 !== 0) {
        continue; // Skips odd numbers
    }
    if (i > 6) {
        break;    // Exits the loop entirely
    }
    console.log("Break/Continue example:", i);
}

// 4. Functions
// Function Declaration (hoisted: can be called before its definition)
function greet(name) {
    console.log(`Hello, ${name}!`);
}
greet("Programmer");

// Function Expression (not hoisted: must be defined before called)
const multiply = function(x, y) {
    return x * y;
};
console.log("Product:", multiply(4, 5));

// Arrow Functions (ES6+) - concise syntax, 'this' binding behaves differently
const add = (a, b) => a + b; // Implicit return for single expression
console.log("Sum:", add(7, 3));

const sayHello = () => {
    console.log("Hello from an arrow function!");
};
sayHello();

// Function with default parameters (ES6+)
function calculateTax(amount, taxRate = 0.05) {
    return amount * taxRate;
}
console.log("Tax for 100:", calculateTax(100));     // Uses default taxRate
console.log("Tax for 100 (10%):", calculateTax(100, 0.10));

// Function with rest parameters (ES6+) - collects arguments into an array
function sumAll(...numbers) {
    let total = 0;
    for (let num of numbers) {
        total += num;
    }
    return total;
}
console.log("Sum of many numbers:", sumAll(1, 2, 3, 4, 5));

// 5. Objects (More Details)
// Object Literal (common way to create objects)
let person = {
    firstName: "John",
    lastName: "Doe",
    age: 30,
    // Method within an object
    greet: function() {
        console.log(`Hi, I'm ${this.firstName} ${this.lastName}.`); // 'this' refers to the object itself
    },
    // Shorthand method syntax (ES6+)
    introduce() {
        console.log(`My name is ${this.firstName}.`);
    }
};
console.log(person.firstName);      // Access property using dot notation
console.log(person['lastName']);    // Access property using bracket notation
person.age = 31;                    // Modify property
person.greet();
person.introduce();

// Constructor Function (older way to create multiple objects of same type)
function Car(make, model) {
    this.make = make;
    this.model = model;
    this.start = function() {
        console.log(`${this.make} ${this.model} is starting.`);
    };
}
let myCar = new Car("Toyota", "Camry"); // Use 'new' keyword
myCar.start();

// ES6 Classes (Syntactic sugar over prototype-based inheritance)
class Animal {
    constructor(name) {
        this.name = name;
    }
    eat() {
        console.log(`${this.name} is eating.`);
    }
}
class Dog extends Animal { // 'extends' keyword for inheritance
    constructor(name, breed) {
        super(name); // Call parent class constructor
        this.breed = breed;
    }
    bark() {
        console.log(`${this.name} (${this.breed}) barks!`);
    }
    // Method Overriding
    eat() {
        console.log(`${this.name} (the dog) is happily munching.`);
    }
}
let spot = new Dog("Spot", "Dalmatian");
spot.eat(); // Calls the overridden method
spot.bark();

// 6. Arrays (More Details)
let numbers = [1, 2, 3, 4, 5];
console.log("Array length:", numbers.length);
numbers.push(6);     // Add to end
numbers.pop();       // Remove from end
numbers.unshift(0);  // Add to beginning
numbers.shift();     // Remove from beginning
console.log("Modified array:", numbers);

// Array methods (Higher-order functions)
numbers.forEach(function(num) {
    console.log("Number in forEach:", num);
});

let doubledNumbers = numbers.map(num => num * 2);
console.log("Doubled numbers (map):", doubledNumbers);

let evenNumbers = numbers.filter(num => num % 2 === 0);
console.log("Even numbers (filter):", evenNumbers);

let sumOfNumbers = numbers.reduce((accumulator, currentValue) => accumulator + currentValue, 0);
console.log("Sum of numbers (reduce):", sumOfNumbers);

// Destructuring Arrays (ES6+)
const [first, second, ...rest] = [10, 20, 30, 40, 50];
console.log(first, second, rest); // 10 20 [30, 40, 50]

// Destructuring Objects (ES6+)
const { firstName, age } = person;
console.log(firstName, age); // John 31

// Spread Operator (...) (ES6+)
const arr1 = [1, 2];
const arr2 = [3, 4];
const combinedArr = [...arr1, ...arr2, 5]; // Concatenate arrays
console.log(combinedArr);

const obj1 = { x: 1, y: 2 };
const obj2 = { z: 3, x: 4 };
const combinedObj = { ...obj1, ...obj2 }; // Merge objects (later keys override)
console.log(combinedObj); // { x: 4, y: 2, z: 3 }

// 7. Asynchronous JavaScript

// Callbacks (traditional way for async operations)
function fetchData(callback) {
    setTimeout(() => { // Simulate network request
        const data = "Data from server";
        callback(data);
    }, 1000); // 1 second delay
}
fetchData(function(receivedData) {
    console.log("Callback received:", receivedData);
});
console.log("Request sent (callback style)...");

// Promises (ES6+ for better async handling)
function fetchDataPromise() {
    return new Promise((resolve, reject) => {
        setTimeout(() => {
            const success = true; // Simulate success or failure
            if (success) {
                resolve("Data from promise");
            } else {
                reject("Error fetching data");
            }
        }, 1500);
    });
}
fetchDataPromise()
    .then(data => {
        console.log("Promise resolved:", data);
        return "More data"; // Chain promises
    })
    .then(nextData => {
        console.log("Chained promise:", nextData);
    })
    .catch(error => {
        console.error("Promise rejected:", error);
    });
console.log("Request sent (promise style)...");

// Async/Await (ES2017+ - Syntactic sugar over Promises for cleaner async code)
async function getAsyncData() {
    try {
        console.log("Request sent (async/await style)...");
        const data = await fetchDataPromise(); // Await resolves the promise
        console.log("Async/Await received:", data);
        // You can await other promises here
    } catch (error) {
        console.error("Async/Await error:", error);
    }
}
getAsyncData();

// 8. Error Handling (try-catch-finally)
try {
    // Code that might throw an error
    let num = JSON.parse("not valid json"); // This will throw a SyntaxError
    console.log(num);
} catch (error) { // The 'error' object contains details about the error
    console.error("Caught an error:", error.message);
    // console.error(error.name); // E.g., "SyntaxError"
    // console.error(error.stack); // Stack trace
} finally {
    console.log("This 'finally' block always executes, regardless of error.");
}

// Throwing custom errors
function divide(a, b) {
    if (b === 0) {
        throw new Error("Division by zero is not allowed."); // Throw a new Error object
    }
    return a / b;
}
try {
    let divResult = divide(10, 0);
    console.log(divResult);
} catch (e) {
    console.error("Error from divide function:", e.message);
}

// 9. Modules (ES6+ - for organizing code across files)
// In a separate file (e.g., mathOperations.js):
// export function add(a, b) { return a + b; }
// export const PI = 3.14;
// export default class Calculator { /* ... */ }

// In this file:
// import { add, PI } from './mathOperations.js';
// import Calculator from './mathOperations.js';
// import * as MathUtils from './mathOperations.js'; // Import all as an object

// 10. DOM Manipulation (Client-side JavaScript)
// (This code would run in a browser's HTML context)
/*
// Get an element by its ID
const myElement = document.getElementById('myId');
if (myElement) {
    myElement.textContent = 'New text from JavaScript!'; // Change text content
    myElement.style.color = 'purple'; // Change style
    myElement.classList.add('highlight'); // Add a CSS class

    // Add an event listener
    myElement.addEventListener('click', () => {
        alert('Element was clicked!');
    });
}

// Create new elements
const newDiv = document.createElement('div');
newDiv.textContent = 'I am a new div!';
document.body.appendChild(newDiv); // Add to the body
*/

console.log("JavaScript execution finished.");