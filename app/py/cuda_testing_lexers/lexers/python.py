#comment
test = u"""doc
string""" + r"""doc
string""" + "long \
string"; #end
11_22_3.44_5 + 0x_FF_aa_b_0 + 0b10_1_0
+ 3. + .3 + 3.0 + 1.2e-3 + CONST

import sys, sys2, sys3
from urllib.parse import urlsplit, urlunparse as unparse
str = r'\test\test' + u'test' + r"\test"
func = lambda x: x
func_str = f"function is defined as {func}"
some_builtins = [None, False, list, bool, sorted, isinstance]

def func(num): #comment
    for n in range(len(sys.argv)):
        print("Arg %s" % s)
        if n>2:
            return True
    raise RuntimeError("not enough args")
    
async def get_json(client, url):  
    """Gets JSON - asynchronously!"""
    async with client.get(url) as response:
        assert test == {'aa': 2, 'bb': None}
        return await response.read()

class MyName(object):      
    @Decor.name('test')
    def fnc(num):
        pass
   
MyName().func1(num)


# 1. Variables and Data Types
# Python is dynamically typed, no explicit declaration needed
string_variable = "Hello, Python!"
integer_variable = 123
float_variable = 3.14159
boolean_true = True
boolean_false = False

# Lists (ordered, mutable collection - similar to Perl arrays or PowerShell arrays)
list_variable = [10, "apple", 20, "banana"]

# Tuples (ordered, immutable collection)
tuple_variable = (1, 2, "three")

# Dictionaries (unordered, mutable key-value pairs - similar to Perl hashes or PowerShell hashtables)
dictionary_variable = {
    "name": "David",
    "age": 25,
    "city": "Paris"
}

# Sets (unordered collection of unique elements)
set_variable = {1, 2, 3, 2, 1} # Will store {1, 2, 3}

# 2. Basic Output
print("String:", string_variable)
print(f"Integer: {integer_variable}") # F-string for formatted output (Python 3.6+)
print("Float: %.2f" % float_variable) # Old-style formatting

# 3. List, Tuple, Dictionary, and Set Access
print("First element of list:", list_variable[0])
print("Second element of list:", list_variable[1])
print("List slice (elements from index 1 to 3):", list_variable[1:4])
print("All list elements:", list_variable)

print("First element of tuple:", tuple_variable[0])

print("Name from dictionary:", dictionary_variable["name"])
print("Age from dictionary:", dictionary_variable.get("age")) # Safer way, returns None if key not found
print("All dictionary keys:", dictionary_variable.keys())
print("All dictionary values:", dictionary_variable.values())

print("Set elements:", set_variable)

# 4. Operators
sum_val = integer_variable + 5 # Arithmetic
concat_str = string_variable + " Welcome!" # String concatenation
is_equal = (integer_variable == 123) # Equality (==, !=, <, >, <=, >=)
greater_than = (float_variable > 3.0)

print("Sum:", sum_val)
print("Concatenation:", concat_str)
print("Is equal:", is_equal)
print("Greater than:", greater_than)

# Logical operators: and, or, not
if (integer_variable > 100) and boolean_true:
    print("Condition met.")

# 5. Control Structures: Conditionals
if integer_variable > 100:
    print("Integer is greater than 100.")
elif integer_variable == 100:
    print("Integer is exactly 100.")
else:
    print("Integer is 100 or less.")

# Ternary operator (conditional expression)
status = "Active" if boolean_true else "Inactive"
print("Status:", status)

# 6. Control Structures: Loops
# For loop (iterating over sequences)
for item in list_variable:
    print("For loop item:", item)

# For loop with range
for i in range(3): # Generates numbers from 0 up to (but not including) 3
    print("For loop iteration (range):", i)

# While loop
count = 0
while count < 2:
    print("While loop iteration:", count)
    count += 1 # Equivalent to count = count + 1

# Loop control statements: break, continue
for i in range(5):
    if i == 2:
        continue # Skip current iteration if i is 2
    if i == 4:
        break    # Exit loop if i is 4
    print("Loop with control:", i)

# 7. Functions
def greet(name, time_of_day="day"): # Default parameter value
    """
    This is a docstring, explaining what the function does.
    It greets a person with a time of day.
    """
    return f"Good {time_of_day}, {name}!"

greeting_message = greet("Eve", time_of_day="evening") # Keyword arguments
print(greeting_message)

def add_numbers(num1, num2):
    return num1 + num2

print("Sum of 20 and 25:", add_numbers(20, 25))

# Lambda functions (anonymous functions)
add_lambda = lambda a, b: a + b
print("Lambda sum:", add_lambda(10, 20))

# 8. Classes and Objects (Object-Oriented Programming)
class Dog:
    # Class attribute
    species = "Canis familiaris"

    # Constructor (initializer)
    def __init__(self, name, age):
        self.name = name # Instance attributes
        self.age = age

    # Instance method
    def bark(self):
        return f"{self.name} says Woof!"

    # Another instance method
    def get_age_in_dog_years(self):
        return self.age * 7

# Create objects (instances)
my_dog = Dog("Buddy", 5)
your_dog = Dog("Lucy", 2)

print(f"{my_dog.name} is a {my_dog.species}.")
print(my_dog.bark())
print(f"{your_dog.name} is {your_dog.get_age_in_dog_years()} dog years old.")

# 9. Modules and Packages
# Import a built-in module
import math
print("Value of PI:", math.pi)
print("Square root of 16:", math.sqrt(16))

# Import specific items from a module
from datetime import datetime, date
current_date = date.today()
print("Current date:", current_date)

# 10. File I/O
file_name = "my_python_output.txt"

# Writing to a file
with open(file_name, 'w') as file: # 'w' for write (creates/overwrites)
    file.write("This is the first line.\n")
    file.write("This is the second line.\n")
print(f"Content written to {file_name}")

# Appending to a file
with open(file_name, 'a') as file: # 'a' for append
    file.write("This line was appended.\n")

# Reading from a file
with open(file_name, 'r') as file: # 'r' for read
    content = file.read()
    print("\nFile content (read all at once):\n", content)

# Reading line by line
with open(file_name, 'r') as file:
    print("File content (read line by line):")
    for line in file:
        print(line.strip()) # .strip() removes leading/trailing whitespace, including newline

# 11. Error Handling (Exceptions)
try:
    result = 10 / 0 # This will cause a ZeroDivisionError
except ZeroDivisionError:
    print("Error: Cannot divide by zero!")
except TypeError:
    print("Error: Type mismatch!")
except Exception as e: # Generic exception handling
    print(f"An unexpected error occurred: {e}")
finally:
    print("This block always executes, regardless of error.")

# 12. List Comprehensions (concise list creation)
squares = [x**2 for x in range(5)]
print("Squares:", squares)

even_numbers = [x for x in range(10) if x % 2 == 0]
print("Even numbers:", even_numbers)

# 13. Decorators (advanced topic for function modification)
def my_decorator(func):
    def wrapper():
        print("Something is happening before the function is called.")
        func()
        print("Something is happening after the function is called.")
    return wrapper

@my_decorator
def say_hello():
    print("Hello!")

print("\nDemonstrating decorator:")
say_hello()

# End of script
print("\nPython syntax demonstration complete.")