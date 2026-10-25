# 1. Variables and Data Types
# Ruby is dynamically typed, no explicit declaration needed.
# Local variables start with a lowercase letter or underscore.
#commment
require 'name.rb'
=begin
 multiline comment
=end

%q{ 
  test name{name} 
} +
%Q{one\ntwo\n#{ 1 + 2 }} +
%q[one\ntwo\n#[ 1 + 2 ]] +
%r/#{name}/i +
%w{one two three} ;

name=/^name/ =~ %r{\w+}
name="Name \
  End" #trailing \
puts 'Escape "\\"'+ 'That\'s right' +?a+?\n+true+false+nil;
num = +1.23 + 1.0e6 + 1e+20 + 0xFA + 0o77 + 0b11 + 0d99 

# Regex
{aaa: /#{x}/i} or {:aaa => /#{x}/xi}
/^xyz\Z/
['xyz' =~ /^xyz\Z/]
'xyz'[/^xyz\Z/]
/^xyz\Z/ =~ 'xyz'
/whitespace
regex
/x
=/[]/
a./.< == true ? b./.< : b./.<.<
n.new(v.match(/(?:src )(#{escape(gsub(/\/\d+\Z/, "").gsub(/\.\d+\Z/, ""))}\.\d+)/)[1])

def create
  @unit = opt["opt"]
  $name = 'value'
end    

def func1
  if (nbr > 1) #must fold
    h = height if (nbr>2) #must not fold
    w = width if (nbr>2) #same
    n.upto(nbr) do (group) #must fold
      h = h + height
    end
    model.commit_operation
  end
end

def func2
  animals = %w(lions tigers bears)
  animals.each{|kind| print kind}
end


string_variable = "Hello, Ruby!"
integer_variable = 456
float_variable = 2.71828
boolean_true = true
boolean_false = false

# Array (ordered, mutable collection)
array_variable = [100, "orange", 200, "grape"]

# Hash (unordered, mutable key-value pairs)
# New hash syntax (preferred since Ruby 1.9)
hash_variable = {
  name: "Charlie", # Symbol key
  age: 35,
  city: "Berlin"
}
# Old hash syntax (still valid, but less common for new code)
old_hash_syntax = { "country" => "Germany", "population" => 83_000_000 }

# Symbols (immutable, unique identifiers often used as hash keys)
my_symbol = :some_key

# 2. Basic Output
puts "String: #{string_variable}" # puts adds a newline automatically
print "Integer: #{integer_variable}\n" # print does not add a newline
puts format("Float: %.2f", float_variable) # Similar to printf

# 3. Array and Hash Access
puts "First element of array: #{array_variable[0]}"
puts "Second element of array: #{array_variable[1]}"
puts "Array slice (elements from index 1 to 3): #{array_variable[1..3]}"
puts "All array elements: #{array_variable.join(', ')}"

puts "Name from hash: #{hash_variable[:name]}" # Accessing symbol key
puts "Age from hash: #{hash_variable['age']}" # String key (if used)
puts "All hash keys: #{hash_variable.keys.join(', ')}"
puts "All hash values: #{hash_variable.values.join(', ')}"

# 4. Operators
sum_val = integer_variable + 10 # Arithmetic
concat_str = string_variable + " Welcome!" # String concatenation
is_equal = (integer_variable == 456) # Equality (==, !=, <, >, <=, >=)
greater_than = (float_variable > 2.0)

puts "Sum: #{sum_val}"
puts "Concatenation: #{concat_str}"
puts "Is equal: #{is_equal}"
puts "Greater than: #{greater_than}"

# Logical operators: && (and), || (or), ! (not)
if (integer_variable > 400) && boolean_true
  puts "Condition met."
end

# 5. Control Structures: Conditionals
if integer_variable > 500
  puts "Integer is greater than 500."
elsif integer_variable == 456
  puts "Integer is exactly 456."
else
  puts "Integer is 500 or less."
end

# Ternary operator
status = boolean_true ? "Active" : "Inactive"
puts "Status: #{status}"

# Unless statement (opposite of if)
unless boolean_false
  puts "This runs because boolean_false is false."
end

# Statement modifiers (single line if/unless)
puts "Value is 200" if array_variable[2] == 200
puts "Value is not 300" unless array_variable[2] == 300

# Case statement (similar to switch)
day = "Monday"
case day
when "Monday"
  puts "Start of the week."
when "Friday"
  puts "End of the work week!"
else
  puts "Mid-week or weekend."
end

# 6. Control Structures: Loops
# For loop (usually used for ranges or specific counts)
for i in 0..2 # Inclusive range
  puts "For loop iteration: #{i}"
end

# Each iterator (preferred for collections, uses blocks)
array_variable.each do |item|
  puts "Each item: #{item}"
end
# Shorter syntax for single-line blocks
hash_variable.each { |key, value| puts "Hash key: #{key}, value: #{value}" }

# While loop
count = 0
while count < 2
  puts "While loop iteration: #{count}"
  count += 1
end

# Until loop (opposite of while)
until count >= 4
  puts "Until loop iteration: #{count}"
  count += 1
end

# Loop control statements: break, next (like continue), redo, retry
for i in 0..4
  if i == 2
    next # Skip current iteration if i is 2
  end
  if i == 4
    break # Exit loop if i is 4
  end
  puts "Loop with control: #{i}"
end

# 7. Methods (Functions)
# Defined with 'def' keyword
def greet(name, time_of_day = "day") # Default parameter value
  # No 'return' keyword needed if the last expression is the return value
  "Good #{time_of_day}, #{name}!"
end

greeting_message = greet("Grace", "afternoon")
puts greeting_message

def add_numbers(num1, num2)
  num1 + num2
end
puts "Sum of 30 and 35: #{add_numbers(30, 35)}"

# 8. Classes and Objects (Object-Oriented Programming)
class Car
  # Class variable (prefixed with @@)
  @@number_of_cars = 0

  # Constructor (initializer)
  def initialize(make, model)
    @make = make   # Instance variables (prefixed with @)
    @model = model
    @@number_of_cars += 1
  end

  # Instance method
  def full_name
    "#{@make} #{@model}"
  end

  # Class method (prefixed with self.)
  def self.total_cars
    @@number_of_cars
  end

  # Attribute accessor (getter and setter)
  attr_accessor :color # Creates color and color= methods

  # Read-only attribute (getter only)
  attr_reader :make

  # Write-only attribute (setter only)
  # attr_writer :owner

end

# Create objects (instances)
car1 = Car.new("Toyota", "Camry")
car2 = Car.new("Honda", "Civic")

puts "Car 1: #{car1.full_name}"
puts "Car 2 make: #{car2.make}" # Using attr_reader

car1.color = "Red" # Using attr_accessor (setter)
puts "Car 1 color: #{car1.color}" # Using attr_accessor (getter)

puts "Total cars created: #{Car.total_cars}" # Calling class method

# 9. Modules (for organizing code and mixins)
module Greeter
  def say_hello
    "Hello from Greeter!"
  end
end

class Person
  include Greeter # Mixin the module methods
  def initialize(name)
    @name = name
  end
end

person = Person.new("Frank")
puts person.say_hello

# 10. Blocks, Procs, and Lambdas (powerful functional features)
# Block passed to a method
[1, 2, 3].map { |num| num * 2 } # map takes a block

# Proc (a block that can be stored in a variable)
my_proc = Proc.new { |a, b| a + b }
puts "Proc result: #{my_proc.call(5, 5)}"

# Lambda (similar to Proc, but stricter about arguments)
my_lambda = lambda { |a, b| a * b }
puts "Lambda result: #{my_lambda.call(5, 5)}"

# 11. File I/O
file_name = "my_ruby_output.txt"

# Writing to a file
File.open(file_name, 'w') do |file| # 'w' for write (creates/overwrites)
  file.puts "This is the first line from Ruby."
  file.puts "This is the second line."
end
puts "Content written to #{file_name}"

# Appending to a file
File.open(file_name, 'a') do |file| # 'a' for append
  file.puts "This line was appended."
end

# Reading from a file
puts "\nFile content (read all at once):"
puts File.read(file_name)

# Reading line by line
puts "File content (read line by line):"
File.foreach(file_name) { |line| puts line.chomp } # chomp removes newline

# 12. Error Handling (Exceptions)
begin
  result = 10 / 0 # This will cause a ZeroDivisionError
rescue ZeroDivisionError
  puts "Error: Cannot divide by zero!"
rescue StandardError => e # Catch any standard error and store it in 'e'
  puts "An unexpected error occurred: #{e.message}"
ensure
  puts "This block always executes, regardless of error."
end

# End of script
puts "\nRuby syntax demonstration complete."