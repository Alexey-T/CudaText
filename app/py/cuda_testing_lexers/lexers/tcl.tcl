# 1. Comments
# This is a single-line comment
# Another comment line

# 2. Basic Output
# 'puts' is the command to print to standard output, adding a newline by default.
puts "--- Tcl Syntax Demonstration ---"
puts "Hello, Tcl!"

# 3. Variables
# 'set' command assigns a value to a variable.
set my_string "This is a Tcl string."
set my_integer 12345
set my_float 6.789
set my_boolean true ;# Tcl treats non-empty strings as true, empty string or 0 as false

# To retrieve the value of a variable, use '$' prefix.
puts "String variable: $my_string"
puts "Integer variable: $my_integer"
puts "Float variable: $my_float"
puts "Boolean variable: $my_boolean"

# Variable substitution inside double quotes
puts "The value of my_integer is $my_integer."

# Command substitution (execute a command and use its result)
puts "The sum of 5 and 3 is [expr {5 + 3}]." ;# 'expr' command evaluates expressions

# 4. Lists (ordered collections)
# Lists are just strings with elements separated by whitespace or enclosed in braces.
set my_list {apple banana cherry} ;# Elements are protected by braces
puts "My list: $my_list"

# Accessing list elements
puts "First element: [lindex $my_list 0]" ;# lindex gets element by index
puts "Last element: [lindex $my_list end]"
puts "Number of elements: [llength $my_list]" ;# llength gets list length

# Appending to a list
lappend my_list "date"
puts "List after appending: $my_list"

# 5. Associative Arrays (Dictionaries/Hashes)
# 'array set' creates or populates an array with key-value pairs.
array set my_array {
    name "Alice"
    age  30
    city "New York"
}
puts "Name from array: $my_array(name)"
puts "City from array: $my_array(city)"

# Getting all keys or values
puts "Array keys: [array names my_array]"
puts "Array values: [array get my_array *]" ;# Returns key-value pairs

# 6. Operators (via 'expr' command)
set x 10
set y 3
puts "x + y = [expr {$x + $y}]"
puts "x - y = [expr {$x - $y}]"
puts "x * y = [expr {$x * $y}]"
puts "x / y = [expr {double($x) / $y}]" ;# Use double() for float division
puts "x % y = [expr {$x % $y}]"

# Comparison operators (also in 'expr')
puts "Is x > y? [expr {$x > $y}]"
puts "Is x == 10? [expr {$x == 10}]"

# Logical operators (also in 'expr')
set is_raining true
set is_cold false
puts "Is raining AND cold? [expr {$is_raining && $is_cold}]"
puts "Is raining OR cold? [expr {$is_raining || $is_cold}]"

# 7. Control Structures: Conditionals
# 'if' command
if {$my_integer > 10000} {
    puts "Integer is large."
} elseif {$my_integer > 1000} {
    puts "Integer is medium."
} else {
    puts "Integer is small."
}

# 'switch' command (similar to case statements)
set fruit "banana"
switch $fruit {
    apple {
        puts "It's an apple."
    }
    banana {
        puts "It's a banana."
    }
    default { ;# Catches all other cases
        puts "Unknown fruit."
    }
}

# 8. Control Structures: Loops
# 'foreach' loop (iterating over lists)
foreach item $my_list {
    puts "Foreach item: $item"
}

# 'for' loop (C-style loop)
for {set i 0} {$i < 3} {incr i} { ;# incr increments a variable
    puts "For loop iteration: $i"
}

# 'while' loop
set counter 0
while {$counter < 2} {
    puts "While loop iteration: $counter"
    incr counter
}

# Loop control: 'break', 'continue'
for {set i 0} {$i < 5} {incr i} {
    if {$i == 2} {
        continue ;# Skip current iteration
    }
    if {$i == 4} {
        break ;# Exit loop
    }
    puts "Loop with control: $i"
}

# 9. Procedures (Functions)
# 'proc' command defines a procedure.
proc greet {name {time_of_day "day"}} { ;# Default value for time_of_day
    # Procedures implicitly return the result of the last command executed,
    # or you can use 'return'.
    return "Good $time_of_day, $name!"
}

set greeting_message [greet "Charlie" "morning"]
puts $greeting_message

proc add_numbers {num1 num2} {
    expr {$num1 + $num2} ;# Result of expr is returned
}
puts "Sum of 50 and 60: [add_numbers 50 60]"

# 10. String Manipulation (using 'string' command)
set sentence "The quick brown fox"
puts "Length of sentence: [string length $sentence]"
puts "Index of 'quick': [string first quick $sentence]"
puts "Uppercase: [string toupper $sentence]"
puts "Replace 'fox' with 'dog': [string map {fox dog} $sentence]"

# 11. File I/O
set file_name "my_tcl_output.txt"

# Open a file for writing ('w' for write, 'a' for append)
set fh [open $file_name w] ;# 'fh' is a file handle
puts $fh "This is the first line written by Tcl."
puts $fh "This is the second line."
close $fh ;# Close the file handle
puts "Content written to $file_name"

# Open a file for appending
set fh [open $file_name a]
puts $fh "This line was appended."
close $fh

# Open a file for reading ('r')
set fh [open $file_name r]
puts "\nReading from file:"
while {[gets $fh line] >= 0} { ;# gets reads a line, returns -1 at EOF
    puts "Read: $line"
}
close $fh

# 12. Error Handling (using 'catch' command)
if [catch {set div_result [expr {10 / 0}]} err_msg] {
    puts "Caught an error: $err_msg"
} else {
    puts "Division result: $div_result"
}

# 13. Sourcing other Tcl files
# You can load and execute another Tcl script using 'source'.
# Example: source "another_script.tcl"

puts "--- Tcl Demonstration Complete ---"