#!/usr/bin/perl
use strict;
use warnings;
use feature 'say'; # Enables 'say' function for printing with newline

# 1. Variables and Data Types
my $scalar_variable = "Hello, Perl!"; # Scalar (string)
my $an_integer = 123;                 # Scalar (integer)
my $a_float = 3.14159;                # Scalar (float)
my $boolean_true = 1;                 # Perl treats 1 as true, 0 as false
my $boolean_false = 0;

my @array_variable = (10, "apple", 20, "banana"); # Array
my %hash_variable = (
    name => "Alice",
    age  => 30,
    city => "New York"
); # Hash (key-value pairs)

# 2. Basic Output
print "Scalar: $scalar_variable\n";
say "Integer: $an_integer"; # Using 'say' from feature 'say'
printf "Float: %.2f\n", $a_float; # Formatted print

# 3. Array and Hash Access
say "First element of array: $array_variable[0]";
say "Second element of array: $array_variable[1]";
say "All array elements: @array_variable"; # Interpolates elements

say "Name from hash: $hash_variable{name}";
say "Age from hash: $hash_variable{'age'}"; # Quotes optional for simple keys
say "All hash keys: " . join(", ", keys %hash_variable);
say "All hash values: " . join(", ", values %hash_variable);

# 4. Operators
my $sum = $an_integer + 5; # Arithmetic
my $concat = $scalar_variable . " Welcome!"; # String concatenation
my $is_equal = ($an_integer == 123); # Numeric equality
my $string_equal = ($scalar_variable eq "Hello, Perl!"); # String equality
my $greater_than = ($a_float > 3.0); # Comparison

say "Sum: $sum";
say "Concatenation: $concat";
say "Is equal (numeric): $is_equal";
say "Is equal (string): $string_equal";
say "Greater than: $greater_than";

# 5. Control Structures: Conditionals
if ($an_integer > 100) {
    say "Integer is greater than 100.";
} elsif ($an_integer == 100) {
    say "Integer is exactly 100.";
} else {
    say "Integer is 100 or less.";
}

# Ternary operator
my $status = ($boolean_true) ? "Active" : "Inactive";
say "Status: $status";

# Unless statement (opposite of if)
unless ($boolean_false) {
    say "This runs because boolean_false is false.";
}

# Statement modifiers (single line if/unless)
say "Value is 20" if $array_variable[2] == 20;
say "Value is not 30" unless $array_variable[2] == 30;

# 6. Control Structures: Loops
# For loop
for (my $i = 0; $i < 3; $i++) {
    say "For loop iteration: $i";
}

# Foreach loop (iterating over arrays/lists)
foreach my $item (@array_variable) {
    say "Foreach item: $item";
}

# While loop
my $count = 0;
while ($count < 2) {
    say "While loop iteration: $count";
    $count++;
}

# Until loop (opposite of while)
my $until_count = 0;
until ($until_count >= 2) {
    say "Until loop iteration: $until_count";
    $until_count++;
}

# 7. Subroutines (Functions)
sub greet {
    my ($name, $time_of_day) = @_; # Access arguments via @_ array
    return "Good $time_of_day, $name!";
}

my $greeting_message = greet("Bob", "morning");
say $greeting_message;

sub add_numbers {
    my ($num1, $num2) = @_;
    return $num1 + $num2;
}
say "Sum of 5 and 7: " . add_numbers(5, 7);

# Subroutine with no arguments
sub say_hello {
    say "Hello from a subroutine!";
}
say_hello();

# 8. Regular Expressions (Pattern Matching and Substitution)
my $text = "The quick brown fox jumps over the lazy dog.";

# Pattern matching (m//)
if ($text =~ /fox/) {
    say "Text contains 'fox'.";
}

# Capturing groups
if ($text =~ /quick (brown) (fox)/) {
    say "Captured: $1 and $2"; # $1 and $2 hold the captured parts
}

# Substitution (s///)
$text =~ s/quick/slow/;
say "After substitution: $text";

# Global substitution
my $sentence = "one two one three";
$sentence =~ s/one/SINGLE/g; # 'g' for global
say "Global substitution: $sentence";

# 9. File I/O (Basic)
my $filename = "my_output.txt";
open(my $fh, '>', $filename) or die "Could not open file '$filename' for writing: $!";
print $fh "This is a line written to the file.\n";
print $fh "Another line.\n";
close($fh);
say "Content written to $filename";

# Reading from a file
open(my $fh_read, '<', $filename) or die "Could not open file '$filename' for reading: $!";
say "Reading from file:";
while (my $line = <$fh_read>) {
    chomp $line; # Remove trailing newline
    say "Read: $line";
}
close($fh_read);

# 10. References (for complex data structures)
my @nested_array = ([1, 2, 3], [4, 5, 6]); # Array of array references
say "Nested array element: " . $nested_array[0]->[1]; # Dereferencing

my %nested_hash = (
    user1 => { id => 101, email => 'user1@example.com' },
    user2 => { id => 102, email => 'user2@example.com' },
);
say "Nested hash email: " . $nested_hash{user1}->{email};

# 11. Modules (using a built-in one for demonstration)
use Cwd; # Change working directory

my $current_dir = getcwd();
say "Current working directory: $current_dir";

# End of script
say "\nPerl syntax demonstration complete.";