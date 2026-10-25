#!/bin/bash
# This is a comprehensive Bash script demonstrating various syntax elements.
# Comments in Bash start with a hash (#).
# The shebang line (#!/bin/bash) specifies the interpreter for the script.

#!/bin/bash
var=value
/ddd/ddd.sh, "test
string", "$(....)"
+= -= /= *= &= |= ^= ~= == != >= <= ( ) [ ] { } $( ) + - / | < >
test `dirname $var 20+30`, `more $var(20) \'escape\` `
$@, $$, $@, $?, $-, $!, $*
%, %%, %+, %-, %1, %ce, %?ce, !!, !-1, !50, 2>
(( 1 -s --d )) (( !2 ))  [[ $str =~ ^$'ab' ]]
$(echo $(pwd)) ; $( [[ -n $1 ]] )

function echo {
	git -a --test "path\"path"
}

func_name() {
}
func_name
dfsdfsdf

while read line  #comment
do
   test $(test "$line")
   len=${#line} $(text here)
   if [[ "$len" -lt "$MINLEN" && "$line" =~ \[*\.\] ]]
     then echo
   fi
done

cat <<-  'indented_without_expansions'
    ${foo}
    indented_without_expansions

variable=$(cat <<SETVAR
HereDoc
SETVAR
)


clear
echo "--- Bash Script Syntax Demonstration ---"
echo ""

# --- 1. Basic Commands ---
echo "--- 1. Basic Commands ---"
echo "Hello, Bash World!"
sleep 2 # Pause execution for 2 seconds
clear   # Clear the terminal screen

echo "Current directory: $(pwd)" # pwd prints the current working directory
echo "Listing files in current directory:"
ls -lh # ls lists directory contents, -l for long format, -h for human-readable sizes
echo ""

# --- 2. Variables ---
echo "--- 2. Variables ---"

# 2.1. Setting User-Defined Variables
my_variable="This is a user-defined variable."
echo "Value of my_variable: $my_variable"

# Variables with spaces don't need quotes during assignment if no other special characters are used,
# but it's good practice to quote values to prevent word splitting or globbing issues.
another_var="Variable with spaces."
echo "Value of another_var: $another_var"

# 2.2. Environment Variables (e.g., $PATH, $USER, $HOME)
echo "Your username is: $USER"
echo "Your home directory is: $HOME"
echo "Your current shell is: $SHELL"
echo ""

# 2.3. Special Variables
echo "Script name: $0"
echo "First argument: $1" # Access command-line arguments (explained later)
echo "Number of arguments: $#"
echo "All arguments as a single string: $*"
echo "All arguments as separate strings (best for loops): $@"
echo "Last command's exit status: $?" # 0 for success, non-zero for failure
echo "Process ID of the current shell: $$"
echo "Process ID of the last background command: $!"
echo ""

# --- 3. Input/Output ---
echo "--- 3. Input/Output ---"

# 3.1. User Input (read)
read -p "Please enter your name: " user_input_name
echo "Hello, $user_input_name!"

# 3.2. Redirection
echo "This line will be written to a file." > output.txt
echo "This line will be appended to the file." >> output.txt
echo "" >> output.txt # Add a blank line
cat output.txt       # cat displays file content
rm output.txt        # Remove the file

# 3.3. Pipes (|) - Chain commands
echo "Piping output: ls -l | grep output"
ls -l | grep "output" # ls -l output is piped to grep, which filters for "output"
echo ""

# --- 4. Control Flow ---
echo "--- 4. Control Flow ---"

# 4.1. IF/ELSE IF/ELSE Conditions
read -p "Enter a number (e.g., 10): " my_number

if [[ "$my_number" -eq 10 ]]; then # -eq for equality (numbers)
    echo "You entered 10!"
elif [[ "$my_number" -lt 10 ]]; then # -lt for less than (numbers)
    echo "You entered a number less than 10."
else # -gt for greater than (numbers)
    echo "You entered a number greater than 10."
fi

# String comparisons: == or = (equality), != (not equal), < (less than), > (greater than)
# Use single brackets [ ] for basic tests, double brackets [[ ]] for more advanced features
# like pattern matching and avoiding word splitting. Prefer [[ ]] in modern Bash.
ccc=true

file_to_check="syntax_demo.sh" # Assuming this script's name
if [[ -f "$file_to_check" ]]; then # -f checks if it's a regular file
    echo "$file_to_check is a regular file."
fi

if [[ -d "/tmp" ]]; then # -d checks if it's a directory
    echo "/tmp is a directory."
fi
echo ""

# 4.2. FOR Loops
echo "Counting from 1 to 5:"
for i in {1..5}; do
    echo "Count: $i"
done
echo ""

echo "Iterating over a list of items:"
for fruit in "apple" "banana" "cherry"; do
    echo "Fruit: $fruit"
done
echo ""

echo "Iterating over files (globbing):"
touch file1.txt file2.log file3.txt # Create dummy files
for file in *.txt; do
    echo "Text file: $file"
done
rm file*.txt file*.log # Clean up dummy files
echo ""

# 4.3. WHILE Loops
echo "While loop (counting down):"
count=3
while [[ $count -gt 0 ]]; do
    echo "Countdown: $count"
    ((count--)) # Decrement count
    sleep 0.5
done


# 4.4. CASE Statements (alternative to long if/elif chains)
read -p "Enter a color (red, green, blue): " color
case "$color" in
    red|RED) # Multiple patterns can be separated by |
        echo "You chose red."
        ;; # Double semicolon terminates a case
    *) # Default case (like else)
        echo "Unknown color: $color"
        ;;
esac
echo ""

# --- 5. Functions ---
echo "--- 5. Functions ---"

# Define a simple function
greet_user() {
    echo "Hello from the greet_user function!"
}

# Call the function
greet_user

# Define a function with arguments
add_numbers() {
    local num1=$1 # Access arguments using $1, $2, etc.
    local num2=$2 # 'local' creates a variable scoped only to the function
    local sum=$((num1 + num2))
    echo "$num1 + $num2 = $sum"
    return $sum # Functions can return an exit status (0-255)
}

# Call the function with arguments
add_numbers 15 25
echo "Last function's exit status (sum in this case): $?" # Check the return value

# Function returning a value (via echo)
get_date_time() {
    date "+%Y-%m-%d %H:%M:%S"
}

current_datetime=$(get_date_time) # Capture function output into a variable
echo "Current Date & Time from function: $current_datetime"
echo ""

# --- 6. File Operations ---
echo "--- 6. File Operations ---"

mkdir "test_folder"
echo "Created test_folder."
mkdir -p "test_folder/sub_folder" # -p creates parent directories if they don't exist
echo "Created sub_folder inside test_folder."

echo "Content for file1" > "test_folder/file1.txt"
echo "Content for file2" > "test_folder/sub_folder/file2.txt"

echo "Listing contents of test_folder:"
ls -l test_folder
echo ""

cp "test_folder/file1.txt" "copied_file.txt"
echo "Copied file1.txt to copied_file.txt."

mv "copied_file.txt" "renamed_file.txt" # mv also renames
echo "Moved/Renamed copied_file.txt to renamed_file.txt."

cat "renamed_file.txt"
echo ""

rm "renamed_file.txt"
echo "Removed renamed_file.txt."

rmdir "test_folder/sub_folder" # rmdir removes empty directories
echo "Removed test_folder/sub_folder."
rm -r "test_folder" # rm -r removes non-empty directories recursively
echo "Removed test_folder."
echo ""

# --- 7. Arithmetic Operations ---
echo "--- 7. Arithmetic Operations ---"

# 7.1. Basic arithmetic with $((...))
num1=20
num2=7
sum=$((num1 + num2))
diff=$((num1 - num2))
prod=$((num1 * num2))
div=$((num1 / num2)) # Integer division
mod=$((num1 % num2)) # Modulo

echo "Arithmetic using \$((...))"
echo "$num1 + $num2 = $sum"
echo "$num1 - $num2 = $diff"
echo "$num1 * $num2 = $prod"
echo "$num1 / $num2 = $div (integer division)"
echo "$num1 % $num2 = $mod"
echo ""

# 7.2. Using 'expr' (older, less preferred)
result_expr=$(expr $num1 + $num2)
echo "Result using expr: $result_expr"
echo ""

# 7.3. Floating point arithmetic with 'bc'
echo "Floating point division using 'bc':"
echo "scale=2; $num1 / $num2" | bc # 'scale' sets decimal places
echo ""

# --- 8. String Manipulation ---
echo "--- 8. String Manipulation ---"

my_string="Hello, Bash Shell Scripting!"
echo "Original string: $my_string"

# 8.1. String length
echo "String length: ${#my_string}"

# 8.2. Substring extraction
echo "Substring (from index 7, 4 chars): ${my_string:7:4}" # Bash
echo "Substring (from index 7 to end): ${my_string:7}"

# 8.3. String replacement
echo "Replace 'Bash' with 'Awesome': ${my_string/Bash/Awesome}"
echo "Replace all 'l' with 'X': ${my_string//l/X}" # // for global replacement
echo ""

# --- 9. Arrays ---
echo "--- 9. Arrays ---"

# 9.1. Indexed Arrays
my_array=("apple" "banana" "cherry" "date")
echo "All elements of array: ${my_array[@]}"
echo "First element: ${my_array[0]}" # Arrays are 0-indexed
echo "Number of elements: ${#my_array[@]}"
echo "Elements from index 1 (banana): ${my_array[@]:1}"
echo "Elements from index 1, 2 elements (banana, cherry): ${my_array[@]:1:2}"

# Loop through array
echo "Iterating through array:"
for item in "${my_array[@]}"; do
    echo "- $item"
done
echo ""

# 9.2. Associative Arrays (Bash 4.0+)
declare -A colors # Declare an associative array
colors["red"]="#FF0000"
colors["green"]="#00FF00"
colors["blue"]="#0000FF"

echo "Value for red: ${colors["red"]}"
echo "All keys: ${!colors[@]}"
echo "All values: ${colors[@]}"
echo ""

# Loop through associative array
echo "Iterating through associative array:"
for key in "${!colors[@]}"; do
    echo "Color: $key, Hex: ${colors[$key]}"
done
echo ""

# --- 10. Here Documents and Here Strings ---
echo "--- 10. Here Documents and Here Strings ---"

# Here Document (for multi-line input to a command)
cat << EOF
This is a here document.
It allows you to provide multi-line input
to a command like 'cat' or 'grep'.
EOF
echo ""

# Here String (for single-line multi-line like input to a command)
grep -o '[0-9]\+' <<< "The number is 123 and 456."
echo ""

# --- 11. Command Line Arguments (More Examples) ---
echo "--- 11. Command Line Arguments ---"
echo "To test this section, run the script like: ./your_script_name.sh arg1 \"arg two\" 3"
echo "Arguments received:"
for arg in "$@"; do
    echo "- $arg"
done
echo ""

# --- 12. Error Handling ---
echo "--- 12. Error Handling ---"

# Check exit status of a command
ls non_existent_file.txt >/dev/null 2>&1 # Redirect stdout and stderr to /dev/null
if [[ $? -ne 0 ]]; then
    echo "Error: 'ls non_existent_file.txt' failed as expected."
fi

# Exit on first error (set -e)
# Try uncommenting this and adding a non-existent command below it.
# set -e
# echo "This line will execute."
# non_existent_command_here # This would cause the script to exit if set -e is active
# echo "This line will NOT execute if set -e is active and previous command failed."
echo ""

# --- End of Script ---
echo "--- End of Bash Script Syntax Demonstration ---"
echo "Press Enter to exit."
read -r # Read a line from stdin, used here to pause for user input
clear