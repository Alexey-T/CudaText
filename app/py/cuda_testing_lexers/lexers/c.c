#include <stdio.h>      // Standard Input/Output functions (printf, scanf, fopen, etc.)
#include <stdlib.h>     // Standard Library functions (malloc, free, exit, atoi, etc.)
#include <string.h>     // String manipulation functions (strlen, strcpy, strcat, strcmp, etc.)
#include <math.h>       // Mathematical functions (sqrt, pow, sin, cos, etc.)
#include <time.h>       // Time and date functions (time, localtime, strftime, etc.)
#include <stdbool.h>    // Boolean type (true, false) - C99 standard
#include <errno.h>      // Error number definitions

// Single-line comment
/*
    Multi-line comment block.
    This program demonstrates various C syntax elements using the standard library.
*/

// ##########################################################################
// #                            Preprocessor Directives                     #
// ##########################################################################

#define PI 3.14159      // Macro definition: text replacement before compilation
#define MAX_BUFFER 256  // Define a constant for buffer size

// ##########################################################################
// #                            Function Declarations                       #
// ##########################################################################
// It's good practice to declare functions before they are used (forward declaration).

void print_separator(const char *title); // Function to print a separator line
int add_numbers(int a, int b);
void swap_values(int *x, int *y); // Function taking pointers (pass by reference)
void demonstrate_arrays();
void demonstrate_strings();
void demonstrate_pointers();
void demonstrate_structs();
void demonstrate_dynamic_memory();
void demonstrate_file_io();
void demonstrate_error_handling();

// ##########################################################################
// #                            Main Function                               #
// ##########################################################################
// The entry point of every C program.

int main(int argc, char *argv[]) {
    print_separator("1. Basic Structure & Variables");

    // 1.1. Basic Data Types and Variable Declaration
    int integer_var = 42;             // Integer
    float float_var = 3.14f;          // Single-precision floating point (f suffix for float literal)
    double double_var = 123.456789;   // Double-precision floating point
    char char_var = 'A';              // Single character
    bool boolean_var = true;          // Boolean (requires <stdbool.h>)

    printf("Integer: %d\n", integer_var);
    printf("Float: %.2f\n", float_var);      // .2f prints 2 decimal places
    printf("Double: %.6lf\n", double_var);   // %lf for double
    printf("Char: %c\n", char_var);
    printf("Boolean: %s\n", boolean_var ? "true" : "false"); // Conditional (ternary) operator

    // 1.2. Constants
    const int DAYS_IN_WEEK = 7; // Constant variable, value cannot be changed
    printf("Days in week (constant): %d\n", DAYS_IN_WEEK);
    printf("PI (macro): %f\n", PI);

    // 1.3. User Input (scanf, getchar)
    print_separator("2. Input/Output");
    int user_int;
    char user_char;
    char user_line[MAX_BUFFER];

    printf("Enter an integer: ");
    if (scanf("%d", &user_int) == 1) { // & is the address-of operator, scanf needs memory address
        printf("You entered: %d\n", user_int);
    } else {
        printf("Invalid integer input.\n");
        // Clear input buffer in case of bad input (important for subsequent inputs)
        while (getchar() != '\n' && getchar() != EOF);
    }

    printf("Enter a character: ");
    // Clear newline character left by previous scanf before using getchar
    while ((user_char = getchar()) == '\n');
    printf("You entered: %c\n", user_char);

    printf("Enter a line of text (up to %d chars): ", MAX_BUFFER - 1);
    // Clear any remaining newlines from previous inputs
    fgets(user_line, MAX_BUFFER, stdin); // Safer than scanf for strings
    user_line[strcspn(user_line, "\n")] = 0; // Remove trailing newline if present
    printf("You entered: \"%s\"\n", user_line);


    // 1.4. Output (printf, putchar)
    printf("Using putchar: ");
    putchar('H');
    putchar('i');
    putchar('\n');

    // ##########################################################################
    // #                            3. Operators                                #
    // ##########################################################################
    print_separator("3. Operators");
    int x = 10, y = 3;

    // Arithmetic Operators
    printf("x + y = %d\n", x + y);
    printf("x - y = %d\n", x - y);
    printf("x * y = %d\n", x * y);
    printf("x / y (integer div) = %d\n", x / y);
    printf("x %% y (modulo) = %d\n", x % y);
    printf("x / y (float div) = %.2f\n", (float)x / y); // Type casting

    // Increment/Decrement Operators
    printf("x++ = %d (x is now %d)\n", x++, x); // Post-increment
    printf("++y = %d (y is now %d)\n", ++y, y); // Pre-increment

    // Relational Operators (return 0 for false, 1 for true)
    printf("x > y: %d\n", x > y);
    printf("x == y: %d\n", x == y);

    // Logical Operators (&& AND, || OR, ! NOT)
    printf("(x > 5 && y < 5): %d\n", (x > 5 && y < 5));
    printf("(x < 5 || y > 5): %d\n", (x < 5 || y > 5));
    printf("!(x == y): %d\n", !(x == y));

    // Assignment Operators
    int z = 10;
    z += 5; // z = z + 5
    printf("z after z += 5: %d\n", z);

    // ##########################################################################
    // #                            4. Control Flow                             #
    // ##########################################################################
    print_separator("4. Control Flow");

    // 4.1. If/Else If/Else
    int grade = 85;
    if (grade >= 90) {
        printf("Grade: A\n");
    } else if (grade >= 80) {
        printf("Grade: B\n");
    } else {
        printf("Grade: C or lower\n");
    }

    // 4.2. Switch/Case
    char choice = 'B';
    switch (choice) {
        case 'A':
            printf("Choice is A.\n");
            break; // Exit switch
        case 'B':
            printf("Choice is B.\n");
            break;
        case 'C':
            printf("Choice is C.\n");
            break;
        default:
            printf("Choice is neither A, B, nor C.\n");
    }

    // 4.3. For Loop
    printf("For loop (1 to 5): ");
    for (int i = 1; i <= 5; i++) {
        printf("%d ", i);
    }
    printf("\n");

    // 4.4. While Loop
    int k = 0;
    printf("While loop (0 to 2): ");
    while (k < 3) {
        printf("%d ", k);
        k++;
    }
    printf("\n");

    // 4.5. Do/While Loop (executes at least once)
    int j = 5;
    printf("Do/While loop (at least once): ");
    do {
        printf("%d ", j);
        j++;
    } while (j < 5); // Condition is false, but runs once
    printf("\n");

    // 4.6. Break and Continue
    printf("Break/Continue demo: ");
    for (int i = 0; i < 10; i++) {
        if (i == 3) {
            continue; // Skip current iteration
        }
        if (i == 7) {
            break; // Exit loop entirely
        }
        printf("%d ", i);
    }
    printf("\n");

    // ##########################################################################
    // #                            5. Functions                                #
    // ##########################################################################
    print_separator("5. Functions");

    int sum_result = add_numbers(10, 20); // Call function by value
    printf("Sum of 10 and 20: %d\n", sum_result);

    int val1 = 100, val2 = 200;
    printf("Before swap: val1 = %d, val2 = %d\n", val1, val2);
    swap_values(&val1, &val2); // Pass addresses (pass by reference)
    printf("After swap: val1 = %d, val2 = %d\n", val1, val2);

    // ##########################################################################
    // #                            6. Arrays                                   #
    // ##########################################################################
    print_separator("6. Arrays");
    demonstrate_arrays();

    // ##########################################################################
    // #                            7. Strings                                  #
    // ##########################################################################
    print_separator("7. Strings");
    demonstrate_strings();

    // ##########################################################################
    // #                            8. Pointers                                 #
    // ##########################################################################
    print_separator("8. Pointers");
    demonstrate_pointers();

    // ##########################################################################
    // #                            9. Structs                                  #
    // ##########################################################################
    print_separator("9. Structs");
    demonstrate_structs();

    // ##########################################################################
    // #                            10. Dynamic Memory Allocation               #
    // ##########################################################################
    print_separator("10. Dynamic Memory Allocation");
    demonstrate_dynamic_memory();

    // ##########################################################################
    // #                            11. File I/O                                #
    // ##########################################################################
    print_separator("11. File I/O");
    demonstrate_file_io();

    // ##########################################################################
    // #                            12. Error Handling                          #
    // ##########################################################################
    print_separator("12. Error Handling with errno");
    demonstrate_error_handling();

    // ##########################################################################
    // #                            13. Command Line Arguments                  #
    // ##########################################################################
    print_separator("13. Command Line Arguments");
    printf("Number of arguments: %d\n", argc);
    printf("Arguments:\n");
    for (int i = 0; i < argc; i++) {
        printf("  argv[%d]: %s\n", i, argv[i]);
    }
    if (argc > 1) {
        int arg_num = atoi(argv[1]); // Convert string argument to integer
        printf("First argument as integer: %d\n", arg_num);
    }
    printf("To test this, compile and run like: ./program_name 123 hello\n");


    // ##########################################################################
    // #                            14. Time and Date                           #
    // ##########################################################################
    print_separator("14. Time and Date");
    time_t raw_time;
    struct tm *info;
    char buffer[80];

    time(&raw_time);
    info = localtime(&raw_time);

    strftime(buffer, sizeof(buffer), "Current local time: %Y-%m-%d %H:%M:%S", info);
    printf("%s\n", buffer);


    print_separator("End of Demonstration");
    return 0; // Indicate successful execution
}

// ##########################################################################
// #                            Function Definitions                        #
// ##########################################################################

// Simple utility function
void print_separator(const char *title) {
    printf("\n%s\n", title);
    printf("----------------------------------------\n");
}

// Function to add two integers
int add_numbers(int a, int b) {
    return a + b;
}

// Function to swap two integers using pointers
void swap_values(int *x, int *y) {
    int temp = *x; // Dereference x to get its value
    *x = *y;       // Assign value at y to address x points to
    *y = temp;     // Assign temp value to address y points to
}

// Demonstrate arrays
void demonstrate_arrays() {
    // One-dimensional array
    int numbers[5] = {10, 20, 30, 40, 50}; // Initialize array
    printf("Elements of numbers array: ");
    for (int i = 0; i < 5; i++) {
        printf("%d ", numbers[i]); // Access elements using index
    }
    printf("\n");

    // Modify an element
    numbers[2] = 35;
    printf("Modified numbers[2]: %d\n", numbers[2]);

    // Multi-dimensional array (2D array, matrix)
    int matrix[2][3] = {{1, 2, 3}, {4, 5, 6}};
    printf("Elements of matrix:\n");
    for (int i = 0; i < 2; i++) {
        for (int j = 0; j < 3; j++) {
            printf("%d ", matrix[i][j]);
        }
        printf("\n");
    }
}

// Demonstrate C-style strings
void demonstrate_strings() {
    char greeting[MAX_BUFFER]; // Declare a character array for string
    char name[50] = "World";
    char message[MAX_BUFFER];

    strcpy(greeting, "Hello"); // Copy string
    strcat(greeting, ", ");    // Concatenate strings
    strcat(greeting, name);
    strcat(greeting, "!");

    printf("Greeting: %s\n", greeting);
    printf("Length of greeting: %zu\n", strlen(greeting)); // %zu for size_t

    // String comparison (strcmp returns 0 if strings are equal)
    if (strcmp(name, "World") == 0) {
        printf("Name is 'World'.\n");
    }

    // Copy with snprintf (safer, prevents buffer overflow)
    snprintf(message, MAX_BUFFER, "The value of PI is approximately %.2f", PI);
    printf("Formatted message: %s\n", message);
}

// Demonstrate pointers
void demonstrate_pointers() {
    int value = 100;
    int *ptr_value; // Declare a pointer to an integer

    ptr_value = &value; // Assign the memory address of 'value' to ptr_value

    printf("Value: %d\n", value);
    printf("Address of value: %p\n", (void*)&value); // %p prints memory address
    printf("Pointer ptr_value stores address: %p\n", (void*)ptr_value);
    printf("Value pointed to by ptr_value: %d\n", *ptr_value); // Dereference pointer to get value

    // Change value via pointer
    *ptr_value = 200;
    printf("Value after changing via pointer: %d\n", value);

    // Pointer arithmetic (for arrays)
    int arr[] = {1, 2, 3, 4, 5};
    int *p = arr; // p points to the first element (arr is already an address)
    printf("arr[0] via pointer: %d\n", *p);
    printf("arr[1] via pointer arithmetic: %d\n", *(p + 1)); // Moves pointer to next int
}

// Define a struct
struct Person {
    char name[50];
    int age;
    float height;
};

// Demonstrate structs
void demonstrate_structs() {
    // Declare a struct variable
    struct Person person1;

    // Access and assign members using dot operator
    strcpy(person1.name, "Alice");
    person1.age = 30;
    person1.height = 1.65f;

    printf("Person 1: Name = %s, Age = %d, Height = %.2f\n",
           person1.name, person1.age, person1.height);

    // Pointers to structs
    struct Person *ptr_person;
    ptr_person = &person1;

    // Access members using arrow operator (->) for struct pointers
    printf("Person 1 (via pointer): Name = %s, Age = %d\n",
           ptr_person->name, ptr_person->age);
}

// Demonstrate dynamic memory allocation
void demonstrate_dynamic_memory() {
    int *dynamic_array;
    int size = 5;

    // Allocate memory for 5 integers
    dynamic_array = (int *) malloc(size * sizeof(int)); // Cast to int* is good practice

    if (dynamic_array == NULL) {
        printf("Memory allocation failed!\n");
        return; // Exit function
    }

    // Initialize and print allocated memory
    printf("Dynamic array elements: ");
    for (int i = 0; i < size; i++) {
        dynamic_array[i] = (i + 1) * 10;
        printf("%d ", dynamic_array[i]);
    }
    printf("\n");

    // Reallocate memory to a new size
    int new_size = 8;
    dynamic_array = (int *) realloc(dynamic_array, new_size * sizeof(int));
    if (dynamic_array == NULL) {
        printf("Memory reallocation failed!\n");
        return;
    }
    printf("Reallocated dynamic array (new size %d): ", new_size);
    for (int i = 0; i < new_size; i++) {
        if (i >= size) { // Initialize new elements
            dynamic_array[i] = 0;
        }
        printf("%d ", dynamic_array[i]);
    }
    printf("\n");

    // Free the allocated memory to prevent memory leaks
    free(dynamic_array);
    printf("Dynamically allocated memory freed.\n");

    // dynamic_array is now a dangling pointer, best practice to set to NULL
    dynamic_array = NULL;
}

// Demonstrate file I/O
void demonstrate_file_io() {
    FILE *file_ptr;
    char file_name[] = "example.txt";
    char buffer[MAX_BUFFER];

    // 11.1. Writing to a file
    file_ptr = fopen(file_name, "w"); // "w" for write mode (creates/overwrites)
    if (file_ptr == NULL) {
        perror("Error opening file for writing"); // Prints system error message
        return;
    }
    fprintf(file_ptr, "This is the first line.\n");
    fputs("This is the second line.\n", file_ptr); // fputs doesn't add newline
    fclose(file_ptr);
    printf("Content written to %s\n", file_name);

    // 11.2. Reading from a file
    file_ptr = fopen(file_name, "r"); // "r" for read mode
    if (file_ptr == NULL) {
        perror("Error opening file for reading");
        return;
    }
    printf("Content of %s:\n", file_name);
    while (fgets(buffer, MAX_BUFFER, file_ptr) != NULL) { // Read line by line
        printf("%s", buffer); // buffer already contains newline from file
    }
    fclose(file_ptr);

    // 11.3. Appending to a file
    file_ptr = fopen(file_name, "a"); // "a" for append mode (creates if not exists, adds to end)
    if (file_ptr == NULL) {
        perror("Error opening file for appending");
        return;
    }
    fprintf(file_ptr, "This line was appended.\n");
    fclose(file_ptr);
    printf("Content appended to %s\n", file_name);

    // 11.4. Character by character I/O
    file_ptr = fopen(file_name, "r");
    if (file_ptr == NULL) {
        perror("Error opening file for char read");
        return;
    }
    printf("Reading char by char:\n");
    int c;
    while ((c = fgetc(file_ptr)) != EOF) {
        putchar(c);
    }
    fclose(file_ptr);

    // Clean up: delete the created file
    if (remove(file_name) == 0) {
        printf("Cleaned up: %s deleted.\n", file_name);
    } else {
        perror("Error deleting file");
    }
}

// Demonstrate error handling with errno
void demonstrate_error_handling() {
    FILE *non_existent_file = fopen("non_existent_path/file.txt", "r");
    if (non_existent_file == NULL) {
        // errno will be set by the failing library function (e.g., EACCES, ENOENT)
        perror("Attempt to open non-existent file"); // perror prints descriptive error message based on errno
        // Alternatively, use strerror(errno) to get the error string
        // printf("Error: %s\n", strerror(errno));
    } else {
        fclose(non_existent_file);
    }
}