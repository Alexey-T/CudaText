// 1. Basic Structure: main function, comments
// `fn main()` is the entry point of every executable Rust program.

/*Cmt
Cmt*/
///Cmt doc
$var, $ var, 'id, '\xFF', i32, 0x10_AB, 1.2e+3, r#Foo

trait Repr {
    fn write_repr(&self, writer: @Writer);
}

impl EscapedCharWriter for @Writer {
    fn write_escaped_char(&self, ch: char) {
        match ch {
            '\t' => self.write_str("\\t"),
            '\\' => self.write_str("\\\\"),
            '\'' => self.write_str("\\'"),
            '"' => self.write_str("\\\""),
            '\x20'..'\x7e' => self.write_char(ch),
        }
    }
}

    #[test]
    fn map() {
        assert_same::<BTreeMap<char, String>>(
            "{
'a': \"Hello\",
'b': \"Bye\",
        }",
        );
    }

fn main() {
    // Single-line comment
    /*
     * Multi-line comment
     */
    println!("--- Rust Syntax Demonstration ---");

    // 2. Variables and Mutability
    // Variables are immutable by default. Use `mut` for mutability.
    let immutable_string = "Hello, Rust!"; // Type inferred as &str (string slice)
    let mut mutable_integer = 123;         // Type inferred as i32 (signed 32-bit integer)
    let float_value: f64 = 3.14159;       // Explicit type annotation (64-bit float)
    let boolean_value = true;

    println!("Immutable String: {}", immutable_string);
    println!("Mutable Integer (before change): {}", mutable_integer);
    mutable_integer = 456; // OK because `mutable_integer` is mutable
    println!("Mutable Integer (after change): {}", mutable_integer);
    println!("Float: {}", float_value);
    println!("Boolean: {}", boolean_value);

    // 3. Data Structures: Tuples, Arrays, Structs, Enums
    // Tuples: Fixed-size collection of different types.
    let my_tuple: (&str, i32, f64) = ("tuple_item", 10, 20.5);
    println!("Tuple: {} {} {}", my_tuple.0, my_tuple.1, my_tuple.2);

    // Arrays: Fixed-size, same-type collection.
    let my_array: [i32; 3] = [1, 2, 3]; // [type; size]
    println!("Array: {} {}", my_array[0], my_array[2]);

    // Vectors (Dynamic Arrays): Growable, same-type collection.
    let mut my_vector: Vec<String> = vec!["apple".to_string(), "banana".to_string()];
    my_vector.push("cherry".to_string());
    println!("Vector: {:?}", my_vector); // `{:?}` for debug printing

    // Structs: Custom data types for grouping related data.
    struct User {
        username: String,
        email: String,
        active: bool,
    }

    let user1 = User {
        username: String::from("alice_smith"),
        email: String::from("alice@example.com"),
        active: true,
    };
    println!("User: {} ({})", user1.username, user1.email);

    // Enums: Define a type by enumerating its possible variants.
    enum Message {
        Quit,
        Move { x: i32, y: i32 },
        Write(String),
        ChangeColor(i32, i32, i32),
    }

    let msg = Message::Write(String::from("hello from enum"));
    // Enums are often used with `match` expressions (see below).

    // 4. Operators
    let a = 10;
    let b = 3;
    println!("Addition: {}", a + b);
    println!("Division: {}", a / b); // Integer division
    println!("Modulo: {}", a % b);

    let s1 = "Rust".to_string();
    let s2 = "aceans".to_string();
    let s3 = s1 + &s2; // String concatenation (s1 is moved)
    println!("Concatenation: {}", s3);

    // 5. Control Flow: if/else, match, loops
    // if/else if/else
    let number = 7;
    if number < 5 {
        println!("Condition is true");
    } else if number == 7 {
        println!("Number is 7");
    } else {
        println!("Condition is false");
    }

    // `match` expression (powerful pattern matching)
    // Exhaustive: must cover all possibilities.
    let coin = "penny";
    let value = match coin {
        "penny" => 1,
        "nickel" => 5,
        "dime" => 10,
        "quarter" => 25,
        _ => { // Catch-all pattern for anything else
            println!("Unknown coin!");
            0 // Value to return for unknown coin
        }
    };
    println!("Value of a {}: {}", coin, value);

    // `loop` (infinite loop, use `break` to exit)
    let mut counter = 0;
    loop {
        counter += 1;
        if counter == 3 {
            break;
        }
    }
    println!("Loop finished at counter: {}", counter);

    // `while` loop
    let mut countdown = 3;
    while countdown > 0 {
        println!("Countdown: {}", countdown);
        countdown -= 1;
    }

    // `for` loop (iterating over collections or ranges)
    let colors = ["red", "green", "blue"];
    for color in colors.iter() { // `.iter()` to get an iterator over elements
        println!("Color: {}", color);
    }
    for num in 1..=3 { // Inclusive range 1 to 3
        println!("Number in range: {}", num);
    }

    // 6. Functions (Methods)
    // Defined with `fn`. Types of parameters and return value must be specified.
    fn add(x: i32, y: i32) -> i32 { // `-> i32` specifies return type
        x + y // No semicolon means this is the return expression
    }
    let result = add(10, 20);
    println!("Function result: {}", result);

    // Function with no return value (or returns `()`, the unit type)
    fn print_message(msg: &str) {
        println!("Message from function: {}", msg);
    }
    print_message("Hello from a custom function!");

    // 7. Ownership, Borrowing, and Lifetimes (Fundamental Rust Concepts)
    // (This example only scratches the surface, it's a deep topic)

    // Ownership: `s1` owns "hello"
    let s_owned = String::from("hello");
    // `s_owned` is moved to `take_ownership`, can't be used after that.
    // take_ownership(s_owned);
    // println!("{}", s_owned); // ERROR: value borrowed here after move

    // Borrowing: `&` creates a reference (borrow)
    let s_ref = String::from("world");
    // `calculate_length` borrows `s_ref`, doesn't take ownership.
    // `s_ref` can still be used after the call.
    let len = calculate_length(&s_ref); // &String -> &str (Deref Coercions)
    println!("The length of '{}' is {}.", s_ref, len);

    // Mutable borrowing: `&mut`
    let mut s_mutable = String::from("hello");
    change_string(&mut s_mutable);
    println!("Changed string: {}", s_mutable);


    // 8. Error Handling: `Result` and `Option` Enums
    // `Option` enum: `Some(T)` or `None`. For potential absence of a value.
    let numbers = [1, 2, 3];
    let first_elem = numbers.get(0); // Returns Some(1)
    let fourth_elem = numbers.get(3); // Returns None

    match first_elem {
        Some(val) => println!("First element: {}", val),
        None => println!("No first element."),
    }

    // `Result` enum: `Ok(T)` or `Err(E)`. For fallible operations.
    fn divide(numerator: f64, denominator: f64) -> Result<f64, String> {
        if denominator == 0.0 {
            Err(String::from("Cannot divide by zero!"))
        } else {
            Ok(numerator / denominator)
        }
    }

    let div_result = divide(10.0, 2.0);
    match div_result {
        Ok(val) => println!("Division result: {}", val),
        Err(err) => println!("Error during division: {}", err),
    }

    let div_error = divide(10.0, 0.0);
    if let Err(e) = div_error { // `if let` for concise error handling
        println!("Specific error: {}", e);
    }

    // The `?` operator (for propagating errors)
    // Used in functions that return `Result` or `Option`.
    // It returns `Err` or `None` if the wrapped value is `Err` or `None`.
    // Otherwise, it unwraps the `Ok` or `Some` value.
    fn try_read_file(path: &str) -> Result<String, std::io::Error> {
        let content = std::fs::read_to_string(path)?; // `?` propagates any IO error
        Ok(content)
    }

    let file_content_result = try_read_file("non_existent_file.txt");
    match file_content_result {
        Ok(content) => println!("File content: {}", content),
        Err(e) => println!("Failed to read file: {}", e),
    }


    // 9. Traits (similar to interfaces or type classes)
    // Define shared behavior for types.
    trait Animal {
        fn speak(&self) -> String;
        fn can_fly(&self) -> bool { // Default implementation
            false
        }
    }

    struct Dog;
    impl Animal for Dog {
        fn speak(&self) -> String {
            String::from("Woof!")
        }
    }

    struct Bird;
    impl Animal for Bird {
        fn speak(&self) -> String {
            String::from("Chirp!")
        }
        fn can_fly(&self) -> bool {
            true
        }
    }

    let my_dog = Dog;
    let my_bird = Bird;
    println!("Dog says: {}", my_dog.speak());
    println!("Can dog fly? {}", my_dog.can_fly());
    println!("Bird says: {}", my_bird.speak());
    println!("Can bird fly? {}", my_bird.can_fly());


    // 10. Modules (for organizing code)
    // Rust code is organized into modules, which can be nested.
    // 'mod' keyword defines a module.
    mod greetings {
        // Functions inside a module are private by default.
        // Use `pub` to make them public.
        pub fn english() {
            println!("Hello!");
        }

        fn spanish() {
            println!("Hola!"); // This is private to 'greetings'
        }

        pub fn greet_in_spanish() {
            spanish(); // Can call private functions within the same module
        }
    }

    greetings::english();
    greetings::greet_in_spanish();
    // greetings::spanish(); // ERROR: `spanish` is private


    println!("--- Demonstration Complete ---");
} // End of main function

// Functions for Ownership/Borrowing demonstration
fn take_ownership(some_string: String) { // `some_string` comes into scope
    println!("Ownership taken: {}", some_string);
} // `some_string` goes out of scope and `drop` is called.

fn calculate_length(s: &String) -> usize { // `s` is a reference to a String
    s.len()
} // `s` goes out of scope. Original String is not dropped.

fn change_string(some_string: &mut String) { // `some_string` is a mutable reference
    some_string.push_str(", world!");
}