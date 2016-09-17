// SYNTAX TEST "Packages/Rust/Rust.sublime-syntax"

// Line comments
// <- comment.line.double-slash
// ^^^^^^^^^^^^^ comment.line.double-slash
/// Line doc comments
// <- comment.line.documentation
// ^^^^^^^^^^^^^ comment.line.documentation

/*!
// <- comment.block.documentation
 // <- comment.block.documentation
//^ comment.block.documentation
Block doc comments
// ^^^^^^^^^^^^^^^^ comment.block.documentation
/* Nested comments */
// ^^^^^^^^^^^^^^^^^^ comment.block.documentation comment.block
*/

let c = 'c';
// <- storage.type
//    ^ keyword.operator
//      ^^^ string.quoted.single
let b = b'c';
// <- storage.type
//    ^ keyword.operator
//      ^ storage.type
//       ^^^ string.quoted.single

let s = "This is a string \x01_\u{007F}_\"_\'_\\_\r_\n_\t_\0";
// <- storage.type
//    ^ keyword.operator
//      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ string.quoted.double
//                        ^^^^ constant.character.escape
//                             ^^^^^^^^ constant.character.escape
//                                      ^^ constant.character.escape
//                                         ^^ constant.character.escape
//                                            ^^ constant.character.escape
//                                               ^^ constant.character.escape
//                                                  ^^ constant.character.escape
//                                                     ^^ constant.character.escape
//                                                        ^^ constant.character.escape
let r = r#"This is a raw string, no escapes! \x00 \0 \n"#;
// <- storage.type
//    ^ keyword.operator
//      ^ storage.type
//       ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ string.quoted.double - constant.character.escape

let bytes = b"This won't escape unicode \u{0123}, but will do \x01_\"_\'_\\_\r_\n_\t_\0";
// <- storage.type
//        ^ keyword.operator
//          ^ storage.type
//           ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ string.quoted.double
//                                                            ^^^^ constant.character.escape
//                                                                 ^^ constant.character.escape
//                                                                    ^^ constant.character.escape
//                                                                       ^^ constant.character.escape
//                                                                          ^^ constant.character.escape
//                                                                             ^^ constant.character.escape
//                                                                                ^^ constant.character.escape
//                                                                                   ^^ constant.character.escape

let raw_bytes = br#"This won't escape anything either \x01 \""#;
// <- storage.type
//            ^ keyword.operator
//              ^^ storage.type
//                ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ string.quoted.double - constant.character.escape

0;
// <- constant.numeric.integer.decimal
1_000u32;
// <- constant.numeric.integer.decimal
 // <- constant.numeric.integer.decimal
//^^^ constant.numeric.integer.decimal
//   ^^^ storage.type - constant.numeric.integer.decimal
1i64;
// <- constant.numeric.integer.decimal
 // <- storage.type - constant.numeric.integer.decimal
//^^ storage.type - constant.numeric.integer.decimal

0.2;
// <- constant.numeric.float
 // <- constant.numeric.float
//^ constant.numeric.float
1_000.0_;
// <- constant.numeric.float
 // <- constant.numeric.float
//^^^^^^ constant.numeric.float
1.0f32;
// <- constant.numeric.float
 // <- constant.numeric.float
//^ constant.numeric.float
// ^^^ storage.type - constant.numeric.float
0.;
// <- constant.numeric.float
 // <- constant.numeric.float
0f64;
// <- constant.numeric.float
 // <- storage.type - constant.numeric.float
//^^ storage.type - constant.numeric.float

0x0;
// <- constant.numeric.integer.hexadecimal
 // <- constant.numeric.integer.hexadecimal
//^ constant.numeric.integer.hexadecimal
0xfa;
// <- constant.numeric.integer.hexadecimal
 // <- constant.numeric.integer.hexadecimal
//^^ constant.numeric.integer.hexadecimal
0xFA_01i32;
// <- constant.numeric.integer.hexadecimal
 // <- constant.numeric.integer.hexadecimal
//^^^^^ constant.numeric.integer.hexadecimal
//     ^^^ storage.type - constant.numeric.integer.hexadecimal

0b1;
// <- constant.numeric.integer.binary
 // <- constant.numeric.integer.binary
//^ constant.numeric.integer.binary
0b0_1u8;
// <- constant.numeric.integer.binary
 // <- constant.numeric.integer.binary
//^^^ constant.numeric.integer.binary
//   ^^ storage.type - constant.numeric.integer.binary

0o0;
// <- constant.numeric.integer.octal
 // <- constant.numeric.integer.octal
//^ constant.numeric.integer.octal
0o0000_0010u64;
// <- constant.numeric.integer.octal
 // <- constant.numeric.integer.octal
//^^^^^^^^^ constant.numeric.integer.octal
//         ^^^ storage.type - constant.numeric.integer.octal

extern crate foo;
// <- keyword.other
//^^^^ keyword.other
//    ^ - keyword.other
//     ^^^^^ keyword.other

mod trafile;
// <- storage.type.module
mod comment;
// <- storage.type.module
mod location;

pub use self::trafile::*;
// <- storage.modifier
//   ^ keyword.other
//      ^^^^^^^^^^^^^^^ meta.path

use std::fmt;
// <- keyword.other
//  ^^^^^ meta.path
//       ^^^ - meta.path
use foo::i32;
//  ^^^^^ meta.path
//       ^^^ - meta.path storage.type
use foo::Bar;
//  ^^^^^ meta.path
use foo::{Baz, QUX, quux};
//  ^^^^^ meta.path
//       ^^^^^^^^^^^^^^^^ meta.block
//             ^^^ constant.other

String my_var = format!("Hello {0}", "World");
// ^^^ support.type
//            ^ keyword.operator
//              ^^^^^^^ support.macro
//                     ^ punctuation.definition.group.begin
//                     ^^^^^^^^^^^^^^^^^^^^^^ meta.group
//                      ^^^^^^^^^^^ string.quoted.double
//                             ^^^ constant.other.placeholder
//                                          ^ punctuation.definition.group.end

my_var = format!("Hello {name}, how are you?",
//     ^ keyword.operator
//       ^^^^^^^ support.macro
//              ^ punctuation.definition.group.begin
//              ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ meta.group
//               ^^^^^^^^^^^^^^^^^^^^^^^^^^^^ string.quoted.double
//                      ^^^^^^ constant.other.placeholder
    name="John");
// ^^^^^^^^^^^^^ meta.group
//      ^ keyword.operator
//       ^^^^^^ string.quoted.double
//             ^ punctuation.definition.group.end

struct BasicStruct(i32);
// ^^^^^^^^^^^^^^^^^^^^ meta.struct
// <- storage.type.struct
//^^^^ storage.type.struct
//     ^^^^^^^^^^^ entity.name.struct
//                ^ punctuation.definition.group.begin
//                 ^^^ storage.type
//                    ^ punctuation.definition.group.end

#[derive(Debug)]
// <- comment.block.attribute
//^^^^^^^^^^^^^^ comment.block.attribute
struct PrintableStruct(Box<i32>);
// ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ meta.struct
// <- storage.type.struct
//^^^^ storage.type.struct
//     ^^^^^^^^^^^^^^^ entity.name.struct
//                    ^ punctuation.definition.group.begin
//                     ^^^^^^^^ meta.generic
//                        ^ punctuation.definition.generic.begin
//                         ^^^ storage.type
//                            ^ punctuation.definition.generic.end
//                             ^ punctuation.definition.group.end

impl fmt::Display for PrintableStruct {
// <- meta.impl
//^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ meta.impl
// <- storage.type.impl
//^^ storage.type.impl
//   ^^^^^ meta.path
//                ^^^ keyword.other
//                    ^^^^^^^^^^^^^^^ entity.name.impl
//                                    ^ meta.block punctuation.definition.block.begin
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
// ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ meta.impl
//  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ meta.function
//  ^^ storage.type.function
//     ^^^ entity.name.function
//        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ meta.function.parameters
//        ^ punctuation.definition.parameters.begin
//         ^ keyword.operator
//          ^^^^ variable.parameter
//                ^ variable.parameter
//                 ^ punctuation.separator
//                   ^ keyword.operator
//                    ^^^ storage.modifier
//                        ^^^^^ meta.path
//                                      ^ punctuation.definition.parameters.end
//                                        ^^ punctuation.separator
//                                           ^^^^^ meta.path
//                                                       ^ meta.block punctuation.definition.block.begin
        write!(f, "{}", self.0)
// ^^^^^^^^^^^^^^^^^^^^^^^^^^^^ meta.function
//      ^^^^^^ support.macro
//            ^^^^^^^^^^^^^^^^^ meta.group
//            ^ punctuation.definition.group.begin
//                ^^^^ string.quoted.double
//                 ^^ constant.other.placeholder
//                            ^ punctuation.definition.group.end
    }
// ^^ meta.function meta.block
//  ^ punctuation.definition.block.end
}
// <- meta.block punctuation.definition.block.end

let logical: bool = true;
//         ^ punctuation.separator
//           ^^^^ storage.type
//                ^ keyword.operator
//                  ^^^^ constant.language
let mut mutable = 12;
//  ^^^ storage.modifier

let ch = '∞';
//       ^^^ string.quoted.single

let tuple = (1.0, 0i32, "Hello");
//          ^^^^^^^^^^^^^^^^^^^^ meta.group
//          ^ punctuation.definition.group.begin
//           ^^^ constant.numeric.float
//                ^ constant.numeric.integer.decimal
//                 ^^^ storage.type
//                      ^^^^^^^ string.quoted.double
//                             ^ punctuation.definition.group.end

let xs: [i32; 5] = [1, 2, 3, 4, 5];
//    ^ punctuation.separator
//      ^^^^^^^^ meta.group
//      ^ punctuation.definition.group.begin
//       ^^^ storage.type
//            ^ constant.numeric.integer.decimal
//             ^ punctuation.definition.group.end
//                 ^^^^^^^^^^^^^^^ meta.group
//                 ^ punctuation.definition.group.begin
//                               ^ punctuation.definition.group.end
let xsl = &xs;
//        ^ keyword.operator

type FnPointer = fn(i32) -> i32;
//   ^^^^^^^^^ entity.name.type
//               ^^ storage.type.function
//                 ^^^^^ meta.group
//                  ^^^ storage.type
//                       ^^ punctuation.separator
//                          ^^^ storage.type

type GenFnPointer = Bar<fn(i32) -> i32>;
//   ^^^^^^^^^^^^ entity.name.type
//                  ^^^^^^^^^^^^^^^^^^^ meta.generic
//                      ^^ storage.type.function
//                        ^^^^^ meta.group
//                         ^^^ storage.type
//                              ^^ punctuation.separator
//                                 ^^^ storage.type
//                                     ^ - meta.generic

type GenFnPointer2 = Bar<extern "C" fn()>;
//   ^^^^^^^^^^^^^ entity.name.type
//                   ^^^^^^^^^^^^^^^^^^^^ meta.generic
//                       ^^^^^^ keyword.other
//                              ^^^ string.quoted.double
//                                  ^^ storage.type.function
//                                       ^ - meta.generic

struct Nil;
// ^^^^^^^ meta.struct
//        ^ - meta.struct
struct Pair(i32, i32);
// ^^^^^^^^^^^^^^^^^^ meta.struct
//          ^^^ storage.type
//               ^^^ storage.type
//                   ^ - meta.struct

enum OperatingSystem
// <- storage.type.enum
// ^^^^^^^^^^^^^^^^^ meta.enum
//   ^^^^^^^^^^^^^^^ entity.name.enum
{
// <- meta.enum meta.block punctuation.definition.block.begin
    Osx,
    Windows,
    Linux,
    Bsd(String),
    //  ^^^^^^ support.type
    Info { field: i32, value: str }
    //   ^ meta.block meta.block punctuation.definition.block.begin
    //            ^^^ storage.type
    //                        ^^^ storage.type
    //                            ^ meta.block meta.block punctuation.definition.block.end
}
// <- meta.enum meta.block punctuation.definition.block.end

const ZERO: u64 = 0;
// <- storage.type
//    ^^^^ constant.other
//        ^ punctuation.separator
//          ^^^ storage.type
//              ^ keyword.operator
//                ^ constant.numeric.integer.decimal
static NAME: &'static str = "John";
// <- storage.type
//           ^ keyword.operator
//            ^^^^^^^ storage.modifier.lifetime
//                    ^^^ storage.type
//                        ^ keyword.operator
//                          ^^^^^^ string.quoted.double


let z = {
//      ^ meta.block punctuation.definition.block.begin
    2 * 5
//  ^ constant.numeric.integer.decimal
//    ^ keyword.operator
//      ^ constant.numeric.integer.decimal
};
// <- meta.block punctuation.definition.block.end

fn my_func(x: i32)
// <- storage.type.function
// ^^^^^^^ entity.name.function
//        ^^^^^^^^ meta.function.parameters
//        ^ punctuation.definition.parameters.begin
//         ^ variable.parameter
//          ^ punctuation.separator
//               ^ punctuation.definition.parameters.end
{
// <-  meta.function meta.block punctuation.definition.block.begin

}
// <-  meta.function meta.block punctuation.definition.block.end

let n = 5;

if n < 0 {
//       ^ meta.block punctuation.definition.block.begin
// <- keyword.control
    print!("{} is negative", n);
} else if n > 0 {
// <- meta.block punctuation.definition.block.end
// ^^^ keyword.control
//              ^ meta.block punctuation.definition.block.begin
//     ^^ keyword.control
    print!("{0} is positive", n);
} else {
// <- meta.block punctuation.definition.block.end
// ^^^ keyword.control
//     ^ meta.block punctuation.definition.block.begin
    print!("{} is zero", n);
// ^^^^^^^^^^^^^^^^^^^^^^^^^ meta.block
}
// <- meta.block punctuation.definition.block.end

let big_n =
//        ^ keyword.operator
    if n < 10 && n > -10 {
//                       ^ meta.block punctuation.definition.block.begin
        10 * n
    } else {
//  ^ meta.block punctuation.definition.block.end
//         ^ meta.block punctuation.definition.block.begin
        n / 2
    };
//  ^ meta.block punctuation.definition.block.end

'label_name: loop {
// ^^^^^^^^ entity.name.label
//         ^ punctuation.separator
//           ^^^^ keyword.control
//                ^ meta.block punctuation.definition.block.begin
    n += 1;
    if n / 2 == 5 {
//       ^ keyword.operator
        continue;
//      ^^^^^^^^ keyword.control
    }
    if n > 9 {
        break;
//      ^^^^^ keyword.control
    }
}

while n < 50 {
//^^^ keyword.control
    n = n * 2;
}

for i in 1..10 {
// <- keyword.control
//    ^^ keyword.operator
//       ^ constant.numeric.integer.decimal
//        ^^ keyword.operator
//          ^^ constant.numeric.integer.decimal
    println!("I: {}", i);
// ^^^^^^^^^^^^^^^^^^^^^^ meta.block
}
// <- meta.block punctuation.definition.block.end

let o = match n {
//      ^^^^^ keyword.control
    1 => "one",
//  ^ constant.numeric.integer.decimal
//    ^^ keyword.operator
//       ^^^^^ string.quoted.double
    2 => "two",
//  ^ constant.numeric.integer.decimal
//    ^^ keyword.operator
//       ^^^^^ string.quoted.double
    3...5 => "a few",
//  ^ constant.numeric.integer.decimal
//   ^^^ keyword.operator
//      ^ constant.numeric.integer.decimal
//        ^^ keyword.operator
//           ^^^^^^^ string.quoted.double
    _ => "lots",
//  ^ keyword.operator
//    ^^ keyword.operator
};

let mut j = BasicStruct(10);
//  ^^^ storage.modifier
//                      ^^ constant.numeric.integer.decimal

if let BasicStruct(i) = j {
// ^^^ storage.type
//                    ^ keyword.operator
//                        ^ meta.block punctuation.definition.block.begin
    println!("Basic value: {}", i);
}
// <- meta.block punctuation.definition.block.end

while let BasicStruct(k) = j {
//^^^ keyword.control
//    ^^^ storage.type
//                       ^ keyword.operator
//                           ^ meta.block punctuation.definition.block.begin
    println!("Constructed example: {}", j)
    j = BasicStruct(j + 1);
    if k > 20 {
        break;
        //^^^ meta.block meta.block keyword.control
    }
}
// <- meta.block punctuation.definition.block.end

fn foo<A>(i: u32, b: i64) -> u32 {
// <- storage.type.function
// ^^^ entity.name.function
//    ^ punctuation.definition.generic.begin
//      ^ punctuation.definition.generic.end
//       ^^^^^^^^^^^^^^^^ meta.function.parameters
//                           ^^^ storage.type
//                               ^ meta.block punctuation.definition.block.begin

}
// <- meta.block punctuation.definition.block.end

// Guards
match n {
// <- keyword.control
    a if n > 5 => println!("Big: {}", a),
//    ^^ keyword.control
//         ^ keyword.operator
//             ^^ keyword.operator
//                ^^^^^^^^ support.macro
    b if n <= 5 => println!("Small: {}", b),
//    ^^ keyword.control
//         ^^ keyword.operator
//              ^^ keyword.operator
//                 ^^^^^^^^ support.macro
//                                  ^^ constant.other.placeholder
}

// Binding
match my_func() {
// ^^ keyword.control
//    ^^^^^^^ support.function
//              ^ meta.block punctuation.definition.block.begin
    0 => println!("None"),
//  ^ constant.numeric.integer.decimal
//    ^^ keyword.operator
//       ^^^^^^^^ support.macro
    res @ 1...9 => println!("Digit: {}", res),
//      ^ keyword.operator
//         ^^^ keyword.operator
//                                  ^^ constant.other.placeholder
    _ => println!("Full number"),
//  ^ keyword.operator
//    ^^ keyword.operator
}
// <- meta.block punctuation.definition.block.end

fn my_other_func(e: OperatingSystem) -> u32 {
// ^^^^^^^^^^^^^ entity.name.function
//               ^ variable.parameter
//                ^ punctuation.separator
}

struct Point
// ^^^^^^^^^ meta.struct
{
// <- meta.struct meta.block punctuation.definition.block.begin
    x: i32,
//  ^ variable.other.member
//   ^ punctuation.separator
//     ^^^ storage.type
    y: i32
//  ^ variable.other.member
//   ^ punctuation.separator
//     ^^^ storage.type
}
// <-  meta.block punctuation.definition.block.end

impl Point
//^^^^^^^^ meta.impl
{
// <- meta.impl meta.block punctuation.definition.block.begin
    fn new(x: i32, y: i32) -> Point
    // <- storage.type.function
    // ^^^ entity.name.function
    {
    // <- meta.function meta.block
        Point {x: x, y: y}
    }

    fn double(&mut self) {
    // ^^^^^^ entity.name.function
        self.x *= 2;
        self.y *= 2;
    }
}

pub fn pub_function() -> bool
// <- storage.modifier
//  ^^ storage.type.function
//     ^^^^^^^^^^^^ entity.name.function
{
// <- meta.function
    return true
}

let inferred_closure = |i, j: u32| i + 1;
//  ^^^^^^^^^^^^^^^^ entity.name.function
//                     ^^^^^^^^^^^^^^^^^ meta.function.closure
//                     ^^^^^^^^^^^ meta.function.parameters
//                     ^ punctuation.definition.parameters.begin
//                      ^ variable.parameter
//                         ^ variable.parameter
//                          ^ punctuation.separator
//                            ^^^ storage.type
//                               ^ punctuation.definition.parameters.end
let closure = || -> i32 { | | 1 + 2 };
//  ^^^^^^^ entity.name.function
//            ^^^^^^^^^^^^^^^^^^^^^^^ meta.function.closure
//            ^^ meta.function.parameters
//            ^ punctuation.definition.parameters.begin
//             ^ punctuation.definition.parameters.end
//                  ^^^ storage.type
//                      ^^^^^^^^^^^^^ meta.block
//                      ^ meta.block punctuation.definition.block.begin
//                            ^ constant.numeric.integer.decimal
//                                ^ constant.numeric.integer.decimal
//                                  ^ meta.block punctuation.definition.block.end

let c = a | b;
//        ^ keyword.operator

call_func(|c| 1 + 2 + c);
//        ^^^^^^^^^^^^^ meta.function.closure
//        ^^^ meta.function.parameters

macro_rules! print_result {
    ($expr:expr) => (
        println!("{:?} = {:?}", stringify!($expr), $expr)
    )
}


pub fn from_buf_reader<T>(s: io::BufReader<T>) -> Result<isize, &'static str>
//                                                              ^ keyword.operator
    where T: io::Read
//  ^ keyword.other
{
    macro_rules! eat_numbers {
        ($count:expr, $msg:expr) => {{
        //                          ^ meta.function meta.block meta.macro meta.block meta.block punctuation.definition.block.begin
        //                           ^ meta.function meta.block meta.macro meta.block meta.block meta.block punctuation.definition.block.begin
            let parse_err = concat!("Err parsing value in ", $msg);
            try!{ eat_numbers(&mut lines, $count, parse_err, missing_err, too_many) }
        //  ^^^^ support.macro
        //      ^ meta.function meta.block meta.macro meta.block meta.block meta.block meta.block punctuation.definition.block.begin
        }}
    };
     // <- meta.function meta.block - meta.macro

    let mut starts_stops = eat_numbers!(relief_count_total * 2, "starts and stops");

    let starts = starts_stops.split_off(relief_count_total);
    let stops = starts_stops;

    unimplemented!();
}

pub mod my_mod {
//^^^^^^^^^^^^^^ meta.module
// <- storage.modifier
//  ^^^ storage.type.module
//      ^^^^^^ entity.name.module
//             ^ meta.block punctuation.definition.block.begin
}
// <- meta.module meta.block punctuation.definition.block.end

struct Val (f64,);
struct GenVal<T>(T,);
//     ^^^^^^^^^ meta.generic
//     ^^^^^^ entity.name.struct
//           ^ punctuation.definition.generic.begin - entity.name.struct
//             ^ punctuation.definition.generic.end

// impl of Val
impl Val {
    fn value(&self) -> &'a f64 { &self.0 }
    //                 ^ keyword.operator
    //                  ^^ storage.modifier.lifetime
}

// impl of GenVal for a generic type `T`
impl <'a, T> GenVal<T> {
//   ^ punctuation.definition.generic.begin
//    ^^ storage.modifier.lifetime
//         ^ punctuation.definition.generic.end
//           ^^^^^^ entity.name.impl
//                 ^^^ - entity.name.impl
//                 ^ punctuation.definition.generic.begin
//                   ^ punctuation.definition.generic.end
    fn value(&self) -> &T { &self.0 }
    //       ^ keyword.operator
    //                 ^ keyword.operator
}

fn print_debug<T: Debug> (t: &T) {
// ^^^^^^^^^^^ entity.name.function
//            ^ punctuation.definition.generic.begin - entity.name.function
//              ^ punctuation.separator
//                     ^ punctuation.definition.generic.end
//                        ^ variable.parameter
//                           ^ keyword.operator
    println!("{:?}", t);
//            ^^^^ constant.other.placeholder
}

impl<'a, T: MyTrait + OtherTrait> PrintInOption for T where
// ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ meta.impl
//   ^^ storage.modifier.lifetime
//        ^ punctuation.separator
//                  ^ keyword.operator
//                                              ^^^ keyword.other
//                                                  ^ entity.name.impl
//                                                    ^^^^^ keyword.other
    Option<T>: Debug {
//^^^^^^^^^^^^^^^^^^^^ meta.impl
    fn print_in_option(self) {
//     ^^^^^^^^^^^^^^^ entity.name.function
        println!("{:?}", Some(self));
    }
}

pub trait Animal {
// <- storage.modifier
//^^^^^^^^^^^^^^^^ meta.trait
//               ^ meta.block punctuation.definition.block.begin
    fn noise(quiet: bool) {
        // Comment
    }
}
// <- meta.trait meta.block punctuation.definition.block.end

fn collect_vec() {
    let _: Vec<(usize, usize)> = (0..10).enumerate().collect::<Vec<_>>();
//         ^^^^^^^^^^^^^^^^^^^ meta.generic
//             ^ punctuation.definition.type.begin
//              ^^^^^ storage.type
//                     ^^^^^ storage.type
//                          ^ punctuation.definition.type.end
//                                                            ^^^^^^^^ meta.generic
//                                                             ^^^^^^ meta.generic meta.generic
//                                                                 ^ keyword.operator
    let _: Vec<(usize, usize)> = vec!();
//                               ^^^^ support.macro
    let _: Vec<(usize, usize)> = vec!{};
//                               ^^^^ support.macro
    let _: Vec<(usize, usize)> = vec![];
//                               ^^^^ support.macro
}

macro_rules! forward_ref_binop [
//                             ^ meta.macro meta.group punctuation.definition.group.begin
    (impl $imp:ident, $method:ident for $t:ty, $u:ty) => {
//        ^^^^ variable.parameter
//             ^^^^^ storage.type
//                    ^^^^^^^ variable.parameter
//                            ^^^^^ storage.type
//                                      ^^ variable.parameter
//                                         ^^ storage.type
//                                             ^^ variable.parameter
//                                                ^^ storage.type
//                                                    ^^ keyword.operator
//                                                       ^ meta.macro meta.group meta.block punctuation.definition.block.begin
        impl<'a, 'b> $imp<&'a $u> for &'b $t {
//      ^^^^ storage.type.impl
//          ^^^^^^^^ meta.generic
//           ^^ storage.modifier.lifetime
//               ^^ storage.modifier.lifetime
//                   ^^^^ variable.other
//                       ^^^^^^^^ meta.generic
//                        ^ keyword.operator
//                         ^^ storage.modifier.lifetime
//                            ^^ variable.other
//                                ^^^ keyword.other
//                                    ^ keyword.operator
//                                     ^^ storage.modifier.lifetime
//                                        ^^ variable.other
//                                           ^ meta.macro meta.group meta.block meta.impl meta.block punctuation.definition.block.begin
            type Output = <$t as $imp<$u>>::Output;
//                        ^^^^^^^^^^^^^^^^ meta.generic
//                                        ^^ meta.path

            #[inline]
//          ^^^^^^^^^ comment.block.attribute
            fn $method(self, other: &'a $u) -> <$t as $imp<$u>>::Output {
//          ^^ storage.type.function
//             ^^^^^^^ variable.other
//                     ^^^^ variable.language
//                                  ^ keyword.operator
//                                   ^^ storage.modifier.lifetime
//                                      ^^ variable.other
//                                          ^^ punctuation.separator
//                                             ^^^^^^^^^^^^^^^^ meta.generic
//                                                             ^^ meta.path
//                                                                      ^ meta.macro meta.group meta.block meta.impl meta.block meta.block punctuation.definition.block.begin
                $imp::$method(*self, *other)
//              ^^^^ variable.other
//                    ^^^^^^^ variable.other
//                            ^ keyword.operator
//                             ^^^^ variable.language
//                                   ^ keyword.operator
            }
        }
    }
]

macro_rules! alternate_group (
//                           ^ meta.macro meta.group punctuation.definition.group.begin
    ($a:expr) => (
//   ^^ variable.parameter
//      ^^^^ storage.type
        println!("Test {}!", $a)
    )
)

macro_rules! kleene_star {
    ($($arg:tt)+) => (
//   ^ meta.macro meta.block meta.group keyword.operator
//    ^ meta.macro meta.block meta.group punctuation.definition.group.begin
//     ^^^^ meta.macro meta.block meta.group variable.other
//         ^^^^^ meta.macro meta.block meta.group
//              ^ meta.macro meta.block meta.group punctuation.definition.group.end
//                ^ meta.macro meta.block keyword.operator
        println!($($arg));
    ),
    ($($arg:tt)*) => (
//     ^^^^ meta.macro meta.block meta.group variable.other
//         ^^^^^ meta.macro meta.block meta.group
//              ^ meta.macro meta.block meta.group punctuation.definition.group.end
//                ^ meta.macro meta.block keyword.operator
        println!($($arg)*);
    ),
    ($($arg:tt);+) => (
//     ^^^^ meta.macro meta.block meta.group variable.other
//         ^^^^^^ meta.macro meta.block meta.group
//               ^ meta.macro meta.block meta.group punctuation.definition.group.end
//                 ^ meta.macro meta.block keyword.operator
        println!($($arg));
    ),
    ($($arg:tt),*) => (
//     ^^^^ meta.macro meta.block meta.group variable.other
//         ^^^^^^ meta.macro meta.block meta.group
//               ^ meta.macro meta.block meta.group punctuation.definition.group.end
//                 ^ meta.macro meta.block keyword.operator
        println!($($arg)*);
    )
}
