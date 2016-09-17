// SYNTAX TEST "Packages/Scala/Scala.sublime-syntax"
// <- source.scala comment.line.double-slash.scala

package fubar
// ^^^^ keyword.control
//      ^^^^^ entity.name.namespace.header.scala

package fubar {
// ^^^^ keyword.control.scala
//      ^^^^^ entity.name.namespace.scoped.scala
// <- meta.namespace.scala
}

import fubar.{Unit, Foo}
// ^^^ keyword.other.import
// <- meta.import.scala
//     ^^^^^ variable.package.scala
//            ^^^^ variable.import.scala

def foo: Baz = 42
//^ storage.type.function.scala
//  ^^^ entity.name.function.scala
//       ^^^ support.class
//           ^ keyword.operator.assignment.scala
//             ^^ constant.numeric.integer.scala

def foo: Baz => Bar = 42
//       ^^^ support.class
//              ^^^ support.class
//                  ^ keyword.operator.assignment.scala


def foo(a: Int, b: Bar): Baz = 42
//^ storage.type.function.scala
//  ^^^ entity.name.function.scala
//      ^ variable.parameter
//         ^^^ storage.type.primitive.scala
//                 ^^^ support.class
//                       ^^^ support.class
//                           ^ keyword.operator.assignment.scala
//                             ^^ constant.numeric.integer.scala

   def +(a: Int)
// ^^^ storage.type.function.scala
//     ^ entity.name.function.scala

   def `this is a test`(a: Int)
// ^^^ storage.type.function.scala
//     ^^^^^^^^^^^^^^^^ entity.name.function.scala

   def ::(a: Int)
// ^^^ storage.type.function.scala
//     ^^ entity.name.function.scala

   def foo_+(a: Int)
// ^^^ storage.type.function.scala
//     ^^^^^ entity.name.function.scala

   def foo_2(a: Int)
// ^^^ storage.type.function.scala
//     ^^^^^ entity.name.function.scala

   def foo42_+(a: Int)
// ^^^ storage.type.function.scala
//     ^^^^^^^ entity.name.function.scala

   def __many_underscores__+(a: Int)
// ^^^ storage.type.function.scala
//     ^^^^^^^^^^^^^^^^^^^^^ entity.name.function.scala

   def foo42_+_abc(a: Int)
// ^^^ storage.type.function.scala
//     ^^^^^^^ entity.name.function.scala
//            ^^^^ - entity.name.function

   def +_foo()
//      ^^^^ - entity.name.function

   def foo[A]
// ^^^ storage.type.function.scala
//     ^^^ entity.name.function.scala
//         ^ support.class

   def foo(implicit bar: Int): Unit
//         ^^^^^^^^ storage.modifier.other

   val foo: Unit
// ^^^ storage.type.stable.scala
//     ^^^ entity.name.val
//          ^^^^ storage.type.primitive.scala

   var foo: Unit
// ^^^ storage.type.volatile.scala
//     ^^^ entity.name.var
//          ^^^^ storage.type.primitive.scala

class Foo[A](a: Bar) extends Baz with Bin
// ^^ storage.type.class.scala
//    ^^^ entity.name.class
//        ^ support.class
//           ^ variable.parameter
//              ^^^ support.class
//                   ^^^^^^^ keyword.declaration.scala
//                           ^^^ entity.other.inherited-class.scala
//                               ^^^^ keyword.declaration.scala
//                                    ^^^ entity.other.inherited-class.scala

   class Foo private[this] (a: Int)(b: String)
//           ^^^^^^^ storage.modifier.access
//                   ^^^^ variable.language.scala
//                          ^ variable.parameter
//                                  ^ variable.parameter

class Foo(x: Int = 42)
//               ^ - support
//               ^ keyword.operator.assignment.scala
//                 ^^ constant.numeric

def foo(x: Int = 42)
//             ^ - support
//             ^ keyword.operator.assignment.scala
//               ^^ constant.numeric

trait Foo
// ^^ storage.type.class.scala
//    ^^^ entity.name.class

object Foo
// ^^^ storage.type.class.scala
//     ^^^ entity.name.class

   type Foo = Bar
// ^^^^ storage.type.scala
//      ^^^ entity.name.type.scala
//          ^ keyword.operator.assignment.scala
//            ^^^ support.class.scala

   type Foo = Bar => Baz
// ^^^^ storage.type.scala
//      ^^^ entity.name.type.scala
//          ^ keyword.operator.assignment.scala
//            ^^^ support.class.scala
//                   ^^^ support.class.scala


  type Foo[A, B, C] = Bar
//         ^ support.class
//            ^ support.class
//               ^ support.class
//                  ^ keyword.operator.assignment.scala

type Foo = Bar {
  def baz: Int
//    ^^^ entity.name.function
}

type Foo = Bar[A] forSome { type A }
//                ^^^^^^^ keyword.declaration.scala

   type Foo
   Bar
// ^^^ support.constant

   42
// ^^ constant.numeric.integer.scala

   .421
// ^^^^ constant.numeric.float.scala

   42D
// ^^^ constant.numeric.float.scala

   42d
// ^^^ constant.numeric.float.scala

   42F
// ^^^ constant.numeric.float.scala

   42f
// ^^^ constant.numeric.float.scala

   42L
// ^^^ constant.numeric.integer.scala

   42l
// ^^^ constant.numeric.integer.scala

   0x0aF9123
// ^^^^^^^^^ constant.numeric.hex.scala

   0.045e-2
// ^^^^^^^^ constant.numeric.float.scala

   'a'
// ^^^ constant.character.literal.scala

   '\u1221'
// ^^^^^^^^ constant.character.literal.scala

   true
// ^^^^ constant.language.scala

   false
// ^^^^^ constant.language.scala

   null
// ^^^^ constant.language.scala

   Nil
// ^^^ support.constant.scala

   None
// ^^^^ support.constant.scala

   this
// ^^^^ variable.language.scala

   super
// ^^^^^ variable.language.scala

   "testing"
// ^ punctuation.definition.string.begin.scala
// ^^^^^^^^^ string.quoted.double.scala
//         ^ punctuation.definition.string.end.scala

  "escaped chars: \u1221 \125 \n"
//                ^^^^^^ constant.character.escape.scala
//                        ^^^ constant.character.escape.scala
//                            ^^ constant.character.escape.scala

  "bad escaping: \p"
//               ^ invalid.illegal.lone-escape.scala

  """escaped in triple: \u1221 \125 \n"""
//^^^ punctuation.definition.string.begin.scala
//                      ^^^^^^ constant.character.escape.scala
//                             ^^^ - constant.character.escape.scala
//                                  ^^ - constant.character.escape.scala
//                                    ^^^ punctuation.definition.string.end.scala

   """testing"""
// ^^^^^^^^^^^^^ string.quoted.triple.scala

   s"testing $a ${42}"
// ^^^^^^^^^ string.quoted.interpolated.scala
// ^ support.function
//           ^^ variable.other
//              ^^ punctuation.definition.expression
//                ^^ constant.numeric.integer.scala
//                  ^ punctuation.definition.expression

   s"""testing $a ${42}"""
// ^^^^^^^^^^^ string.quoted.triple.interpolated.scala
// ^ support.function
//             ^^ variable.other
//                ^^ punctuation.definition.expression
//                  ^^ constant.numeric.integer.scala
//                    ^ punctuation.definition.expression
//                     ^^^ string.quoted.triple.interpolated.scala

   f"formatted: x: $x%+,.3f ca"
// ^ support.function
//                  ^ variable.other.scala
//                   ^^^^^^ constant.other.formatting.scala

   f"formatted: date: $x%T "
// ^ support.function
//                    ^ variable.other.scala
//                      ^^ constant.other.formatting.scala

   Unit
// ^^^^ storage.type.primitive.scala

   Byte
// ^^^^ storage.type.primitive.scala

   Short
// ^^^^^ storage.type.primitive.scala

   Int
// ^^^ storage.type.primitive.scala

   Long
// ^^^^ storage.type.primitive.scala

   Float
// ^^^^^ storage.type.primitive.scala

   Double
// ^^^^^^ storage.type.primitive.scala

   Boolean
// ^^^^^^^ storage.type.primitive.scala

   String
// ^^^^^^ support.constant

   // this is a comment
// ^^^^^^^^^^^^^^^^^^^^ comment.line.double-slash.scala

/*
// <- comment.block.scala
*/

/**
// <- comment.block.documentation.scala
*/

   if
// ^^ keyword.control.flow.scala

   else
// ^^^^ keyword.control.flow.scala

   do
// ^^ keyword.control.flow.scala

   while
// ^^^^^ keyword.control.flow.scala

   for
// ^^^ keyword.control.flow.scala

   yield
// ^^^^^ keyword.control.flow.scala

   match
// ^^^^^ keyword.control.flow.scala

   case =>
// ^^^^ keyword.other.declaration.scala

   macro
// ^^^^^ keyword.other.scala

   type
// ^^^^ storage.type.scala

   return
// ^^^^^^ keyword.control.flow.jump.scala

   throw
// ^^^^^ keyword.control.flow.jump.scala

   catch
// ^^^^^ keyword.control.exception.scala

   finally
// ^^^^^^^ keyword.control.exception.scala

   ???
// ^^^ keyword.other.scala

   try
// ^^^ keyword.control.exception.scala

   forSome
// ^^^^^^^ keyword.declaration.scala

   new
// ^^^ keyword.other.scala

   extends
// ^^^^^^^ keyword.declaration.scala

   with
// ^^^^ keyword.declaration.scala

   class
// ^^^^^ storage.type.class.scala

   trait
// ^^^^^ storage.type.class.scala

   object
// ^^^^^^ storage.type.class.scala

   def
// ^^^ storage.type.function.scala

   val
// ^^^ storage.type.stable.scala

   var
// ^^^ storage.type.volatile.scala

   import
// ^^^^^^ keyword.other.import.scala

   package
// ^^^^^^^ keyword.control.scala

   private
// ^^^^^^^ storage.modifier.access

   protected
// ^^^^^^^^^ storage.modifier.access

   abstract
// ^^^^^^^^ storage.modifier.other

   final
// ^^^^^ storage.modifier.other

   lazy
// ^^^^ storage.modifier.other

   sealed
// ^^^^^^ storage.modifier.other

   implicit
// ^^^^^^^^ storage.modifier.other

   override
// ^^^^^^^^ storage.modifier.other

   val t: ({ type λ[α] = Foo[α] })#λ
//        ^^^^^^^^^^^^^^ comment.block.scala
//                       ^^^ support.class
//                           ^ comment.block.empty.scala
//                              ^^^^ comment.block.scala

   val t: ({ type λ[α, β] = Foo[α, β] })#λ
//        ^^^^^^^^^^^^^^^^^ comment.block.scala
//                          ^^^ support.class
//                              ^ comment.block.empty.scala
//                                 ^ comment.block.empty.scala
//                                    ^^^^ comment.block.scala

   a :: b :: Nil
// ^^^^^^^^^ source.scala
//           ^^^ support.constant.scala

  (a :: b :: Nil)
// ^^^^^^^^^ source.scala
//           ^^^ support.constant.scala

   a: Int
// ^^ source.scala
//    ^^^ storage.type.primitive.scala

  (a: Int)
// ^ source.scala
//    ^^^ storage.type.primitive.scala

   a: Foo
//    ^^^ support.class

   case (abc: Foo, cba @ _) =>
// ^^^^ keyword.other.declaration.scala
//       ^^^ variable.parameter
//            ^^^ support.class
//                 ^^^ variable.parameter
//                       ^ variable.language.underscore.scala
//                          ^^ keyword.operator.arrow.scala

   case abc @ `abc` =>
//      ^^^ variable.parameter
//          ^ keyword.operator.at.scala
//            ^ punctuation.definition.identifier.scala
//                ^ punctuation.definition.identifier.scala
//                  ^^ keyword.operator.arrow.scala
//            ^^^^^ - variable.parameter

   case foo: (Int => Boolean) :: _ =>
//                               ^ variable.language.underscore.scala

   case /* testing */ =>
//      ^^^^^^^^^^^^^ comment.block.scala

   case // testing
//      ^^^^^^^^^^ comment.line.double-slash.scala
   =>

   case 42 =>
//      ^^ constant.numeric.integer.scala

   case 'a' =>
//      ^^^ constant.character.literal.scala

   case 'foo =>
//      ^^^^ constant.other.symbol

   case "foo" =>
//      ^^^^^ string.quoted.double.scala

   case """foo""" =>
//      ^^^^^^^^^ string.quoted.triple.scala

   case q"""..$foo""" =>
//      ^^^^^^ string.quoted.triple.interpolated.scala
//            ^^^^ variable.other
//                ^^^ string.quoted.triple.interpolated.scala

   case <foo/> =>
//      ^^^^^^ text.xml
//       ^^^ entity.name.tag

   case true =>
//      ^^^ constant.language.scala

   case _ ⇒ _
//          ^ - keyword

   case _ if stuff =>
//        ^^ keyword.control.flow.scala
//           ^^^^^ - entity.name

   val abc @ `abc`
// ^^^ storage.type.stable.scala
//     ^^^ entity.name.val
//         ^ keyword.operator.at.scala
//           ^ punctuation.definition.identifier.scala
//               ^ punctuation.definition.identifier.scala
//           ^^^^^ - entity.name

   _
// ^ - keyword

   val ble @ `abc` = _
// ^^^ storage.type.stable.scala
//     ^^^ entity.name.val
//         ^ keyword.operator.at.scala
//           ^^^^^ - entity.name
//                 ^ keyword.operator.assignment.scala
//                   ^ - keyword

   case object Thingy extends Other
// ^^^^ storage.type.class.scala
//      ^^^^^^ storage.type.class.scala
//             ^^^^^^ entity.name.class.scala
//                    ^^^^^^^ keyword.declaration.scala
//                            ^^^^^ entity.other.inherited-class.scala

   case object Thingy extends (Foo => Bar)
// ^^^^ storage.type.class.scala
//      ^^^^^^ storage.type.class.scala
//             ^^^^^^ entity.name.class.scala
//                    ^^^^^^^ keyword.declaration.scala
//                             ^^^ support.class

   case class
// ^^^^ keyword.other.declaration.scala
//      ^^^^^ storage.type.class.scala

=>     // this is here to act as a random terminator to the above partial syntax

   case class Thingy(abc: Int) extends Other
// ^^^^ storage.type.class.scala
//      ^^^^^ storage.type.class.scala
//            ^^^^^^ entity.name.class.scala
//                   ^^^ variable.parameter
//                             ^^^^^^^ keyword.declaration.scala
//                                     ^^^^^ entity.other.inherited-class.scala
//

   for {
// ^^^ keyword.control.flow.scala

     a <- _
//   ^ variable.parameter
//     ^^ keyword.operator.assignment.scala
//        ^ - keyword

     a ← _
//   ^ variable.parameter
//     ^ keyword.operator.assignment.scala
//       ^ - entity.name

     (b, c @ _) <- _
//    ^ variable.parameter
//       ^ variable.parameter
//         ^ keyword.operator.at.scala
//           ^ variable.language.underscore.scala
//              ^^ keyword.operator.assignment.scala
//                 ^ - keyword
       _
//     ^ - entity.name

     testing = _
//   ^^^^^^^ variable.parameter
//           ^ keyword.operator.assignment.scala
//             ^ - variable.operator.scala

     testing = {
//   ^^^^^^^ variable.parameter
       testing = false
//     ^^^^^^^ - entity.name
//             ^ keyword.operator.assignment.scala
     }

     testing = (
//   ^^^^^^^ variable.parameter
       testing = false
//     ^^^^^^^ - variable.parameter
     )

     val testing = 42
//   ^^^ storage.type.stable.scala
//       ^^^^^^^ variable.parameter
   } abc
//   ^^^ - variable.parameter

   for (a <- _; (b, c @ _) ← d; val abc = e) f
// ^^^ keyword.control.flow.scala
//      ^ variable.parameter
//           ^ - keyword
//               ^ variable.parameter
//                  ^ variable.parameter
//                    ^ keyword.operator.at.scala
//                      ^ variable.language.underscore.scala
//                         ^ keyword.operator.assignment.scala
//                           ^ - variable.parameter
//                              ^^^ storage.type.stable.scala
//                                  ^^^ variable.parameter
//                                        ^ - variable.parameter
//                                           ^ - variable.parameter

   for {
     sss <- { {} }
//   ^^^ variable.parameter
     qqq <- stuff
//   ^^^ variable.parameter
   }

   for {
     back <- Traverse[Option]
//   ^^^^ variable.parameter
//           ^^^^^^^^ support.constant
//                    ^^^^^^ support.class
       .traverse[Free, Stuff](res) { r => }
//      ^^^^^^^^ - entity.name
//                            ^^^ - entity.name
//                                   ^ - entity.name
   }


  val baseSettings: Seq[Def.Setting[_]] = _
//    ^^^^^^^^^^^^ entity.name.val.scala
//                  ^^^ support.class
//                                  ^ - keyword

  for {
    r <- blah
  } yield r.copy(foo = a)
//        ^ - entity.name
//          ^^^^ - entity.name
//               ^^^ - entity.name

  {
    case foo.Bar => 42
//       ^^^ - variable
//          ^ punctuation.accessor.scala
    case Bar.foo => 42
//           ^^^ - variable
//          ^ punctuation.accessor.scala
  }

   val Foo = 42
//     ^^^ entity.name.val

   val (Foo, x) = 42
//      ^^^ support.constant.scala
//           ^ entity.name.val

{
  Set[Foo[A, A] forSome { type A }, A]
//                                  ^ support.class
}
    def foo: Int
//      ^^^ entity.name.function

// fubar
// <- source.scala comment.line.double-slash

new Foo
//  ^^^ support.class.scala

new (Foo ~> Bar)
//   ^^^ support.class.scala
//       ^^ support.type.scala
//          ^^^ support.class.scala

  class Foo(val bar: Baz) extends AnyVal
//          ^^^ storage.type.scala
//                        ^^^^^^^ keyword.declaration.scala
//                                ^^^^^^ entity.other.inherited-class.scala

  class Foo(implicit bar: Baz) extends AnyVal
//          ^^^^^^^^ storage.modifier.other
//                             ^^^^^^^ keyword.declaration.scala
//                                     ^^^^^^ entity.other.inherited-class.scala

   val Stuff(f1, v1) = ???
//     ^^^^^ support.constant.scala

new Foo(new Foo)
//      ^^^ keyword.other.scala

new Foo.Bar.Baz
//     ^ punctuation.accessor.scala
//      ^^^ support.class.scala
//         ^ punctuation.accessor.scala
//          ^^^ support.class.scala

new Foo#Bar#Baz
//     ^ punctuation.accessor.scala
//      ^^^ support.class.scala
//         ^ punctuation.accessor.scala
//          ^^^ support.class.scala

type Foo = Foo.Bar
//            ^ punctuation.accessor.scala

type Foo = Foo#Bar
//            ^ punctuation.accessor.scala

val x: OptionT[({ type λ[α] = Foo[α, Int] })#λ, String] = ???
//             ^^^^^^^^^^^^^^ comment.block
//                                ^ comment.block.empty
//                                        ^^^^ comment.block

class Foo[+A]
//        ^ keyword.operator

class Foo[-A]
//        ^ keyword.operator

class Foo[A <: Int]
//          ^^ keyword.operator

class Foo[A >: Int]
//          ^^ keyword.operator

class Foo[A <% Int]
//          ^^ keyword.operator

class Foo[A: Int]
//         ^ keyword.operator

type Foo <: Bar
//       ^^ keyword.operator
//          ^^^ support.class

type Foo >: Bar
//       ^^ keyword.operator
//          ^^^ support.class

   { a => ??? }
//   ^ variable.parameter

   { (a, b) => ??? }
//    ^ variable.parameter
//       ^ variable.parameter

   { a: Int => ??? }
//   ^ variable.parameter
//      ^^^ storage.type.primitive.scala

   { (a: Int, b: Int) => ??? }
//    ^ variable.parameter
//       ^^^ storage.type.primitive.scala
//            ^ variable.parameter
//               ^^^ storage.type.primitive.scala

   (a) => ???
//  ^ variable.parameter

   (a, b) => ???
//  ^ variable.parameter
//     ^ variable.parameter

   (a: Int) => ???
//  ^ variable.parameter
//     ^^^ storage.type.primitive.scala

   (a: Int, b: Int) => ???
//  ^ variable.parameter
//     ^^^ storage.type.primitive.scala
//          ^ variable.parameter
//             ^^^ storage.type.primitive.scala

   (a: Int, b: Int) ⇒ ???
//  ^ variable.parameter
//     ^^^ storage.type.primitive.scala
//          ^ variable.parameter
//             ^^^ storage.type.primitive.scala
//                  ^ storage.type.function.arrow

   a => ???
// ^ variable.parameter

   a: Int => ???
// ^ variable.parameter
//    ^^^ storage.type.primitive.scala

   case _ if thing =>
// ^^^^ keyword.other.declaration.scala
//           ^^^^^ - variable.parameter
//                 ^^ - storage.type.function.arrow

   a =>a
// ^ variable.parameter

   a =>42
// ^ variable.parameter
//     ^^ constant.numeric.integer.scala

  (a: Int => Boolean) => 42
//        ^^ keyword.operator.arrow.scala
//           ^^^^^^^ storage.type

  (a: Foo[A] forSome { type A }) => 42
// ^ variable.parameter
//    ^^^ support.class
//           ^^^^^^^ keyword.declaration.scala

   ()
// ^^ constant.language.scala

   Foo()
// ^^^ support.constant.scala
//    ^^ - constant.language.scala

  Foo[A]()
//      ^^ - constant.language.scala

  foo[A]()
//      ^^ - constant.language.scala

foo() bar ()
// ^^ - constant.language.scala
//        ^^ constant.language.scala

foo()()
//   ^^ - constant.language.scala

foo(())()
//  ^^ constant.language.scala
//     ^^ - constant.language.scala

   () => 42
// ^^ - constant.language.scala
//    ^^ storage.type.function.arrow

"testing /*comments*/"
//       ^^^^^^^^^^^^ string.quoted.double
//       ^^^^^^^^^^^^ - comment

   cb: ((Throwable \/ Unit) => Unit) => 42
// ^^ variable.parameter
//                 ^^ support.type.scala
//                                   ^^ storage.type.function.arrow

def foo(a: A <:< B)
//           ^^^ support.type.scala

def foo(a: A >:> B)
//           ^^^ support.type.scala

def foo(a: A =:= B)
//           ^^^ support.type.scala

def foo(a: A =:= B = null)
//                 ^ keyword.operator.assignment.scala
//                   ^^^^ constant.language.scala

def foo(a: A :: B)
//           ^^ support.type.scala

class Foo(a: A <:< B)
//             ^^^ support.type.scala

class Foo(a: A >:> B)
//             ^^^ support.type.scala

class Foo(a: A =:= B)
//             ^^^ support.type.scala

class Foo(a: A =:= B = null)
//                   ^ keyword.operator.assignment.scala
//                     ^^^^ constant.language.scala

class Foo(a: A :: B)
//             ^^ support.type.scala

import foo
// <- meta.import.scala
//     ^^^ variable.import.scala

import foo; import bar
//     ^^^ variable.import.scala
//          ^^^^^^ keyword.other.import.scala
//                 ^^^ variable.import.scala

import foo.bar
//     ^^^^^^^ variable.package.scala

import foo.{bar, bar => baz, bar=>baz}
//         ^^^^^^^^^^^^^^^^^ meta.import.selector.scala
//          ^^^ variable.import.scala
//               ^^^ variable.import.renamed-from.scala
//                   ^^ keyword.operator.arrow.scala
//                      ^^^ variable.import.renamed-to.scala
//                           ^^^ variable.import.renamed-from.scala
//                              ^^ keyword.operator.arrow.scala
//                                ^^^ variable.import.renamed-to.scala


import foo.{
   bar => bin
// ^^^ variable.import.renamed-from.scala
//        ^^^ variable.import.renamed-to.scala
}

import foo._
//         ^ variable.language.underscore.scala

import foo.{Foo => _}
//                 ^ variable.language.underscore.scala

for {} yield ()
//     ^^^^^ keyword.control.flow.scala
//           ^^ constant.language.scala

   42.bar
//   ^ - constant.numeric.scala

  baz[m.type]
//      ^^^^ keyword.other.scala

foo: m.type
//     ^^^^ keyword.other.scala

   ==
// ^^ - keyword

offset >= 0
//     ^^ - keyword

{
  case chunk #: h =>
//              ^ variable.parameter
}

val chunk #: h = ???
//           ^ entity.name.val

for {
  if things >= stuff
//   ^^^^^^ - variable.parameter
//             ^^^^^ - variable.parameter
}


for (if things >= stuff)
//      ^^^^^^ - variable.parameter
//                ^^^^^ - variable.parameter

   _
// ^ variable.language.underscore.scala

foo._1
//  ^ - variable.language.scala

foo_=
//  ^ - keyword

foo_
// ^ - variable.language

foo({ _ => () })
//    ^ variable.language.underscore.scala
//      ^^ storage.type.function.arrow

foo({ _: Unit => () })
//    ^ variable.language.underscore.scala
//            ^^ storage.type.function.arrow

  stuff: _*
//       ^^ keyword.operator.varargs.scala

  case _ @ _* =>
//         ^^ keyword.operator.varargs.scala

  val _ @ _* = things
//        ^^ keyword.operator.varargs.scala

s"testing ${things} and more!"
//          ^^^^^^ - string
//          ^^^^^^ source.scala.embedded

foo.bar
// ^ punctuation.accessor.scala

(foo, bar)
//  ^ punctuation.separator.scala

case (foo, bar) =>
//       ^ punctuation.separator.scala

val (foo, bar) = ???
//      ^ punctuation.separator.scala

foo eq bar
//  ^^ keyword.operator.word.scala

new Config()
//        ^^ - constant

val A: Foo = stuff
//  ^ entity.name.val.scala

type Maybe[A] = { type Inner = A; def x: Int }
//                                ^^ storage.type.function.scala
//                                    ^ entity.name.function.scala

   for {
// ^^^ keyword.control.flow.scala
      stuff = sequenceU.map(_.flatten) // thingy
     _ <- fooinConns.map(_.map(t => { }))
//     ^^ keyword.operator.assignment.scala
   } yield ()

new Foo with Bar with Baz
//      ^^^^ keyword.declaration.scala
//           ^^^ support.class.scala
//               ^^^^ keyword.declaration.scala
//                    ^^^ support.class.scala

type Thing = Foo with Bar with Baz
//               ^^^^ keyword.declaration.scala
//                    ^^^ support.class.scala

Foo[Foo with Bar]
//      ^^^^ keyword.declaration.scala
//           ^^^ support.class.scala

xs: Foo with Bar
//      ^^^^ keyword.declaration.scala
//           ^^^ support.class.scala

   classTag[U]
// ^^^^^ - storage.type

   s"before ${classTag[U] stuff} after"
//   ^^^^^^ string.quoted.interpolated.scala
//            ^^^^^ - storage.type
//                               ^^^^^ string.quoted.interpolated.scala

{
  case Stuff(thing, other) =>
//           ^^^^^ variable.parameter.scala
//                  ^^^^^ variable.parameter.scala
}

{
   case (x, y: Int => String) => ()
//                 ^^ keyword.operator.arrow.scala
//                    ^^^^^^ support.class.scala
}

{
   case (foo.bar, _) => ()
//           ^^^ - variable
}

val Stuff(thing, other) = ???
//        ^^^^^ entity.name.val.scala
//               ^^^^^ entity.name.val.scala

   x: List[Int] => ()
// ^ variable.parameter.scala
//              ^^ storage.type.function.arrow.scala

/** private */ class Foo
//             ^^^^^ storage.type.class

   foo
// ^^^ - comment

   /*
   /*
   test
// ^^^^ comment.block.scala
   */
   test
// ^^^^ comment.block.scala
   */
   test
// ^^^^ - comment

   /**
   /**
   test
// ^^^^ comment.block.documentation.scala
   */
   test
// ^^^^ comment.block.documentation.scala
   */
   test
// ^^^^ - comment

def <(a: Int) = 42
//  ^ entity.name.function.scala
//    ^ variable.parameter.scala

   <thing foo="42"/>
//  ^^^^^ text.xml entity.name.tag.xml
//        ^^^ text.xml entity.other.attribute-name.localname.xml
//            ^ text.xml string.quoted.double.xml punctuation.definition.string.begin.xml
//             ^^ text.xml string.quoted.double.xml
//               ^ text.xml string.quoted.double.xml punctuation.definition.string.end.xml

   <!-- not a comment -->
// ^^^^^^^^^^^^^^^^^^^^^^ - comment

   <foo bar="test" baz='test' bin={ 42 }>
// ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ text.xml meta.tag.xml
//                                      ^ text.xml meta.tag.xml
//                     ^ text.xml string.quoted.single.xml punctuation.definition.string.begin.xml
//                          ^ text.xml string.quoted.single.xml punctuation.definition.string.end.xml
//                                  ^^ source.scala constant.numeric.integer.scala
     {
       42 + "thing"
//     ^^^^^^^^^^^^ - text.xml
//     ^^ source.scala constant.numeric.integer.scala
//          ^^^^^^^ source.scala string.quoted.double.scala
       // comments!
//     ^^^^^^^^^^^^ source.scala comment.line.double-slash.scala

       <nested/>
//     ^^^^^^^^^ - text.xml text.xml
//     ^^^^^^^^^ text.xml meta.tag.xml
//      ^^^^^^ entity.name.tag.xml
     }

     "stuff"
//   ^^^^^^^ - string

     <!-- comments -->
//   ^^^^^^^^^^^^^^^^^ comment.block.xml

     <thing/>

     <more>
       more tags!
       /* not a comment */
//     ^^^^^^^^^^^^^^^^^^^ - comment
     </more>
   </foo>

   </thing>
// invalid.illegal.bad-closing-tag.xml

   <?xml version="1.0"?>
// ^^^^^^^^^^^^^^^^^^^^^ invalid.illegal.reserved-proc-instr.xml

   <?xml
// ^^^^^ invalid.illegal.reserved-proc-instr.xml

   <?xmll?>
// ^^^^^^^^ - invalid

   <?foo thing="false"?>
// ^^ punctuation.definition.tag.begin.xml
//   ^^^ entity.name.tag.xml
//             ^^^^^^^ string.quoted.double.xml
//                    ^^ punctuation.definition.tag.end.xml

   <!-- not a comment -->
// ^^^^^^^^^^^^^^^^^^^^^^ - comment

   <foo a="&" b="<" c=">"/>
// ^^^^^^^^^^^^^^^^^^^^^^^^ text.xml meta.tag.xml
// ^ punctuation.definition.tag.begin.xml
//         ^ invalid.illegal.bad-ampersand.xml
//               ^ invalid.illegal.missing-entity.xml
//                     ^ invalid.illegal.missing-entity.xml
//                       ^^ punctuation.definition.tag.end.xml

   <foo a="&amp;"/>
// ^^^^^^^^^^^^^^^^ text.xml meta.tag.xml
// ^ punctuation.definition.tag.begin.xml
//          ^^^ constant.character.entity.xml

   <foo>
// ^ punctuation.definition.tag.begin.xml
     &amp;
//   ^^^^^ constant.character.entity.xml - meta.tag.xml
   </foo>
