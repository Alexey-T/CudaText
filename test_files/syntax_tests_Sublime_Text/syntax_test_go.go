// SYNTAX TEST "Packages/Go/Go.sublime-syntax"
package examples

import (
// <- meta.import keyword.control.import
//^^^^^^ meta.import
//     ^ meta.group punctuation.definition.group
	"fmt"
	//^^^ meta.import meta.group
	// <- string.quoted.double
	//^^^ string.quoted.double
)
// <- meta.import meta.group punctuation.definition.group

import "strings"
// <- meta.import keyword.control.import
//^^^^^^^^^^^^^^ meta.import
//     ^^^^^^^^^ string.quoted.double

var valid int = 0
// <- meta.initialization.explicit storage.type
//  ^^^^^ meta.initialization.explicit variable.other

var var1, var2, var3
// <- meta.initialization.explicit storage.type
//  ^ variable.other
//      ^ punctuation.separator - variable.other
//        ^ variable.other
//            ^ punctuation.separator - variable.other
//              ^ variable.other

var1 := 1
// <- meta.initialization.short variable.other
//   ^^ keyword.operator.initialize

var1, var2 := imported.Vals
// <- meta.initialization.short variable.other
//^^^^^^^^^^^ meta.initialization.short
//  ^ punctuation.separator - variable.other
//    ^ variable.other
//         ^^ keyword.operator.initialize

var (
// <- meta.initialization.multiple storage.type
//  ^ meta.group punctuation.definition.group
	//  var commented int = 0
	//  ^ comment.line

	variable = 1
	var2, var3 = func_call()
	var3 = string(123)
)
// <- meta.group punctuation.definition.group

func test(data MyStruct)
{
	data.fooBar
	//  ^ punctuation.accessor
	//   ^^^^^^ variable.other.member
}

const (
	graveAccentString = `highlights %s and %[1]s`
	//                              ^ constant.other.placeholder
	//                                      ^ constant.other.placeholder
	normalString = "highlights %q and %[1]s"
	//                          ^ constant.other.placeholder
	//                                 ^ constant.other.placeholder
	dynamicFieldWidths = "test string %[1]*.[2]*f %*.*f"
	//                                  ^ constant.other.placeholder
	//                                              ^ constant.other.placeholder
)

type myStruct struct {
//^^^^^^^^^^^^^^^^^^^^ meta.struct
// <- storage.type
//   ^^^^^^^^ entity.name.struct
//            ^ storage.type
//                   ^ meta.block punctuation.definition.block.begin
	Field1 string          `tag1:""`
	// <- variable.other.member
	//     ^ storage.type
	//                     ^ string.quoted
	Field2 []string        `tag1:""`
	// <- variable.other.member
	//     ^^ meta.brackets
	//     ^ punctuation.definition.brackets.begin
	//      ^ punctuation.definition.brackets.end
	//       ^ storage.type
	//                     ^ string.quoted
	Field3 interface{}     `tag1:""`
	// <- variable.other.member
	//     ^ storage.type
	//              ^^ meta.block
	//              ^ punctuation.definition.block.begin
	//               ^ punctuation.definition.block.end
	//                     ^ string.quoted
	field4 map[string]uint `tag1:""`
	// <- variable.other.member
	//     ^ storage.type
	//        ^^^^^^^^ meta.brackets
	//        ^ punctuation.definition.brackets.begin
	//         ^ storage.type
	//               ^ punctuation.definition.brackets.end
	//                ^ storage.type
	//                     ^ string.quoted
	field5 package.MyType  `tag1:""`
	// <- variable.other.member
	package.MyOtherType
	//^^^^^^ - variable.other.member
	//      ^ variable.other.member
	*LocalType
	// <- keyword.operator
	//^ variable.other.member
}
// <- meta.struct meta.block punctuation.definition.block.end

type FuncContainer struct {
	FirstFunc   func(arg string)
	// <- variable.other.member
	//          ^^^^^^^^^^^^^^^^ meta.function
	//          ^ storage.type
	//               ^^^ variable.parameter
	//                   ^ storage.type
	SecondFunc  func(arg interface{})
	// <- variable.other.member
	//          ^^^^^^^^^^^^^^^^ meta.function
	//          ^ storage.type
	//               ^^^ variable.parameter
	//                   ^ storage.type
	SeventhFunc func(arg string)
	// <- variable.other.member
	//          ^^^^^^^^^^^^^^^^ meta.function
	//          ^ storage.type
	//               ^^^ variable.parameter
	//                   ^ storage.type
}

type LocalType /* Comment */ map[int32]int64
//^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ meta.type
// <- storage.type
//   ^^^^^^^^^ entity.name.type
//             ^^^^^^^^^^^^^ comment.block
//                           ^ storage.type
//                              ^^^^^^^ meta.brackets
//                              ^ punctuation.definition.brackets.begin
//                                    ^ punctuation.definition.brackets.end

func myFunc (nonHighlightedPrimitiveArg string, foo bar) {
//^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ meta.function
//^^^^^^^^^ meta.function.declaration
//         ^ - meta.function.declaration
//          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ meta.function.parameters
//                                                      ^ - meta.function.parameters
// ^ storage.type
//     ^ entity.name.function
//            ^ variable.parameter
//                                      ^ storage.type - variable
//                                            ^ punctuation.separator - variable
//                                              ^ variable.parameter
//                                                  ^ - variable
//                                                       ^ meta.block punctuation.definition.block
	return "test string"
//  ^ meta.function meta.block keyword.control
//            ^ string.quoted.double
}

func myFunc3(param /* */ , param2 string, param3 package.MyType) {
//^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ meta.function
//   ^ entity.name.function
//           ^ variable.parameter
//                 ^^^^^ comment.block
//                       ^ punctuation.separator
//                         ^ variable.parameter
//                                ^ storage.type
}

func
// <- storage.type

func myFunc2(nonHighlightedPrimitiveArg string,/* */  foo, foobaz bar) (nonHighlightedPrimitiveReturn, anotherType) {
//^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ meta.function
//^^^^^^^^^^ meta.function.declaration
//          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ meta.function.parameters
//                                                                    ^ - meta.function.parameters
//                                                                    ^ - meta.function.return-type
//                                                                     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ meta.function.return-type
//                                                                                                                 ^ - meta.function.return-type
// ^ storage.type
//     ^ entity.name.function
//            ^ variable.parameter
//                                      ^ storage.type
//                                            ^ punctuation.separator - variable
//                                             ^^^^^ comment.block
//                                                    ^ variable.parameter
//                                                         ^ variable.parameter
//                                                               ^ - variable
//                                                                   ^ meta.group punctuation.definition.group.end
//                                                                     ^ meta.group punctuation.definition.group.begin
//                                                                                                   ^ punctuation.separator
	return "test string"
	// <- meta.function meta.block keyword.control
	//      ^ string.quoted.double
}

func (v *Type) myFunc(nonHighlightedPrimitiveArg /* Test, comments(!) */ [10]string, foo bar) (
//^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ meta.function
//^^^^^^^^^^^^^^^^^^^ meta.function.declaration
//                   ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ meta.function.parameters - meta.function.declaration
//                                                                                           ^ - meta.function.parameters
//                                                                                           ^ - meta.function.return-type
//                                                                                            ^ meta.function.return-type
// ^ storage.type
//    ^ variable.parameter.receiver
//               ^ entity.name.function
//                       ^ variable.parameter
	nonHighlightedPrimitiveReturn, anotherType) {
	// ^ meta.function.return-type
	//                           ^ punctuation.separator
	variable[5].foo()
	//      ^^^ meta.brackets
	//      ^ punctuation.definition.brackets.begin.go
	//       ^ constant.numeric
	//        ^ punctuation.definition.brackets.end.go
	//         ^^^^^^ meta.function-call.method
	//         ^ punctuation.accessor
	//             ^^ meta.group
	//             ^ punctuation.definition.group.begin
	//              ^ punctuation.definition.group.end
	return ""
	// <- meta.function meta.block keyword.control
}

func () {
// <- storage.type
//^^^^^^^ meta.function
//   ^^ meta.function.parameters meta.group
//   ^ punctuation.definition.group.begin
//    ^ punctuation.definition.group.end
//     ^ - meta.function.parameters
//     ^ - meta.function.return-type
}

type funcTypeExample func(param one) (myType)
//^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ meta.type
//   ^^^^^^^^^^^^^^^ entity.name.type
//                   ^^^^^^^^^^^^^^^^^^^^^^^^ meta.function
//                   ^^^^ meta.function.declaration.anonymous
//                       ^^^^^^^^^^^ meta.function.parameters
//                        ^ variable.parameter
//                              ^ - variable.parameter
//                                  ^ - meta.function.parameters
//                                  ^ - meta.function.return-type
//                                   ^^^^^^^^ meta.function.return-type meta.group
//                                   ^ punctuation.definition.group.begin
//                                          ^ punctuation.definition.group.end

func (t funcTypeExample) foobar() {}
//^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ meta.function
//^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ meta.function.declaration
// ^ storage.type
//    ^ variable.parameter.receiver
//                       ^ entity.name.function
//                             ^^ meta.function.parameters meta.group
//                                ^^ meta.block

func () {
	switch {
	// <- keyword.control
	//     ^ punctuation.definition.block.begin
	case byte, rune, string:
	// <- keyword.control
	//   ^^^^ storage.type
	//       ^ punctuation.separator
	//         ^^^^ storage.type
	//             ^ punctuation.separator
	//               ^^^^^^ storage.type
	//                     ^ punctuation.separator
	case 1, 1.23, "str", 'c', true, false:
	// <- keyword.control
	//   ^ constant.numeric
	//    ^ punctuation.separator
	//      ^^^^ constant.numeric
	//          ^ punctuation.separator
	//            ^^^^^ string.quoted.double
	//                 ^ punctuation.separator
	//                   ^^^ string.quoted.single
	//                      ^ punctuation.separator
	//                        ^^^^ constant.language
	//                            ^ punctuation.separator
	//                              ^^^^^ constant.language
	//                                   ^ punctuation.separator
		fallthrough
		// <- keyword.control
	default:
	// <- keyword.control
	//     ^ punctuation.separator
	}
	// <- punctuation.definition.block.end

	Label:
	// <- entity.name.label
	//   ^ punctuation.separator
}
