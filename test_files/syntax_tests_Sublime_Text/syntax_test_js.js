// SYNTAX TEST "Packages/JavaScript/JavaScript.sublime-syntax"

import TheirClass from "./mypath";
// ^ keyword.control.import-export
//                  ^ keyword.control.import-export

import {identifier, otherIdentifier} from "somewhere";
// ^ keyword.control.import-export
//       ^ meta.import meta.block variable.other.readwrite

import thing, {identifier as otherIdentifier}, * as otherName from "otherplace";
// ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ meta.import
// ^ keyword.control.import-export
//      ^ variable.other.readwrite
//            ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ meta.block
//                ^ variable.other.readwrite
//                        ^ keyword.control.import-export
//                                     ^ variable.other.readwrite
//                                             ^ constant.other.js
//                                                             ^ keyword.control.import-export

import 'module'
// ^^^^^^^^^^^^ meta.import

export { name1, name2 as name3 };
//^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ meta.export
//^ keyword.control.import-export
//     ^^^^^^^^^^^^^^^^^^^^^^^^^ meta.block
//            ^ punctuation.separator.comma
//                    ^^ keyword.control.import-export

export let name1, name2;
//^^^^^^^^^^^^^^^^^^^^^ meta.export
//^ keyword.control.import-export
//     ^^^ storage.type
//              ^ punctuation.separator.comma

export var name3;
//^^^^^^^^^^^^^^ meta.export
//^ keyword.control.import-export
//     ^^^ storage.type

export const name1 = 5;
//^^^^^^^^^^^^^^^^^^^^ meta.export
//^ keyword.control.import-export
//     ^^^^^ storage.type
//                 ^ keyword.operator.assignment

export default expression;
//^^^^^^^^^^^^^^^^^^^^^^^ meta.export
//^ keyword.control.import-export
//     ^ keyword.control.import-export

export default function (a) { }
//^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ meta.export
//^ keyword.control.import-export
//     ^ keyword.control.import-export
//             ^ storage.type

export default function name1(b) { }
//^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ meta.export
//^ keyword.control.import-export
//     ^ keyword.control.import-export
//             ^ storage.type
//                      ^ entity.name.function

export { name1 as default };
//^^^^^^^^^^^^^^^^^^^^^^^^^ meta.export
//^ keyword.control.import-export
//     ^^^^^^^^^^^^^^^^^^^^ meta.block
//             ^ keyword.control.import-export
//                ^ keyword.control.import-export

export * from "./othermod";
//^^^^^^^^^^^^^^^^^^^^^^^^ meta.export
//^ keyword.control.import-export
//     ^ constant.other
//       ^ keyword.control.import-export

export { name1, name2 } from "./othermod";
//^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ meta.export
//^ keyword.control.import-export
//     ^^^^^^^^^^^^^^^^ meta.block
//                      ^ keyword.control.import-export

export { import1 as name1, import2 as name2, nameN } from "./othermod";
//^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ meta.export
//^ keyword.control.import-export
//     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ meta.block
//               ^ keyword.control.import-export
//                                 ^ keyword.control.import-export
//                                                   ^ keyword.control.import-export

// This object literal is technically broken since foo() does not have a
// method body, but we include it here to ensure that highlighting is not
// broken as the user is typing
let a = { otherIdentifier, foo(), baz: 1 }
//      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ meta.object-literal
//        ^^^^^^^^^^^^^^^ variable.other.readwrite
//                         ^^^ entity.name.function
//                                ^^^ meta.object-literal.key

someFunction({
    prop1, prop2, prop3
//  ^ variable.other.readwrite
//         ^ variable.other.readwrite
//                ^ variable.other.readwrite
});

function foo() {
// <- meta.function.declaration
 // <- meta.function.declaration
  // <- meta.function.declaration
// ^^^^^^^^^^^ meta.function.declaration - meta.function.anonymous
// ^ storage.type.function
//        ^ entity.name.function
}
// <- meta.block

var bar = function() {
//  ^^^^^^^^^^^^^^^^ meta.function.declaration - meta.function.anonymous
// <- storage.type
//   ^ variable.other.readwrite entity.name.function
//         ^ storage.type.function
}

baz = function*()
// <- meta.function.declaration
 // <- meta.function.declaration
  // <- meta.function.declaration
// ^^^^^^^^^^^^^^ meta.function.declaration - meta.function.anonymous
// <- variable.other.readwrite entity.name.function
//     ^ storage.type.function
//            ^ keyword.generator.asterisk
{

}

if (true)
// <- keyword.control.conditional
{
    bar()
}

// This is a comment function() { }
// <- punctuation.definition.comment
// ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ comment.line.double-slash

/**
// <- comment.block.documentation punctuation.definition.comment
*/

/*
// <- comment.block punctuation.definition.comment
*/

<!-- -->
// <- comment.block.html punctuation.definition.comment

#! /usr/bin/env node
// <- comment.line.shebang punctuation.definition.comment

 #! /usr/bin/env node
//^^^^^^^^^^^^^^^^^^^ - comment.line.shebang

/*@cc_on
// <- punctuation.definition.comment
//^ punctuation.definition.keyword
//^^^^^^ comment.block.conditional keyword.control.conditional
if (@_jscript_version==5.6) {}
//  ^^^^^^^^^^^^^^^^^ variable.other.conditional
//  ^ punctuation.definition.variable
//                   ^^ keyword.operator.comparison
@*/

var str = '\':';
var str2 = NaN;
// <- storage.type
//   ^ variable.other.readwrite
//       ^ keyword.operator.assignment
//         ^^^ constant.language.nan

tag`Hello ${ a + b } world\nanother ${expression}.`;
// <- variable.function.tagged-template.js
// ^ punctuation.definition.string.template.begin.js
//   ^ string.template.js
//        ^ punctuation.definition.template-expression.begin.js
//           ^ variable.other.readwrite.js
//             ^ keyword.operator.arithmetic.js
//               ^ meta.template.expression.js source.js.embedded.expression
//                 ^ punctuation.definition.template-expression.end.js
//                        ^ constant.character.escape.js
//                                                ^ punctuation.definition.string.template.end.js

mylabel:
// ^ entity.name.label
//     ^ punctuation.separator

// Ensure a ternary identifier isn't recognized as a label
true ? str : str2
//      ^ - entity.name.label

a = test ? a + b : c;
//         ^ variable.other.readwrite
//             ^ variable.other.readwrite

{{foo}}
// ^ meta.tag.mustache.js
{{#bar}}{{/bar}}
// ^ meta.tag.mustache.js
//         ^ meta.tag.mustache.js

var obj = {
//        ^ meta.object-literal - meta.block
    key: bar,
    // <- meta.object-literal.key
    $key2: "string value",
    // ^ meta.object-literal.key
    //   ^ - constant.other
    //        ^ string.quoted.double
    $key3: 0,
    // <- meta.object-literal.key.dollar punctuation.dollar
     // <- meta.object-literal.key.dollar - punctuation.dollar
    $keyFunc: function() {
//  ^^^^^^^^^^^^^^^^^^^^ meta.function.declaration - meta.function.anonymous
    // <- meta.object-literal.key.dollar entity.name.function punctuation.dollar
     // <- meta.object-literal.key.dollar entity.name.function - punctuation.dollar
    }

    "key4": true,
    // <- meta.object-literal.key string.quoted.double
    //    ^ punctuation.separator.key-value - string
    'key5': false,
    // <- meta.object-literal.key string.quoted.single
    //    ^ punctuation.separator.key-value - string
    //      ^^^^^ constant.language.boolean.false

    funcKey: function() {
//  ^^^^^^^^^^^^^^^^^^^ meta.function.declaration - meta.function.anonymous
    // ^ meta.object-literal.key entity.name.function
    },

    func2Key: function func2Key() {
//  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ meta.function.declaration - meta.function.anonymous
    // ^ meta.object-literal.key entity.name.function
    },

    funcKeyArrow: () => {
//  ^^^^^^^^^^^^^^^^^^^ meta.function.declaration - meta.function.anonymous
    // ^ meta.object-literal.key entity.name.function
    },

    "funcStringKey": function funcStringKey()
//  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ meta.function.declaration - meta.function.anonymous
    // ^ meta.object-literal.key string.quoted.double entity.name.function
    { },

    'funcStringKey': function() {
//  ^^^^^^^^^^^^^^^^^^^^^^^^^^^ meta.function.declaration - meta.function.anonymous
    // ^ meta.object-literal.key string.quoted.single entity.name.function
    },

    'funcStringKeyArrow': () => {
//  ^^^^^^^^^^^^^^^^^^^^^^^^^^^ meta.function.declaration - meta.function.anonymous
    // ^ meta.object-literal.key string.quoted.single entity.name.function
    },

    "func\\String2KeyArrow": (foo) => {
//  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ meta.function.declaration - meta.function.anonymous
    // ^ meta.object-literal.key string.quoted.double entity.name.function
    //    ^ constant.character.escape
    },

    key: 'str' + (true ? 'true' : 'false'),
//                ^^^^ constant.language.boolean.true

    qux()
//  ^^^^^ meta.function.declaration - meta.function.anonymous
    // <- entity.name.function
    {}

    static foo(bar) {
//  ^^^^^^^^^^^^^^^ meta.function.declaration - meta.function.anonymous
    // ^ storage.type
    //      ^entity.name.function
    }

    *baz(){
//  ^^^^^^ meta.function.declaration - meta.function.anonymous
    // <- keyword.generator.asterisk
    // ^ entity.name.function
    }
}
// <- meta.object-literal - meta.block

var $ = function(baz) {
//  ^^^^^^^^^^^^^^^^^ meta.function.declaration - meta.function.anonymous
//  ^ variable.other.dollar.only punctuation.dollar entity.name.function
}

$()
// <- variable.other.dollar.only punctuation.dollar

$foo = null;
// <- variable.other.dollar punctuation.dollar
// ^ variable.other.dollar - punctuation.dollar
//     ^^^^ constant.language.null

baz = "";
// <- variable.other.readwrite
//     ^ string.quoted.double

var qux = 100;
// <- storage.type
//   ^ variable.other.readwrite
//         ^ constant.numeric

if (Infinity > qux) {
// ^^^^^^^^^^^^^^^ meta.conditional
//  ^^^^^^^^ constant.language.infinity
    a;
//  ^ meta.conditional meta.block
}
// <- meta.conditional meta.block

if (foo bar)
    baz = "test"

do {
// <- meta.do-while
// ^ meta.block
    qux += 1
//  ^^^^^^^^ meta.do-while meta.block
} while(qux < 20);
// <- meta.block
// ^^^^^^^^^^^^^^ meta.do-while - meta.block
// ^^^^ keyword.control.loop
//      ^^^^^^^^ meta.group

for (var i = 0; i < 10; i++) {
// ^^^^^^^^^^^^^^^^^^^^^^^^^^^ meta.for
//   ^^^^^^^^^^^^^^^^^^^^^^ meta.group
//                           ^ meta.block
    i += 1;
//  ^^^^^^^ meta.for meta.block
}
// <- meta.block

while (true)
// ^^^^^^^^^ meta.while
//     ^^^^ meta.group
{
// <- meta.block
    yield;
//  ^^^^^ keyword.control.flow
    break;
//  ^^^^^^ meta.while meta.block
}
// <- meta.block

with (undefined) {
// <- keyword.control.with
//^^^^^^^^^^ meta.with
//    ^^^^^^^^^ constant.language.undefined
    return;
}

switch ($foo) {
// ^^^^^^^^^^^^ meta.switch
//      ^^^^ meta.group
//            ^ meta.block
    case foo:
    // ^ meta.switch meta.block keyword.control.switch
    //      ^ - punctuation.separator.key-value
        qux = 1;
        break;
        // ^ keyword.control.loop
    case "baz":
    // ^ keyword.control.switch
    //        ^ - punctuation.separator.key-value string
        qux = 2;
        break;
        // ^ keyword.control.loop
    default:
    // ^ meta.switch meta.block keyword.control.switch
    //     ^ - punctuation.separator.key-value
        qux = 3;
}
// <- meta.block

try {
// <- meta.try keyword.control.trycatch
// ^^ meta.try
//  ^ meta.block
    foobar = qux.bar();
//  ^^^^^^^^^^^^^^^^^^^ meta.try meta.block
} catch (e) {
// <- meta.block
// ^^^^^^^ meta.catch
//       ^ meta.group
//          ^ meta.block
    foobar = 0
//  ^^^^^^^^^^ meta.catch meta.block
} finally {
// <- meta.block
// ^^^^^^^^ meta.finally
//        ^ meta.block
    foobar += 1
//  ^^^^^^^^^^^ meta.finally meta.block
}
// <- meta.block

class MyClass extends TheirClass {
// <- storage.type.class
//    ^^^^^^^ entity.name.class
//            ^^^^^^^ storage.modifier.extends
//                               ^ meta.block
    constructor(el)
//  ^^^^^^^^^^^^^^^ meta.function.declaration - meta.function.anonymous
    // ^ entity.name.function
    {
//  ^ meta.class meta.block meta.block punctuation.definition.block
        $.foo = "";
        super(el);
    }
//  ^ meta.class meta.block meta.block punctuation.definition.block

    get foo()
//  ^^^^^^^^^ meta.function.declaration - meta.function.anonymous
    // <- storage.type.accessor
    //   ^ entity.name.function
    {
        return this._foo;
    }

    static foo(baz) {
//  ^^^^^^^^^^^^^^^ meta.function.declaration - meta.function.anonymous
    // ^ storage.type
    //       ^ entity.name.function

    }

    qux()
//  ^^^^^ meta.function.declaration - meta.function.anonymous
    { }
//  ^ meta.class meta.block meta.block punctuation.definition.block

    get bar () {
//  ^^^^^^^^^^ meta.function.declaration - meta.function.anonymous
//             ^ meta.class meta.block meta.block punctuation.definition.block
    // <- storage.type.accessor
    //   ^ entity.name.function
        return false;
    }

    baz() { return null }
//  ^^^^^ meta.function.declaration - meta.function.anonymous
    // <- entity.name.function

    get() { return "foobar"; }
//  ^^^^^ meta.function.declaration.js
//  ^^^ entity.name.function.js - storage.type.accessor

    set (value) { return value; }
//  ^^^^^^^^^^^ meta.function.declaration.js
//       ^^^^^ variable.parameter.function.js
//  ^^^ entity.name.function.js - storage.type.accessor

    set  abc(value1, value2) { return value1 + value2; }
//  ^^^^^^^^^^^^^^^^^^^^^^^^ meta.function.declaration.js
//  ^^^ storage.type.accessor - entity.name.function.js
//       ^^^ entity.name.function.js
//           ^^^^^^ variable.parameter.function.js
//                 ^ punctuation.separator.parameter.function.js
//                   ^^^^^^ variable.parameter.function.js
}
// <- meta.block

class Foo extends React.Component {
//                      ^ entity.other.inherited-class
    constructor()
    {}

    [foo.bar](arg) {
//   ^^^^^^^ entity.name.function
//            ^^^ variable.parameter
        return this.a;
    }
}

() => {}
// <- meta.function.declaration punctuation.definition.parameters
 // <- meta.function.declaration punctuation.definition.parameters
//^^^ meta.function.anonymous meta.function.declaration
//    ^^ meta.block punctuation.definition.block

const test = ({a, b, c=()=>({active:false}) }) => {}
//    ^ entity.name.function
//           ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ meta.function.declaration
//            ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ meta.block
//            ^ punctuation.definition.block.begin
//             ^ variable.parameter
//                ^ variable.parameter
//                   ^ variable.parameter
//                     ^^^^ meta.function.declaration meta.function.declaration
//                     ^^ punctuation.definition.parameters
//                         ^^^^^^^^^^^^^^^^ meta.group
//                                   ^ constant.language
//                                          ^ punctuation.definition.block.end

// We can't currently detect this properly, but we need to consume => properly
([a,
  b]) => { }
//    ^^ storage.type.function.arrow

MyClass.foo = function() {}
// ^^^^^^^^^^^^^^^^^^^^^ meta.function.declaration - meta.function.anonymous
// ^ support.class
//       ^ entity.name.function

MyClass.foo = () => {}
// ^^^^^^^^^^^^^^^^ meta.function.declaration - meta.function.anonymous
// ^ support.class
//       ^ entity.name.function

xhr.onload = function() {}
// ^^^^^^^^^^^^^^^^^^^^ meta.function.declaration - meta.function.anonymous
// <- support.class.js
//  ^ entity.name.function

xhr.onload = () => {}
// ^^^^^^^^^^^^^^^ meta.function.declaration - meta.function.anonymous
// <- support.class.js
//  ^ entity.name.function

var simpleArrow = foo => bar
//  ^^^^^^^^^^^^^^^^^^^^ meta.function.declaration - meta.function.anonymous
//   ^ entity.name.function
//                 ^ variable.parameter.function
//                    ^ storage.type.function.arrow

var Proto = () => {
//  ^^^^^^^^^^^^^ meta.function.declaration - meta.function.anonymous
//   ^ entity.name.function
//             ^ storage.type.function.arrow
    this._var = 1;
}

Proto.prototype.getVar = () => this._var
// ^^^^^^^^^^^^^^^^^^^^^^^^^^^ meta.function.declaration - meta.function.anonymous
// ^ support.class
//     ^ support.constant.prototype
//                ^ entity.name.function
//                           ^ storage.type.function.arrow

Class3.prototype = function() {
// ^^^^^^^^^^^^^ meta.prototype.declaration
// ^^^^^^^^^^^^^^^^^^^^^^^^^^ meta.function.declaration - meta.function.anonymous
// ^ support.class
//       ^ support.constant.prototype
}

Proto.prototype.attr
// ^^^^^^^^^^^^ meta.prototype.access
// ^ support.class
//     ^ support.constant.prototype
//               ^ meta.property.object

Proto.prototype = {
    funcName: function() {
//  ^^^^^^^^^^^^^^^^^^^^ meta.function.declaration
//  ^^^^^^^^ entity.name.function
//          ^ punctuation.separator.key-value
    }
}

sources.DOM.status()
sources.DOM
// <- variable.other.object
    .status()
    // ^ meta.function-call.method variable.function

return new Promise(resolve => preferenceObject.set({value}, resolve));
//                                                                  ^ meta.function-call.constructor punctuation.definition.group

var anotherSingle = function(){a = param => param; return param2 => param2 * a}
//                                 ^ meta.function.declaration variable.parameter.function - meta.function.anonymous
//                                          ^ meta.block meta.block variable.other.readwrite
//                                               ^ meta.block punctuation.terminator.statement
//                                                        ^ meta.function.anonymous meta.function.declaration variable.parameter.function
//                                                                           ^ meta.block meta.block variable.other.readwrite
//                                                                            ^ meta.block punctuation.definition.block

baz(foo(x => x('bar')))
//                   ^ meta.function-call meta.function-call punctuation.definition.group
//                    ^ meta.function-call punctuation.definition.group

func(a, b)
// ^ variable.function
//  ^ meta.group punctuation.definition.group
//   ^ meta.group variable.other.readwrite
//       ^ meta.group punctuation.definition.group

var instance = new Constructor(param1, param2)
//                 ^ variable.type
//                            ^ meta.group punctuation.definition.group
//                             ^ meta.group variable.other.readwrite
//                                           ^ meta.group punctuation.definition.group

this.func()
// <- variable.language.this
self.func()
// <- variable.language.self
arguments;
// <- variable.language.arguments
super.func();
// <- variable.language.super
foo.constructor;
//  ^^^^^^^^^^^ variable.language.constructor
foo.__proto__
//  ^^^^^^^^^ variable.language.proto

var Constructor = function() {
    this._var = 1;
    // ^ variable.language.this
    this._method = function() {}
    // ^ variable.language.this
    //    ^ entity.name.function - meta.function.anonymous
}

// Tests to ensure the new keyword is highlighted properly even when the
// following element is not an identifier
var abc = new ABC(
//         ^ meta.instance.constructor keyword.operator.new
//               ^ meta.instance.constructor meta.function-call.constructor
    'my-name-is-abc',
    new (function () {
//  ^ meta.instance.constructor meta.function-call.constructor meta.instance.constructor keyword.operator.new
//      ^ meta.instance.constructor meta.function-call.constructor meta.instance.constructor meta.function-call.constructor meta.group
        var foo = 1;
//      ^ meta.instance.constructor meta.function-call.constructor meta.instance.constructor meta.function-call.constructor meta.group meta.block
    })
);

new Date().getTime()
// ^^^^^^^ meta.instance.constructor
//  ^^^^^^ meta.function-call.constructor
//  ^^^^ support.class.builtin
//        ^^^^^^^^^^ - meta.instance.constructor

new $();
//  ^ variable.type.dollar.only punctuation.dollar

new $Dollar();
//  ^ variable.type.dollar punctuation.dollar

void {
    'test1': [],
    'test2': new SomeObjectHash["default"],
//               ^^^^^^^^^^^^^^^^^^^^^^^^^ meta.function-call.constructor
//                             ^ meta.brackets
    'test3': "asdf"
}
// <- meta.object-literal punctuation.definition.block

// This tests parsing semi-broken object literals, which should help while a
// user is in the middle of typing code
void {
    key1: true
//  ^^^^ meta.object-literal.key.js
    key2: 0
//  ^^^^ meta.object-literal.key.js
    key3: function()
//  ^^^^ meta.object-literal.key.js entity.name.function
    {

    }
}

a = b.c
d = e.f
// <- variable.other.readwrite

foo.bar().baz
// ^ punctuation.accessor
//       ^ punctuation.accessor

width/2 + lineStart * Math.sin(i * 30 * π/180)
//   ^ keyword.operator.arithmetic
//                  ^ keyword.operator.arithmetic
//                         ^^^^^^^^^^^^^^^^^^^ meta.function-call.method

var reg = /a+/gimy.exec('aabb')
//        ^^^^^^^^ string.regexp
//            ^^^^ keyword.other
//                ^ punctuation.accessor

'aabbcc'.replace(/b+/, 'd')
//               ^^^^ string.regexp
//                 ^ keyword.operator.quantifier.regexp

/a+/
// <- string.regexp

'foo'.bar() / baz
//            ^ variable.other.readwrite

var g = 50

g / 20 + 30 /g
//^ keyword.operator.arithmetic
//          ^ keyword.operator.arithmetic

var h = foo() / 20 + 30 /g
//            ^ keyword.operator.arithmetic
//                      ^ keyword.operator.arithmetic

foo['bar']/ 20 + 30 /g
//        ^ keyword.operator.arithmetic
//                  ^ keyword.operator.arithmetic

foo.bar/ 20 + 30 /g
//     ^ keyword.operator.arithmetic
//               ^ keyword.operator.arithmetic

var result = 200 / 400 + 500 /
//               ^ keyword.operator.arithmetic
//                           ^ keyword.operator.arithmetic
100;

var re = /
//       ^ string.regexp punctuation.definition.string.begin
[a-z]
// ^ string.regexp.js
/g
// <- string.regexp.js punctuation.definition.string.end

var π = 3.141592653
//  ^ variable.other.readwrite

var angle = 2*π / count // angle between circles
//              ^ keyword.operator.arithmetic

var angle = 2*π / count /* angle between circles */
//              ^ keyword.operator.arithmetic

a = /foo\/bar/g // Ensure handling of escape / in regex detection
//    ^ string.regexp
//       ^ constant.character.escape

var re = /^\/[^/]+/
//             ^ constant.other.character-class.set.regexp
//               ^ keyword.operator.quantifier

(y - 1) / ((x - 1) / -2)
//      ^ keyword.operator.arithmetic
//        ^ punctuation.definition.group
(y - 1) / ((x - 1) /  2)
//    ^ punctuation.definition.group
//      ^ keyword.operator.arithmetic
 y      / ((x - 1) / -2)

define(['common'], function(common) {
//                 ^ meta.function.anonymous meta.function.declaration
    var namedFunc = function() {
//        ^ meta.function.declaration - meta.function.anonymous
    }
});

new FooBar(function(){
//          ^ meta.function.anonymous meta.function.declaration
    var namedFunc2 = function() {
//        ^ meta.function.declaration - meta.function.anonymous
    }
})

['foo'].bar = function() {
//      ^ meta.property.object entity.name.function - meta.function.anonymous
}

['foo'].$ = function() {
//      ^ meta.property.object.dollar.only entity.name.function - meta.function.anonymous
}

['foo'].$bar = function() {
//      ^ meta.property.object.dollar entity.name.function - meta.function.anonymous
}

{
// <- meta.block punctuation.definition.block
    let foo = 1;
//  ^^^ meta.block storage.type
//      ^^^ variable.other.readwrite
}
// <- meta.block punctuation.definition.block

var test =
{a: 1}
// <- meta.object-literal punctuation.definition.block

// Handle multi-line "concise" arrow function bodies
var conciseFunc = () =>
  foo
//^^^ meta.block variable.other.readwrite
  .bar()
//^^^^^^ meta.block

// Handle an arrow function in a parenthetical group
(myFunc = (a) => a*2)
// <- meta.group punctuation.definition.group
// ^^^^ entity.name.function
//                  ^ meta.group punctuation.definition.group - meta.block

var o = { a: i => i * 2, b: i => i * 3 }
//        ^ entity.name.function
//                     ^ - meta.block
//                       ^ entity.name.function

function test() {
    return {a: 1};
//         ^^^^^^ meta.object-literal
//          ^ meta.object-literal.key
//             ^ constant.numeric
}

$.each({})
// <- variable.other.object.dollar.only punctuation.dollar
//     ^ meta.object-literal

$varname.method()
// <- variable.other.object.dollar punctuation.dollar - variable.other.object.dollar.only
// ^ variable.other.object.dollar

$.fn.new_plugin = function() {}
// <- support.class.dollar.only punctuation.dollar

$var.fn.name = () => {}
// <- support.class.dollar punctuation.dollar - support.class.dollar.only
// ^ support.class.dollar - punctuation.dollar

someFunction(() => [() => 'X']);
//                           ^ punctuation.definition.brackets

string = 'invalid
//               ^ invalid.illegal.newline

hex = 0xFA.5;
//    ^^^^^^ invalid.illegal.numeric.hex

octal = 079.0;
//      ^^^^^ invalid.illegal.numeric.octal

strayBracket = ());
//               ^ invalid.illegal.stray-bracket-end

function optionalParam(b=0) {};
//                    ^ punctuation.definition.parameters.begin
//                      ^^ meta.parameter.optional
//                        ^ punctuation.definition.parameters.end

var path = require('path');
//  ^^^^ support.module.node

foo = path.join(__dirname, bar);
//              ^^^^^^^^^ support.type.object.node

nodeClass = new Buffer();
//              ^^^^^^ support.class.node

var CONST;
//  ^^^^^ variable.other.constant

err = new Error();
//        ^^^^^ support.class.error
