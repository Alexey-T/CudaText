/* SYNTAX TEST "Packages/C++/C++.sublime-syntax" */
#ifndef IGUARD_
 /* <- keyword.control.import */
#define IGUARD_
 /* <- keyword.control.import.define */
struct foo* alloc_foo();
/* <- storage.type */
       /* <- - entity.name.type */
            /* <- entity.name.function */
#endif
 /* <- keyword.control.import */

// The following example ensures that comments at the end of preprocessor
// directives don't mess with context transitions
int func() {
/*  ^ entity.name.function */
    #if( EXTAL == 40000 )       /* 40 MHz */
/*  ^ keyword.control.import */
        #define PLL_RFD_PHI1    10      // PLL0_PH1 = 40MHz
/*      ^ keyword.control.import */
/*                              ^^ constant.numeric */
/*                                      ^ comment.line */
    #endif
/*  ^ keyword.control.import */
}
/* <- meta.function meta.block punctuation.section.block.end */
 /* <- - meta.function meta.block */

#define MACRO_WITH_CURLY_BRACE {
/*^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ meta.preprocessor.macro */
/* <- keyword.control.import.define */
/*      ^ entity.name.constant */

#define MACRO_WITH_CURLY_BRACE_2 }
/*^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ meta.preprocessor.macro */
/* <- keyword.control.import.define */
/*      ^ entity.name.constant */


#pragma foo(bar, \
"baz", \
1)
/* <- meta.preprocessor */

#define MY_MACRO(a, b)
/*^^^^^^^^^^^^^^^^^^^^ meta.preprocessor.macro */
/*              ^^^^^^ meta.preprocessor.macro.parameters */
/*      ^^^^^^^^ entity.name.function.preprocessor */
/*                   ^ punctuation.section.group.end */

#define max(a, b, \
/*^^^^^^^^^^^^^^^^^ meta.preprocessor.macro */ \
/*         ^^^^^^^^ meta.preprocessor.macro.parameters */ \
/* <- keyword.control.import.define */ \
/*      ^ entity.name.function.preprocessor */ \
/*         ^ punctuation.section.group.begin */ \
/*          ^ variable.parameter */ \
/*           ^ punctuation.separator */ \
/* */ \
/* <- comment.block */ \
 c)  ((a>b) ? (a>c?a:c) : (b>c?b:c))
 /* <- meta.preprocessor.macro meta.group variable.parameter */
  /* <- meta.preprocessor.macro meta.group punctuation.section.group.end */
 /*               ^ keyword.operator.ternary */
 /*                 ^ keyword.operator.ternary */

#if 0
#ifdef moo
/* <- - keyword.control */
#endif
/* <- - keyword.control */
#endif

/////////////////////////////////////////////
// Strings
/////////////////////////////////////////////

char str1[] = "abc";
/*            ^ punctuation.definition.string.begin */
/*             ^ string.quoted.double */
/*                ^ punctuation.definition.string.end */

char str2[] = u8"abc";
/*            ^ storage.type.string */
/*              ^ punctuation.definition.string.begin */
/*               ^ string.quoted.double */

char16_t str3[] = u"abc";
/*                ^ storage.type.string */
/*                 ^ punctuation.definition.string.begin */
/*                  ^ string.quoted.double */

char32_t str4[] = U"abc";
/*                ^ storage.type.string */
/*                 ^ punctuation.definition.string.begin */
/*                  ^ string.quoted.double */

wchar_t str5[] = L"abc";
/*               ^ storage.type.string */
/*                ^ punctuation.definition.string.begin */
/*                 ^ string.quoted.double */

char str6[] = "\a|\b|\e|\f|\n|\r|\t|\v|\'|\"|\?";
/*             ^^ constant.character.escape */
/*                ^^ constant.character.escape */
/*                   ^^ constant.character.escape */
/*                      ^^ constant.character.escape */
/*                         ^^ constant.character.escape */
/*                            ^^ constant.character.escape */
/*                               ^^ constant.character.escape */
/*                                  ^^ constant.character.escape */
/*                                     ^^ constant.character.escape */
/*                                        ^^ constant.character.escape */
/*                                           ^^ constant.character.escape */

char str7[] = "\0|\012";
/*             ^^ constant.character.escape */
/*                ^^^^ constant.character.escape */

char str8[] = "\x0a|\x41|\xA|\x000065";
/*             ^^^^ constant.character.escape */
/*                  ^^^^ constant.character.escape */
/*                       ^^^ constant.character.escape */
/*                           ^^^^^^^^ constant.character.escape */

char16_t str9[] = u"\u0063";
/*                  ^^^^^^ constant.character.escape */

char32_t str10[] = U"\U00000063";
/*                   ^^^^^^^^^^ constant.character.escape */

char str11[] = "\q";
/*              ^^ invalid.illegal.unknown-escape */

scanf("%ms %as %*[, ]", &buf);
/*     ^^^ constant.other.placeholder */
/*         ^^^ constant.other.placeholder */
/*             ^^^^^^ constant.other.placeholder */

char rawStr1[] = R"("This is a raw string")";
/*               ^ storage.type.string */
/*                ^ punctuation.definition.string.begin */
/*                  ^ string.quoted.double */
/*                                        ^ punctuation.definition.string.end */

char rawStr2[] = R"A*!34( )" )A*!34";
/*               ^ storage.type.string */
/*                ^ punctuation.definition.string.begin */
/*                      ^ punctuation.definition.string.begin */
/*                        ^ string.quoted.double */
/*                           ^ punctuation.definition.string.end */
/*                                 ^ punctuation.definition.string.end */


/////////////////////////////////////////////
// Storage Types
/////////////////////////////////////////////

void* ptr;
/* <- storage.type */

bool b;
/* <- storage.type */

char ch;
/* <- storage.type */

char16_t ch16;
/* <- storage.type */

char32_t ch32;
/* <- storage.type */

wchar_t wch;
/* <- storage.type */

unsigned int ui;
/* <- storage.type */
/*       ^ storage.type */

signed long l;
/* <- storage.type */
/*     ^ storage.type */

short s;
/* <- storage.type */

auto a = 2;
/* <- storage.type */

decltype(s) dt;
/* <- storage.type */
/*      ^ punctuation.section.group.begin */
/*        ^ punctuation.section.group.end */

float f;
/* <- storage.type */

double d;
/* <- storage.type */

typedef int my_int;
/* <- storage.type */
/*          ^ entity.name.type */

typedef struct Books {
/*      ^ storage.type */
/*             ^ - entity.name.type */
   char title[50];
   int book_id;
} Book;
/*^ entity.name.type */

typedef struct Books Book;
/*             ^ - entity.name.type.struct */
/*                   ^ entity.name.type.typedef */

template class MyStack<int, 6>;
/* <- storage.type.template */
/*                    ^ punctuation.section.generic */
/*                     ^ storage.type */
/*                          ^ constant.numeric */
/*                           ^ punctuation.section.generic */

template<class typeId, int N> class tupleTmpl;
/* <- storage.type.template */
/*^^^^^^^^^^^^^^^^^^^^^^^^^^^ meta.template */
/*      ^ punctuation.section.generic.begin */
/*       ^ storage.type */
/*                      ^ storage.type */
/*                          ^ punctuation.section.generic.end */

template<typename First = U<V>, typename... Rest> class tupleVariadic;
/* <- storage.type.template */
/*      ^ punctuation.section.generic.begin */
/*       ^ storage.type */
/*                         ^ punctuation.section.generic.begin */
/*                           ^ punctuation.section.generic.end */
/*                            ^ punctuation.separator */
/*                                      ^^^ keyword.operator.variadic */
/*                                              ^ punctuation.section.generic.end */

template<typename Foo> inline struct Foo* baz()
/*                     ^^^^^^ storage.modifier */
/*                                   ^ - entity.name */
/*                                        ^^^ meta.function entity.name.function */
{}

template<typename A, typename B>
void classname<A, B>::methodName() {
/*   ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ meta.function */
/*            ^ punctuation.section.generic.begin */
/*                 ^ punctuation.section.generic.end */
/*                  ^^ punctuation.accessor */
/*                    ^^^^^^^^^^ entity.name.function */
}

template<typename C>
void funcName<C>() {
/*   ^^^^^^^^^^^^^^^ meta.function */
/*   ^^^^^^^^ entity.name.function */
/*           ^ punctuation.section.generic.begin */
/*             ^ punctuation.section.generic.end */
}

typedef std :: vector<std::vector<int> > Table;
/*          ^^ punctuation.accessor */
/*                   ^ punctuation.section.generic.begin */
/*                               ^ punctuation.section.generic.begin */
/*                                   ^ punctuation.section.generic.end */
/*                                     ^ punctuation.section.generic.end */

/////////////////////////////////////////////
// Storage Modifiers
/////////////////////////////////////////////

alignas(16) char array[256];
/* <- storage.modifier */
/*     ^ punctuation.section.group.begin */
/*        ^ punctuation.section.group.end */

const int XYZ = 2;
/* <- storage.modifier */

constexpr int ABC = 3 + 5;
/* <- storage.modifier */

thread_local int x;
/* <- storage.modifier */


/////////////////////////////////////////////
// Control Keywords
/////////////////////////////////////////////

static_assert(x >= 0);
/* <- keyword.operator */

noexcept(f());
/*       ^^^ meta.function-call */
/* <- keyword.operator */

if (x < 5)
/* <- keyword.control */
{}
else
/* <- keyword.control */
{}

switch (x)
/* <- keyword.control */
{
case 1:
/* <- keyword.control */
    break;
    /* <- keyword.control */
default:
/* <- keyword.control */
    break;
    /* <- keyword.control */
}

do
/* <- keyword.control */
{
    if (y == 3)
        continue;
        /* <- keyword.control */
} while (y < x);
/*^ keyword.control */

switch (a) {
    case 1: break;
/*        ^ punctuation.separator */
    case 100 - 10: break;
/*               ^ punctuation.separator */
    default: break;
/*         ^ punctuation.separator */
}

goto label;
/* <- keyword.control */

try
/* <- keyword.control */
{
    throw std :: string("xyz");
    /* <- keyword.control */
    /*    ^^^^^^^^^^^^^ variable.function */
    /*        ^^ punctuation.accessor */
}
catch (...)
/* <- keyword.control */
{
}

int* ptr = new int(2);
/*         ^ keyword.control */

delete ptr;
/* <- keyword.control */

return 123;
/* <- keyword.control */


/////////////////////////////////////////////
// Operator Keywords
/////////////////////////////////////////////

int x = alignof(char);
/*      ^ keyword.operator.word */
/*             ^ punctuation.section.group.begin */
/*                  ^ punctuation.section.group.end */

int x = sizeof(char);
/*      ^ keyword.operator.word */
/*            ^ punctuation.section.group.begin */
/*                 ^ punctuation.section.group.end */


/////////////////////////////////////////////
// Cast Keywords
/////////////////////////////////////////////

const_cast<int>(2.0);
/* <- keyword.operator.word.cast */
/*        ^ punctuation.section.generic.begin */
/*         ^ storage.type */
/*            ^ punctuation.section.generic.end */

dynamic_cast<int>(2.0);
/* <- keyword.operator.word.cast */

reinterpret_cast<int>(2.0);
/* <- keyword.operator.word.cast */

static_cast<int>(2.0);
/* <- keyword.operator.word.cast */


/////////////////////////////////////////////
// Language Constants
/////////////////////////////////////////////

bool t = true;
/*       ^ constant.language */

bool f = false;
/*       ^ constant.language */

int* p = nullptr;
/*       ^ constant.language */

char ch[] = __func__;
/*          ^ constant.language */


/////////////////////////////////////////////
// Support Constants
/////////////////////////////////////////////

std::cout << __FILE__ << '\n';
/*           ^ support.constant */
/* ^^ punctuation.accessor */

std :: cout << __FUNCTION__ << '\n';
/*  ^^ punctuation.accessor */
/*             ^ support.constant */

std::cout << __LINE__ << '\n';
/*           ^ support.constant */


/////////////////////////////////////////////
// Numeric Constants
/////////////////////////////////////////////

dec1 = 1234567890;
/*     ^ constant.numeric */
/*              ^ constant.numeric */

dec2 = 1'924'013;
/*     ^ constant.numeric */
/*             ^ constant.numeric */

dec3 = 124ul;
/*     ^ constant.numeric */
/*         ^ constant.numeric */

dec4 = 9'204lu;
/*     ^ constant.numeric */
/*           ^ constant.numeric */

dec5 = 2'354'202'076LL;
/*     ^ constant.numeric */
/*                   ^ constant.numeric */

int oct1 = 01234567;
/*         ^ constant.numeric */
/*                ^ constant.numeric */

int oct2 = 014'70;
/*         ^ constant.numeric */
/*              ^ constant.numeric */

int hex1 = 0x1234567890ABCDEF;
/*         ^ constant.numeric */
/*                          ^ constant.numeric */

int hex2 = 0X1234567890ABCDEF;
/*         ^ constant.numeric */
/*                          ^ constant.numeric */

int hex3 = 0x1234567890abcdef;
/*         ^ constant.numeric */
/*                          ^ constant.numeric */

int hex4 = 0xA7'45'8C'38;
/*         ^ constant.numeric */
/*                     ^ constant.numeric */

int bin1 = 0b010110;
/*         ^ constant.numeric */
/*                ^ constant.numeric */

int bin2 = 0B010010;
/*         ^ constant.numeric */
/*                ^ constant.numeric */

int bin3 = 0b1001'1101'0010'1100;
/*         ^ constant.numeric */
/*                             ^ constant.numeric */

units1 = 134h;
/*       ^ constant.numeric */
/*          ^ constant.numeric */

units2 = 147min;
/*       ^ constant.numeric */
/*            ^ constant.numeric */

units3 = 357s;
/*       ^ constant.numeric */
/*          ^ constant.numeric */

units4 = 234_custom;
/*       ^ constant.numeric */
/*                ^ constant.numeric */

fixed1 = 123.456;
/*       ^ constant.numeric */
/*             ^ constant.numeric */

fixed2 = 12.;
/*       ^ constant.numeric */
/*         ^ constant.numeric */

fixed3 = .35;
/*       ^ constant.numeric */
/*         ^ constant.numeric */

fixed4 = 1'843'290.245'123;
/*       ^ constant.numeric */
/*                       ^ constant.numeric */

fixed5 = 0.3f;
/*       ^ constant.numeric */
/*          ^ constant.numeric */

fixed6 = 0.82L;
/*       ^ constant.numeric */
/*           ^ constant.numeric */

float sci1 = 1.23e10;
/*           ^ constant.numeric */
/*                 ^ constant.numeric */

float sci2 = 13e5;
/*           ^ constant.numeric */
/*              ^ constant.numeric */

float sci3 = 14.23e+14;
/*           ^ constant.numeric */
/*                   ^ constant.numeric */

float sci4 = 14e+14;
/*           ^ constant.numeric */
/*                ^ constant.numeric */

float sci5 = 18.84e-12;
/*           ^ constant.numeric */
/*                   ^ constant.numeric */

float sci6 = 46e-14;
/*           ^ constant.numeric */
/*                ^ constant.numeric */

float sci7 = 2'837e1'000;
/*           ^ constant.numeric */
/*                     ^ constant.numeric */

float sci8 = 23e-1'000;
/*           ^ constant.numeric */
/*                   ^ constant.numeric */

double sci_hex = 0xc1.01AbFp-1;
/*               ^^^^^^^^^^^^^ constant.numeric */

/////////////////////////////////////////////
// Functions
/////////////////////////////////////////////

// function prototype
void abcdWXYZ1234();
/*   ^^^^^^^^^^^^^^ meta.function */
/*   ^ entity.name.function */
/*               ^^ meta.function.parameters meta.group */
/*               ^ punctuation.section.group.begin */
/*                ^ punctuation.section.group.end */

// function definition
void abcdWXYZ1234()
/*   ^^^^^^^^^^^^^^ meta.function */
/*   ^ entity.name.function */
/*               ^^ meta.function.parameters meta.group */
/*               ^ punctuation.section.group.begin */
/*                ^ punctuation.section.group.end */
{
/* <- meta.function meta.block punctuation.section.block.begin */
}
/* <- meta.function meta.block punctuation.section.block.end */

struct foo **alloc_foo();
/*           ^^^^^^^^^^^ meta.function */
/*                    ^^ meta.function.parameters meta.group */
/*                    ^ punctuation.section.group.begin */
/*                     ^ punctuation.section.group.end */
/* ^ storage.type */
/*     ^ - entity.name.struct */
/*         ^^ keyword.operator */
/*           ^ entity.name.function */

long func
/*   ^^^^ meta.function entity.name.function */
(int x, void *MYMACRO(y) ) {
/*^^^^^^^^^^^^^^^^^^^^^^^^^^ meta.function */
/*^^^^^^^^^^^^^^^^^^^^^^^^ meta.function.parameters meta.group */
/* <- meta.function.parameters meta.group punctuation.section.group.begin */
/*   ^ variable.parameter */
/*            ^ -entity.name.function */
/*            ^^^^^^^^^^ meta.function-call */
/*                   ^^^ meta.group meta.function-call meta.group */
/*                   ^ punctuation.section.group.begin */
/*                     ^ punctuation.section.group.end */
/*                       ^ punctuation.section.group.end */
    // Ensure < and > aren't parsed as a generic
    if (foo < bar && baz > bar ) {
/*          ^ keyword.operator.comparison */
/*                       ^ keyword.operator.comparison */

    label:
/*  ^ entity.name.label */
/*       ^ punctuation.separator */
        do {
            break;
        } while(true);

    }
}
/* <- meta.function meta.block punctuation.section.block.end */

MACRO1
RETURN_TYPE
/* <- - entity.name.function */
func_name() {
/* < entity.name.function */
}

MACRO1 void * MACRO2 myfuncname () {
/*     ^ storage.type */
/*          ^ keyword.operator */
/*                   ^ entity.name.function */

    struct Args {
/*  ^ storage.type */
/*         ^ entity.name.struct */
        void* hello;
        void* foobar;
        Args()
/*      ^ entity.name.function.constructor */
        : hellp(nullptr),
/*      ^ punctuation.separator.initializer-list */
          foobar(nullptr)
        {
        }
    };

    struct Args args2;
/*  ^ storage.type */
/*         ^ - entity */

    class LocalFoo MYMACRO
/*  ^ storage.type */
/*        ^ entity.name.class */
/*                 ^ - entity */
    {
        LocalFoo() {}
/*      ^ entity.name.function.constructor */
    }

    class LocalFoo test;
/*  ^ storage.type */
/*        ^ - entity */

}

static const uint32_t * const MACRO funcname();
/* ^ storage.modifier */
/*     ^ storage.modifier */
/*           ^ support.type */
/*                    ^ keyword.operator */
/*                      ^ storage.modifier */
/*                                  ^ entity.name.function */

void FooBar :: baz(int a)
/*   ^^^^^^^^^^^^^^^^^^^^ meta.function */
/*   ^^^^^^^^^^^^^ entity.name.function */
/*          ^^ punctuation.accessor */
/*                ^^^^^^^ meta.function.parameters meta.group */
/*                ^ punctuation.section.group.begin */
/*                     ^ variable.parameter */
/*                      ^ punctuation.section.group.end */
{

}

FooBar::FooBar(int a)
/*^^^^^^^^^^^^^^^^^^^ meta.function */
/*^^^^^^^^^^^^ entity.name.function */
/*            ^^^^^^^ meta.function.parameters meta.group */
/*            ^ punctuation.section.group.begin */
/*             ^^^ storage.type */
/*                  ^ punctuation.section.group.end */
{
}

FooBar :: FooBar(int a) & =
/*^^^^^^^^^^^^^^^^^^^^^^^^^ meta.function */
/*^^^^^^^^^^^^^^ entity.name.function */
/*              ^^^^^^^ meta.function.parameters meta.group */
/*              ^ punctuation.section.group.begin */
/*               ^^^ storage.type */
/*                   ^ variable.parameter */
/*                    ^ punctuation.section.group.end */
/*                      ^ keyword.operator */
/*                        ^ keyword.operator.assignment */
default;
/*^^^^^ meta.function storage.modifier */

FooBar::~FooBar
/*^^^^^^^^^^^^^ meta.function entity.name.function */
() { }
/* <- meta.function.parameters meta.group punctuation.section.group.begin */
 /* <- meta.function.parameters meta.group punctuation.section.group.end */
/*^^^^ meta.function */

ThisIsAReallyReallyLongClassNameThatRequiresWrappingCodeInTheMiddleOfAPath::
    ThisIsAReallyReallyLongClassNameThatRequiresWrappingCodeInTheMiddleOfAPath()
/* <- entity.name.function */
    : var_name(nullptr) {
}

bool FooBar::operator==() {}
/*   ^^^^^^^^^^^^^^^^^^^^^^^ meta.function */
/*   ^^^^^^^^^^^^^^^^^^ entity.name.function */
/*                     ^^ meta.function.parameters meta.group */
/*                     ^ punctuation.section.group.begin */
/*                      ^ punctuation.section.group.end */
/*                        ^^ meta.block */
/*                        ^ punctuation.section.block.begin */
/*                         ^ punctuation.section.block.end */


myns::FooBar::~FooBar() { }
/*^^^^^^^^^^^^^^^^^^^^^^^^^ meta.function */
/*                   ^^ meta.function.parameters meta.group */
/*                   ^ punctuation.section.group.begin */
/*                    ^ punctuation.section.group.end */
/*                      ^^^ meta.block */
/*                      ^ punctuation.section.block.begin */
/*                        ^ punctuation.section.block.end */
/*^^^^^^^^^^^^^^^^^^^ entity.name.function */

    extern "C" void test_in_extern_c_block()
/*                  ^^^^^^^^^^^^^^^^^^^^^^^^ meta.function */
/*                                        ^^ meta.function.parameters meta.group */
/*                                        ^ punctuation.section.group.begin */
/*                                         ^ punctuation.section.group.end */
/*                  ^ entity.name.function */
{
/* <- meta.function meta.block punctuation.section.block.begin */
}
/* <- meta.function meta.block punctuation.section.block.end */

#ifdef __cplusplus
extern "C" {
#endif

#if 0
void test_in_extern_c_block()
/*   ^ comment.block */
{
}
#else
void test_in_extern_c_block()
/*   ^^^^^^^^^^^^^^^^^^^^^^^^ meta.function */
/*                         ^^ meta.function.parameters meta.group */
/*                         ^ punctuation.section.group.begin */
/*                          ^ punctuation.section.group.end */
/*   ^ entity.name.function */
{
/* <- meta.function meta.block punctuation.section.block.begin */
}
/* <- meta.function meta.block punctuation.section.block.end */
#endif

#ifdef __cplusplus
}
#endif
/* ^ - meta.extern-c */

gener<int> func_returning_generic(int a);
/*         ^ entity.name.function */

std::vector<std::uint8_t> func_returning_path_generic(int a);
/*         ^ punctuation.section.generic */
/*                        ^ entity.name.function */


long double operator "" _km (long double x);
/*          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ meta.function */
/*                          ^^^^^^^^^^^^^^^ meta.function.parameters */
/*          ^^^^^^^^^^^^^^^ entity.name.function */

/////////////////////////////////////////////
// Namespace
/////////////////////////////////////////////

namespace myNameSpace {}
/* <- keyword.control */
/*        ^ entity.name.namespace */

namespace new_name = current_name;
/* <- keyword.control */

using namespace NAME __attribute__((visibility ("hidden")));
/* <- keyword.control */
/*    ^ keyword.control */
/*                   ^ storage.modifier */
/*                                               ^ string */

using namespace myNameSpace;
/* <- keyword.control */
/*    ^ keyword.control */

namespace ns :: abc /* Neither this comment... */
/*^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ meta.namespace */
/*        ^^^^^^^^^ entity.name.namespace */
/*           ^^ punctuation.accessor */
/*                  ^ comment.block */
// ...nor this comment is highlighted
/* <- comment.line */
{
/* <- meta.namespace meta.block punctuation.section.block.begin */
    void nsfunc() {
        /* ^ entity.name.function */
    }
}
/* <- meta.namespace meta.block punctuation.section.block.end */

namespace tl {
/*^^^^^^^^^^^^ meta.namespace */
/* ^ keyword.control */
    namespace {
/*  ^ keyword.control */
/*  ^^^^^^^^^^^ meta.namespace meta.namespace */
/*            ^ meta.block meta.block punctuation.section.block.begin */
        void nested_func(){}
/*           ^ entity.name.function */
    }
}

MACRONAME namespace ns3 {}
/*        ^ keyword.control */

/////////////////////////////////////////////
// Classes, structs, unions and enums
/////////////////////////////////////////////

class BaseClass;
/*^^^^^^^^^^^^^ meta.class */
/*    ^ - meta.class meta.class */
/*    ^^^^^^^^^ entity.name.class.forward-decl */

class BaseClass // comment
/* <- storage.type */
/*    ^ entity.name.class */
{
public :
/* <- storage.modifier */
    tupleTmpl<int,2> max(tupleGen<int,2> a, tupleGen<int,2> b);
/*                   ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ meta.method */
/*                      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ meta.method.parameters meta.group */
/*                      ^ punctuation.section.group.begin */
/*                                       ^ variable.parameter */
/*                                        ^ punctuation.separator */
/*                                                          ^ variable.parameter */
/*                                                           ^ punctuation.section.group.end */
/*           ^ punctuation.section.generic.begin */
/*            ^ storage.type */
/*                 ^ punctuation.section.generic.end */
/*                               ^ punctuation.section.generic.begin */
/*                                ^ storage.type */
/*                                     ^ punctuation.section.generic.end */
protected:
/* <- storage.modifier */
private:
/* <- storage.modifier */

    static int a = 1 << 4;
    /* <- storage.modifier */
    /*               ^^ keyword.operator.arithmetic */

    static int x;
    /* <- storage.modifier */

    virtual void doSomething() const = 0;
    /*           ^^^^^^^^^^^^^^^^^^^^^^^ meta.method */
    /*           ^ - meta.method meta.method */
    /*                      ^^ meta.method.parameters meta.group */
    /*                      ^ punctuation.section.group.begin */
    /*                       ^ punctuation.section.group.end */
    /* <- storage.modifier */
    /*           ^ entity.name.function */
    /*                         ^ storage.modifier */
    /*                                 ^ constant.numeric */

    template<typename A>
    void func(){}
/*       ^^^^^^^^ meta.method */
/*           ^^ meta.method.parameters meta.group */
/*           ^ punctuation.section.group.begin */
/*            ^ punctuation.section.group.end */
/*             ^^ meta.block */
/*             ^ punctuation.section.block.begin */
/*              ^ punctuation.section.block.end */
/*       ^^^^ entity.name.function */

    template<typename A>
    void func<A>(){}
/*       ^^^^^^^^^^^ meta.method */
/*       ^^^^ entity.name.function */
/*           ^ punctuation.section.generic.begin */
/*             ^ punctuation.section.generic.end */
/*              ^^ meta.method.parameters meta.group */
/*              ^ punctuation.section.group.begin */
/*               ^ punctuation.section.group.end */
/*                ^^ meta.block */
/*                ^ punctuation.section.block.begin */
/*                 ^ punctuation.section.block.end */

    template<typename A>
    BaseClass(){}
/*  ^^^^^^^^^^^^^ meta.method */
/*  ^^^^^^^^^ meta.method.constructor */
/*  ^^^^^^^^^ entity.name.function */

    ~BaseClass() {}
/*  ^^^^^^^^^^^^^^^ meta.method */
/*  ^^^^^^^^^^ meta.method.destructor */
/*  ^^^^^^^^^^ entity.name.function */

    BaseClass operator [] ()
/*            ^^^^^^^^^^^^^^ meta.method */
/*                        ^^ meta.method.parameters */
/*            ^^^^^^^^^^^ entity.name.function */
    {}

    BaseClass operator=
/*            ^^^^^^^^^ meta.method */
/*            ^^^^^^^^^ entity.name.function */
    () {}
/*  ^^^^^ meta.method */
/*  ^^ meta.method.parameters */
/*     ^^ meta.block */
};

class DerivedClass : public ::BaseClass // Comment
/*                          ^ entity.other.inherited-class */
/*                                      ^ comment.line */
{
    ~DerivedClass() override;
    /* <- meta.method.destructor */
    /*^^^^^^^^^^^^^^^^^^^^^^ meta.method */
    /*^^^^^^^^^^^ meta.method.destructor */
    /*              ^ storage.modifier */
    virtual void doSomething() const override final;
    /*                         ^ storage.modifier */
    /*                               ^ storage.modifier */
    /*                                        ^ storage.modifier */
 protected:
/*^^^^^^^^ storage.modifier */
  DerivedClass() override
/*^^^^^^^^^^^^^^^^^^^^^^^ meta.method */
/*^^^^^^^^^^^^ meta.method.constructor */
/*                ^ storage.modifier */
      : a(a),
/*    ^^^^^^^ meta.method.constructor.initializer-list */
        base_id_(BaseClass::None().ToInt()),
/*      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ meta.method.constructor.initializer-list */
/*      ^ variable.other.readwrite.member */
/*                        ^ punctuation.accessor */
/*               ^^^^^^^^^^^^^^^^^ meta.function-call */
/*                          ^^^^ variable.function */
/*                                ^^^^^^ meta.method-call */
/*                                 ^^^^^ variable.function */
/*                                         ^ punctuation.separator */
        bounds_(NULL),
/*      ^^^^^^^^^^^^^^ meta.method.constructor.initializer-list */
        bit_field_(0) {
/*      ^^^^^^^^^^^^^^^ meta.method */
/*      ^^^^^^^^^^^^^ meta.method.constructor.initializer-list */
            char * a = "sprintf";
            char * b = sprintf("Testing %s", a);
/*                     ^^^^^^^^^ meta.function-call */
/*                     ^^^^^^^ support.function.C99 */

            base_id_->foobar(1, "foo");
/*                  ^^^^^^^^^^ meta.method-call */
/*                    ^^^^^^ variable.function */

            base_id_->~BaseClass();
/*                  ^^^^^^^^^^^^^^ meta.method-call */
/*                    ^^^^^^^^^^ variable.function */
        }
/*      ^ meta.method meta.block punctuation.section.block.end */

/* <- - meta.class.constructor.initializer-list */

    typedef std::vector<int> qux;
/*                           ^^^ entity.name.type.typedef */
};


template<typename A>
class class1<A> : class2<A>
/*    ^^^^^^ entity.name.class */
/*          ^ punctuation.section.generic */
/*            ^ punctuation.section.generic */
/*                ^^^^^^ entity.other.inherited-class */
/*                      ^ punctuation.section.generic */
/*                        ^ punctuation.section.generic */
{}

class FooBar {
/*           ^ meta.class meta.block punctuation.section.block.begin */
    explicit FooBar(int a);
/*           ^^^^^^^^^^^^^ meta.method */
/*  ^^^^^^^^ storage.modifier */
/*           ^^^^^^ entity.name.function */
    FooBar() =default;
/*  ^^^^^^ entity.name.function */
/*           ^ keyword.operator.assignment */
/*            ^^^^^^^ storage.modifier */
    FooBar(void) =default;
/*  ^^^^^^ entity.name.function */
/*         ^^^^ storage.type */
/*               ^ keyword.operator.assignment */
/*                ^^^^^^^ storage.modifier */

    // If a constructor name is on a line without the opening (, it is not
    // highlighted as a method/constructor. This prevents a bunch of false
    // positives, including data types as they are being typed into a struct.
    FooBar
/*  ^ - entity.name.function */
    ();

    virtual ~FooBar();
/*          ^^^^^^^ entity.name.function */

#ifndef DEBUG
    ~FooBar();
/*  ^^^^^^^ entity.name.function */
#endif

    void insert () {}
/*       ^^^^^^ entity.name.function */

    explicit operator bool
/*                    ^^^^ entity.name.function */
    () {}

    FooBar::~FooBar();
/*  ^^^^^^^^^^^^^^^ entity.name.function */

    void FooBar::
    Baz() {
    /* <- entity.name.function */
    }

    auto f(int a) -> decltype(a.begin()) override final;
/*                ^^ punctuation.separator */
/*                             ^ punctuation.accessor */
/*                              ^^^^^ variable.function */
/*                                       ^ storage.modifier */
/*                                                ^ storage.modifier */

    auto g() -> std::vector<int> override final {
/*           ^^ punctuation.separator */
/*                 ^^ punctuation.accessor */
/*                          ^ storage.type */
/*                               ^ storage.modifier */
/*                                        ^ storage.modifier */
        this->g(1);
/*      ^ variable.language */
    }

private:
/*^^^^^ storage.modifier */

    VISIBILITY_MACRO
/*  ^ - entity.name.function */
    myns::subns::MyDataType
/*  ^ - entity.name.function */
    and_now_method_name();
/*  ^ entity.name.function */

    VISIBILITY_MACRO
/*  ^ - entity.name.function */
    std::shared_future<std::vector<myns::mysubns::MyDataType>>
/*  ^ - entity.name.function */
/*     ^^ punctuation.accessor */
/*                    ^ punctuation.section.generic.begin */
/*                        ^^ punctuation.accessor */
/*                                ^ punctuation.section.generic.begin */
/*                                     ^^ punctuation.accessor */
    and_now_method_name2();
/*  ^ entity.name.function */

    enum
/*  ^^^^ meta.enum storage.type */
    {
/*  ^ meta.enum punctuation.section.block.begin */
        A = 1,
        B = 20 / 5
    }
/*  ^ meta.enum punctuation.section.block.end */
/*   ^ - meta.enum */

    friend int func(int a, int b);
/*  ^ storage.modifier */
/*         ^ storage.type */
/*             ^ - entity.name.function */
/*             ^ - meta.function-call */

    friend int func(int a, int b) {
/*  ^ storage.modifier */
/*         ^ storage.type */
/*             ^ - entity.name.function */
/*             ^ - meta.function-call */
/*                                ^ meta.class meta.block meta.block punctuation.section.block.begin */
        int a = 1;
    }
/*  ^ meta.class meta.block meta.block punctuation.section.block.end */
/*   ^ - meta.class meta.block meta.block */

    friend class ::FooBar;
/*  ^ storage.modifier */
/*         ^ storage.type
/*               ^^ punctuation.accessor */
/*                 ^ - entity */

    #if 0
    /*  ^ constant.numeric */
    int disabled_func() {
    /*  ^ comment.block */
    }
    #endif

    #if 1
    /*  ^ constant.numeric */
    int enabled_func() {}
    /*  ^ entity.name.function */
    #else
    int disabled_func() {
    /*  ^ comment.block */
    }
    #endif

    MACRO_NOT_CONSTRUCTOR(FooBar);
/*  ^ meta.function-call variable.function - entity.name.function */

    MACRO_NOT_CONSTRUCTOR2(FooBar,
/*  ^ meta.function-call variable.function - entity.name.function */
        FriendClass);
}
/* <- meta.class meta.block punctuation.section.block.end */
 /* <- - meta.class meta.block */

struct bar {
/*^^^^^^^^^^ meta.struct */
/*^^^^ storage.type */
/*     ^^^ entity.name.struct */
/*         ^ meta.block punctuation.section.block.begin */
    bar()
/*  ^^^^^ meta.method */
/*  ^^^ entity.name.function */
    {}
}
/* <- meta.struct meta.block punctuation.section.block.end */
 /* <- - meta.struct meta.block */

enum baz {
/*^^^^^^^^ meta.enum */
/* <- meta.enum storage.type */
/*   ^^^ entity.name.enum */
/*       ^ meta.block punctuation.section.block.begin */
    FOO = 1,
/*      ^ keyword.operator.assignment */
/*        ^ constant.numeric */
    BAR = 2,
    BAZ = 3,
#if 0
/*  ^ constant.numeric */
    QUX = 4,
/*  ^ comment.block */
#endif
}
/* <- meta.enum meta.block punctuation.section.block.end */
 /* <- - meta.enum meta.block */

enum class qux : std::uint8_t
/*^^^^^^^^^^^^^^^^^^^^^^^^^^^ meta.enum */
/*^^^^^^^^ storage.type */
/*         ^^^ entity.name.enum */
/*             ^ punctuation.separator */
/*               ^^^^^^^^^^^^ entity.other.inherited-class */
{
/* <- meta.block punctuation.section.block.begin */
    FOO = 1,
    BAR = 2,
/*      ^ keyword.operator.assignment */
/*        ^ constant.numeric */
    BAZ = 3
}
/* <- meta.enum meta.block punctuation.section.block.end */
 /* <- - meta.enum meta.block */

enum LineEnding : uint32_t;
/*^^^^^^^^^^^^^^^^^^^^^^^^ meta.enum */
/*   ^^^^^^^^^^ entity.name.enum.forward-decl */
/*              ^ punctuation.separator */
/*                ^^^^^^^^ entity.other.inherited-class */
/*                        ^ - meta.enum */

union foobaz {
/* <- meta.union storage.type */
/*    ^^^^^^ entity.name.union */
/*           ^ meta.block punctuation.section.block.begin */
}
/* <- meta.union meta.block punctuation.section.block.end */
 /* <- - meta.union meta.block */

// Class name that looks like a possible macro
class SP {}
/*    ^^ entity.name.class */

class MyClass MACRO MACRO2
/* ^ storage.type */
/*    ^ entity.name.class */
/*            ^ - entity */
/*                  ^ - entity */
{}

// Class using macro to handle __declspec() on Windows
class SK_API SkBitmap {}
/*           ^ entity.name.class */
class __declspec(property(get=foo)) SkBitmap {}
/*               ^ storage.modifier */
/*                        ^ variable.parameter */
/*                                  ^ entity.name.class */
class __declspec(align(8)) SkBitmap {}
/*               ^ storage.modifier */
/*                     ^ constant.numeric */
/*                         ^ entity.name.class */
class __declspec(dllimport) SkBitmap {}
/*               ^ constant.other */
/*                          ^ entity.name.class */

/////////////////////////////////////////////
// Test preprocessor branching and C blocks
/////////////////////////////////////////////

int foo(int val, float val2[], bool val3 = false)
/*  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ meta.function */
/*     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ meta.function.parameters meta.group */
/*     ^ punctuation.section.group.begin */
/*                                              ^ punctuation.section.group.end */
/*          ^^^ variable.parameter */
/*             ^ punctuation.separator */
/*                     ^^^^ variable.parameter */
/*                         ^^ meta.brackets */
/*                         ^ punctuation.section.brackets.begin */
/*                          ^ punctuation.section.brackets.end */
/*                           ^ punctuation.separator */
/*                             ^^^^ storage.type */
/*                                  ^^^^ variable.parameter */
/*                                       ^ keyword.operator.assignment */
/*                                         ^^^^^ constant.language */
{
    myClass *result;
    result->kk = func(val);
/*        ^^ punctuation.accessor */
    if (result == 0) {
        return 0;
#if CROSS_SCOPE_MACRO
 /* <- keyword.control.import */
    } else if (result > 0) {
        return 1;
#endif
 /* <- keyword.control.import */
    }
/*  ^ meta.block meta.block punctuation.section.block.end */
/*   ^ - meta.block meta.block */

#ifndef DEBUG
    if (check_debug()) {
        val /= 2;
#endif
        val += 1;
#ifndef DEBUG
    }
/*  ^ meta.function meta.block meta.block punctuation.section.block.end */
/*   ^ - meta.block meta.block */
#endif

#ifdef FOO
 /* <- keyword.control.import */
    int foobar
/*      ^^^^^^ - entity.name.function */
    ;

    if (val == -1) {
/*                 ^ meta.block meta.block punctuation.section.block.begin */
#else
 /* <- keyword.control.import */
    if (val == -2) {
/*                 ^ meta.block meta.block punctuation.section.block.begin */
#endif
 /* <- keyword.control.import */
        val += 1;
    }
/*  ^ meta.block punctuation.section.block.end */
/*   ^ - meta.block meta.block */

    return -1;
}
/* <- meta.function punctuation.section.block.end */
 /* <- - meta.function */

#if 0
/*  ^ constant.numeric */
int disabled_func() {
/*  ^ comment.block */
}
#endif

#if 1
/*  ^ constant.numeric */
int enabled_func() {}
/*  ^ entity.name.function */
#else
int disabled_func() {
/*  ^ comment.block */
}
#endif

#if 1
    int a = 1;
    #if 0
/*      ^ constant.numeric */
        int b = 2;
/*      ^ comment.block */
    #else
        int c = 3;
    #endif
#else
    int d = 4;
/*  ^ comment.block */
#endif

/////////////////////////////////////////////
// Matching various function definitions
/////////////////////////////////////////////

const int foo = 1;
/*        ^ - entity.name.function */
int a;
/*  ^ - entity.name.function */

int /* comment */ * myfunc
/* <- storage.type */
/*  ^ comment.block */
/*                ^ keyword.operator */
/*                  ^^^^^^ entity.name.function */
(int * a)
/* <- punctuation.section.group.begin */
/*   ^ keyword.operator */
/*      ^ punctuation.section.group.end */
{

}

void MyClass3::
foo() {
/* <- entity.name.function */
}

MyClass3::
~MyClass3() {
/* <- entity.name.function */
}

Glib::ustring Node::_getDragTip(GdkEventMotion */*event*/);
/*                                              ^^^^^^^^^ comment */

MACRO1 void * MACRO2 myfuncname () {
/*     ^ storage.type */
/*          ^ keyword.operator */
/*                   ^ entity.name.function */

    label:
/*  ^ entity.name.label */
/*       ^ punctuation.separator */
        do {
            break;
        } while(true);

    switch (a) {
        case 1: break;
/*            ^ punctuation.separator */
        case 100 - 10: break;
/*                   ^ punctuation.separator */
        default: break;
/*             ^ punctuation.separator */
    }

}

static const uint32_t * const MACRO funcname();
/* ^ storage.modifier */
/*     ^ storage.modifier */
/*           ^ support.type */
/*                    ^ keyword.operator */
/*                      ^ storage.modifier */
/*                                  ^ entity.name.function */

MACRO int
/*    ^ storage.type */
funcname2
/* ^ entity.name.function */
()
{

}

MACRO_CALL(int) macro_prefixed_func(){}
/*^^^^^^^^^^^^^ meta.function-call */
/*        ^^^^^ meta.group */
/*        ^ punctuation.section.group.begin */
/*            ^ punctuation.section.group.end */
/*              ^ entity.name.function */

int* return_type_pointer_no_space(){}
/*   ^ entity.name.function */

int& return_type_ref_no_space(){}
/*   ^ entity.name.function */

// Make sure there isn't an incorrect match here since this is not a valid
// function definition
int32
/* <- - entity.name.function */
() {}

/////////////////////////////////////////////
// Paths/identifiers
/////////////////////////////////////////////

void sayHi()
{
    std::cout << "Hi!\n";
/*     ^ punctuation.accessor */
    Print<int>(3);
/*  ^ variable.function */
    func_call(foo
/*  ^ meta.function-call */
    );

    if (::std::foo()) {}
/*      ^^^^^^^^^^ variable.function */
/*      ^^ punctuation.accessor */
/*           ^^ punctuation.accessor */

    foobaz<int>();
/*  ^^^^^^^^^^^^^ meta.function-call */
/*  ^^^^^^ variable.function */
/*        ^ punctuation.section.generic.begin */
/*            ^ punctuation.section.generic.end */
/*             ^^ meta.group */

    foobaz<>();
/*  ^^^^^^^^^^ meta.function-call */
/*  ^^^^^^ variable.function */
/*        ^ punctuation.section.generic.begin */
/*         ^ punctuation.section.generic.end */
/*          ^^ meta.group */

    foobaz<0>();
/*  ^^^^^^^^^^^ meta.function-call */
/*  ^^^^^^ variable.function */
/*        ^ punctuation.section.generic.begin */
/*         ^ constant.numeric */
/*          ^ punctuation.section.generic.end */
/*           ^^ meta.group */

    int a[5];
/*       ^^^ meta.brackets */
/*       ^ punctuation.section.brackets.begin */
/*         ^ punctuation.section.brackets.end */
}
