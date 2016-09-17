// SYNTAX TEST "Packages/D/D.sublime-syntax"

module test;

import std.stdio;

shared static this()
// ^ storage.modifier
//      ^ storage.modifier
//             ^ entity.name
{
}

__gshared int a = 5_000;
// ^ storage.modifier
//         ^ storage.type
//                 ^ constant.numeric
shared int b = 5000;
// ^ storage.modifier
//      ^ storage.type
//              ^ constant.numeric

int c = 0x0;
//       ^ constant.numeric
int d = 0x0_00;
//          ^ constant.numeric
int e = .0_0;
//         ^ constant.numeric

template testTemplate(X)
{
    static if (is(X))
    // ^ storage.modifier
    //      ^ keyword.control
    {
        enum test = true;
        immutable int f = 0;
        // ^ storage.modifier
        //         ^ storage.type
    }
    else static if (!is(X))
    // ^ keyword.control
    //    ^ storage.modifier
    //          ^ keyword.control
    {
        enum test = true;
        string g = "test";
        // ^ storage.type
    }
    else
        enum test = false;
}

@safe pure nothrow @nogc unittest {}
// ^ storage.attribute
//     ^ storage.modifier
//            ^ storage.modifier
//                  ^ storage.attribute

auto takeByRef(ref int h)
// ^ storage.type
//              ^ storage.modifier
//                 ^ storage.type
{
    synchronized
    // ^ storage.modifier
    {
        h += 1;
    }
}

void main(char[][] args)
//    ^ meta.function entity.name.function
{
// <- meta.function meta.block
    foreach(argc, argv; args)
    {
    // <- meta.function meta.block meta.block
        auto cl = new CmdLin(argc, argv);
        //         ^ keyword.other
        //              ^ meta.block - keyword.type
        writefln(cl.argnum, cl.suffix, " arg: %s", cl.argv);
        delete cl;
        // ^ keyword.other
    }

    struct specs
    {
        int count, allocated;
    }

    specs argspecs(char[][] args)
    in {
    // ^ meta.function meta.block meta.function meta.block
        assert (args.length > 0);
    }
    out (result) {
        assert(result.count == CmdLin.total);
        //                       ^ meta.block - keyword.type
        assert(result.allocated > 0);
    }
    body {
    // ^ meta.function meta.block meta.function keyword.control.statement
    //   ^ meta.function meta.block meta.function meta.block
        specs* s = new specs;
         s.count = args.length;
         s.allocated = typeof(args).sizeof;
         foreach(argv; args)
            s.allocated += argv.length * typeof(argv[0]).sizeof;
        return *s;
    }

    char[] argcmsg  = "argc = %d";
    char[] allocmsg = "allocated = %d";
    writefln(argcmsg ~ ", " ~ allocmsg,
         argspecs(args).count,argspecs(args).allocated);
}
// ^ source.d

class CmdLin
{
// <- meta.class meta.block
    private {
        //  ^ meta.class meta.block meta.block
        int _argc;
        char[] _argv;
        static uint _totalc;
    }

  public:
    this(int argc, char[] argv)
    {
        _argc = argc + 1;
        _argv = argv;
        _totalc++;
    }

    ~this()
    // ^ entity.name
    {
    }

    int argnum()
    {
        struct Foo {
        }
        return _argc;
    }

    char[] argv()
    {
        return _argv;
    }

    wchar[] suffix()
    // ^ storage.type
    //          ^ entity.name
    {
        wchar[] suffix;
        switch(_argc)
        {
        case 1:
            suffix = "st";
            break;
        case 2:
            suffix = "nd";
            break;
        case 3:
            suffix = "rd";
            break;
        default:
             suffix = "th";
        }
        return suffix;
    }

    static typeof(_totalc) total()
    // ^ storage.modifier
    //      ^ storage.type
    //                      ^ entity.name
    {
        return _totalc;
    }

    invariant
    {
        assert(_argc > 0);
        assert(_totalc >= _argc);
    }
}
 // <- - meta.class

extern(C):
struct the_c_struct;

class MyClass {
// <- storage.type
}
