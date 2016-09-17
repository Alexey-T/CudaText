// SYNTAX TEST "Packages/C#/C#.sublime-syntax"

class X
// ^ storage.modifier
{

    [Usage("Foo bar")]
    // ^ meta.method.attribute
    void Run([Usage("help text")] int x, int y)
    // ^ storage.type
    //    ^ entity.name.function
    //           ^ meta.method.attribute
    //                 ^ string.quoted.double
    //                             ^ storage.type
    //                                    ^ storage.type
    {
    }
}

string verbatim = @"This is a test "" of a verbatim string literal - C:\User"
//                ^^ string.quoted.double.literal punctuation.definition.string.begin
//                                 ^^ constant.character.escape
//                                                                     ^ string.quoted.double.literal - constant
//                                                                          ^ string.quoted.double.literal punctuation.definition.string.end

class A
{
   public A(int x, int y) {}
   // ^ storage.modifier
   //     ^ entity.name.function
}
class B: A
{
   public B(int x, int y): base(x + y, x - y) {}
   //                       ^ meta.method.base-call
   //                              ^ meta.method.base-call
}


public class GenericList<T>
{
    void Add(T input) { }
}
class TestGenericList
{
    private class ExampleClass { }
    static void Main()
    {
        GenericList<int> list1 = new GenericList<int>();
        //           ^ storage.type

        GenericList<string> list2 = new GenericList<string>();

        GenericList<ExampleClass> list3 = new GenericList<ExampleClass>();
    }
}

public partial class Employee
//     ^ storage.modifier
//                   ^ entity.name.type.class
{
    public void DoWork()
    {
    }
}

public class Coo
{
    public Object text = ObjectMaker.MakeSomeText ("In order to publish your text, you need to do some texty things 'Like this' and then say hello.");
    //                                                                                                                                              ^ - string
    public Vector curves;
    int Zoo()
    //  ^ entity.name.function
    {}
}

class Syntax
{
    public decimal decimal1 = 1.0m;
    //                        ^^^  constant.numeric.source.cs
    //                           ^ storage.type.numeric.cs
    public decimal decimal2 = 2.0M;
    //                        ^^^  constant.numeric.source.cs
    //                           ^ storage.type.numeric.cs
    public double double1 = 1.0d;
    //                      ^^^  constant.numeric.source.cs
    //                         ^ storage.type.numeric.cs
    public double double2 = 2.0D;
    //                      ^^^  constant.numeric.source.cs
    //                         ^ storage.type.numeric.cs
    public float float1 = 1.0f;
    //                    ^^^  constant.numeric.source.cs
    //                       ^ storage.type.numeric.cs
    public float float2 = 2.0F;
    //                    ^^^  constant.numeric.source.cs
    //                       ^ storage.type.numeric.cs
    public long long1 = 1l;
    //                  ^  constant.numeric.source.cs
    //                   ^ storage.type.numeric.cs
    public long long2 = 2L;
    //                  ^  constant.numeric.source.cs
    //                   ^ storage.type.numeric.cs
    public ulong ulong1 = 1ul;
    //                    ^   constant.numeric.source.cs
    //                     ^^ storage.type.numeric.cs
    public ulong ulong2 = 2UL;
    //                    ^   constant.numeric.source.cs
    //                     ^^ storage.type.numeric.cs
    public ulong ulong3 = 3lu;
    //                    ^   constant.numeric.source.cs
    //                     ^^ storage.type.numeric.cs
    public ulong ulong4 = 4LU;
    //                    ^   constant.numeric.source.cs
    //                     ^^ storage.type.numeric.cs
    public ulong ulong5 = 5uL;
    //                    ^   constant.numeric.source.cs
    //                     ^^ storage.type.numeric.cs
    public ulong ulong6 = 6Ul;
    //                    ^   constant.numeric.source.cs
    //                     ^^ storage.type.numeric.cs
    public ulong ulong7 = 7lU;
    //                    ^   constant.numeric.source.cs
    //                     ^^ storage.type.numeric.cs
    public ulong ulong8 = 8Lu;
    //                    ^   constant.numeric.source.cs
    //                     ^^ storage.type.numeric.cs
    public ulong bad = 1UU;
    //                  ^^ - storage.type.numeric.cs
}
