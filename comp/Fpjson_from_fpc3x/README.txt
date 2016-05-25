This package implements JSON support for FPC.

You might want to have a look at the lazarus jsonviewer tool, written using
fpJSON (see lazarus/tools/jsonviewer). It visualizes the fpJSON data and
shows how to program using fpjson.

JSON support consists of 3 parts:

unit fpJSON contains the data representation. Basically, it defines a set of
classes:

TJSONData
+- TJSONNumber
   +- TJSONIntegerNumber
   +- TJSONFloatNumber
   +- TJSONInt64Number
+- TJSONString
+- TJSONBoolean
+- TJSONNull
+- TJSONObject
+- TJSONArray

The TJSONData.JSONType property is an enumerated:
TJSONtype = (jtUnknown, jtNumber, jtString, jtBoolean, jtNull, jtArray, jtObject);

Which allows to determine the type of a value.

The following methods exist:

Procedure Clear;
  Clears the value. For arrays and objects, removes all elements/members
Function Clone : TJSONData;
  Creates an exact replica of the valye
property Count: Integer;
  For simple values this is zero, for complex values this is the number of
  elements/members. Read only.
property Items[Index: Integer]: TJSONData
  For simple values, reading this will result in an error. For complex
  values, this gives access to the members.
property Value: variant;
  The value as a variant. Only for simple values.
Property AsString : TJSONStringType:
   The value as a string. Only for simple values.
Property AsFloat : TJSONFloat;
  The value as a float (double). only for simple values.
Property AsInteger : Integer ;
  The value as an integer. only for simple values.
Property AsInt64 : Int64;
  The value as an 64-bit integer. only for simple values.
Property AsBoolean : Boolean ;
  The value as a boolean.
Property IsNull : Boolean ;
  Is the value Null ?
Property AsJSON : TJSONStringType 
  Return the value in JSON notation. For simple and complex values.

The TJSONArray type provides access to the elements in the array in the
following ways:

Property Types[Index : Integer] : TJSONType;
 Indexed access to the types of the elements in the array.
Property Nulls[Index : Integer] : Boolean 
 Checks if the Index-the element is NULL.
Property Integers[Index : Integer] : Integer
  Read/Write element values as integers.
Property Int64s[Index : Integer] : Int64 
  Read/Write element values as 64-bit integers.
Property Strings[Index : Integer] : TJSONStringType;
  Read/Write element values as strings.
Property Floats[Index : Integer] : TJSONFloat ;
  Read/Write element values as floats (doubles).
Property Booleans[Index : Integer] : Boolean;
  Read/Write element values as booleans.
Property Arrays[Index : Integer] : TJSONArray;
  Read/Write element values as arrays.
Property Objects[Index : Integer] : TJSONObject;
  Read/Write element values a strings

Reading an element as a type which is incompatible, will result in an
exception. For instance if element 5 is an object value, then the following
will result in an exception:
  i:=i+Array.Integers[5]

The TJSONObject type similarly provides access to the elements in the array
using the member names:
property Names[Index : Integer] : TJSONStringType;
  Indexed access to the member names.
property Elements[AName: string] : TJSONData;
  Read/Write a member as a raw TJSONData value.
Property Types[AName : String] : TJSONType Read GetTypes;
  Read/Write the type of a member.
Property Nulls[AName : String] : Boolean;
  Read/Write a member as a NULL value.
Property Floats[AName : String] : TJSONFloat;
  Read/Write a member as a float value (double)
Property Integers[AName : String] : Integer;
  Read/Write a member as an integer value
Property Int64s[AName : String] : Int64;
  Read/Write a member as an 64-bit integer value
Property Strings[AName : String] : TJSONStringType;
  Read/Write a member as a string value.
Property Booleans[AName : String] : Boolean;
  Read/Write a member as a boolean value.
Property Arrays[AName : String] : TJSONArray;
  Read/Write a member as an array value.
Property Objects[AName : String] : TJSONObject
  Read/Write a member as an object value.

Members can be added with the Add() call, which exists in various overloaded
forms:
   function Add(const AName: TJSONStringType; Const AValue): Integer;
Where the type of AVAlue is one of the supported types: 
integer, int64, double, string, TJSONArray or TJSONObject.

The Delete() call deletes an element from an array or object. The element is
freed.

Important remark:
The array and object classes own their members: the members are destroyed as
they are deleted. For this, the Extract() call exists: it removes an
element/member from the array/object, without destroying it.

Converting from string/stream to JSONData
=========================================

The fpjson unit contains a GetJSON() function which accepts a string or a
stream as a parameter. The function will parse the JSON in the stream and 
the return value is a TJSONData value corresponding to the JSON.
The function works with a callback, which is set by the JSONParser unit.
The JSONParser unit simply needs to be included in the project.

The parsing happens with default settings for the parser class.
You can override this behaviour by creating your own callback, 
and creating the parser with different settings.

Enumerator support
==================

the TJSONData class offers support for an enumerator, hence the 
For e in JSON do
construct can be used. The enumerator is a TJSONEnum value, which has 3
members:
Key : The key of the element 
     (name in TJSONObject, Index in TJSONArray, empty otherwise)
KeyNum: The index of the element.
     (Index in TJSONArray/TJSONObject, 0 otherwise)
Value : The value of the element
     (These are the member values for TJSONArray/TJSONObject, and is the
     element itself otherwise)

While the enumerator is looping, it is not allowed to change the content of
the array or object, and the value may not be freed.

Scanner/Parser
==============

The JSONSCanner unit contains a scanner for JSON data: TJSONScanner. 
Currently it does not support full unicode, only UTF-8 is supported.

The JSONParser unit contains the parser for JSON data: TJSONParser. 
It uses to scanner to read the tokens. The scanner is created automatically.


The Parse method will parse the data that was passed to the parser and will
return the JSON value.

Sample use:

Var
  P : TJSONParser;
  S : String;
  D : TJSONObject;

begin
  P:=TJSONParser.Create('{ "top": 10, "left": 20}');
  try
    D:=P.Parse as TJSONObject;
    Writeln('Top : ',D.Integers['top']);
    Writeln('Left : ',D.Integers['left']);
    D.free;
  Finally
    P.free;
  end;
end;

Note that the member names are case sensitive. 

As an alternative, a stream may be passed to the constructor of TJSONParser.

The scanner and parser support the 'Strict' property. 
Strict JSON syntax requires the member names of an object to be strings:
{ "top": 10, "left": 20}
However, due to the sloppy definition of Javascript (and hence JSON), 
the following type of JSON notation is frequently encountered:
{ top: 10, left: 20}
By default, this sloppy notation is accepted. Setting 'Strict' to true will
reject this.

A second effect of the Strict property is the requirement of " as a string
delimiter. A single quote is also often found in Javascript and JSON:
{ title: 'A nice title' }
By default, this is accepted. Setting 'Strict' to true will reject this.

Customizing the classes : Factory support
=========================================

The various classes created by the methods can be customized. 
This can be useful to create customized descendents, for example to attach
extra data to the various values. All instances of TJSONData are created
through the CreateJSON() functions, which use a set of customizable classes
to create the JSONData structures.

All functions which somehow create a new instance (clone, add, insert, parsing)
use the CreateJSON functions.

Which classes need to be created for a specific value is enumerated in

TJSONInstanceType = (jitUnknown, jitNumberInteger,jitNumberInt64,jitNumberFloat,
                       jitString, jitBoolean, jitNull, jitArray, jitObject);

when a Int64 value must be instantiated, the class identified with 
jitNumberInt64 is instantiated.

To customize the classes, the new class can be set using SetJSONInstanceType:

Procedure SetJSONInstanceType(AType : TJSONInstanceType; AClass : TJSONDataClass);
Function GetJSONInstanceType(AType : TJSONInstanceType) : TJSONDataClass;

The function checks whether sane classes are specified.;