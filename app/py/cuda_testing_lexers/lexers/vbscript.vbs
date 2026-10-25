' 1. Comments
' This is a single-line comment.
Rem This is also a single-line comment (Rem stands for Remark).

' 2. Basic Output
' WScript.Echo is used for console output when run with cscript.exe,
' or a message box when run with wscript.exe.
WScript.Echo "--- VBScript Syntax Demonstration ---"
WScript.Echo "Hello, VBScript!"

' 3. Variables and Data Types
' Variables are declared using Dim. VBScript is dynamically typed (all variables are Variants).
Dim myString
Dim myInteger
Dim myFloat
Dim myBoolean

myString = "This is a VBScript string."
myInteger = 12345
myFloat = 6.789
myBoolean = True ' Or False

WScript.Echo "String variable: " & myString ' Use & for string concatenation
WScript.Echo "Integer variable: " & myInteger
WScript.Echo "Float variable: " & myFloat
WScript.Echo "Boolean variable: " & myBoolean

' Empty variable (default state)
Dim emptyVar
WScript.Echo "Empty variable value: " & IsEmpty(emptyVar) ' Returns True
WScript.Echo "Empty variable type: " & VarType(emptyVar) ' Returns 0 (vbEmpty)

' Nothing (used for object variables)
Dim myObject
Set myObject = Nothing ' Assign Nothing to an object variable

' 4. Constants
' Constants are declared with Const.
Const PI = 3.14159
Const MyName = "VBScript Demo"
WScript.Echo "Constant PI: " & PI

' 5. Arrays (Collections)
' Declare a fixed-size array
Dim fixedArray(2) ' 0, 1, 2 (3 elements)
fixedArray(0) = "Red"
fixedArray(1) = "Green"
fixedArray(2) = "Blue"
WScript.Echo "Fixed array element 0: " & fixedArray(0)

' Declare a dynamic array
Dim dynamicArray()
ReDim dynamicArray(1) ' Now has 2 elements (0 and 1)
dynamicArray(0) = "Apple"
dynamicArray(1) = "Banana"
WScript.Echo "Dynamic array element 1: " & dynamicArray(1)

ReDim Preserve dynamicArray(2) ' Preserve existing data, add one more element
dynamicArray(2) = "Cherry"
WScript.Echo "Dynamic array element 2 (after ReDim Preserve): " & dynamicArray(2)

' 6. Operators
Dim x, y, opResult
x = 10
y = 3

opResult = x + y
WScript.Echo "Addition (x + y): " & opResult

opResult = x / y ' Floating-point division
WScript.Echo "Division (x / y): " & opResult

opResult = x \ y ' Integer division
WScript.Echo "Integer Division (x \ y): " & opResult

opResult = x Mod y ' Modulo
WScript.Echo "Modulo (x Mod y): " & opResult

opResult = x ^ 2 ' Exponentiation
WScript.Echo "Exponentiation (x ^ 2): " & opResult

' Comparison operators: =, <>, <, >, <=, >=, Is (for objects)
WScript.Echo "Is x = 10? " & (x = 10)
WScript.Echo "Is x <> y? " & (x <> y) ' Not equal

' Logical operators: And, Or, Not, Xor, Eqv, Imp
Dim isRaining, isCold
isRaining = True
isCold = False
WScript.Echo "Is raining And cold? " & (isRaining And isCold)
WScript.Echo "Is raining Or cold? " & (isRaining Or isCold)

' 7. Control Structures: Conditionals
' If...Then...ElseIf...Else...End If
If myInteger > 10000 Then
    WScript.Echo "Integer is large."
ElseIf myInteger > 1000 Then
    WScript.Echo "Integer is medium."
Else
    WScript.Echo "Integer is small."
End If

' Select Case
Dim fruit
fruit = "Banana"
Select Case fruit
    Case "Apple"
        WScript.Echo "It's an apple."
    Case "Banana"
        WScript.Echo "It's a banana."
    Case Else ' Catches all other cases
        WScript.Echo "Unknown fruit."
End Select

' 8. Control Structures: Loops
' For...Next loop
For i = 0 To 2
    WScript.Echo "For loop iteration: " & i
Next

' For Each...Next loop (for collections/arrays)
For Each item In fixedArray
    WScript.Echo "For Each item: " & item
Next

' Do While...Loop
Dim counter
counter = 0
Do While counter < 2
    WScript.Echo "Do While loop iteration: " & counter
    counter = counter + 1
Loop

' Do...Loop While (ensures at least one execution)
counter = 0
Do
    WScript.Echo "Do...Loop While iteration: " & counter
    counter = counter + 1
Loop While counter < 2

' Do Until...Loop
counter = 0
Do Until counter >= 2
    WScript.Echo "Do Until loop iteration: " & counter
    counter = counter + 1
Loop

' Loop control: Exit For, Exit Do
For i = 0 To 4
    If i = 2 Then
        ' Continue equivalent: In VBScript, you often structure loops to skip
        ' by putting the rest of the code in an If block.
        ' There's no direct 'Continue' keyword like in other languages.
    ElseIf i = 4 Then
        Exit For ' Exit the loop
    Else
        WScript.Echo "Loop with control: " & i
    End If
Next

' 9. Procedures (Subroutines) and Functions
' Subroutine (does not return a value)
Sub Greet(name, Optional timeOfDay)
    If IsMissing(timeOfDay) Then
        timeOfDay = "day"
    End If
    WScript.Echo "Good " & timeOfDay & ", " & name & "!"
End Sub

' Call subroutine
Call Greet("David")
Call Greet("Emily", "evening")

' Function (returns a value)
Function AddNumbers(num1, num2)
    AddNumbers = num1 + num2 ' Assign return value to function name
End Function

Dim sumResult
sumResult = AddNumbers(70, 80)
WScript.Echo "Sum of 70 and 80: " & sumResult

' 10. String Manipulation
Dim fullSentence
fullSentence = "The quick brown fox jumps over the lazy dog."
WScript.Echo "Length of sentence: " & Len(fullSentence)
WScript.Echo "First 3 chars: " & Left(fullSentence, 3)
WScript.Echo "Last 3 chars: " & Right(fullSentence, 3)
WScript.Echo "Substring (from 5, 5 chars): " & Mid(fullSentence, 5, 5)
WScript.Echo "Position of 'fox': " & InStr(fullSentence, "fox")
WScript.Echo "Replace 'fox' with 'cat': " & Replace(fullSentence, "fox", "cat")
WScript.Echo "Uppercase: " & UCase(fullSentence)
WScript.Echo "Lowercase: " & LCase(fullSentence)

' 11. Working with Objects (e.g., FileSystemObject)
' Requires creating an object instance using CreateObject
Dim fso, ts
Set fso = CreateObject("Scripting.FileSystemObject")
Dim filePath
filePath = fso.GetAbsolutePathName(".\my_vbscript_output.txt") ' Get full path relative to script

' Write to a file
On Error Resume Next ' Enable basic error handling
Set ts = fso.CreateTextFile(filePath, True) ' True to overwrite if exists
If Err.Number <> 0 Then
    WScript.Echo "Error creating file: " & Err.Description
    Err.Clear
Else
    ts.WriteLine "This is the first line written by VBScript."
    ts.WriteLine "This is the second line."
    ts.Close
    WScript.Echo "Content written to " & filePath
End If
On Error GoTo 0 ' Turn off error handling

' Read from a file
On Error Resume Next
Set ts = fso.OpenTextFile(filePath, 1) ' 1 for reading
If Err.Number <> 0 Then
    WScript.Echo "Error opening file for read: " & Err.Description
    Err.Clear
Else
    WScript.Echo VbCrLf & "Reading from file:"
    Do While Not ts.AtEndOfStream
        WScript.Echo "Read: " & ts.ReadLine
    Loop
    ts.Close
End If
On Error GoTo 0

' Clean up object references
Set fso = Nothing
Set ts = Nothing

' 12. Error Handling (On Error Resume Next)
' VBScript's error handling is primarily done with On Error Resume Next
' and then checking the Err object.
On Error Resume Next ' Tells VBScript to continue execution on error
Dim num1, num2, divResult
num1 = 10
num2 = 0 ' This will cause a division by zero error

divResult = num1 / num2

If Err.Number <> 0 Then ' Check if an error occurred
    WScript.Echo "Caught an error (number " & Err.Number & "): " & Err.Description
    Err.Clear ' Clear the error object
Else
    WScript.Echo "Division result: " & divResult
End If
On Error GoTo 0 ' Resets error handling to default (script terminates on error)

WScript.Echo "--- VBScript Demonstration Complete ---"