; This is a comprehensive AutoHotkey v2 script demonstrating various syntax elements.
; Comments in AutoHotkey start with a semicolon (;).
/*
    Block comments can be used for multi-line explanations.
    This script is designed to be run with AutoHotkey v2.
*/

/* Comment 
*/
#SingleInstance force
#!^+&<>*~$Key:Test
MsgBox testing... 20.30 + A_AppData + %2%
FileExist() %VarName% `n
/*
MsgBox "This line is commented out (disabled)."
MsgBox "Common mistake:" */ " this does not end the comment."
MsgBox "This line is commented out."
*/
MsgBox "This line is not commented out."
/* This is also valid, but no other code can share the line. */
MsgBox "This line is not commented out."
Tray:
{
WinWait, "Test" + 100 + LButton
RegDelete, HKCU, Software\Micro, TotalMouseEX
}

Add(x, y)
{
  return 0
}


#SingleInstance Force ; Ensures only one instance of this script runs at a time.
#Requires AutoHotkey v2.0 ; Explicitly states the required AHK version.
SetWorkingDir A_ScriptDir ; Sets the working directory to the script's directory.

; ##########################################################################
; #                            1. Hotkeys                                  #
; ##########################################################################

; Hotkeys are key combinations that trigger actions.
; Syntax: Hotkey::Action (for simple hotkeys)
;         Hotkey::{
;             ; Multiline actions
;         }

^j:: ; Ctrl + J hotkey
{
    MsgBox "You pressed Ctrl + J!"
}

#k:: ; Win + K hotkey (Windows Key is '#')
{
    MsgBox "You pressed Win + K!"
}

; Combinations (e.g., Ctrl+Shift+Alt+Z)
; ^ = Ctrl, + = Shift, ! = Alt, # = Win
^+!z:: ; Ctrl + Shift + Alt + Z
{
    MsgBox "Complex hotkey activated!"
}

; Window-specific hotkey (only active when Notepad is active)
#HotIf WinActive("ahk_class Notepad") ; Context-sensitive hotkeys
^s:: ; Ctrl + S in Notepad will show a message, not save
{
    MsgBox "Ctrl+S in Notepad! (This prevents saving.)"
}
#HotIf ; Turns off context-sensitive hotkeys

; Hotkey to reload the script (useful during development)
^r:: ; Ctrl + R
{
    Reload
}

; Hotkey to exit the script
^q:: ; Ctrl + Q
{
    MsgBox "Exiting script."
    ExitApp
}

; ##########################################################################
; #                           2. Hotstrings                                #
; ##########################################################################

; Hotstrings expand a typed string into another string or trigger actions.
; Syntax: ::typed_string::replacement_string
;         :*:typed_string::{ ... } (for multi-line actions without 'EndChars')

::btw::by the way
::afaik::as far as I know

; Hotstring with multiple lines (need to use a function or code block)
; :* will fire immediately without needing an ending character.
:*:sig::
{
    SendInput "{Raw}Kind regards,{Raw}{Enter 2}"
    SendInput "{Raw}Your Name{Raw}{Enter}"
    SendInput "{Raw}Your Company{Raw}"
}

; ##########################################################################
; #                           3. Variables                                 #
; ##########################################################################

; Variables are created implicitly by assignment. No declaration needed.
; Variable names are case-insensitive.

myString := "Hello, AutoHotkey!"
myNumber := 123
myBoolean := true

MsgBox "myString: " myString "`nmyNumber: " myNumber "`nmyBoolean: " myBoolean

; Accessing built-in variables (start with A_)
MsgBox "Current script path: " A_ScriptFullPath "`n"
    . "Current user: " A_UserName "`n"
    . "Current time: " A_Hour ":" A_Min ":" A_Sec

; ##########################################################################
; #                           4. Input/Output                              #
; ##########################################################################

; 4.1. Message Boxes (MsgBox)
MsgBox "This is a simple message box."
MsgBox "Title", "This is a message box with a custom title."
MsgBox "Icon", "Information message.", "I" ; 'I' for Information icon
MsgBox "Question", "Do you want to continue?", "YesNo" ; Yes/No buttons

; Check MsgBox button result
result := MsgBox("Do you really want to continue?", "Confirmation", "YesNo")
If (result == "Yes") {
    MsgBox "You clicked Yes."
} Else {
    MsgBox "You clicked No."
}

; 4.2. InputBox (get user input)
userInput := InputBox("Enter your name:", "Name Input")
If (userInput.Result == "OK") {
    MsgBox "You entered: " userInput.Value
} Else {
    MsgBox "Input cancelled."
}

; 4.3. ToolTip (temporary on-screen text)
ToolTip "This is a tooltip. It will disappear in 3 seconds."
Sleep 3000 ; Wait 3000 milliseconds (3 seconds)
ToolTip ; Clears the tooltip

; 4.4. OutputDebug (for debugging with tools like DebugView)
OutputDebug "Debug message: Script started."
; To see this, you need a debugger like Sysinternals DebugView running.

; ##########################################################################
; #                           5. Control Flow                              #
; ##########################################################################

; 5.1. If/Else If/Else
num := 25
If (num > 30) {
    MsgBox "Number is greater than 30."
} Else If (num < 20) {
    MsgBox "Number is less than 20."
} Else {
    MsgBox "Number is between 20 and 30 (inclusive)."
}

; String comparison (case-insensitive by default)
str1 := "Hello"
str2 := "hello"
If (str1 = str2) {
    MsgBox "Strings are equal (case-insensitive)."
}

; Case-sensitive string comparison
If (str1 == str2) { ; Use '==' for case-sensitive comparison
    MsgBox "Strings are equal (case-sensitive)."
} Else {
    MsgBox "Strings are NOT equal (case-sensitive)."
}

; 5.2. Loop (repeats a block of code)
MsgBox "Starting loop for 3 iterations."
Loop 3 {
    MsgBox "Loop iteration: " A_Index
}

; 5.3. For Loop (iterating over collections)
; Simple numeric for loop
MsgBox "Starting For loop (numeric)."
For i, val in [10, 20, 30] { ; i gets the index (1-based), val gets the value
    MsgBox "Index: " i ", Value: " val
}

; Loop over a map/object (associative array)
myMap := {Name: "Alice", Age: 30, City: "New York"}
MsgBox "Iterating over an object (map)."
For key, value in myMap {
    MsgBox "Key: " key ", Value: " value
}

; 5.4. GoSub (jumps to a subroutine - older style, prefer functions)
; Labels start with a colon.
MsgBox "Calling GoSub example."
GoSub MySubroutine
MsgBox "Back from subroutine."

MySubroutine:
{
    MsgBox "Inside MySubroutine."
    Return ; Returns from the subroutine
}

; ##########################################################################
; #                           6. Functions                                 #
; ##########################################################################

; Define a simple function
MyFunction() {
    MsgBox "This is a custom function."
}

; Call the function
MyFunction()

; Function with parameters and return value
AddNumbers(numA, numB) {
    local sum := numA + numB ; 'local' makes variables local to the function
    return sum
}

; Call the function and store result
total := AddNumbers(5, 7)
MsgBox "Sum of 5 and 7 is: " total

; Function with optional parameters and default values
Greet(name := "Guest") {
    MsgBox "Hello, " name "!"
}
Greet("Bob")
Greet() ; Uses default "Guest"

; ##########################################################################
; #                        7. String Manipulation                          #
; ##########################################################################

myString := "  AutoHotkey Scripting is powerful!  "
MsgBox "Original: '" myString "'"

; 7.1. String length
MsgBox "Length: " StrLen(myString)

; 7.2. Trim whitespace
trimmedString := StrTrim(myString)
MsgBox "Trimmed: '" trimmedString "'"

; 7.3. Substring
sub := SubStr(trimmedString, 14, 10) ; Start at 14th char, take 10 chars
MsgBox "Substring: '" sub "'"

; 7.4. Replace (StrReplace)
replacedString := StrReplace(trimmedString, "Scripting", "Automation")
MsgBox "Replaced: '" replacedString "'"

; 7.5. InStr (find substring)
pos := InStr(trimmedString, "powerful")
MsgBox "Position of 'powerful': " pos

; 7.6. Upper/Lower Case
upper := StrUpper(trimmedString)
lower := StrLower(trimmedString)
MsgBox "Uppercase: " upper "`nLowercase: " lower

; ##########################################################################
; #                         8. Arithmetic                                  #
; ##########################################################################

numA := 10
numB := 3

MsgBox "10 + 3 = " (numA + numB)
MsgBox "10 - 3 = " (numA - numB)
MsgBox "10 * 3 = " (numA * numB)
MsgBox "10 / 3 = " (numA / numB) ; Floating point division
MsgBox "10 // 3 = " (numA // numB) ; Integer division (floor)
MsgBox "10 % 3 = " (numA % numB) ; Modulo

; Increment/Decrement
i := 0
i++ ; i becomes 1
MsgBox "i after i++: " i
i-- ; i becomes 0
MsgBox "i after i--: " i

; ##########################################################################
; #                         9. File Operations                             #
; ##########################################################################

testFile := "test_file.txt"
testDir := "TestFolder"

; 9.1. FileAppend (write to file)
FileAppend "This is the first line." "`n", testFile
FileAppend "This is the second line." "`n", testFile
MsgBox "Appended text to " testFile

; 9.2. FileRead (read from file)
fileContent := FileRead(testFile)
MsgBox "Content of " testFile ":`n" fileContent

; 9.3. FileExist / DirExist
If FileExist(testFile) {
    MsgBox testFile " exists."
}
If !DirExist(testDir) { ; ! means NOT
    DirCreate testDir
    MsgBox "Created directory: " testDir
}

; 9.4. FileCopy / FileMove / FileDelete / DirRemove
FileCopy testFile, "copied_" testFile
MsgBox "Copied " testFile " to copied_" testFile

FileMove "copied_" testFile, "moved_" testFile
MsgBox "Moved copied_" testFile " to moved_" testFile

FileDelete "moved_" testFile
MsgBox "Deleted moved_" testFile

DirRemove testDir
MsgBox "Removed directory: " testDir

; ##########################################################################
; #                        10. Window Management                           #
; ##########################################################################

; Requires a Notepad window to be open for demonstration
MsgBox "Please open Notepad now for window management demo. Click OK when ready."

; Find Notepad window
If WinExist("ahk_class Notepad") {
    WinActivate "ahk_class Notepad"
    MsgBox "Activated Notepad."

    WinMaximize "ahk_class Notepad"
    MsgBox "Maximized Notepad."
    Sleep 1000

    WinMinimize "ahk_class Notepad"
    MsgBox "Minimized Notepad."
    Sleep 1000

    WinRestore "ahk_class Notepad"
    MsgBox "Restored Notepad."
    Sleep 1000

    WinMove "ahk_class Notepad", 100, 100 ; Move to X=100, Y=100
    MsgBox "Moved Notepad to (100, 100)."
    Sleep 1000

    ; WinClose "ahk_class Notepad" ; Uncomment to close Notepad
    ; MsgBox "Closed Notepad."
} Else {
    MsgBox "Notepad window not found. Skipping window management demo."
}

; ##########################################################################
; #                   11. Mouse and Keyboard Simulation                    #
; ##########################################################################

MsgBox "Ready to demonstrate mouse/keyboard simulation. Click OK."
Sleep 1000 ; Give yourself a second to move mouse away

; 11.1. Send (simulate key presses and text)
; SendRaw: sends text as is, no special characters interpreted
; SendInput: fastest and most reliable send mode
SendInput "This text was typed by AutoHotkey!{Enter}"
SendInput "It also supports special keys like {AppsKey} (Context Menu)."
; Try typing this: SendInput "{Ctrl Down}c{Ctrl Up}" ; Simulate Ctrl+C

; 11.2. Click (simulate mouse clicks)
; Click "x, y" or just "Click" for left click at current position
; You might need to adjust coordinates based on your screen resolution.
; Click 100, 200 ; Clicks at screen coordinates (100, 200)
; Click "Right" ; Right clicks at current mouse position
; MsgBox "Demonstrated click (commented out by default)."

; 11.3. MouseMove (move mouse cursor)
; MouseMove 500, 500, 0 ; Move to (500,500) instantly (speed 0)
; MsgBox "Demonstrated MouseMove (commented out by default)."

; ##########################################################################
; #                    12. Objects (Associative Arrays)                    #
; ##########################################################################

; Objects are key-value pairs (like dictionaries/maps in other languages)
employee := {FirstName: "John", LastName: "Doe", Age: 45, Active: true}

MsgBox "Employee: " employee.FirstName " " employee.LastName
employee.Age := 46 ; Change a property
MsgBox "New Age: " employee.Age

; Add a new property
employee.Department := "IT"
MsgBox "Department: " employee.Department

; Check if a key exists
If employee.Has("City") {
    MsgBox "Employee has a City."
} Else {
    MsgBox "Employee does NOT have a City."
}

; Remove a property
employee.Delete("Active")
If !employee.Has("Active") {
    MsgBox "Removed 'Active' property."
}

; ##########################################################################
; #                        13. Error Handling                              #
; ##########################################################################

; 13.1. Try/Catch (modern AHK v2 way)
MsgBox "Demonstrating Try/Catch for error handling."
Try {
    ; Attempt to perform an action that might fail, e.g., reading a non-existent file
    ; FileRead returns a blank string and throws an error if file doesn't exist by default.
    nonExistentContent := FileRead("non_existent_file_for_demo.txt")
    MsgBox "Content: " nonExistentContent ; This line won't be reached if error
} Catch as e { ; 'as e' captures the error object
    MsgBox "An error occurred:`n" e.Message "`n" e.What
}
MsgBox "Script continues after Try/Catch."

; --- End of Script ---
MsgBox "This concludes the AutoHotkey v2 Script Syntax Demonstration."

; Exit the script when it reaches the end.
ExitApp