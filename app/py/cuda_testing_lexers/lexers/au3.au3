; This is a comprehensive AutoIt script demonstrating various syntax elements.
; Comments in AutoIt start with a semicolon (;).
; Or you can use a #Region ... #EndRegion block for code organization, which also acts like a comment if collapsed.

#Region Directives
; Directives configure script behavior.
#NoTrayIcon        ; Do not show the AutoIt tray icon. Useful for silent scripts.
#RequireAdmin      ; Request administrator privileges if not already running as admin.
#Include <MsgBoxConstants.au3> ; Include constants for MsgBox functions.
#EndRegion Directives

; ##########################################################################
; #                            1. Basic Structure                          #
; ##########################################################################

; AutoIt scripts execute sequentially from top to bottom.
; It's common to have an 'Exit' or 'While 1' loop at the end for GUI scripts.

ConsoleWrite("--- AutoIt Script Syntax Demonstration ---" & @CRLF) ; @CRLF is newline

; ##########################################################################
; #                           2. Variables                                 #
; ##########################################################################

; Variables are declared using Local, Global, or Dim.
; Local: Scoped to the function/block. Recommended.
; Global: Available throughout the entire script.
; Dim: Old syntax, acts like Local if inside function, Global if outside.

Local $myString = "Hello, AutoIt!"
Global $myNumber = 123
Local $myBoolean = True ; Case-insensitive True/False

MsgBox($MB_OK, "2. Variables", "myString: " & $myString & @CRLF & _
    "myNumber: " & $myNumber & @CRLF & _
    "myBoolean: " & $myBoolean) ; '&' concatenates strings

; Accessing built-in macros (start with @)
MsgBox($MB_OK, "Built-in Macros", "Current script path: " & @ScriptFullPath & @CRLF & _
    "Current user: " & @UserName & @CRLF & _
    "Current time: " & @HOUR & ":" & @MIN & ":" & @SEC)

; ##########################################################################
; #                           3. Input/Output                              #
; ##########################################################################

; 3.1. Message Boxes (MsgBox) - Already demonstrated above

; 3.2. InputBox (get user input)
Local $userInput = InputBox("3. Input/Output", "Enter your name:", "Default Name")
If Not @error Then ; @error is 0 if no error, non-zero if error (e.g., user cancelled)
    MsgBox($MB_OK, "InputBox Result", "You entered: " & $userInput)
Else
    MsgBox($MB_OK, "InputBox Result", "Input cancelled or error occurred.")
EndIf

; 3.3. ToolTip (temporary on-screen text)
ToolTip("This is a tooltip. It will disappear in 3 seconds.")
Sleep(3000) ; Wait 3000 milliseconds (3 seconds)
ToolTip("") ; Clears the tooltip

; 3.4. ConsoleWrite (for debugging or command-line output)
ConsoleWrite("Debug message: Script started." & @CRLF)

; ##########################################################################
; #                           4. Control Flow                              #
; ##########################################################################

; 4.1. If/ElseIf/Else
Local $num = 25
If $num > 30 Then
    MsgBox($MB_OK, "4. Control Flow", "Number is greater than 30.")
ElseIf $num < 20 Then
    MsgBox($MB_OK, "4. Control Flow", "Number is less than 20.")
Else
    MsgBox($MB_OK, "4. Control Flow", "Number is between 20 and 30 (inclusive).")
EndIf

; String comparison (case-insensitive by default)
Local $str1 = "Hello"
Local $str2 = "hello"
If $str1 = $str2 Then
    MsgBox($MB_OK, "String Comparison", "Strings are equal (case-insensitive).")
EndIf

; Case-sensitive string comparison (StringCompare)
If StringCompare($str1, $str2, 0) = 0 Then ; 0 for case-sensitive, 1 for case-insensitive
    MsgBox($MB_OK, "String Comparison", "Strings are equal (case-sensitive).")
Else
    MsgBox($MB_OK, "String Comparison", "Strings are NOT equal (case-sensitive).")
EndIf

; 4.2. Select/Case (similar to switch/case)
Local $color = InputBox("Select/Case", "Enter a color (red, green, blue):")
Select
    Case $color = "red" Or $color = "RED" ; Multiple patterns
        MsgBox($MB_OK, "Select/Case", "You chose red.")
    Case $color = "green"
        MsgBox($MB_OK, "Select/Case", "You chose green.")
    Case $color = "blue"
        MsgBox($MB_OK, "Select/Case", "You chose blue.")
    Case Else ; Default case
        MsgBox($MB_OK, "Select/Case", "Unknown color: " & $color)
EndSelect

; 4.3. For/Next Loop (numeric loop)
MsgBox($MB_OK, "For Loop", "Starting For loop for 3 iterations.")
For $i = 1 To 3 Step 1
    MsgBox($MB_OK, "For Loop", "Loop iteration: " & $i)
Next

; For In loop (iterating over arrays/collections)
Local $myArray = ["apple", "banana", "cherry"]
MsgBox($MB_OK, "For In Loop", "Iterating over array:")
For $item In $myArray
    MsgBox($MB_OK, "For In Loop", "Item: " & $item)
Next

; 4.4. While/WEnd Loop
Local $count = 3
MsgBox($MB_OK, "While Loop", "Starting While loop (counting down):")
While $count > 0
    MsgBox($MB_OK, "While Loop", "Countdown: " & $count)
    $count -= 1 ; $count = $count - 1
    Sleep(500)
WEnd
MsgBox($MB_OK, "While Loop", "Blast off!")

; 4.5. Do/Until Loop (guaranteed to run at least once)
Local $choice
Do
    $choice = InputBox("Do/Until Loop", "Enter 'quit' to exit this loop:", "")
Until $choice = "quit"
MsgBox($MB_OK, "Do/Until Loop", "You typed 'quit'. Exiting loop.")

; ##########################################################################
; #                           5. Functions                                 #
; ##########################################################################

; Define a simple function
Func MyFunction()
    MsgBox($MB_OK, "5. Functions", "This is a custom function.")
EndFunc ; All functions end with EndFunc

; Call the function
MyFunction()

; Function with parameters and return value
Func AddNumbers($numA, $numB)
    Local $sum = $numA + $numB
    Return $sum
EndFunc

; Call the function and store result
Local $total = AddNumbers(15, 25)
MsgBox($MB_OK, "Function Return", "Sum of 15 and 25 is: " & $total)

; Function with optional parameters and default values
Func Greet($name = "Guest") ; Default value for name if not provided
    MsgBox($MB_OK, "Function Optional Param", "Hello, " & $name & "!")
EndFunc
Greet("Bob")
Greet() ; Uses default "Guest"

; ##########################################################################
; #                           6. Arrays (and Maps)                         #
; ##########################################################################

; 6.1. Indexed Arrays (1-based by default, can be changed with Opt("ArrayBase", 0))
Local $indexedArray[3] ; Declare an array with 3 elements (indices 0, 1, 2 or 1, 2, 3)
$indexedArray[0] = "First" ; Assigning to elements
$indexedArray[1] = "Second"
$indexedArray[2] = "Third"

MsgBox($MB_OK, "Indexed Array", "All elements: " & @CRLF & _
    $indexedArray[0] & @CRLF & $indexedArray[1] & @CRLF & $indexedArray[2])
MsgBox($MB_OK, "Indexed Array", "First element: " & $indexedArray[0] & @CRLF & _
    "Number of elements: " & UBound($indexedArray)) ; UBound gets array size

; Dynamic array creation (without explicit size)
Local $dynamicArray = ["one", "two", "three", "four"]
MsgBox($MB_OK, "Dynamic Array", "Fourth element of dynamic array: " & $dynamicArray[3])

; 6.2. Maps (Associative Arrays / Dictionaries)
; Requires #include <Map.au3> in production code if using map functions, but built-in functions work without.
Local $myMap = Map("Name", "Alice", "Age", 30, "City", "New York")

MsgBox($MB_OK, "Map (Associative Array)", "Employee Name: " & $myMap["Name"])
$myMap["Age"] = 31 ; Update a value
MsgBox($MB_OK, "Map (Associative Array)", "Updated Age: " & $myMap["Age"])

; Add a new key-value pair
$myMap["Occupation"] = "Engineer"
MsgBox($MB_OK, "Map (Associative Array)", "Occupation: " & $myMap["Occupation"])

; Check if a key exists (MapHasKey)
; If MapHasKey($myMap, "Address") Then MsgBox($MB_OK, "Map", "Has Address")

; Iterate over a Map (using For In)
Local $mapOutput = "Map Contents:" & @CRLF
For $key In $myMap
    $mapOutput &= $key & ": " & $myMap[$key] & @CRLF
Next
MsgBox($MB_OK, "Map Iteration", $mapOutput)

; ##########################################################################
; #                        7. String Manipulation                          #
; ##########################################################################

Local $testString = "  AutoIt Scripting is powerful!  "
MsgBox($MB_OK, "7. String Manipulation", "Original: '" & $testString & "'")

; 7.1. String length
MsgBox($MB_OK, "String Length", "Length: " & StringLen($testString))

; 7.2. Trim whitespace
Local $trimmedString = StringStripWS($testString, 3) ; 3 = remove all leading/trailing
MsgBox($MB_OK, "Trimmed String", "Trimmed: '" & $trimmedString & "'")

; 7.3. Substring
Local $sub = StringMid($trimmedString, 13, 10) ; Start at 13th char (1-based), take 10 chars
MsgBox($MB_OK, "Substring", "Substring: '" & $sub & "'")

; 7.4. Replace (StringReplace)
Local $replacedString = StringReplace($trimmedString, "Scripting", "Automation")
MsgBox($MB_OK, "String Replace", "Replaced: '" & $replacedString & "'")

; 7.5. InStr (find substring)
Local $pos = StringInStr($trimmedString, "powerful")
MsgBox($MB_OK, "InStr", "Position of 'powerful': " & $pos)

; 7.6. Upper/Lower Case
Local $upper = StringUpper($trimmedString)
Local $lower = StringLower($trimmedString)
MsgBox($MB_OK, "Case Conversion", "Uppercase: " & $upper & @CRLF & "Lowercase: " & $lower)

; ##########################################################################
; #                         8. Arithmetic                                  #
; ##########################################################################

Local $numA = 10
Local $numB = 3

MsgBox($MB_OK, "8. Arithmetic", "10 + 3 = " & ($numA + $numB))
MsgBox($MB_OK, "8. Arithmetic", "10 - 3 = " & ($numA - $numB))
MsgBox($MB_OK, "8. Arithmetic", "10 * 3 = " & ($numA * $numB))
MsgBox($MB_OK, "8. Arithmetic", "10 / 3 = " & ($numA / $numB)) ; Floating point division
MsgBox($MB_OK, "8. Arithmetic", "10 \ 3 = " & ($numA \ $numB)) ; Integer division
MsgBox($MB_OK, "8. Arithmetic", "10 Mod 3 = " & ($numA Mod $numB)) ; Modulo

; Increment/Decrement
Local $i = 0
$i++ ; $i becomes 1
MsgBox($MB_OK, "Increment/Decrement", "$i after $i++: " & $i)
$i-- ; $i becomes 0
MsgBox($MB_OK, "Increment/Decrement", "$i after $i--: " & $i)

; ##########################################################################
; #                         9. File System Operations                      #
; ##########################################################################

Local $testFile = @ScriptDir & "\test_file.txt"
Local $testDir = @ScriptDir & "\TestFolder"

; 9.1. FileOpen / FileWriteLine / FileClose (write to file)
Local $fileHandle = FileOpen($testFile, 1) ; 1 = overwrite, 9 = append (Binary mode is different)
If $fileHandle = -1 Then
    MsgBox($MB_OK, "File Write Error", "Unable to open " & $testFile)
Else
    FileWriteLine($fileHandle, "This is the first line.")
    FileWriteLine($fileHandle, "This is the second line.")
    FileClose($fileHandle)
    MsgBox($MB_OK, "File Write", "Written text to " & $testFile)
EndIf

; 9.2. FileRead (read from file)
Local $fileContent = FileRead($testFile)
If @error Then
    MsgBox($MB_OK, "File Read Error", "Error reading " & $testFile & ": " & @error)
Else
    MsgBox($MB_OK, "File Read", "Content of " & $testFile & ":" & @CRLF & $fileContent)
EndIf

; 9.3. FileExists / DirExists
If FileExists($testFile) Then
    MsgBox($MB_OK, "File Exists", $testFile & " exists.")
EndIf
If Not DirExists($testDir) Then
    DirCreate($testDir)
    MsgBox($MB_OK, "Dir Create", "Created directory: " & $testDir)
EndIf

; 9.4. FileCopy / FileMove / FileDelete / DirRemove
FileCopy($testFile, @ScriptDir & "\copied_" & FileGetBaseName($testFile) & ".txt")
MsgBox($MB_OK, "File Copy", "Copied " & FileGetBaseName($testFile) & " to copied_")

FileMove(@ScriptDir & "\copied_" & FileGetBaseName($testFile) & ".txt", @ScriptDir & "\renamed_" & FileGetBaseName($testFile) & ".txt")
MsgBox($MB_OK, "File Move/Rename", "Moved/Renamed copied_ to renamed_")

FileDelete(@ScriptDir & "\renamed_" & FileGetBaseName($testFile) & ".txt")
MsgBox($MB_OK, "File Delete", "Deleted renamed_")

FileDelete($testFile) ; Clean up original file
MsgBox($MB_OK, "File Delete", "Deleted original test_file.txt")

DirRemove($testDir) ; DirRemove only removes empty directories by default
MsgBox($MB_OK, "Dir Remove", "Removed directory: " & $testDir)

; ##########################################################################
; #                        10. Process Management                          #
; ##########################################################################

MsgBox($MB_OK, "10. Process Management", "Launching Notepad. Please wait.")
Local $pid = Run("notepad.exe") ; Run a program, returns PID or 0 if failure
If $pid = 0 Then
    MsgBox($MB_OK, "Process Run Error", "Failed to run Notepad.")
Else
    MsgBox($MB_OK, "Process Run", "Notepad launched with PID: " & $pid)
    ProcessWait($pid, 5) ; Wait for Notepad to be active for up to 5 seconds
    If Not @error Then
        MsgBox($MB_OK, "Process Wait", "Notepad is running.")
    Else
        MsgBox($MB_OK, "Process Wait", "Notepad did not become active in 5 seconds.")
    EndIf

    Sleep(2000)
    ProcessClose($pid) ; Close the process by PID
    MsgBox($MB_OK, "Process Close", "Closed Notepad.")
EndIf

; ##########################################################################
; #                        11. Window Management                           #
; ##########################################################################

MsgBox($MB_OK, "11. Window Management", "Re-launching Notepad for window management demo. Click OK when ready.")
Run("notepad.exe")
WinWait("Untitled - Notepad", "", 10) ; Wait up to 10 seconds for window to appear

If WinExists("Untitled - Notepad") Then
    WinActivate("Untitled - Notepad")
    MsgBox($MB_OK, "WinActivate", "Activated Notepad.")

    WinMaximize("Untitled - Notepad")
    MsgBox($MB_OK, "WinMaximize", "Maximized Notepad.")
    Sleep(1000)

    WinMinimize("Untitled - Notepad")
    MsgBox($MB_OK, "WinMinimize", "Minimized Notepad.")
    Sleep(1000)

    WinRestore("Untitled - Notepad")
    MsgBox($MB_OK, "WinRestore", "Restored Notepad.")
    Sleep(1000)

    WinMove("Untitled - Notepad", 100, 100) ; Move to X=100, Y=100
    MsgBox($MB_OK, "WinMove", "Moved Notepad to (100, 100).")
    Sleep(1000)

    ; Get text from a control (e.g., the Edit control in Notepad)
    Local $notepadText = ControlGetText("Untitled - Notepad", "", "Edit1")
    If Not @error Then
        MsgBox($MB_OK, "ControlGetText", "Text in Notepad: " & @CRLF & $notepadText)
    Else
        MsgBox($MB_OK, "ControlGetText", "Could not get text from Notepad control. Error: " & @error)
    EndIf

    ; Set text in a control
    ControlSetText("Untitled - Notepad", "", "Edit1", "This text was set by AutoIt!")
    MsgBox($MB_OK, "ControlSetText", "Set text in Notepad.")
    Sleep(1000)

    ; ControlClick (clicks on a specific control, e.g., File menu)
    ; ControlClick("Untitled - Notepad", "", "[CLASS:ToolbarWindow32; INSTANCE:1]", "left", 1, 10, 10)
    ; MsgBox($MB_OK, "ControlClick", "Attempted to click on Notepad's toolbar (adjust coords if needed).")

    WinClose("Untitled - Notepad")
    MsgBox($MB_OK, "WinClose", "Closed Notepad.")
Else
    MsgBox($MB_OK, "Win Management", "Notepad window not found. Skipping window management demo.")
EndIf

; ##########################################################################
; #                   12. Mouse and Keyboard Simulation                    #
; ##########################################################################

MsgBox($MB_OK, "12. Mouse/Keyboard Sim", "Ready to demonstrate mouse/keyboard simulation. Click OK.")
Sleep(1000) ; Give yourself a second to move mouse away

; 12.1. Send (simulate key presses and text)
Send("This text was typed by AutoIt!{ENTER}")
Send("It also supports special keys like {APPSKEY} (Context Menu).")
; Send("^c") ; Simulate Ctrl+C

; 12.2. MouseClick (simulate mouse clicks)
; MouseClick("left", 100, 200, 1, 0) ; Clicks left button at (100, 200), 1 click, speed 0 (instant)
; MsgBox($MB_OK, "MouseClick", "Demonstrated click (commented out by default).")

; 12.3. MouseMove (move mouse cursor)
; MouseMove(500, 500, 0) ; Move to (500,500) instantly (speed 0)
; MsgBox($MB_OK, "MouseMove", "Demonstrated MouseMove (commented out by default).")

; ##########################################################################
; #                       13. Basic GUI Creation                           #
; ##########################################################################

MsgBox($MB_OK, "13. GUI Creation", "Demonstrating a simple GUI. Click OK.")

#Include <GUIConstantsEx.au3> ; Required for GUI event constants

Local $hGUI = GUICreate("My First AutoIt GUI", 300, 200) ; Create the GUI window
Local $idButton = GUICtrlCreateButton("Click Me!", 100, 80, 100, 30) ; Create a button

GUISetState(@SW_SHOW, $hGUI) ; Show the GUI

; Event loop for GUI
While 1
    Local $msg = GUIGetMsg()
    Select
        Case $msg = $GUI_EVENT_CLOSE ; User clicked the close button (X)
            ExitLoop
        Case $msg = $idButton ; User clicked our "Click Me!" button
            MsgBox($MB_OK, "GUI Event", "Button was clicked!")
    EndSelect
WEnd

GUIDelete($hGUI) ; Delete the GUI when loop exits
MsgBox($MB_OK, "GUI Creation", "GUI closed.")

; ##########################################################################
; #                      14. Error Handling (@error)                       #
; ##########################################################################

MsgBox($MB_OK, "14. Error Handling", "Demonstrating @error checking.")

; Attempt to open a non-existent file for reading
Local $errFileHandle = FileOpen("non_existent_file_for_demo.txt", 0) ; 0 for read mode
If $errFileHandle = -1 Then
    MsgBox($MB_OK, "Error Handling", "FileOpen failed as expected. @error = " & @error & @CRLF & _
        "Extended @error = " & @extended)
Else
    FileClose($errFileHandle)
EndIf

; ##########################################################################
; #                      15. Debugging (Trace)                             #
; ##########################################################################

; Use ConsoleWrite or the built-in Trace function (requires SciTE editor or similar)
; To use Trace: View -> Trace Output in SciTE.
; Opt("TrayIconDebug", 1) ; Enables debugging output to tray icon.

MsgBox($MB_OK, "15. Debugging", "Check the Console for ConsoleWrite messages.")
ConsoleWrite("This is a message from ConsoleWrite in the script." & @CRLF)

; --- End of Script ---
MsgBox($MB_OK, "End of Script", "This concludes the AutoIt Script Syntax Demonstration.")

; AutoIt scripts, when not using a GUI loop, will exit when they reach the end.
; For GUI scripts, the While 1 loop keeps them running until an exit event.
; If no GUI or hotkeys, the script will exit.
; Using a final MsgBox or Sleep helps keep the console open for viewing output if run directly.
Sleep(1000)