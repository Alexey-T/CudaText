rem Batch file
@echo off
echo Params: %1 %2 %3
@rem
  @rem Comment
:label
::comment
somecmd Test Param
copy c:\%dirname%\*.pas "d:\dir name"
if %errorlevel% equ 2 echo File not found
if errorlevel 1 echo Error in copy action
goto label
ping 10.10.10.100
for /f "usebackq" %%i in (`PowerShell ^(Get-Date^).ToString^('yy'^)`) do set datestamp=%%i
set revision=%1
set package_version=11.0.1-%revision:~0,8%

@ECHO OFF
REM This is a comprehensive batch script demonstrating various syntax elements.
REM Comments can be made using 'REM' or '::'.
:: The @ECHO OFF command prevents commands from being displayed in the console as they execute.
TITLE Batch Script Syntax Demonstration

REM ##########################################################################
REM #                            1. Basic Commands                           #
REM ##########################################################################

ECHO.
ECHO --- 1. Basic Commands ---
ECHO Hello, World! This is a simple message.
PAUSE
REM PAUSE halts execution until a key is pressed.
CLS
REM CLS clears the console screen.

REM ##########################################################################
REM #                           2. Variables                                 #
REM ##########################################################################

ECHO.
ECHO --- 2. Variables ---

REM 2.1. Setting User-Defined Variables
SET myVariable=This is a user-defined variable.
ECHO Value of myVariable: %myVariable%

REM 2.2. Setting Variables with Spaces (use quotes, but don't include in reference)
SET anotherVar="Variable with spaces."
ECHO Value of anotherVar: %anotherVar%
REM To use the value without quotes, substring can be used:
ECHO Value without quotes: %anotherVar:~1,-1%

REM 2.3. Environment Variables (e.g., %PATH%, %USERNAME%, %DATE%, %TIME%)
ECHO Your username is: %USERNAME%
ECHO Current date: %DATE%
ECHO Current time: %TIME%
ECHO Current directory: %CD%
ECHO SystemDrive: %SystemDrive%

REM 2.4. Numeric Variables and Arithmetic (SET /A)
SET /A num1=10
SET /A num2=5
SET /A sum=%num1% + %num2%
SET /A difference=num1 - num2
SET /A product=num1 * num2
SET /A quotient=num1 / num2
SET /A remainder=num1 %% num2 REM %% is used for modulo in batch
ECHO.
ECHO Numeric Operations:
ECHO %num1% + %num2% = %sum%
ECHO %num1% - %num2% = %difference%
ECHO %num1% * %num2% = %product%
ECHO %num1% / %num2% = %quotient%
ECHO %num1% %% %num2% = %remainder%

PAUSE

REM ##########################################################################
REM #                          3. Input/Output                               #
REM ##########################################################################

ECHO.
ECHO --- 3. Input/Output ---

REM 3.1. User Input (SET /P)
SET /P userInput="Please enter your name: "
ECHO Hello, %userInput%!

REM 3.2. Redirection
ECHO This line will be written to a file. > output.txt
ECHO This line will be appended to the file. >> output.txt
ECHO. >> output.txt REM Add a blank line to the file
TYPE output.txt
REM TYPE displays the content of a text file.

REM 3.3. Redirecting Error Output (2>)
DIR nonExistentFolder 2> error_log.txt
ECHO.
ECHO An error message (if any) was redirected to error_log.txt.
TYPE error_log.txt
DEL error_log.txt output.txt REM Clean up created files

PAUSE

REM ##########################################################################
REM #                            4. Control Flow                             #
REM ##########################################################################

ECHO.
ECHO --- 4. Control Flow ---

REM 4.1. IF Conditions
SET /P myNumber="Enter a number (e.g., 10): "
IF "%myNumber%"=="10" (
    ECHO You entered 10!
) ELSE IF "%myNumber%" LSS "10" (
    ECHO You entered a number less than 10.
) ELSE (
    ECHO You entered a number greater than 10.
)

REM IF String comparison operators: EQU (equal), NEQ (not equal), LSS (less than), LEQ (less or equal), GTR (greater than), GEQ (greater or equal)
REM Use double quotes for string comparison to avoid issues with empty variables.

REM 4.2. IF EXIST / NOT EXIST
IF EXIST "%SystemDrive%\Windows" (
    ECHO Windows directory exists.
) ELSE (
    ECHO Windows directory not found.
)

REM 4.3. IF DEFINED / NOT DEFINED
SET definedVar=
IF DEFINED definedVar (
    ECHO definedVar is defined.
) ELSE (
    ECHO definedVar is NOT defined.
)

REM 4.4. IF %ERRORLEVEL% (checking return codes)
REM Some commands set %ERRORLEVEL% to 0 for success, non-zero for failure.
XCOPY "nonExistentFile.txt" "destination" >NUL 2>&1
IF %ERRORLEVEL% NEQ 0 (
    ECHO XCOPY failed (as expected, file doesn't exist).
)

PAUSE

ECHO.
ECHO --- 4.5. FOR Loops ---

REM 4.5.1. FOR /L (numeric loop)
ECHO Counting from 1 to 5:
FOR /L %%i IN (1,1,5) DO (
    ECHO %%i
)

REM 4.5.2. FOR /F (parsing files or strings)
REM Example: Reading lines from a file
ECHO Line 1 > temp_file.txt
ECHO Line 2 with space > temp_file.txt
ECHO Line 3 >> temp_file.txt

ECHO.
ECHO Reading temp_file.txt:
FOR /F "tokens=*" %%a IN (temp_file.txt) DO (
    ECHO File Content: %%a
)
DEL temp_file.txt

REM Example: Parsing a string
ECHO.
ECHO Parsing a string:
FOR /F "tokens=1,2,3 delims=," %%a IN ("apple,banana,cherry") DO (
    ECHO Fruit 1: %%a
    ECHO Fruit 2: %%b
    ECHO Fruit 3: %%c
)
REM delims specifies delimiters (default is space/tab)
REM tokens specifies which tokens to take. * means all remaining.

REM 4.5.3. FOR /D (directories)
ECHO.
ECHO Listing subdirectories in current folder:
FOR /D %%d IN (*) DO (
    ECHO Directory: %%d
)

REM 4.5.4. FOR /R (recursive directory traversal)
REM Find all .txt files recursively from current directory
ECHO.
ECHO Listing .txt files recursively from current directory:
FOR /R . %%f IN (*.txt) DO (
    ECHO Found: %%f
)

REM 4.5.5. FOR %%i IN (set) DO (command) - iterating over files
ECHO.
ECHO Listing .bat files in current directory:
FOR %%f IN (*.bat) DO (
    ECHO Batch file: %%f
)

PAUSE

ECHO.
ECHO --- 4.6. GOTO and CALL (Subroutines) ---

REM GOTO jumps to a label.
GOTO :MySubroutine

:MySubroutine
ECHO This is inside MySubroutine.
GOTO :ContinueScript

:ContinueScript
ECHO Back in the main script.

REM CALL executes another batch file or a label within the same script as a subroutine.
CALL :AnotherFunction "Parameter 1" "Parameter 2"
ECHO Returned from AnotherFunction.

GOTO :EOF REM Jumps to end of file, preventing further execution if placed before labels.

:AnotherFunction
ECHO.
ECHO This is AnotherFunction.
ECHO Received parameter 1: %1
ECHO Received parameter 2: %2
SET /A result=10 + 20
ECHO Calculated result in function: %result%
EXIT /B %result% REM EXIT /B returns from the subroutine and sets ERRORLEVEL to result.
REM Note: %ERRORLEVEL% would hold 'result' if RETURN or EXIT /B %result% were used.
REM For simple CALL of a label, variables changed in subroutine are retained.

REM ##########################################################################
REM #                          5. File Operations                            #
REM ##########################################################################

ECHO.
ECHO --- 5. File Operations ---

MD "TestFolder"
ECHO Created TestFolder.
MD "TestFolder\SubFolder"
ECHO Created SubFolder inside TestFolder.
ECHO Content for file1 > TestFolder\file1.txt
ECHO Content for file2 > TestFolder\SubFolder\file2.txt
ECHO.

DIR TestFolder
REM DIR lists files and directories.

CD TestFolder
ECHO Current directory changed to: %CD%
CD ..
ECHO Current directory changed back to: %CD%

COPY TestFolder\file1.txt copied_file.txt
ECHO Copied TestFolder\file1.txt to copied_file.txt.
REN copied_file.txt renamed_file.txt
ECHO Renamed copied_file.txt to renamed_file.txt.
MOVE TestFolder\SubFolder\file2.txt moved_file.txt
ECHO Moved file2.txt from SubFolder to current directory.

TYPE renamed_file.txt
TYPE moved_file.txt

DEL renamed_file.txt moved_file.txt
ECHO Deleted renamed_file.txt and moved_file.txt.

RD "TestFolder\SubFolder"
ECHO Removed TestFolder\SubFolder.
RD "TestFolder"
ECHO Removed TestFolder.

PAUSE

REM ##########################################################################
REM #                        6. System Information                           #
REM ##########################################################################

ECHO.
ECHO --- 6. System Information ---

REM SYSTEMINFO (can be slow)
REM ECHO.
REM ECHO System Information:
REM SYSTEMINFO | FINDSTR /B /C:"OS Name" /C:"OS Version" /C:"System Type"

REM WMIC (Windows Management Instrumentation Command-line)
ECHO.
ECHO Using WMIC to get CPU information:
WMIC CPU GET Name, NumberOfCores, NumberOfLogicalProcessors /FORMAT:LIST

PAUSE

REM ##########################################################################
REM #                        7. Advanced Features                            #
REM ##########################################################################

ECHO.
ECHO --- 7. Advanced Features ---

REM 7.1. String Manipulation (Variable Substring and Replacement)
SET myString="Hello, World! Welcome to the world of Batch!"
ECHO Original string: %myString%
ECHO Substring (first 5 chars): %myString:~0,5%
ECHO Substring (from 7th char to end): %myString:~7%
ECHO Replace "World" with "Universe": %myString:World=Universe%
ECHO Replace "world" (case-insensitive) with "galaxy": %myString:~1,-1:world=galaxy% REM Remove quotes first, then replace

REM 7.2. CHOICE command (User menu)
ECHO.
ECHO Choose an option:
CHOICE /C YNC /M "Do you want to continue (Y), No (N), or Cancel (C)?"
IF ERRORLEVEL 3 ECHO You chose Cancel.
IF ERRORLEVEL 2 ECHO You chose No.
IF ERRORLEVEL 1 ECHO You chose Yes.

REM ERRORLEVEL is set by CHOICE: 1 for first choice, 2 for second, etc.
REM Check ERRORLEVEL from highest to lowest.

REM 7.3. TIMEOUT (Delay execution)
ECHO.
ECHO Waiting for 3 seconds...
TIMEOUT /T 3 /NOBREAK >NUL
ECHO Done waiting.

PAUSE

REM ##########################################################################
REM #                       8. Command Line Arguments                        #
REM ##########################################################################

ECHO.
ECHO --- 8. Command Line Arguments ---
ECHO To test this, save the script as e.g., 'test.bat' and run from CMD:
ECHO   test.bat Arg1 "Arg 2" Arg3

ECHO Script name: %0
ECHO First argument: %1
ECHO Second argument: %2
ECHO All arguments: %*
ECHO Number of arguments: %~c0 REM This isn't reliable for all systems, better to count with a loop.
ECHO Drive and path of script: %~dp0
ECHO File name of script: %~nx0

PAUSE

REM ##########################################################################
REM #                      9. Special Variables                              #
REM ##########################################################################

ECHO.
ECHO --- 9. Special Variables ---
ECHO Current directory: %CD%
ECHO Date: %DATE%
ECHO Time: %TIME%
ECHO Error Level: %ERRORLEVEL%
ECHO Random number: %RANDOM% (Between 0 and 32767)
ECHO Command Processor path: %COMSPEC%
ECHO Number of arguments in a FOR loop: %%~nxF (e.g. filename and extension)

PAUSE

ECHO.
ECHO This concludes the Batch Script Syntax Demonstration.
ECHO Press any key to exit.
PAUSE >NUL