# Requires -Version 5.1 # Or a higher version like 7.0 for newer features
# Comment
#Requires -Modules @{Name}
param( [string] $infile = $(throw "Enter filename") )
$myname = "$infile.utf8"
get-content -Path $infile | out-file $outfile -encoding utf8
$null, $IsWindows, $event,  -ne, -lt, -ge, int32, datetime
$private:myname = "My name is `"Joe"", hi" + 'Joe''test'
$2:string = @"
sdfsdfsd
one
"@
fgfgdfgd
<# Block comment
dfsdfsd
#>
0xBEEF, 0b010, 10, 20kb, 1tb + 2n, 1.2e-3, 1.2tb 
100u, 100D, 100l, 100uL, 100us, 100uy, 100y
1e2, 1.e2, 0x1e2, 0x1e2L, 0x1e2D, 482D
482gb, 482ngb, 0x1e2lgb, 0b1011011, 0xFFFFFFFF, 0xFFFFFFFFu

${ugly variable name `{`} ...} = "12"
function Do-Something {
    Write-Host 'I did something!'
}

# 1. Variables and Data Types
# PowerShell uses "$" for variables. Type inference is common, but you can explicitly cast.
$stringVariable = "Hello, PowerShell!"
$integerVariable = 123
$floatVariable = 3.14159
$booleanTrue = $true
$booleanFalse = $false

# Arrays (collections of objects)
$arrayVariable = 10, "apple", 20, "banana" # Comma-separated
$anotherArray = @(1, 2, 3) # Using array literal

# Hashtables (key-value pairs, similar to Perl's hashes)
$hashTableVariable = @{
    Name = "Bob"
    Age  = 40
    City = "London"
}

# 2. Basic Output
Write-Host "String: $stringVariable" # Write-Host is for console output
Write-Output "Integer: $integerVariable" # Write-Output sends to the pipeline
$floatVariable | Write-Host -Format String -ForegroundColor Green # Piping to Write-Host with formatting

# 3. Array and Hashtable Access
Write-Host "First element of array: $($arrayVariable[0])"
Write-Host "Second element of array: $($arrayVariable[1])"
Write-Host "All array elements: $($arrayVariable -join ', ')" # Joining for output

Write-Host "Name from hashtable: $($hashTableVariable.Name)" # Dot notation for keys
Write-Host "City from hashtable: $($hashTableVariable['City'])" # Bracket notation also works
Write-Host "All hashtable keys: $($hashTableVariable.Keys -join ', ')"
Write-Host "All hashtable values: $($hashTableVariable.Values -join ', ')"

# 4. Operators
$sum = $integerVariable + 5 # Arithmetic
$concat = $stringVariable + " Welcome!" # String concatenation (addition operator)
$isEqual = ($integerVariable -eq 123) # Numeric equality (-eq, -ne, -gt, -lt, etc.)
$stringEqual = ($stringVariable -eq "Hello, PowerShell!") # String equality
$greaterThan = ($floatVariable -gt 3.0) # Comparison

Write-Host "Sum: $sum"
Write-Host "Concatenation: $concat"
Write-Host "Is equal (numeric): $isEqual"
Write-Host "Is equal (string): $stringEqual"
Write-Host "Greater than: $greaterThan"

# Logical operators: -and, -or, -not
if (($integerVariable -gt 100) -and ($booleanTrue)) {
    Write-Host "Condition met."
}

# 5. Control Structures: Conditionals
if ($integerVariable -gt 100) {
    Write-Host "Integer is greater than 100."
} elseif ($integerVariable -eq 100) {
    Write-Host "Integer is exactly 100."
} else {
    Write-Host "Integer is 100 or less."
}

# Ternary operator (PowerShell 7+)
$status = if ($booleanTrue) { "Active" } else { "Inactive" }
Write-Host "Status: $status"

# Switch statement
$color = "blue"
switch ($color) {
    "red"   { Write-Host "It's red." }
    "blue"  { Write-Host "It's blue." }
    default { Write-Host "Unknown color." }
}

# 6. Control Structures: Loops
# For loop
for ($i = 0; $i -lt 3; $i++) {
    Write-Host "For loop iteration: $i"
}

# ForEach-Object (pipeline processing)
# Get-Process | ForEach-Object { Write-Host "Process Name: $($_.ProcessName)" } # Example with a real cmdlet

# ForEach (iterating over collections)
foreach ($item in $arrayVariable) {
    Write-Host "ForEach item: $item"
}

# While loop
$count = 0
while ($count -lt 2) {
    Write-Host "While loop iteration: $count"
    $count++
}

# Do-While / Do-Until
$doCount = 0
do {
    Write-Host "Do-While iteration: $doCount"
    $doCount++
} while ($doCount -lt 2)

# 7. Functions (Cmdlets)
function Say-Greeting {
    param(
        [Parameter(Mandatory=$true)]
        [string]$Name,
        [string]$TimeOfDay = "day" # Default value
    )
    Write-Output "Good $TimeOfDay, $Name!"
}

$greetingMessage = Say-Greeting -Name "Charlie" -TimeOfDay "evening"
Write-Host $greetingMessage

function Add-Numbers {
    param (
        [int]$Num1,
        [int]$Num2
    )
    return $Num1 + $Num2
}
Write-Host "Sum of 10 and 15: $(Add-Numbers -Num1 10 -Num2 15)"

# 8. Script Blocks and the Pipeline
# Script block is a block of code, often used with cmdlets like ForEach-Object, Where-Object
$numbers = 1, 2, 3, 4, 5
$evenNumbers = $numbers | Where-Object { $_ % 2 -eq 0 } # $_ represents the current pipeline object
Write-Host "Even numbers: $($evenNumbers -join ', ')"

$numbers | ForEach-Object {
    $square = $_ * $_
    Write-Host "Square of $_ is $square"
}

# 9. Working with Objects (PowerShell's core strength)
# Get a list of running processes
$processes = Get-Process

# Select specific properties
$processes | Select-Object Name, Id, CPU

# Filter objects
$notepadProcesses = $processes | Where-Object { $_.Name -eq "notepad" }
Write-Host "Notepad processes found: $($notepadProcesses.Count)"

# Sort objects
$processes | Sort-Object CPU -Descending | Select-Object -First 5 Name, CPU

# 10. File System Operations
$filePath = ".\my_powershell_output.txt" # Current directory

# Write content to a file
"This is a line written by PowerShell." | Out-File $filePath
Add-Content -Path $filePath -Value "This is another line appended."
Write-Host "Content written to $filePath"

# Read content from a file
$fileContent = Get-Content -Path $filePath
Write-Host "Reading from file:"
$fileContent | ForEach-Object { Write-Host "Read: $_" }

# Remove a file
# Remove-Item -Path $filePath -Force # Uncomment to delete the file after demo

# 11. Error Handling
Try {
    Get-Service -Name "NonExistentService" -ErrorAction Stop # -ErrorAction Stop makes it a terminating error
} Catch {
    Write-Error "An error occurred: $($_.Exception.Message)"
} Finally {
    Write-Host "Error handling block finished."
}

# 12. Modules (using a built-in one for demonstration)
Import-Module Microsoft.PowerShell.Management # Usually auto-loaded, but explicit import
Get-Command -Module Microsoft.PowerShell.Management # List cmdlets from this module

# End of script
Write-Host "`nPowerShell syntax demonstration complete."