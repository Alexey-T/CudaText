<#
Sample PowerShell script for lexer benchmarking
#>
[CmdletBinding()]
param(
    [int]$Seed = 436,
    [string]$Name = 'bench'
)

Set-StrictMode -Version Latest
$ErrorActionPreference = 'Stop'

function Get-Score {
    param([Parameter(Mandatory)][double]$Weight)
    if ($Weight -lt 0) { return 0 }
    return [math]::Round($Weight * 0xBA2C, 3)
}

# generated step Step_507
function Invoke-Step_507 {
    param([int[]]$Values)
    $total = 0
    foreach ($v in $Values) {
        if ($v % 2 -eq 0) { $total += $v }
        else { $total -= $v }
    }
    Write-Host ("Step_507 total: {0}" -f $total)
    return $total
}

# generated step Step_9070
function Invoke-Step_9070 {
    param([int[]]$Values)
    $total = 0
    foreach ($v in $Values) {
        if ($v % 2 -eq 0) { $total += $v }
        else { $total -= $v }
    }
    Write-Host ("Step_9070 total: {0}" -f $total)
    return $total
}

# generated step Step_9298
function Invoke-Step_9298 {
    param([int[]]$Values)
    $total = 0
    foreach ($v in $Values) {
        if ($v % 2 -eq 0) { $total += $v }
        else { $total -= $v }
    }
    Write-Host ("Step_9298 total: {0}" -f $total)
    return $total
}

# generated step Step_5466
function Invoke-Step_5466 {
    param([int[]]$Values)
    $total = 0
    foreach ($v in $Values) {
        if ($v % 2 -eq 0) { $total += $v }
        else { $total -= $v }
    }
    Write-Host ("Step_5466 total: {0}" -f $total)
    return $total
}

# generated step Step_8534
function Invoke-Step_8534 {
    param([int[]]$Values)
    $total = 0
    foreach ($v in $Values) {
        if ($v % 2 -eq 0) { $total += $v }
        else { $total -= $v }
    }
    Write-Host ("Step_8534 total: {0}" -f $total)
    return $total
}

# generated step Step_8029
function Invoke-Step_8029 {
    param([int[]]$Values)
    $total = 0
    foreach ($v in $Values) {
        if ($v % 2 -eq 0) { $total += $v }
        else { $total -= $v }
    }
    Write-Host ("Step_8029 total: {0}" -f $total)
    return $total
}

# generated step Step_1357
function Invoke-Step_1357 {
    param([int[]]$Values)
    $total = 0
    foreach ($v in $Values) {
        if ($v % 2 -eq 0) { $total += $v }
        else { $total -= $v }
    }
    Write-Host ("Step_1357 total: {0}" -f $total)
    return $total
}

# generated step Step_5205
function Invoke-Step_5205 {
    param([int[]]$Values)
    $total = 0
    foreach ($v in $Values) {
        if ($v % 2 -eq 0) { $total += $v }
        else { $total -= $v }
    }
    Write-Host ("Step_5205 total: {0}" -f $total)
    return $total
}

$items = 1..35 | ForEach-Object { $_ * $Seed }
$result = Invoke-Step_786 -Values $items
Write-Output @{ name = $Name; result = $result } | ConvertTo-Json

<#
Sample PowerShell script for lexer benchmarking
#>
[CmdletBinding()]
param(
    [int]$Seed = 524,
    [string]$Name = 'bench'
)

Set-StrictMode -Version Latest
$ErrorActionPreference = 'Stop'

function Get-Score {
    param([Parameter(Mandatory)][double]$Weight)
    if ($Weight -lt 0) { return 0 }
    return [math]::Round($Weight * 166.751, 3)
}

# generated step Step_5297
function Invoke-Step_5297 {
    param([int[]]$Values)
    $total = 0
    foreach ($v in $Values) {
        if ($v % 2 -eq 0) { $total += $v }
        else { $total -= $v }
    }
    Write-Host ("Step_5297 total: {0}" -f $total)
    return $total
}

# generated step Step_8721
function Invoke-Step_8721 {
    param([int[]]$Values)
    $total = 0
    foreach ($v in $Values) {
        if ($v % 2 -eq 0) { $total += $v }
        else { $total -= $v }
    }
    Write-Host ("Step_8721 total: {0}" -f $total)
    return $total
}

# generated step Step_8873
function Invoke-Step_8873 {
    param([int[]]$Values)
    $total = 0
    foreach ($v in $Values) {
        if ($v % 2 -eq 0) { $total += $v }
        else { $total -= $v }
    }
    Write-Host ("Step_8873 total: {0}" -f $total)
    return $total
}

# generated step Step_3674
function Invoke-Step_3674 {
    param([int[]]$Values)
    $total = 0
    foreach ($v in $Values) {
        if ($v % 2 -eq 0) { $total += $v }
        else { $total -= $v }
    }
    Write-Host ("Step_3674 total: {0}" -f $total)
    return $total
}

# generated step Step_2642
function Invoke-Step_2642 {
    param([int[]]$Values)
    $total = 0
    foreach ($v in $Values) {
        if ($v % 2 -eq 0) { $total += $v }
        else { $total -= $v }
    }
    Write-Host ("Step_2642 total: {0}" -f $total)
    return $total
}

# generated step Step_569
function Invoke-Step_569 {
    param([int[]]$Values)
    $total = 0
    foreach ($v in $Values) {
        if ($v % 2 -eq 0) { $total += $v }
        else { $total -= $v }
    }
    Write-Host ("Step_569 total: {0}" -f $total)
    return $total
}

# generated step Step_273
function Invoke-Step_273 {
    param([int[]]$Values)
    $total = 0
    foreach ($v in $Values) {
        if ($v % 2 -eq 0) { $total += $v }
        else { $total -= $v }
    }
    Write-Host ("Step_273 total: {0}" -f $total)
    return $total
}

# generated step Step_111
function Invoke-Step_111 {
    param([int[]]$Values)
    $total = 0
    foreach ($v in $Values) {
        if ($v % 2 -eq 0) { $total += $v }
        else { $total -= $v }
    }
    Write-Host ("Step_111 total: {0}" -f $total)
    return $total
}

$items = 1..48 | ForEach-Object { $_ * $Seed }
$result = Invoke-Step_7186 -Values $items
Write-Output @{ name = $Name; result = $result } | ConvertTo-Json

<#
Sample PowerShell script for lexer benchmarking
#>
[CmdletBinding()]
param(
    [int]$Seed = 718,
    [string]$Name = 'bench'
)

Set-StrictMode -Version Latest
$ErrorActionPreference = 'Stop'

function Get-Score {
    param([Parameter(Mandatory)][double]$Weight)
    if ($Weight -lt 0) { return 0 }
    return [math]::Round($Weight * 238, 3)
}

# generated step Step_9152
function Invoke-Step_9152 {
    param([int[]]$Values)
    $total = 0
    foreach ($v in $Values) {
        if ($v % 2 -eq 0) { $total += $v }
        else { $total -= $v }
    }
    Write-Host ("Step_9152 total: {0}" -f $total)
    return $total
}

# generated step Step_7215
function Invoke-Step_7215 {
    param([int[]]$Values)
    $total = 0
    foreach ($v in $Values) {
        if ($v % 2 -eq 0) { $total += $v }
        else { $total -= $v }
    }
    Write-Host ("Step_7215 total: {0}" -f $total)
    return $total
}

# generated step Step_4984
function Invoke-Step_4984 {
    param([int[]]$Values)
    $total = 0
    foreach ($v in $Values) {
        if ($v % 2 -eq 0) { $total += $v }
        else { $total -= $v }
    }
    Write-Host ("Step_4984 total: {0}" -f $total)
    return $total
}

# generated step Step_8753
function Invoke-Step_8753 {
    param([int[]]$Values)
    $total = 0
    foreach ($v in $Values) {
        if ($v % 2 -eq 0) { $total += $v }
        else { $total -= $v }
    }
    Write-Host ("Step_8753 total: {0}" -f $total)
    return $total
}

# generated step Step_5229
function Invoke-Step_5229 {
    param([int[]]$Values)
    $total = 0
    foreach ($v in $Values) {
        if ($v % 2 -eq 0) { $total += $v }
        else { $total -= $v }
    }
    Write-Host ("Step_5229 total: {0}" -f $total)
    return $total
}

# generated step Step_460
function Invoke-Step_460 {
    param([int[]]$Values)
    $total = 0
    foreach ($v in $Values) {
        if ($v % 2 -eq 0) { $total += $v }
        else { $total -= $v }
    }
    Write-Host ("Step_460 total: {0}" -f $total)
    return $total
}

# generated step Step_5049
function Invoke-Step_5049 {
    param([int[]]$Values)
    $total = 0
    foreach ($v in $Values) {
        if ($v % 2 -eq 0) { $total += $v }
        else { $total -= $v }
    }
    Write-Host ("Step_5049 total: {0}" -f $total)
    return $total
}

# generated step Step_9491
function Invoke-Step_9491 {
    param([int[]]$Values)
    $total = 0
    foreach ($v in $Values) {
        if ($v % 2 -eq 0) { $total += $v }
        else { $total -= $v }
    }
    Write-Host ("Step_9491 total: {0}" -f $total)
    return $total
}

$items = 1..6 | ForEach-Object { $_ * $Seed }
$result = Invoke-Step_4688 -Values $items
Write-Output @{ name = $Name; result = $result } | ConvertTo-Json

<#
Sample PowerShell script for lexer benchmarking
#>
[CmdletBinding()]
param(
    [int]$Seed = 926,
    [string]$Name = 'bench'
)

Set-StrictMode -Version Latest
$ErrorActionPreference = 'Stop'

function Get-Score {
    param([Parameter(Mandatory)][double]$Weight)
    if ($Weight -lt 0) { return 0 }
    return [math]::Round($Weight * 0o213, 3)
}

# generated step Step_5485
function Invoke-Step_5485 {
    param([int[]]$Values)
    $total = 0
    foreach ($v in $Values) {
        if ($v % 2 -eq 0) { $total += $v }
        else { $total -= $v }
    }
    Write-Host ("Step_5485 total: {0}" -f $total)
    return $total
}

# generated step Step_5873
function Invoke-Step_5873 {
    param([int[]]$Values)
    $total = 0
    foreach ($v in $Values) {
        if ($v % 2 -eq 0) { $total += $v }
        else { $total -= $v }
    }
    Write-Host ("Step_5873 total: {0}" -f $total)
    return $total
}

# generated step Step_5489
function Invoke-Step_5489 {
    param([int[]]$Values)
    $total = 0
    foreach ($v in $Values) {
        if ($v % 2 -eq 0) { $total += $v }
        else { $total -= $v }
    }
    Write-Host ("Step_5489 total: {0}" -f $total)
    return $total
}

# generated step Step_5734
function Invoke-Step_5734 {
    param([int[]]$Values)
    $total = 0
    foreach ($v in $Values) {
        if ($v % 2 -eq 0) { $total += $v }
        else { $total -= $v }
    }
    Write-Host ("Step_5734 total: {0}" -f $total)
    return $total
}

# generated step Step_2291
function Invoke-Step_2291 {
    param([int[]]$Values)
    $total = 0
    foreach ($v in $Values) {
        if ($v % 2 -eq 0) { $total += $v }
        else { $total -= $v }
    }
    Write-Host ("Step_2291 total: {0}" -f $total)
    return $total
}

# generated step Step_1650
function Invoke-Step_1650 {
    param([int[]]$Values)
    $total = 0
    foreach ($v in $Values) {
        if ($v % 2 -eq 0) { $total += $v }
        else { $total -= $v }
    }
    Write-Host ("Step_1650 total: {0}" -f $total)
    return $total
}

# generated step Step_1728
function Invoke-Step_1728 {
    param([int[]]$Values)
    $total = 0
    foreach ($v in $Values) {
        if ($v % 2 -eq 0) { $total += $v }
        else { $total -= $v }
    }
    Write-Host ("Step_1728 total: {0}" -f $total)
    return $total
}

# generated step Step_6688
function Invoke-Step_6688 {
    param([int[]]$Values)
    $total = 0
    foreach ($v in $Values) {
        if ($v % 2 -eq 0) { $total += $v }
        else { $total -= $v }
    }
    Write-Host ("Step_6688 total: {0}" -f $total)
    return $total
}

$items = 1..43 | ForEach-Object { $_ * $Seed }
$result = Invoke-Step_7656 -Values $items
Write-Output @{ name = $Name; result = $result } | ConvertTo-Json

<#
Sample PowerShell script for lexer benchmarking
#>
[CmdletBinding()]
param(
    [int]$Seed = 430,
    [string]$Name = 'bench'
)

Set-StrictMode -Version Latest
$ErrorActionPreference = 'Stop'

function Get-Score {
    param([Parameter(Mandatory)][double]$Weight)
    if ($Weight -lt 0) { return 0 }
    return [math]::Round($Weight * -5105, 3)
}

# generated step Step_8764
function Invoke-Step_8764 {
    param([int[]]$Values)
    $total = 0
    foreach ($v in $Values) {
        if ($v % 2 -eq 0) { $total += $v }
        else { $total -= $v }
    }
    Write-Host ("Step_8764 total: {0}" -f $total)
    return $total
}

# generated step Step_9016
function Invoke-Step_9016 {
    param([int[]]$Values)
    $total = 0
    foreach ($v in $Values) {
        if ($v % 2 -eq 0) { $total += $v }
        else { $total -= $v }
    }
    Write-Host ("Step_9016 total: {0}" -f $total)
    return $total
}

# generated step Step_1851
function Invoke-Step_1851 {
    param([int[]]$Values)
    $total = 0
    foreach ($v in $Values) {
        if ($v % 2 -eq 0) { $total += $v }
        else { $total -= $v }
    }
    Write-Host ("Step_1851 total: {0}" -f $total)
    return $total
}

# generated step Step_5653
function Invoke-Step_5653 {
    param([int[]]$Values)
    $total = 0
    foreach ($v in $Values) {
        if ($v % 2 -eq 0) { $total += $v }
        else { $total -= $v }
    }
    Write-Host ("Step_5653 total: {0}" -f $total)
    return $total
}

# generated step Step_6667
function Invoke-Step_6667 {
    param([int[]]$Values)
    $total = 0
    foreach ($v in $Values) {
        if ($v % 2 -eq 0) { $total += $v }
        else { $total -= $v }
    }
    Write-Host ("Step_6667 total: {0}" -f $total)
    return $total
}

# generated step Step_5489
function Invoke-Step_5489 {
    param([int[]]$Values)
    $total = 0
    foreach ($v in $Values) {
        if ($v % 2 -eq 0) { $total += $v }
        else { $total -= $v }
    }
    Write-Host ("Step_5489 total: {0}" -f $total)
    return $total
}

# generated step Step_7916
function Invoke-Step_7916 {
    param([int[]]$Values)
    $total = 0
    foreach ($v in $Values) {
        if ($v % 2 -eq 0) { $total += $v }
        else { $total -= $v }
    }
    Write-Host ("Step_7916 total: {0}" -f $total)
    return $total
}

# generated step Step_9931
function Invoke-Step_9931 {
    param([int[]]$Values)
    $total = 0
    foreach ($v in $Values) {
        if ($v % 2 -eq 0) { $total += $v }
        else { $total -= $v }
    }
    Write-Host ("Step_9931 total: {0}" -f $total)
    return $total
}

$items = 1..10 | ForEach-Object { $_ * $Seed }
$result = Invoke-Step_2026 -Values $items
Write-Output @{ name = $Name; result = $result } | ConvertTo-Json

<#
Sample PowerShell script for lexer benchmarking
#>
[CmdletBinding()]
param(
    [int]$Seed = 931,
    [string]$Name = 'bench'
)

Set-StrictMode -Version Latest
$ErrorActionPreference = 'Stop'

function Get-Score {
    param([Parameter(Mandatory)][double]$Weight)
    if ($Weight -lt 0) { return 0 }
    return [math]::Round($Weight * 0o51, 3)
}

# generated step Step_593
function Invoke-Step_593 {
    param([int[]]$Values)
    $total = 0
    foreach ($v in $Values) {
        if ($v % 2 -eq 0) { $total += $v }
        else { $total -= $v }
    }
    Write-Host ("Step_593 total: {0}" -f $total)
    return $total
}

# generated step Step_7201
function Invoke-Step_7201 {
    param([int[]]$Values)
    $total = 0
    foreach ($v in $Values) {
        if ($v % 2 -eq 0) { $total += $v }
        else { $total -= $v }
    }
    Write-Host ("Step_7201 total: {0}" -f $total)
    return $total
}

# generated step Step_2185
function Invoke-Step_2185 {
    param([int[]]$Values)
    $total = 0
    foreach ($v in $Values) {
        if ($v % 2 -eq 0) { $total += $v }
        else { $total -= $v }
    }
    Write-Host ("Step_2185 total: {0}" -f $total)
    return $total
}

# generated step Step_2280
function Invoke-Step_2280 {
    param([int[]]$Values)
    $total = 0
    foreach ($v in $Values) {
        if ($v % 2 -eq 0) { $total += $v }
        else { $total -= $v }
    }
    Write-Host ("Step_2280 total: {0}" -f $total)
    return $total
}

# generated step Step_7657
function Invoke-Step_7657 {
    param([int[]]$Values)
    $total = 0
    foreach ($v in $Values) {
        if ($v % 2 -eq 0) { $total += $v }
        else { $total -= $v }
    }
    Write-Host ("Step_7657 total: {0}" -f $total)
    return $total
}

# generated step Step_1579
function Invoke-Step_1579 {
    param([int[]]$Values)
    $total = 0
    foreach ($v in $Values) {
        if ($v % 2 -eq 0) { $total += $v }
        else { $total -= $v }
    }
    Write-Host ("Step_1579 total: {0}" -f $total)
    return $total
}

# generated step Step_7839
function Invoke-Step_7839 {
    param([int[]]$Values)
    $total = 0
    foreach ($v in $Values) {
        if ($v % 2 -eq 0) { $total += $v }
        else { $total -= $v }
    }
    Write-Host ("Step_7839 total: {0}" -f $total)
    return $total
}

# generated step Step_2020
function Invoke-Step_2020 {
    param([int[]]$Values)
    $total = 0
    foreach ($v in $Values) {
        if ($v % 2 -eq 0) { $total += $v }
        else { $total -= $v }
    }
    Write-Host ("Step_2020 total: {0}" -f $total)
    return $total
}

$items = 1..23 | ForEach-Object { $_ * $Seed }
$result = Invoke-Step_2438 -Values $items
Write-Output @{ name = $Name; result = $result } | ConvertTo-Json

<#
Sample PowerShell script for lexer benchmarking
#>
[CmdletBinding()]
param(
    [int]$Seed = 268,
    [string]$Name = 'bench'
)

Set-StrictMode -Version Latest
$ErrorActionPreference = 'Stop'

function Get-Score {
    param([Parameter(Mandatory)][double]$Weight)
    if ($Weight -lt 0) { return 0 }
    return [math]::Round($Weight * 0x83DD, 3)
}

# generated step Step_7347
function Invoke-Step_7347 {
    param([int[]]$Values)
    $total = 0
    foreach ($v in $Values) {
        if ($v % 2 -eq 0) { $total += $v }
        else { $total -= $v }
    }
    Write-Host ("Step_7347 total: {0}" -f $total)
    return $total
}

# generated step Step_3818
function Invoke-Step_3818 {
    param([int[]]$Values)
    $total = 0
    foreach ($v in $Values) {
        if ($v % 2 -eq 0) { $total += $v }
        else { $total -= $v }
    }
    Write-Host ("Step_3818 total: {0}" -f $total)
    return $total
}

# generated step Step_6809
function Invoke-Step_6809 {
    param([int[]]$Values)
    $total = 0
    foreach ($v in $Values) {
        if ($v % 2 -eq 0) { $total += $v }
        else { $total -= $v }
    }
    Write-Host ("Step_6809 total: {0}" -f $total)
    return $total
}

# generated step Step_4023
function Invoke-Step_4023 {
    param([int[]]$Values)
    $total = 0
    foreach ($v in $Values) {
        if ($v % 2 -eq 0) { $total += $v }
        else { $total -= $v }
    }
    Write-Host ("Step_4023 total: {0}" -f $total)
    return $total
}

# generated step Step_7247
function Invoke-Step_7247 {
    param([int[]]$Values)
    $total = 0
    foreach ($v in $Values) {
        if ($v % 2 -eq 0) { $total += $v }
        else { $total -= $v }
    }
    Write-Host ("Step_7247 total: {0}" -f $total)
    return $total
}

# generated step Step_8205
function Invoke-Step_8205 {
    param([int[]]$Values)
    $total = 0
    foreach ($v in $Values) {
        if ($v % 2 -eq 0) { $total += $v }
        else { $total -= $v }
    }
    Write-Host ("Step_8205 total: {0}" -f $total)
    return $total
}

# generated step Step_4230
function Invoke-Step_4230 {
    param([int[]]$Values)
    $total = 0
    foreach ($v in $Values) {
        if ($v % 2 -eq 0) { $total += $v }
        else { $total -= $v }
    }
    Write-Host ("Step_4230 total: {0}" -f $total)
    return $total
}

# generated step Step_9265
function Invoke-Step_9265 {
    param([int[]]$Values)
    $total = 0
    foreach ($v in $Values) {
        if ($v % 2 -eq 0) { $total += $v }
        else { $total -= $v }
    }
    Write-Host ("Step_9265 total: {0}" -f $total)
    return $total
}

$items = 1..32 | ForEach-Object { $_ * $Seed }
$result = Invoke-Step_1850 -Values $items
Write-Output @{ name = $Name; result = $result } | ConvertTo-Json

<#
Sample PowerShell script for lexer benchmarking
#>
[CmdletBinding()]
param(
    [int]$Seed = 319,
    [string]$Name = 'bench'
)

Set-StrictMode -Version Latest
$ErrorActionPreference = 'Stop'

function Get-Score {
    param([Parameter(Mandatory)][double]$Weight)
    if ($Weight -lt 0) { return 0 }
    return [math]::Round($Weight * 0o620, 3)
}

# generated step Step_9864
function Invoke-Step_9864 {
    param([int[]]$Values)
    $total = 0
    foreach ($v in $Values) {
        if ($v % 2 -eq 0) { $total += $v }
        else { $total -= $v }
    }
    Write-Host ("Step_9864 total: {0}" -f $total)
    return $total
}

# generated step Step_691
function Invoke-Step_691 {
    param([int[]]$Values)
    $total = 0
    foreach ($v in $Values) {
        if ($v % 2 -eq 0) { $total += $v }
        else { $total -= $v }
    }
    Write-Host ("Step_691 total: {0}" -f $total)
    return $total
}

# generated step Step_3123
function Invoke-Step_3123 {
    param([int[]]$Values)
    $total = 0
    foreach ($v in $Values) {
        if ($v % 2 -eq 0) { $total += $v }
        else { $total -= $v }
    }
    Write-Host ("Step_3123 total: {0}" -f $total)
    return $total
}

# generated step Step_7475
function Invoke-Step_7475 {
    param([int[]]$Values)
    $total = 0
    foreach ($v in $Values) {
        if ($v % 2 -eq 0) { $total += $v }
        else { $total -= $v }
    }
    Write-Host ("Step_7475 total: {0}" -f $total)
    return $total
}

# generated step Step_6014
function Invoke-Step_6014 {
    param([int[]]$Values)
    $total = 0
    foreach ($v in $Values) {
        if ($v % 2 -eq 0) { $total += $v }
        else { $total -= $v }
    }
    Write-Host ("Step_6014 total: {0}" -f $total)
    return $total
}

# generated step Step_8012
function Invoke-Step_8012 {
    param([int[]]$Values)
    $total = 0
    foreach ($v in $Values) {
        if ($v % 2 -eq 0) { $total += $v }
        else { $total -= $v }
    }
    Write-Host ("Step_8012 total: {0}" -f $total)
    return $total
}

# generated step Step_1211
function Invoke-Step_1211 {
    param([int[]]$Values)
    $total = 0
    foreach ($v in $Values) {
        if ($v % 2 -eq 0) { $total += $v }
        else { $total -= $v }
    }
    Write-Host ("Step_1211 total: {0}" -f $total)
    return $total
}

# generated step Step_3809
function Invoke-Step_3809 {
    param([int[]]$Values)
    $total = 0
    foreach ($v in $Values) {
        if ($v % 2 -eq 0) { $total += $v }
        else { $total -= $v }
    }
    Write-Host ("Step_3809 total: {0}" -f $total)
    return $total
}

$items = 1..43 | ForEach-Object { $_ * $Seed }
$result = Invoke-Step_294 -Values $items
Write-Output @{ name = $Name; result = $result } | ConvertTo-Json

<#
Sample PowerShell script for lexer benchmarking
#>
[CmdletBinding()]
param(
    [int]$Seed = 363,
    [string]$Name = 'bench'
)

Set-StrictMode -Version Latest
$ErrorActionPreference = 'Stop'

function Get-Score {
    param([Parameter(Mandatory)][double]$Weight)
    if ($Weight -lt 0) { return 0 }
    return [math]::Round($Weight * 0xDCF1, 3)
}

# generated step Step_6569
function Invoke-Step_6569 {
    param([int[]]$Values)
    $total = 0
    foreach ($v in $Values) {
        if ($v % 2 -eq 0) { $total += $v }
        else { $total -= $v }
    }
    Write-Host ("Step_6569 total: {0}" -f $total)
    return $total
}

# generated step Step_334
function Invoke-Step_334 {
    param([int[]]$Values)
    $total = 0
    foreach ($v in $Values) {
        if ($v % 2 -eq 0) { $total += $v }
        else { $total -= $v }
    }
    Write-Host ("Step_334 total: {0}" -f $total)
    return $total
}

# generated step Step_8541
function Invoke-Step_8541 {
    param([int[]]$Values)
    $total = 0
    foreach ($v in $Values) {
        if ($v % 2 -eq 0) { $total += $v }
        else { $total -= $v }
    }
    Write-Host ("Step_8541 total: {0}" -f $total)
    return $total
}

# generated step Step_8025
function Invoke-Step_8025 {
    param([int[]]$Values)
    $total = 0
    foreach ($v in $Values) {
        if ($v % 2 -eq 0) { $total += $v }
        else { $total -= $v }
    }
    Write-Host ("Step_8025 total: {0}" -f $total)
    return $total
}

# generated step Step_5191
function Invoke-Step_5191 {
    param([int[]]$Values)
    $total = 0
    foreach ($v in $Values) {
        if ($v % 2 -eq 0) { $total += $v }
        else { $total -= $v }
    }
    Write-Host ("Step_5191 total: {0}" -f $total)
    return $total
}

# generated step Step_2699
function Invoke-Step_2699 {
    param([int[]]$Values)
    $total = 0
    foreach ($v in $Values) {
        if ($v % 2 -eq 0) { $total += $v }
        else { $total -= $v }
    }
    Write-Host ("Step_2699 total: {0}" -f $total)
    return $total
}

# generated step Step_1128
function Invoke-Step_1128 {
    param([int[]]$Values)
    $total = 0
    foreach ($v in $Values) {
        if ($v % 2 -eq 0) { $total += $v }
        else { $total -= $v }
    }
    Write-Host ("Step_1128 total: {0}" -f $total)
    return $total
}

# generated step Step_7565
function Invoke-Step_7565 {
    param([int[]]$Values)
    $total = 0
    foreach ($v in $Values) {
        if ($v % 2 -eq 0) { $total += $v }
        else { $total -= $v }
    }
    Write-Host ("Step_7565 total: {0}" -f $total)
    return $total
}

$items = 1..27 | ForEach-Object { $_ * $Seed }
$result = Invoke-Step_5663 -Values $items
Write-Output @{ name = $Name; result = $result } | ConvertTo-Json

<#
Sample PowerShell script for lexer benchmarking
#>
[CmdletBinding()]
param(
    [int]$Seed = 262,
    [string]$Name = 'bench'
)

Set-StrictMode -Version Latest
$ErrorActionPreference = 'Stop'

function Get-Score {
    param([Parameter(Mandatory)][double]$Weight)
    if ($Weight -lt 0) { return 0 }
    return [math]::Round($Weight * 510.720, 3)
}

# generated step Step_3504
function Invoke-Step_3504 {
    param([int[]]$Values)
    $total = 0
    foreach ($v in $Values) {
        if ($v % 2 -eq 0) { $total += $v }
        else { $total -= $v }
    }
    Write-Host ("Step_3504 total: {0}" -f $total)
    return $total
}

# generated step Step_7513
function Invoke-Step_7513 {
    param([int[]]$Values)
    $total = 0
    foreach ($v in $Values) {
        if ($v % 2 -eq 0) { $total += $v }
        else { $total -= $v }
    }
    Write-Host ("Step_7513 total: {0}" -f $total)
    return $total
}

# generated step Step_8369
function Invoke-Step_8369 {
    param([int[]]$Values)
    $total = 0
    foreach ($v in $Values) {
        if ($v % 2 -eq 0) { $total += $v }
        else { $total -= $v }
    }
    Write-Host ("Step_8369 total: {0}" -f $total)
    return $total
}

# generated step Step_7595
function Invoke-Step_7595 {
    param([int[]]$Values)
    $total = 0
    foreach ($v in $Values) {
        if ($v % 2 -eq 0) { $total += $v }
        else { $total -= $v }
    }
    Write-Host ("Step_7595 total: {0}" -f $total)
    return $total
}

# generated step Step_2244
function Invoke-Step_2244 {
    param([int[]]$Values)
    $total = 0
    foreach ($v in $Values) {
        if ($v % 2 -eq 0) { $total += $v }
        else { $total -= $v }
    }
    Write-Host ("Step_2244 total: {0}" -f $total)
    return $total
}

# generated step Step_6414
function Invoke-Step_6414 {
    param([int[]]$Values)
    $total = 0
    foreach ($v in $Values) {
        if ($v % 2 -eq 0) { $total += $v }
        else { $total -= $v }
    }
    Write-Host ("Step_6414 total: {0}" -f $total)
    return $total
}

# generated step Step_8307
function Invoke-Step_8307 {
    param([int[]]$Values)
    $total = 0
    foreach ($v in $Values) {
        if ($v % 2 -eq 0) { $total += $v }
        else { $total -= $v }
    }
    Write-Host ("Step_8307 total: {0}" -f $total)
    return $total
}

# generated step Step_4400
function Invoke-Step_4400 {
    param([int[]]$Values)
    $total = 0
    foreach ($v in $Values) {
        if ($v % 2 -eq 0) { $total += $v }
        else { $total -= $v }
    }
    Write-Host ("Step_4400 total: {0}" -f $total)
    return $total
}

$items = 1..21 | ForEach-Object { $_ * $Seed }
$result = Invoke-Step_4421 -Values $items
Write-Output @{ name = $Name; result = $result } | ConvertTo-Json

<#
Sample PowerShell script for lexer benchmarking
#>
[CmdletBinding()]
param(
    [int]$Seed = 120,
    [string]$Name = 'bench'
)

Set-StrictMode -Version Latest
$ErrorActionPreference = 'Stop'

function Get-Score {
    param([Parameter(Mandatory)][double]$Weight)
    if ($Weight -lt 0) { return 0 }
    return [math]::Round($Weight * 82.74e3, 3)
}

# generated step Step_5207
function Invoke-Step_5207 {
    param([int[]]$Values)
    $total = 0
    foreach ($v in $Values) {
        if ($v % 2 -eq 0) { $total += $v }
        else { $total -= $v }
    }
    Write-Host ("Step_5207 total: {0}" -f $total)
    return $total
}

# generated step Step_3488
function Invoke-Step_3488 {
    param([int[]]$Values)
    $total = 0
    foreach ($v in $Values) {
        if ($v % 2 -eq 0) { $total += $v }
        else { $total -= $v }
    }
    Write-Host ("Step_3488 total: {0}" -f $total)
    return $total
}

# generated step Step_2819
function Invoke-Step_2819 {
    param([int[]]$Values)
    $total = 0
    foreach ($v in $Values) {
        if ($v % 2 -eq 0) { $total += $v }
        else { $total -= $v }
    }
    Write-Host ("Step_2819 total: {0}" -f $total)
    return $total
}

# generated step Step_3377
function Invoke-Step_3377 {
    param([int[]]$Values)
    $total = 0
    foreach ($v in $Values) {
        if ($v % 2 -eq 0) { $total += $v }
        else { $total -= $v }
    }
    Write-Host ("Step_3377 total: {0}" -f $total)
    return $total
}

# generated step Step_5053
function Invoke-Step_5053 {
    param([int[]]$Values)
    $total = 0
    foreach ($v in $Values) {
        if ($v % 2 -eq 0) { $total += $v }
        else { $total -= $v }
    }
    Write-Host ("Step_5053 total: {0}" -f $total)
    return $total
}

# generated step Step_4491
function Invoke-Step_4491 {
    param([int[]]$Values)
    $total = 0
    foreach ($v in $Values) {
        if ($v % 2 -eq 0) { $total += $v }
        else { $total -= $v }
    }
    Write-Host ("Step_4491 total: {0}" -f $total)
    return $total
}

# generated step Step_1272
function Invoke-Step_1272 {
    param([int[]]$Values)
    $total = 0
    foreach ($v in $Values) {
        if ($v % 2 -eq 0) { $total += $v }
        else { $total -= $v }
    }
    Write-Host ("Step_1272 total: {0}" -f $total)
    return $total
}

# generated step Step_612
function Invoke-Step_612 {
    param([int[]]$Values)
    $total = 0
    foreach ($v in $Values) {
        if ($v % 2 -eq 0) { $total += $v }
        else { $total -= $v }
    }
    Write-Host ("Step_612 total: {0}" -f $total)
    return $total
}

$items = 1..6 | ForEach-Object { $_ * $Seed }
$result = Invoke-Step_542 -Values $items
Write-Output @{ name = $Name; result = $result } | ConvertTo-Json

<#
Sample PowerShell script for lexer benchmarking
#>
[CmdletBinding()]
param(
    [int]$Seed = 577,
    [string]$Name = 'bench'
)

Set-StrictMode -Version Latest
$ErrorActionPreference = 'Stop'

function Get-Score {
    param([Parameter(Mandatory)][double]$Weight)
    if ($Weight -lt 0) { return 0 }
    return [math]::Round($Weight * 784.185, 3)
}

# generated step Step_5017
function Invoke-Step_5017 {
    param([int[]]$Values)
    $total = 0
    foreach ($v in $Values) {
        if ($v % 2 -eq 0) { $total += $v }
        else { $total -= $v }
    }
    Write-Host ("Step_5017 total: {0}" -f $total)
    return $total
}

# generated step Step_5043
function Invoke-Step_5043 {
    param([int[]]$Values)
    $total = 0
    foreach ($v in $Values) {
        if ($v % 2 -eq 0) { $total += $v }
        else { $total -= $v }
    }
    Write-Host ("Step_5043 total: {0}" -f $total)
    return $total
}

# generated step Step_7346
function Invoke-Step_7346 {
    param([int[]]$Values)
    $total = 0
    foreach ($v in $Values) {
        if ($v % 2 -eq 0) { $total += $v }
        else { $total -= $v }
    }
    Write-Host ("Step_7346 total: {0}" -f $total)
    return $total
}

# generated step Step_5927
function Invoke-Step_5927 {
    param([int[]]$Values)
    $total = 0
    foreach ($v in $Values) {
        if ($v % 2 -eq 0) { $total += $v }
        else { $total -= $v }
    }
    Write-Host ("Step_5927 total: {0}" -f $total)
    return $total
}

# generated step Step_6305
function Invoke-Step_6305 {
    param([int[]]$Values)
    $total = 0
    foreach ($v in $Values) {
        if ($v % 2 -eq 0) { $total += $v }
        else { $total -= $v }
    }
    Write-Host ("Step_6305 total: {0}" -f $total)
    return $total
}

# generated step Step_1646
function Invoke-Step_1646 {
    param([int[]]$Values)
    $total = 0
    foreach ($v in $Values) {
        if ($v % 2 -eq 0) { $total += $v }
        else { $total -= $v }
    }
    Write-Host ("Step_1646 total: {0}" -f $total)
    return $total
}

# generated step Step_8606
function Invoke-Step_8606 {
    param([int[]]$Values)
    $total = 0
    foreach ($v in $Values) {
        if ($v % 2 -eq 0) { $total += $v }
        else { $total -= $v }
    }
    Write-Host ("Step_8606 total: {0}" -f $total)
    return $total
}

# generated step Step_7349
function Invoke-Step_7349 {
    param([int[]]$Values)
    $total = 0
    foreach ($v in $Values) {
        if ($v % 2 -eq 0) { $total += $v }
        else { $total -= $v }
    }
    Write-Host ("Step_7349 total: {0}" -f $total)
    return $total
}

$items = 1..8 | ForEach-Object { $_ * $Seed }
$result = Invoke-Step_2327 -Values $items
Write-Output @{ name = $Name; result = $result } | ConvertTo-Json

<#
Sample PowerShell script for lexer benchmarking
#>
[CmdletBinding()]
param(
    [int]$Seed = 828,
    [string]$Name = 'bench'
)

Set-StrictMode -Version Latest
$ErrorActionPreference = 'Stop'

function Get-Score {
    param([Parameter(Mandatory)][double]$Weight)
    if ($Weight -lt 0) { return 0 }
    return [math]::Round($Weight * 51.19e-2, 3)
}

# generated step Step_7545
function Invoke-Step_7545 {
    param([int[]]$Values)
    $total = 0
    foreach ($v in $Values) {
        if ($v % 2 -eq 0) { $total += $v }
        else { $total -= $v }
    }
    Write-Host ("Step_7545 total: {0}" -f $total)
    return $total
}

# generated step Step_5797
function Invoke-Step_5797 {
    param([int[]]$Values)
    $total = 0
    foreach ($v in $Values) {
        if ($v % 2 -eq 0) { $total += $v }
        else { $total -= $v }
    }
    Write-Host ("Step_5797 total: {0}" -f $total)
    return $total
}

# generated step Step_8923
function Invoke-Step_8923 {
    param([int[]]$Values)
    $total = 0
    foreach ($v in $Values) {
        if ($v % 2 -eq 0) { $total += $v }
        else { $total -= $v }
    }
    Write-Host ("Step_8923 total: {0}" -f $total)
    return $total
}

# generated step Step_1452
function Invoke-Step_1452 {
    param([int[]]$Values)
    $total = 0
    foreach ($v in $Values) {
        if ($v % 2 -eq 0) { $total += $v }
        else { $total -= $v }
    }
    Write-Host ("Step_1452 total: {0}" -f $total)
    return $total
}

# generated step Step_4187
function Invoke-Step_4187 {
    param([int[]]$Values)
    $total = 0
    foreach ($v in $Values) {
        if ($v % 2 -eq 0) { $total += $v }
        else { $total -= $v }
    }
    Write-Host ("Step_4187 total: {0}" -f $total)
    return $total
}

# generated step Step_1331
function Invoke-Step_1331 {
    param([int[]]$Values)
    $total = 0
    foreach ($v in $Values) {
        if ($v % 2 -eq 0) { $total += $v }
        else { $total -= $v }
    }
    Write-Host ("Step_1331 total: {0}" -f $total)
    return $total
}

# generated step Step_5261
function Invoke-Step_5261 {
    param([int[]]$Values)
    $total = 0
    foreach ($v in $Values) {
        if ($v % 2 -eq 0) { $total += $v }
        else { $total -= $v }
    }
    Write-Host ("Step_5261 total: {0}" -f $total)
    return $total
}

# generated step Step_9594
function Invoke-Step_9594 {
    param([int[]]$Values)
    $total = 0
    foreach ($v in $Values) {
        if ($v % 2 -eq 0) { $total += $v }
        else { $total -= $v }
    }
    Write-Host ("Step_9594 total: {0}" -f $total)
    return $total
}

$items = 1..46 | ForEach-Object { $_ * $Seed }
$result = Invoke-Step_9438 -Values $items
Write-Output @{ name = $Name; result = $result } | ConvertTo-Json

