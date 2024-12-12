#!/usr/bin/env pwsh
##############################################################################################################

Function Show-Usage {
    Return "
Usage: pwsh -File $($PSCommandPath) [OPTIONS]
Options:
    build   Build program
"
}

Function Request-File {
    ForEach ($REPLY in $args) {
        $params = @{
            Uri = $REPLY
            OutFile = (Split-Path -Path $REPLY -Leaf).Split('?')[0]
        }
        Invoke-WebRequest @params | Out-Null
        Return $params.OutFile
    }
}

Function Install-Program {
    While ($Input.MoveNext()) {
        Switch ((Split-Path -Path $Input.Current -Leaf).Split('.')[-1]) {
            'msi' {
                & msiexec /passive /package $Input.Current | Out-Host
            }
            'exe' {
                & ".\$($Input.Current)" /SP- /VERYSILENT /SUPPRESSMSGBOXES /NORESTART | Out-Host
            }
        }
        Remove-Item $Input.Current
    }
}

Function Build-Project {
    $VAR = @{
        Use = 'use'
        Cmd = 'lazbuild'
        Url = 'https://netix.dl.sourceforge.net/project/lazarus/Lazarus%20Windows%2064%20bits/Lazarus%203.6/lazarus-3.6-fpc-3.2.2-win64.exe?viasf=1'
        Path = "C:\Lazarus"
    }
    Try {
        Get-Command $VAR.Cmd
    } Catch {
        Request-File $VAR.Url | Install-Program
        $env:PATH+=";$($VAR.Path)"
        Get-Command $VAR.Cmd
    }
    If (Test-Path -Path $($VAR.Use)) {
        & git submodule update --init --recursive --force --remote | Out-Host
        $COMPONENTS = "$($VAR.Use)\components.txt"
        If (Test-Path -Path $COMPONENTS) {
            Get-Content -Path $COMPONENTS | ForEach-Object {
                If ((! (& $VAR.Cmd --verbose-pkgsearch $_ | Out-Null)) &&
                    (! (& $VAR.Cmd --add-package $_ | Out-Null)) &&
                    (! (Test-Path -Path "$($VAR.Use)\$($_)"))) {
                        $OutFile = Request-File "https://packages.lazarus-ide.org/$($_).zip"
                        Expand-Archive -Path $OutFile -DestinationPath "$($VAR.Use)\$($_)" -Force
                        Remove-Item $OutFile
                    }
            }
        }
        Get-ChildItem -Filter '*.lpk' -Recurse -File –Path 'use' | ForEach-Object {
            & $VAR.Cmd --add-package-link $_ | Out-Host
        }
    }
    Get-ChildItem -Filter '*.lpi' -Recurse -File –Path 'app' | ForEach-Object {
        & $VAR.Cmd --no-write-project --recursive $_ | Out-Host
    }
}

Function Switch-Action {
    $ErrorActionPreference = 'stop'
    Set-PSDebug -Strict -Trace 1
    Invoke-ScriptAnalyzer -EnableExit -Path $PSCommandPath
    If ($args.count -gt 0) {
        Switch ($args[0]) {
            'build' {
                Build-Project
            }
            Default {
                Show-Usage
            }
        }
    } Else {
        Show-Usage
    }
}

##############################################################################################################
Switch-Action @args | Out-Null
