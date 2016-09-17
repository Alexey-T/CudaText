:: SYNTAX TEST "Packages/Batch File/Batch File.sublime-syntax"

   REM I'm a (com|ment)
:: ^^^                  keyword.command.rem.dosbatch
::     ^^^^^^^^^^^^^^^^ comment.line.rem.dosbatch
::           ^          invalid.illegal.unexpected-character.dosbatch
::               ^      invalid.illegal.unexpected-character.dosbatch
::                    ^ invalid.illegal.unexpected-character.dosbatch

REM
   not a comment
:: ^^^^^^^^^^^^^ - comment.line.rem.dosbatch

   :: Me too!
:: ^^         punctuation.definition.comment.dosbatch
:: ^^^^^^^^^^ comment.line.colon.dosbatch

   :+ Me too!
:: ^^         punctuation.definition.comment.dosbatch

   := Me too!
:: ^^         punctuation.definition.comment.dosbatch

   :, Me too!
:: ^^         punctuation.definition.comment.dosbatch

   :; Me too!
:: ^^         punctuation.definition.comment.dosbatch

   : Me too!
:: ^^         punctuation.definition.comment.dosbatch

ECHO : Not a comment
::   ^^^^^^^^^^^^^^^ - comment

ECHO : Not a comment ^
  do not break out of an echo with an escaped newline
::   ^^^ - keyword.operator
::       ^^^^^ - keyword.command

ECHO &:: A comment
::   ^ keyword.operator.conditional.dosbatch
::    ^^ punctuation.definition.comment.dosbatch
::    ^^^^^^^^^^^^ comment.line.colon.dosbatch

  :: an indented comment
::^^ punctuation.definition.comment.dosbatch
::^^^^^^^^^^^^^^^^^^^^^^ comment.line.colon.dosbatch

   ECHO "foo"
::      ^       punctuation.definition.string.begin.dosbatch
::      ^^^^^   string.quoted.double.dosbatch
::          ^   punctuation.definition.string.end.dosbatch

ECHO "
::    ^ invalid.illegal.newline.dosbatch

   @ECHO OFF
:: ^ keyword.operator.at.dosbatch

   @
:: ^ - keyword.operator.at.dosbatch

   GOTO:EOF
:: ^^^^ keyword.control.statement.dosbatch

:: Redirection
   ECHO Hello World! > hello.txt
:: ^^^^                keyword.command.dosbatch
::                   ^ keyword.operator.redirection.dosbatch

   ECHO >> NUL
::      ^^     keyword.operator.redirection.dosbatch
::         ^^^ constant.language

   dir > f.txt 2>&1
::     ^ keyword.operator.redirection.dosbatch
::              ^^ keyword.operator.redirection.dosbatch

   ECHO || ECHO && ECHO &
::      ^^ keyword.operator.conditional.dosbatch
::              ^^ keyword.operator.conditional.dosbatch
::                      ^ keyword.operator.conditional.dosbatch

:: Conditionals
   IF foo EQU bar
:: ^^         keyword.control.conditional.dosbatch
::        ^^^ keyword.operator.comparison.dosbatch

   IF NOT foo EQU bar
:: ^^             keyword.control.conditional.dosbatch
::    ^^^         keyword.operator.logical.dosbatch
::            ^^^ keyword.operator.comparison.dosbatch

   IF foo == bar
:: ^^         keyword.control.conditional.dosbatch
::        ^^  keyword.operator.comparison.dosbatch

   FOR %%G IN (0,9) DO (md %%G)
:: ^^^                 keyword.control.repeat.dosbatch
::             ^       constant.numeric.dosbatch

   FIND "a" |
::          ^ keyword.operator.pipe.dosbatch

  :This is a #%@$虎 strange label
::^    punctuation.separator.dosbatch
:: ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ entity.name.label.dosbatch

  :End
::^    punctuation.separator.dosbatch
:: ^^^ entity.name.label.dosbatch

   ECHO %1 %* %~dpf$PATH:5 %~1
::      ^ punctuation.definition.variable.dosbatch
::      ^^ variable.parameter.dosbatch
::         ^ punctuation.definition.variable.dosbatch
::         ^^ variable.parameter.dosbatch
::            ^ punctuation.definition.variable.dosbatch
::            ^^^^^^^^^^^ variable.parameter.dosbatch
::                         ^ punctuation.definition.variable.dosbatch
::                         ^^^ variable.parameter.dosbatch

   ECHO %variable% !variable!
::      ^ punctuation.definition.variable.begin.dosbatch
::      ^^^^^^^^^^ variable.other.readwrite.dosbatch
::               ^ punctuation.definition.variable.end.dosbatch
::                 ^ punctuation.definition.variable.begin.dosbatch
::                 ^^^^^^^^^^ variable.other.readwrite.dosbatch
::                          ^ punctuation.definition.variable.end.dosbatch

ECHO %sub:str1=str2% !sub:str1=str2!
::   ^^^^^^^^^^^^^^^ variable.other.readwrite.dosbatch
::   ^ punctuation.definition.variable.begin.dosbatch
::       ^ punctuation.separator.dosbatch
::        ^^^^^^^^^ meta.variable.substitution.dosbatch
::        ^^^^ string.unquoted.dosbatch
::            ^ punctuation.separator.dosbatch
::             ^^^^ string.unquoted.dosbatch
::                 ^ punctuation.definition.variable.end.dosbatch
::                   ^^^^^^^^^^^^^^^ variable.other.readwrite.dosbatch
::                   ^ punctuation.definition.variable.begin.dosbatch
::                       ^ punctuation.separator.dosbatch
::                        ^^^^^^^^^ meta.variable.substitution.dosbatch
::                        ^^^^ string.unquoted.dosbatch
::                            ^ punctuation.separator.dosbatch
::                             ^^^^ string.unquoted.dosbatch
::                                 ^ punctuation.definition.variable.end.dosbatch


ECHO %substr:~0,-2% !substr:~0,-2!
::   ^^^^^^^^^^^^^^ variable.other.readwrite.dosbatch
::   ^ punctuation.definition.variable.begin.dosbatch
::          ^^ punctuation.separator.dosbatch
::            ^^^^ meta.variable.substring.dosbatch
::            ^ constant.numeric.dosbatch
::             ^ punctuation.separator.dosbatch
::              ^^ constant.numeric.dosbatch
::                ^ punctuation.definition.variable.end.dosbatch
::                  ^^^^^^^^^^^^^^ variable.other.readwrite.dosbatch
::                  ^ punctuation.definition.variable.begin.dosbatch
::                         ^^ punctuation.separator.dosbatch
::                           ^^^^ meta.variable.substring.dosbatch
::                           ^ constant.numeric.dosbatch
::                            ^ punctuation.separator.dosbatch
::                             ^^ constant.numeric.dosbatch
::                               ^ punctuation.definition.variable.end.dosbatch

ECHO %b:~-5% !b:~+5!
::   ^^^^^^^ variable.other.readwrite.dosbatch
::   ^ punctuation.definition.variable.begin.dosbatch
::     ^^ punctuation.separator.dosbatch
::       ^^ meta.variable.substring.dosbatch
::       ^^ constant.numeric.dosbatch
::         ^ punctuation.definition.variable.end.dosbatch
::           ^^^^^^^ variable.other.readwrite.dosbatch
::           ^ punctuation.definition.variable.begin.dosbatch
::             ^^ punctuation.separator.dosbatch
::               ^^ meta.variable.substring.dosbatch
::               ^^ constant.numeric.dosbatch
::                 ^ punctuation.definition.variable.end.dosbatch

ECHO !t:%foo%=%bar:~0,-4%!
::   ^^^^^^^^^^^^^^^^^^^^^ variable.other.readwrite.dosbatch
::   ^ punctuation.definition.variable.begin.dosbatch
::      ^^^^^ meta.variable.substitution.dosbatch variable.other.readwrite.dosbatch
::      ^ meta.variable.substitution.dosbatch punctuation.definition.variable.begin.dosbatch
::          ^ meta.variable.substitution.dosbatch punctuation.definition.variable.end.dosbatch
::            ^ meta.variable.substitution.dosbatch punctuation.definition.variable.begin.dosbatch
::            ^^^^^^^^^^^ meta.variable.substitution.dosbatch variable.other.readwrite.dosbatch
::                  ^^^^ meta.variable.substitution.dosbatch meta.variable.substring.dosbatch
::                      ^ meta.variable.substitution.dosbatch punctuation.definition.variable.end.dosbatch
::                    ^^ meta.variable.substitution.dosbatch constant.numeric.dosbatch
::                       ^ punctuation.definition.variable.end.dosbatch

ECHO %t:foo=!bar:~0,-4!%
::   ^^^^^^^^^^^^^^^^^^^ variable.other.readwrite.dosbatch
::   ^ punctuation.definition.variable.begin.dosbatch
::          ^ meta.variable.substitution.dosbatch punctuation.definition.variable.begin.dosbatch
::          ^^^^^^^^^^^ meta.variable.substitution.dosbatch variable.other.readwrite.dosbatch
::                ^^^^ meta.variable.substitution.dosbatch meta.variable.substring.dosbatch
::                    ^ meta.variable.substitution.dosbatch punctuation.definition.variable.end.dosbatch
::                  ^^ meta.variable.substitution.dosbatch constant.numeric.dosbatch
::                     ^ punctuation.definition.variable.end.dosbatch

ECHO Not% a variable
::      ^^^^^^^^^^^^ - variable.other.readwrite.dosbatch
::   ^^^             - keyword.operator.logical.dosbatch

ECHO Not! a variable
::      ^^^^^^^^^^^^ - variable.other.readwrite.dosbatch
::   ^^^             - keyword.operator.logical.dosbatch

:: Numerics
SET /A r = 010 + 0x20 - 24
::         ^^^ constant.numeric.dosbatch
::               ^^^^ constant.numeric.dosbatch
::                      ^^ constant.numeric.dosbatch

:: Escape Characters
ECHO %% ^^! ^&
::   ^^ constant.character.escape.dosbatch
::      ^^^ constant.character.escape.dosbatch
::          ^^ constant.character.escape.dosbatch

:: command SET /A specific operators
   set /a "num%%=5"
::        ^^^^^^^^^ meta.expression.set.dosbatch
::            ^^^ keyword.operator.assignment.augmented.dosbatch

   set /a "num&=3"
::            ^^ keyword.operator.assignment.augmented.dosbatch

   set /a "num*=5"
::            ^^ keyword.operator.assignment.augmented.dosbatch

   set /a "num+=5"
::            ^^ keyword.operator.assignment.augmented.dosbatch

   set /a "num-=5"
::            ^^ keyword.operator.assignment.augmented.dosbatch

   set /a "num/=5"
::            ^^ keyword.operator.assignment.augmented.dosbatch

   set /a "num<<=2"
::            ^^^ keyword.operator.assignment.augmented.dosbatch

   set /a "num=!5"
::            ^ keyword.operator.assignment.dosbatch
::             ^ keyword.operator.logical.dosbatch

   set /a "num=(2+3)*5"
::             ^^^^^ meta.group.dosbatch
::             ^ punctuation.section.group.begin.dosbatch
::                 ^ punctuation.section.group.end.dosbatch
::            ^ keyword.operator.assignment.dosbatch
::               ^ keyword.operator.arithmetic.dosbatch
::                  ^ keyword.operator.arithmetic.dosbatch

   set /a "num=2,result=num*5"
::            ^ keyword.operator.assignment.dosbatch
::              ^ punctuation.separator.dosbatch
::                     ^ keyword.operator.assignment.dosbatch
::                         ^ keyword.operator.arithmetic.dosbatch

   set /a "num=2<<3"
::            ^ keyword.operator.assignment.dosbatch
::              ^^ keyword.operator.arithmetic.dosbatch

   set /a "num=2>>3"
::            ^ keyword.operator.assignment.dosbatch
::              ^^ keyword.operator.arithmetic.dosbatch

   set /a "num=5%%2"
::            ^ keyword.operator.assignment.dosbatch
::              ^^ keyword.operator.arithmetic.dosbatch

   set /a "num=5&3"
::            ^ keyword.operator.assignment.dosbatch
::              ^ keyword.operator.arithmetic.dosbatch

   set /a "num=5^3"
::            ^ keyword.operator.assignment.dosbatch
::              ^ keyword.operator.arithmetic.dosbatch

   set /a "num=5|3"
::            ^ keyword.operator.assignment.dosbatch
::              ^ keyword.operator.arithmetic.dosbatch

   set /a "num^=3"
::            ^^ keyword.operator.assignment.augmented.dosbatch

   set /a "num=num*5"
::            ^ keyword.operator.assignment.dosbatch
::                ^ keyword.operator.arithmetic.dosbatch

   set /a "num=num+5"
::            ^ keyword.operator.assignment.dosbatch
::                ^ keyword.operator.arithmetic.dosbatch

   set /a "num=num-5"
::            ^ keyword.operator.assignment.dosbatch
::                ^ keyword.operator.arithmetic.dosbatch

   set /a "num=num/5"
::            ^ keyword.operator.assignment.dosbatch
::                ^ keyword.operator.arithmetic.dosbatch

   set /a "num=~5"
::            ^ keyword.operator.assignment.dosbatch
::             ^ keyword.operator.arithmetic.dosbatch

   set /a "num>>=2"
::            ^^^ keyword.operator.assignment.augmented.dosbatch

   set /a "num|=3"
::            ^^ keyword.operator.assignment.augmented.dosbatch

   set /a century=year/100, next=century+1
::               ^ keyword.operator.assignment.dosbatch
::                    ^ keyword.operator.arithmetic.dosbatch
::                        ^ punctuation.separator.dosbatch
::                              ^ keyword.operator.assignment.dosbatch
::                                      ^ keyword.operator.arithmetic.dosbatch

  SET T=%TIME: =0%
::^^^ keyword.command
::    ^ variable.other.readwrite
::     ^ keyword.operator.assignment
::      ^^^^^^^^^^ variable.other.readwrite

IF "%FOO%" == "BAR" ( SET BAZ=42 )
::                  ^ punctuation.section.group.begin
::                  ^^^^^^^^^^^^^^ meta.group
::                               ^ punctuation.section.group.end

:: See http://ss64.com/nt/syntax-brackets.html
IF EXIST MyFile.txt (ECHO Some(more)Potatoes)
:: <- keyword.control
:: ^ keyword.other
::                  ^^^^^^^^^^^^^^^^ meta.group
::                   ^ keyword.command
::                                  ^ - meta.group

IF EXIST MyFile.txt (ECHO Some[more]Potatoes)
:: <- keyword.control
:: ^ keyword.other
::                  ^^^^^^^^^^^^^^^^^^^^^^^^^ meta.group
::                   ^ keyword.command

set "hello"="world"
:: <- keyword.command
::  ^ - variable.other.readwrite
::   ^^^^^^ variable.other.readwrite
::         ^ keyword.operator.assignment
::                ^ punctuation.definition.string.end
::                 ^ - string

set foo=bar
:: <- keyword.command
::  ^^^ variable.other.readwrite
::     ^ keyword.operator.assignment

set  foo = bar
:: <- keyword.command
::  ^ - variable.other.readwrite
::   ^^^^ variable.other.readwrite
::       ^ keyword.operator.assignment

set  hello world = bar
:: <- keyword.command
::  ^ - variable.other.readwrite
::   ^^^^^^^^^^^^ variable.other.readwrite
::               ^ keyword.operator.assignment

set abc /a = 1+2
:: <- keyword.command
::  ^^^^^^^ variable.other.readwrite
::         ^ keyword.operator.assignment - meta.expression.set

set "foobar=test"
:: <- keyword.command
::  ^ - variable.other.readwrite
::   ^^^^^^ variable.other.readwrite
::         ^ keyword.operator.assignment
::              ^ punctuation.definition.string.end

set  " foo = bar"
:: <- keyword.command
::   ^^ - variable.other.readwrite
::     ^^^^ variable.other.readwrite
::         ^ keyword.operator.assignment
::              ^ punctuation.definition.string.end

set test rem = hi
:: <- keyword.command
::       ^^^^^^^^^ - comment
::  ^^^^^^^^^ variable.other.readwrite
::           ^ keyword.operator.assignment

set hello_world
:: <- keyword.command
::  ^^^^^^^^^^^ variable.other.readwrite.dosbatch

set /A hello_world
:: <- keyword.command
::     ^^^^^^^^^^^ meta.expression.set
::                ^ - meta.expression.set

powershell get-date -uformat "%%Y%%m%%d" > today.txt
::                           ^^^^^^^^^^^ string.quoted.double.dosbatch
::                            ^^ constant.character.escape.dosbatch
::                              ^ - constant.character.escape.dosbatch
::                               ^^ constant.character.escape.dosbatch
::                                 ^ - constant.character.escape.dosbatch
::                                  ^^ constant.character.escape.dosbatch
::                                    ^ - constant.character.escape.dosbatch

:: the following example was inspired by http://stackoverflow.com/a/14634551/4473405
set /p today=<today.txt
:: ^^^^ - variable.other.readwrite.dosbatch
::     ^^^^^ variable.other.readwrite.dosbatch
::          ^ keyword.operator.assignment.dosbatch
::           ^ keyword.operator.redirection.dosbatch
ren example.txt example_%today%.txt
::                      ^ punctuation.definition.variable.begin.dosbatch
::                      ^^^^^^^ variable.other.readwrite.dosbatch
::                            ^ punctuation.definition.variable.end.dosbatch

::                        | this is a deliberate trailing space
set /p today=enter a date: 
:: ^^^^ - variable.other.readwrite.dosbatch
::     ^^^^^ variable.other.readwrite.dosbatch
::          ^ keyword.operator.assignment.dosbatch
::           ^^^^^^^^^^^^^^ meta.prompt.set.dosbatch - variable.other.readwrite.dosbatch
::                         ^ - meta.prompt.set.dosbatch
set /p today=enter a date: REM :: this is not a comment
:: ^^^^ - variable.other.readwrite.dosbatch
::     ^^^^^ variable.other.readwrite.dosbatch
::          ^ keyword.operator.assignment.dosbatch
::           ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ meta.prompt.set.dosbatch - variable.other.readwrite.dosbatch - comment
::                                                     ^ - meta.prompt.set.dosbatch
set /p today=
:: ^^^^ - variable.other.readwrite.dosbatch
::     ^^^^^ variable.other.readwrite.dosbatch
::          ^ keyword.operator.assignment.dosbatch
::           ^ - meta.prompt.set.dosbatch

SETLOCAL EnableDelayedExpansion
::^^^^^^ keyword.command.dosbatch
  SET /P example="what is the answer? " & echo you have answered: !example!
::   ^^^^ - variable.other.readwrite.dosbatch
::       ^^^^^^^ variable.other.readwrite.dosbatch
::              ^ keyword.operator.assignment.dosbatch
::               ^^^^^^^^^^^^^^^^^^^^^^ meta.prompt.set.dosbatch string
::                                      ^ keyword.operator.conditional.dosbatch - meta.prompt.set.dosbatch - string
::                                        ^^^^ keyword.command.dosbatch
::                                                                ^^^^^^^^^ variable.other.readwrite.dosbatch
ENDLOCAL
::^^^^^^ keyword.command.dosbatch
