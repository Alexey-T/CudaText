' SYNTAX TEST "Packages/ASP/HTML-ASP.sublime-syntax"
<!DOCTYPE html>
<html>
'<- meta.tag.structure.any.html punctuation.definition.tag.begin.html - source.asp.embedded.html
'^^^^ meta.tag.structure.any.html entity.name.tag.structure.any.html
'    ^ meta.tag.structure.any.html punctuation.definition.tag.end.html
 <%@ TRANSACTION = Required %>
'^^ punctuation.section.embedded.begin.asp
'    ^^^^^^^^^^^ constant.language.processing-directive.asp
'                ^ punctuation.separator.key-value.asp
'                           ^^ punctuation.section.embedded.end.asp
<%
   @language = VBScript ENABLESESSIONSTATE=False @@LCID=1033
'   ^^^^^^^^ constant.language.processing-directive.asp
'            ^ punctuation.separator.key-value.asp
'                       ^^^^^^^^^^^^^^^^^^ constant.language.processing-directive.asp
'                                         ^ punctuation.separator.key-value.asp
'                                                  ^^^^ constant.language.processing-directive.asp
'                                                      ^ punctuation.separator.key-value.asp
' comments are not supported here
'^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ - comment
%>
<% @
                 transaction
'                ^^^^^^^^^^^ constant.language.processing-directive.asp
= required
'<- punctuation.separator.key-value.asp
%>
<% transaction=required %>
'  ^^^^^^^^^^^ variable.other.asp - constant.language.processing-directive.asp
<head>
'<- meta.tag
    <title>ASP Test Page: <%=Request.ServerVariables("SCRIPT_NAME")%></title>
    '                     ^^^ punctuation.section.embedded.begin.asp - source.asp
    '                        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ source.asp
    '                        ^^^^^^^ support.class.asp
    '                               ^ punctuation.accessor.asp
    '                                ^^^^^^^^^^^^^^^ support.class.collection.asp
    '                                                              ^^ punctuation.section.embedded.end.asp - source.asp
    '                                                                ^^^^^^^^ meta.tag
</head>
<body>
    <%
   '^^ punctuation.section.embedded.begin.asp
    'this is a comment
   '^ punctuation.definition.comment.asp
   '^^^^^^^^^^^^^^^^^^^ comment.line.apostrophe.asp
    
    Option Explicit
   '^^^^^^^^^^^^^^^ keyword
    
    Class TestClass
   '^^^^^^^^^^^^^^^ meta.class.asp meta.class.identifier.asp
   '^^^^^ storage.type.asp
   '      ^^^^^^^^^ meta.class.asp meta.class.identifier.asp entity.name.class.asp
        ' comment
       '^^^^^^^^^^ comment.line.apostrophe.asp
        Private m_NameVar
       '^^^^^^^ storage.modifier.asp - meta.method.identifier.asp
       '        ^^^^^^^^^ variable.other.asp
        Private m_CategoryVar
        Public Const a = 2e-3, b2="c""": const c = 4, const =
       '^^^^^^^^^^^^ storage.modifier.asp
       '             ^ entity.name.constant.asp
       '               ^ keyword.operator.asp
       '                 ^^^^ constant.numeric.asp
       '                     ^ punctuation.separator.variable-declaration.asp
       '                       ^^ entity.name.constant.asp
       '                         ^ keyword.operator.asp
       '                          ^^^^^ string.quoted.double.asp
       '                               ^ punctuation.terminator.statement.asp
       '                                 ^^^^^ storage.modifier.asp
       '                                              ^^^^^ invalid.illegal.name.asp - entity.name.constant.asp
'<- - invalid.illegal.unexpected-token.asp
        Const d = &HAB
       '^^^^^ storage.modifier.asp
       '          ^^^^ constant.numeric.asp
        Const e = "I am an unclosed string
        '         ^^^^^^^^^^^^^^^^^^^^^^^^ string.quoted.double.asp
        '                                 ^ invalid.illegal.unclosed-string.asp
'<- - invalid.illegal.unclosed-string.asp
        
        private sub Class_Initialize ()
       '^^^^^^^ storage.modifier.asp
       '        ^^^ storage.type.function.asp
       '            ^^^^^^^^^^^^^^^^ support.function.magic.event.asp
            m_NameVar = ""
           '^^^^^^^^^ variable.other.asp
           '            ^ punctuation.definition.string.begin.asp
           '             ^ punctuation.definition.string.end.asp
           '            ^^ string.quoted.double.asp
           '              ^ - string
        End Sub
        
        Public Property Get Name()
       '^^^^^^^^^^^^^^^^^^^^^^^^^^ meta.method.asp meta.method.identifier.asp
       '^^^^^^ storage.modifier.asp
       '       ^^^^^^^^^^^^ storage.type.function.asp
       '                    ^^^^ entity.name.function.asp
       '                        ^ punctuation.section.parameters.begin.asp - entity.name.function.asp
       '                         ^ punctuation.section.parameters.end.asp
            Name = m_NameVar
'<- meta.method.asp meta.method.body.asp - meta.method.identifier.asp
        End Property
       '^^^^^^^^^^^^ meta.method.asp
       '^^^^^^^^^^^^ storage.type.function.end.asp
       '            ^ - meta.method.asp - meta.method.body.asp - storage.type.function.end.asp
        
        public property let Name(nameParam)
       '^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ meta.method.asp meta.method.identifier.asp
       '^^^^^^ storage.modifier.asp
       '       ^^^^^^^^^^^^ storage.type.function.asp
       '                    ^^^^ entity.name.function.asp - storage.type.function.asp
       '                        ^ punctuation.section.parameters.begin.asp - entity.name.function.asp
       '                         ^^^^^^^^^ variable.parameter.function.asp
       '                                  ^ punctuation.section.parameters.end.asp
            m_NameVar = nameParam
        End Property
        
        Public Property Get Category
       '^^^^^^^^^^^^^^^^^^^^^^^^^^^^ meta.method.asp meta.method.identifier.asp
       '       ^^^^^^^^^^^^ storage.type.function.asp
       '                    ^^^^^^^^ entity.name.function.asp
            Category = m_CategoryVar
'<- meta.method.asp meta.method.body.asp - meta.method.identifier.asp
        End Property
        
        Property Let Category(categoryParam
       '^^^^^^^^^^^^^^^^^^^^^ meta.method.asp meta.method.identifier.asp
       '^^^^^^^^^^^^ storage.type.function.asp
       '             ^^^^^^^^ entity.name.function.asp
       '                     ^ punctuation.section.parameters.begin.asp
       '                      ^^^^^^^^^^^^^ variable.parameter.function.asp
       '                                   ^ invalid.illegal.unexpected-end-of-statement.asp
            m_CategoryVar = categoryParam
'<- meta.method.asp meta.method.body.asp - meta.method.identifier.asp
        End Property
       '^^^^^^^^^^^^ meta.method.asp
       '^^^^^^^^^^^^ storage.type.function.end.asp
       '            ^ - meta.method.asp - meta.method.body.asp - storage.type.function.end.asp
        
        Private Property Set Category(categoryParam) Set m_CategoryVar = categoryParam End Property
       '^^^^^^^ storage.modifier.asp
       '        ^^^^^^^^^^^^ meta.method.asp meta.method.identifier.asp storage.type.function.asp - meta.method.body.asp
       '                     ^^^^^^^^ entity.name.function.asp
       '                             ^ punctuation.section.parameters.begin.asp
       '                              ^^^^^^^^^^^^^ variable.parameter.function.asp
       '                                           ^ punctuation.section.parameters.end.asp
       '                 ^^^ storage.type.function.asp - keyword - storage.type.asp
       '                                            ^ meta.method.asp meta.method.body.asp - meta.method.identifier.asp
       '                                                                               ^^^^^^^^^^^^ storage.type.function.end.asp
       '                                                                                           ^ - meta.method.asp - meta.method.body.asp
        
        Public Default Function Go rem no parens
       '^^^^^^^^^^^^^^^^^^^^^^^^^^ meta.method.asp meta.method.identifier.asp
       '^^^^^^^^^^^^^^ storage.modifier.asp
       '               ^^^^^^^^ storage.type.function.asp
       '                        ^^ entity.name.function.asp
       '                           ^^^^^^^^^^^^^ comment.line.rem.asp
            Go = "going... going... gone!"
'<- meta.method.asp meta.method.body.asp - meta.method.identifier.asp
           '^^ - invalid.illegal
           '   ^ keyword.operator.asp
        End Function
       '^^^^^^^^^^^^ meta.method.asp
       '^^^^^^^^^^^^ storage.type.function.end.asp
       '            ^ - meta.method.asp - meta.method.body.asp
        
        Sub Test123:Call DoSomething:End Sub
       '^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ meta.method.asp
       '^^^ meta.method.asp meta.method.identifier.asp storage.type.function.asp
       '    ^^^^^^^ meta.method.identifier.asp entity.name.function.asp
       '           ^ punctuation.terminator.statement.asp
       '            ^^^^^^^^^^^^^^^^ meta.method.body.asp
       '                             ^^^^^^^ storage.type.function.end.asp
       '                                    ^ - meta.method.asp - meta.method.body.asp
        
        Class SubClass ' nested classes are not allowed
'<- - invalid.illegal.unexpected-token.asp
       '^^^^^ invalid.illegal.unexpected-token.asp
       '      ^^^^^^^^ invalid.illegal.unexpected-token.asp
       '               ^ comment.line.apostrophe.asp - invalid.illegal.unexpected-token.asp
        
        Sub Test1234 Set x = y
                    '^^^ invalid.illegal.unexpected-token.asp
        End Sub
       '^^^^^^^ meta.class.asp meta.method.asp storage.type.function.end.asp
        
        code_in_class_definition_but_outside_of_a_method = "invalid"
       '^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ invalid.illegal.unexpected-token.asp
       '                                                 ^ invalid.illegal.unexpected-token.asp
       '                                                   ^^^^^^^^^ invalid.illegal.unexpected-token.asp
    End Class
   '^^^^^^^^^ storage.type.class.end.asp
   '^^^^^^^^^ meta.class.body.asp
   '         ^ - storage.type.class.end.asp - meta.class.body.asp
    
    Const blah = _
    
                 "blah"rem-"testing"
    '            ^^^^^^ string.quoted.double.asp
    '                  ^^^^^^^^^^^^^^ comment.line.rem.asp - string.quoted.double.asp
    
    Class TestClass2 Public Sub TestSub () Response.Write("wow") End Sub End Class
   '^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ meta.class.asp
   '^^^^^^^^^^^^^^^^ meta.class.identifier.asp
   '^^^^^ storage.type.asp
   '      ^^^^^^^^^^ entity.name.class.asp
   '                ^ - meta.class.identifier.asp
   '                 ^^^^^^^^^^^^^^^^^^^^^ meta.method.asp meta.method.identifier.asp
   '                 ^^^^^^ storage.modifier.asp
   '                        ^^^ storage.type.function.asp
   '                            ^^^^^^^ entity.name.function.asp
   '                                    ^^ punctuation.section.parameters
   '                                      ^ meta.method.asp meta.method.body.asp - meta.method.identifier.asp
   '                                                             ^^^^^^ storage.type.function.end.asp
   '                                                                     ^^^^^^^^^ meta.class.body.asp
   '                                                                     ^^^^^^^^^ storage.type.class.end.asp
   '                                                                              ^ - meta.class
    
    Const abc = 'fgfg
   '^^^^^ storage.modifier.asp
   '      ^^^ entity.name.constant.asp
    Private a, b
   '^^^^^^^ storage.modifier.asp
   '        ^ variable.other.asp
   '         ^ punctuation.separator.variable-declaration.asp
   '           ^ variable.other.asp
    Set a = new TestClass
   '^^^ keyword.other.set.asp - storage.type.function.asp
   '    ^ variable.other.asp
   '        ^^^ keyword.other.new.asp
    a.Name = blah
   '^ variable.other.asp
   ' ^ punctuation.accessor.asp
    
    Function GetModifiedDate(path) ' test function
   '^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ meta.method.asp meta.method.identifier.asp
   '                               ^ punctuation.definition.comment.asp
   '                               ^^^^^^^^^^^^^^^^ comment.line.apostrophe.asp
   '                              ^ - meta.method.identifier.asp
   '^^^^^^^^ storage.type.function.asp
   '         ^^^^^^^^^^^^^^^ entity.name.function.asp
   '                        ^ punctuation.section.parameters.begin.asp
   '                         ^^^^ variable.parameter.function.asp
   '                             ^ punctuation.section.parameters.end.asp
        On Error Resume Next
'<- meta.method.body.asp - comment.line.apostrophe.asp
       '^^^^^^^^^^^^^^^^^^^^ storage.type.asp
        Set fs = Server.CreateObject("Scripting.FileSystemObject")
       '^^^ keyword.other.set.asp
       '       ^ keyword.operator.asp
       '         ^^^^^^ support.class.asp
       '               ^ punctuation.accessor.asp
       '                ^^^^^^^^^^^^ support.function
       '                             ^ punctuation.definition.string.begin.asp
       '                             ^^^^^^^^^^^^^^^^^^^^^^^^^^^^ string.quoted.double.asp - punctuation.accessor.asp
       '                                                        ^ punctuation.definition.string.end.asp
        With fs
       '^^^^ keyword.control.flow.asp
       '     ^^ variable.other.asp
       '       ^ meta.with.block.asp
            Set rs = .GetFile(Server.MapPath(path))
           '         ^ meta.with.block.asp punctuation.accessor.asp
           '                        ^ meta.with.block.asp punctuation.accessor.asp
            GetModifiedDate = rs.DateLastModified
            Set rs = Nothing
           '^^^ keyword.other.set.asp
           '       ^ keyword.operator.asp
           '         ^^^^^^^ storage.type.asp
            .Close
           '^ punctuation.accessor.asp
           ' ^^^^^ variable
            . Close
           '^ punctuation.accessor.asp
           ' ^ - invalid.illegal
           '  ^^^^^ variable
        End With
       '^^^^^^^^ keyword.control.flow.asp
       '        ^ - meta.with.block.asp
       
        Set fs = Nothing
        On Error Goto 0
       '^^^^^^^^^^^^^^^ storage.type.asp
       '               ^ - storage.type.asp
    End Function
   '^^^^^^^^^^^^ storage.type.function.end.asp
    
    modified = GetModifiedDate("demo_lastmodified.asp")
   '           ^^^^^^^^^^^^^^^ variable
    
    Sub Example ()
   '^^^^^^^^^^^^^^ meta.method.asp meta.method.identifier.asp
   '^^^ storage.type.function.asp
   '    ^^^^^^^ entity.name.function.asp
   '           ^ - entity.name.function.asp
   '            ^^ punctuation.section.parameters
        div = 4
       '    ^ keyword.operator.asp
       '      ^ constant.numeric.asp
       
        If 1<div And 2>1 Then ' if block
       '^^ keyword.control.flow.asp
       '  ^^^^^^^^^^^^^^^ meta.between-if-and-then.asp
       '    ^ keyword.operator.asp - punctuation.definition.tag.begin.html
       '         ^^^ keyword.operator.logical.asp
       '              ^ keyword.operator.asp - punctuation.definition.tag.end.html
       '                 ^^^^ keyword.control.flow.asp
       '                     ^ meta.if.block.asp
            Exit Sub
           '^^^^^^^^ keyword.control.flow.asp
        ElseIf "abc>"<>"def= then " Then
       '^^^^^^ keyword.control.flow.asp
       '      ^^^^^^^^^^^^^^^^^^^^^^ meta.between-if-and-then.asp
       '           ^ string.quoted.double.asp - keyword.operator.asp
       '             ^^ keyword.operator.asp
       '                   ^ string.quoted.double.asp - keyword.operator.asp
       '                     ^^^^ string.quoted.double.asp - keyword.control.flow.asp
       '                            ^^^^ keyword.control.flow.asp
       '                                ^ meta.if.block.asp
        ElseIf new TestClass Then
       '       ^^^ keyword.other.new.asp
       '           ^^^^^^^^^ variable
        Else
       '^^^^ keyword.control.flow.asp
       '    ^ meta.if.block.asp
            example = example - 1
            '                 ^ keyword.operator.asp
            example -= 1
            '       ^^ invalid.illegal.unexpected-token.asp
            example = example \ 5
            '                 ^ keyword.operator.asp
        End If
       '^^^^^^ keyword.control.flow.asp
       '      ^ - meta.if.block.asp
    End Sub
   '^^^^^^^ storage.type.function.end.asp
    
    If 1 _
       = 2 Then Call DoSomething
    '      ^^^^ keyword.control.flow.asp
    '          ^^^^^^^^^^^^^^^^^ meta.if.line.asp - meta.if.block.asp
    '           ^^^^ keyword.other.call.asp
    '                ^^^^^^^^^^^ variable
    '                           ^ - meta.if.line.asp
    
    Function IIf(expr, true_part, false_part) ' https://support.microsoft.com/en-us/kb/219271
    '            ^^^^ variable.parameter.function.asp
    '                  ^^^^^^^^^ variable.parameter.function.asp
    '                             ^^^^^^^^^^ variable.parameter.function.asp
        If expr Then IIf = true_part Else IIf = false_part
        '           ^^^^^^^^^^^^^^^^^ meta.if.line.asp
        '             ^^ - keyword.control.flow.asp
        '                                ^^^^^^^^^^^^^^^^^ meta.if.line.asp
        '                                                 ^ - meta.if.line.asp
    End Function
    
    If a is not nothing then a.b = a.b + 2
    '    ^^^^^^ invalid.illegal.unexpected-token.asp
    '           ^^^^^^^ storage.type.asp
    '                   ^^^^ keyword.control.flow.asp
    ' ^^^^^^^^^^^^^^^^^^ meta.between-if-and-then.asp
    '                       ^^^^^^^^^^^^^^ meta.if.line.asp
    '                                     ^ - meta.if.line.asp
    If not a is nothing then a.b = a.b + 2
    ' ^^^^^^^^^^^^^^^^^^ meta.between-if-and-then.asp
    '                       ^^^^^^^^^^^^^^ meta.if.line.asp
    '                        ^ variable.other.asp
    '                         ^ punctuation.accessor.asp
    '                          ^ variable
    '                                     ^ - meta.if.line.asp
    '        ^^^^^^ - invalid.illegal.unexpected-token.asp
    '        ^^ keyword.operator.logical.asp
    '           ^^^^^^^ storage.type.asp
    
    If a Then
        '    ^ meta.if.block.asp - meta.if.line.asp
        DoSomething ( invalid_token_inside_parens, 2, if )
       '                                              ^^ invalid.illegal.unexpected-token.literal.asp
       '^^^^^^^^^^^ variable
    ElseIf a = b Then AnotherSomething ' despite this being on the same line as the ElseIf, the End If is still required because the opening if statement was a block
    '                ^ meta.if.block.asp - meta.if.line.asp
    Else DoSomethingElse ' despite this being on the same line as the Else, the End If is still required because the opening if statement was a block
   '^^^^ keyword.control.flow.asp
   '    ^^^^^^^^^^^^^^^^ meta.if.block.asp
    End If
   '      ^ - meta.if.block.asp
    
    If a Then Call
    
    Dim str1_ REM example
   '^^^ storage.modifier.asp
   '       ^ - constant.numeric.asp
   '        ^ - punctuation.separator.continuation.line.asp
   '          ^^^ punctuation.definition.comment.asp
   '          ^^^^^^^^^^^^ comment.line.rem.asp
    str1 = "this is a ""hello-world""" & " " &"""" + "test" +_ 'continue on next line, comments not allowed here
'<- - comment.line.rem.asp
    '                 ^^ string.quoted.double.asp constant.character.escape.apostrophe.asp
    '                        ^ string.quoted.double.asp - keyword.operator.asp
    '                              ^^ string.quoted.double.asp constant.character.escape.apostrophe.asp - punctuation.definition.string.end.asp
    '                                ^ string.quoted.double.asp punctuation.definition.string.end.asp
    '                                  ^ keyword.operator.asp
    '                                    ^^^ string.quoted.double.asp
    '                                        ^ keyword.operator.asp
    '                                         ^^^^ string.quoted.double.asp
    '                                          ^^ constant.character.escape.apostrophe.asp - punctuation.definition.string.end.asp
    '                                              ^ keyword.operator.asp
    '                                                        ^ punctuation.separator.continuation.line.asp
    '                                                          ^^^^^^^^^^^^^ invalid.illegal.expected-end-of-line.asp - comment.line.apostrophe.asp
                Chr(34) & vbCrLf _ +
    '           ^^^ support.function.vb.asp
    '                     ^^^^^^ support.type.vb.asp
    '                            ^ punctuation.separator.continuation.line.asp
    '                              ^ invalid.illegal.expected-end-of-line.asp
    
    value = 1/2
    '        ^ keyword.operator.asp
    value = &HFF mod 3
    '       ^^^^ constant.numeric.asp
    '            ^^^ keyword.operator.asp
    Select Case call value:Case 1
   '^^^^^^^^^^^ keyword.control.flow.asp
   '            ^^^^ meta.select.block.asp invalid.illegal.unexpected-token.literal.asp
   '                      ^ meta.select.block.asp punctuation.terminator.statement.asp
   '                       ^^^^ keyword.control.flow.asp
        Case 2
       '^^^^ keyword.control.flow.asp
'<- meta.select.block.asp
        Case
        Case vbNullString
       '^^^^ keyword.control.flow.asp
        '    ^^^^^^^^^^^^ support.type.vb.asp
        Case Else
       '^^^^^^^^^ keyword.control.flow.asp
            If value >= 4 and Value<=5 Then MsgBox("no end if required here")
            '        ^^ keyword.operator.asp
            '                      ^^ keyword.operator.asp
            '                               ^^^^^^ support.function.vb.asp
            If Not (value Is Nothing) then valueis = vbFalse
            '  ^^^ keyword.operator.logical.asp
            '             ^^ keyword.operator.logical.asp
            '                                   ^^ - keyword.operator
            '                                        ^^^^^^^ support.type.vb.asp
'<- meta.select.block.asp
    End Select
   '^^^^^^^^^^ keyword.control.flow.asp
   '          ^ - meta.select.block.asp
    'the underscore in this comment should be ignored _ as should the colon :
    '                                                 ^ comment.line.apostrophe.asp - keyword - invalid.illegal - punctuation
    '                                                                       ^ comment.line.apostrophe.asp - keyword - punctuation - invalid.illegal
    
    Sub Test _
 _ '         ^ punctuation.separator.continuation.line.asp
 _ '^^^ storage.type.function.asp
 _ '    ^^^^ entity.name.function.asp
 _ '^^^^^^^^^ meta.method.asp meta.method.identifier.asp
            (_
 _ '        ^ meta.method.asp meta.method.identifier.asp punctuation.section.parameters.begin.asp
 _ '         ^ punctuation.separator.continuation.line.asp
             abc, ByRef def _
 _          '^^^ meta.method.asp meta.method.identifier.asp variable.parameter.function.asp
 _          '   ^ meta.method.asp meta.method.identifier.asp punctuation.separator.parameter-declaration.asp
 _          '     ^^^^^ meta.method.asp meta.method.identifier.asp storage.modifier.reference.asp
 _          '           ^^^ meta.method.asp meta.method.identifier.asp variable.parameter.function.asp
 _          '               ^ meta.method.asp meta.method.identifier.asp punctuation.separator.continuation.line.asp
             )
            '^ meta.method.asp meta.method.identifier.asp punctuation.section.parameters.end.asp
        
        Dim x _
           ,y()
        '    ^^ meta.array.definition.asp punctuation.section.array
        a)
        '^ invalid.illegal.stray-close-bracket.asp
        a(
        ReDim arr(2,b)
       '^^^^^ storage.modifier.asp
       '      ^^^ variable.other.asp
       '         ^^^^^ meta.array.definition.asp
       '          ^ constant.numeric.asp
       '           ^ punctuation.separator.array.asp
       '            ^ variable.other.asp - invalid.illegal - constant.numeric
       '             ^ punctuation.section.array.end.asp
        
        ReDim arr(dim, other)
        '         ^^^ invalid.illegal
        '        ^ - invalid.illegal
        '            ^^^^^^^^ - invalid.illegal.unexpected-token.asp - invalid.illegal.name.asp
        ReDim Dim(2,4)
        '     ^^^ invalid.illegal.name.asp
        
'<- - invalid.illegal.unexpected-token.asp

        Dim a(0,&H5)
        '    ^^^^^^^ meta.array.definition.asp
        '    ^ punctuation.section.array.begin.asp
        '     ^ constant.numeric.asp
        '      ^ punctuation.separator.array.asp
        '       ^^^ constant.numeric.asp
        '          ^ punctuation.section.array.end.asp
        b = a Is Empty : Dim loop,nope : Dim foobar
       '^^^^^^^^^^^^^^^^^^^^^ - invalid.illegal.unexpected-token.asp - invalid.illegal.name.asp
       '                               ^^^^^^^^^^^^^ - invalid.illegal.unexpected-token.asp - invalid.illegal.name.asp
        '              ^ punctuation.terminator.statement.asp
        '                ^^^ storage.modifier.asp
        '                    ^^^^ invalid.illegal.name.asp - variable.other.asp
        '                              ^ punctuation.terminator.statement.asp
        '                                ^^^ storage.modifier.asp
        Dim hello_world, 2
        '                ^ invalid.illegal.unexpected-token.asp - variable.other.asp
        '                 ^ - invalid.illegal.unexpected-token.asp
        
        ReDim Preserve arr ( &HA,c)
       '^^^^^^^^^^^^^^ storage.modifier.asp
       '               ^^^ variable.other.asp
       '                   ^^^^^^^^ meta.array.definition.asp
       '                   ^ punctuation.section.array.begin.asp
       '                     ^^^ constant.numeric.asp
       '                        ^ punctuation.separator.array.asp
       '                          ^ punctuation.section.array.end.asp
        For x = LBound(a) to UBound(a) Step 2 'test
       '^^^ keyword.control.flow.asp
       '    ^ variable.other.asp
       '      ^ keyword.operator.asp
       '        ^^^^^^ support.function.vb.asp
       '               ^ variable.other.asp
       '                            ^ variable.other.asp
       '                  ^^ keyword.control.flow.asp
       '                     ^^^^^^ support.function.vb.asp
       '                               ^^^^ keyword.control.flow.asp
       '                                    ^ constant.numeric.asp
       '                                     ^ meta.for.block.asp
       '                                      ^^^^^^ comment.line.apostrophe.asp
            a(x) = x * 10
           '^ variable.other.asp
           '  ^ variable.other.asp
           ' ^ - variable.other.asp
           '   ^ - variable.other.asp
           '       ^ variable.other.asp
           '         ^ keyword.operator.asp
        Next
       '^^^^ keyword.control.flow.asp
       '    ^ - meta.for.block.asp
    End Sub
    
    Sub NoParens fg
    '   ^^^^^^^^ meta.method.asp meta.method.identifier.asp entity.name.function.asp
    '            ^^ invalid.illegal.unexpected-token.asp - meta.method.identifier.asp
    End Sub
    
    Function NoParams'()
   '^^^^^^^^^^^^^^^^^ meta.method.asp meta.method.identifier.asp
   '         ^^^^^^^^ entity.name.function.asp
   '                 ^^^ comment.line.apostrophe.asp - entity.name.function.asp - invalid.illegal.unexpected-token-after-method-declaration.asp - punctuation.section.parameters
        NoParams = InStr(1, "hello_'", "L", vbTextCompare)
'<- meta.method.body.asp - invalid.illegal.unexpected-token-after-method-declaration.asp - meta.method.identifier.asp
       '^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ - invalid.illegal.unexpected-token-after-method-declaration.asp - meta.method.asp meta.method.identifier.asp - invalid.illegal.expected-end-of-line.asp - comment.line.rem.asp
        '          ^^^^^ support.function.vb.asp
        '                                   ^^^^^^^^^^^^^ support.type.vb.asp
        Test = True Xor False
        '      ^^^^ storage.type.asp
        '           ^^^ keyword.operator.logical.asp
        '               ^^^^^ storage.type.asp
    End Function
    Sub Test2
   '^^^ meta.method.asp meta.method.identifier.asp storage.type.function.asp
    '   ^^^^^ meta.method.asp meta.method.identifier.asp entity.name.function.asp
        hello = world
       '^ - entity.name.function.asp - meta.method.asp meta.method.identifier.asp
    End Sub
    Call Test
   '^^^^ keyword.other.call.asp
    Call NoParams
    
         Sub Wow (test _ 'test
 ,def _ '^^^^^^^^^^^^^^^^^^^^^^ meta.method.asp meta.method.identifier.asp
      _ '^^^ storage.type.function.asp
        '    ^^^ entity.name.function.asp
        '                ^^^^^ invalid.illegal.expected-end-of-line.asp
    ) ' this bracket doesn't form part of the method declaration - the line above is missing a _ and contains non-whitespace
   '^ meta.method.body.asp - meta.method.identifier.asp - punctuation.section.parameters.end.asp
        MsgBox "hi", vbOkCancel or vbExclamation or vbDefaultButton1, "title"
        '            ^^^^^^^^^^ support.type.vb.asp - variable.other.asp
        '                       ^^ keyword.operator.logical.asp
        '                          ^^^^^^^^^^^^^ support.type.vb.asp
        '                                           ^^^^^^^^^^^^^^^^ support.type.vb.asp
    End Sub
    
    Dim [], [ažė+, (], [dim], [a(4)]
    '   ^^ variable.other.asp
    '     ^ punctuation.separator.variable-declaration.asp - meta.variable-definition.asp
    '       ^^^^^^^^^ variable.other.asp - keyword - punctuation
    '                  ^^^^^ variable.other.asp
    '                       ^ punctuation.separator.variable-declaration.asp
    '                         ^^^^^^ variable.other.asp - meta.array.definition.asp - punctuation.section.array
    [dim] = 5 ^ 2
   '^^^^^ variable.other.asp
    '         ^ keyword.operator.asp
    
    Function [abcdef()hij] (blah)
   '^^^^^^^^ storage.type.function.asp
   '         ^^^^^^^^^^^^^ entity.name.function.asp
   '                      ^ - entity.name.function.asp
   '                        ^^^^ variable.parameter.function.asp
        [abcdef()hij] = blah
       '^^^^^^^^^^^^^ variable.other.asp
       '                ^^^^ variable.other.asp
    End Function
   '^^^^^^^^^^^^ storage.type.function.end.asp
    Response.Write [abcdef()hij]("hello")
   '               ^^^^^^^^^^^^^ variable.other.asp
    
    End If ' nothing to end
   '^^^ invalid.illegal.unexpected-token.asp
' check that the incomplete if gets dropped off after the end of the line
'<- - meta.between-if-and-then.asp invalid.illegal.unexpected-end-of-statement.asp
   
    For Each cookie In Request.Cookies
   '<- - meta.between-if-and-then.asp
   '^^^^^^^^ keyword.control.flow.asp
   '         ^^^^^^ variable.other.asp
   '                ^^ keyword.control.flow.asp
   '                                  ^ meta.for.block.asp
        Response.Write(vbCrLf & cookie)
       '^^^^^^^^ support.class.asp
       '        ^ punctuation.accessor.asp
       '               ^^^^^^ support.type.vb.asp
       '                      ^ keyword.operator.asp
       '                        ^^^^^^ variable.other.asp
        If x = y Then Exit For
       '^^ keyword.control.flow.asp
       '   ^ variable.other.asp
       '       ^ variable.other.asp
       '         ^^^^ keyword.control.flow.asp
       '             ^^^^^^^^^ meta.if.line.asp
       '              ^^^^^^^^ keyword.control.flow.asp
       '                      ^ - meta.if.line.asp
        Response.Write("----" & vbCrLf)
       '^^^^^^^^ support.class.asp
        
        abc = x >< y
       '        ^^ invalid.illegal.unexpected-token.asp
        abc = x <> y
       '        ^^ keyword.operator.asp
    Next
   '^^^^ keyword.control.flow.asp
   '    ^ - meta.for.block.asp
    
    If 
   '^^ keyword.control.flow.asp
   '  ^ meta.between-if-and-then.asp
    ' the above if statement has no "Then"
   '^ - meta.between-if-and-then.asp
    
    Do
   '^^ meta.do.block.asp keyword.control.flow.asp
        Exit Do
       '^^^^^^^ meta.do.block.asp keyword.control.flow.asp
    Loop
   '^^^^ keyword.control.flow.asp
   '    ^ - meta.do.block.asp
    
    Do
        Exit Do
    Loop Until x = y
   '^^^^^^^^^^ keyword.control.flow.asp
   '           ^ variable.other.asp
   '             ^ keyword.operator.asp
   '               ^ variable.other.asp
   '                ^ - meta.do.block.asp
    
    Do
        Exit Do
    Loop While x <> y
   '^^^^^^^^^^ keyword.control.flow.asp
   '           ^ variable.other.asp
   '                ^ variable.other.asp
   '                 ^ - meta.do.block.asp - meta.while.block.asp
    
    While True
   '^^^^^ meta.while.block.asp keyword.control.flow.asp
        ' ^^^^ storage.type.asp - variable.other.asp
    Wend
   '^^^^ keyword.control.flow.asp
   '    ^ - meta.while.block.asp
   
    Application.Lock
   '^^^^^^^^^^^ support.class.asp - variable.other.asp
   '           ^ punctuation.accessor.asp
   '            ^^^^ support.function.asp - variable.other.asp
    Application("NumVisits") = Application("NumVisits") + 1
   '^^^^^^^^^^^ support.class.asp
    Application.Unlock
   '^^^^^^^^^^^ support.class.asp
   '           ^ punctuation.accessor.asp
   '            ^^^^^^ support.function.asp
    %>
    This application page has been visited
    <%= Application("NumVisits") %> times!
    <%
    
    Sub Application_OnStart()
   '^^^ storage.type.function.asp
   '    ^^^^^^^^^^^^^^^^^^^ entity.name.function.asp support.function.magic.event.asp
        Application("NumVisits") = 0
    End Sub
   '^^^^^^^ storage.type.function.end.asp
    
    Sub Application_OnStartup()
   '    ^^^^^^^^^^^^^^^^^^^^^ entity.name.function.asp - support.function.magic.event.asp
    End Sub
    
    Sub Another_Test()rem
   '^^^ storage.type.function.asp
   '    ^^^^^^^^^^^^ entity.name.function.asp
   '                ^^ punctuation.section.parameters
   '                  ^^^^ comment.line.rem.asp - meta.method.identifier.asp
        Const ForAppending = 8
       '^^^^^ meta.method.body.asp storage.modifier.asp
        
        Set objFSO = CreateObject("Scripting.FileSystemObject")
       '    ^^^^^^ variable.other.asp
        Set objTextFile = objFSO.OpenTextFile("d:\logfile.c", ForAppending, True)
       '                                                      ^^^^^^^^^^^^ variable.other.asp
        
        Dim Line
       '    ^^^^ variable.other.asp
        Line = 0
       '^^^^ variable.other.asp
        Do While Line < 2000
       '^^^^^^^^ keyword.control.flow.asp
       '^^^^^^^^^^^^^^^^^^^^^ meta.do.block.asp - meta.while.block.asp
       '         ^^^^ variable.other.asp
       '              ^ keyword.operator.asp
       '                ^^^^ constant.numeric.asp
            objTextFile.WriteLine("long l" & Line & " = " & Line)
           '^^^^^^^^^^^ variable.other.asp
           '            ^^^^^^^^^ variable
            Line = Line + 1
            Dim WshShell 'http://stackoverflow.com/a/2237479/4473405
            Set WshShell=Server.CreateObject("WScript.Shell")
            WshShell.Run "waitfor /T " & numberOfSecond & "SignalThatWontHappen", , True
            
            do until Line = Line
           '^^^^^^^^ keyword.control.flow.asp
           '         ^^^^ variable.other.asp
           '                ^^^^ variable.other.asp
           '^^^^^^^^^^^^^^^^^^^^^ meta.do.block.asp meta.do.block.asp - invalid.illegal
                ' this code will never run
            loop
           '^^^^ keyword.control.flow.asp
        Loop
       '^^^^ keyword.control.flow.asp
       '    ^ - meta.do.block.asp
        
        objTextFile.Close
        
        Set objTextFile = Nothing
        Set objFSO = Nothing
    End _
      Sub
    ' ^^^ storage.type.function.end.asp - meta.method.identifier.asp
    
    a=3.4*.5*6.*0.25
    ' ^^^ constant.numeric.asp
    '     ^^ constant.numeric.asp
    '        ^^ constant.numeric.asp
    '           ^^^^ constant.numeric.asp
    a=a+0.8
    '   ^^^ constant.numeric.asp
    ExampleSub 3.4,.5,6.,&HA,&H2,7*2.1e2,9,-3.402823E+38, 3.402823E38 ,1.401298E-45,Round(4.94065645841247E-324),a2,2a,123.456.789.321.654.321
    '          ^^^ constant.numeric.asp
    '              ^^ constant.numeric.asp
    '                 ^^ constant.numeric.asp
    '                    ^^^ constant.numeric.asp
    '                        ^^^ constant.numeric.asp
    '                            ^ constant.numeric.asp
    '                              ^^^^^ constant.numeric.asp
    '                                    ^ constant.numeric.asp
    '                                       ^^^^^^^^^^^^ constant.numeric.asp
    '                                                     ^^^^^^^^^^^ constant.numeric.asp
    '                                                                  ^^^^^^^^^^^^ constant.numeric.asp
    '                                                                               ^^^^^ support.function.vb.asp
    '                                                                                     ^^^^^^^^^^^^^^^^^^^^^ constant.numeric.asp
    '                                                                                                            ^^ - constant.numeric.asp
    '                                                                                                                ^ - constant.numeric.asp
    '                                                                                                                         ^^^^^^^^ - constant.numeric.asp
    
    response.write(test..b)
    '                  ^ punctuation.accessor.asp
    '                   ^ invalid.illegal.unexpected-token.asp
    
    [abc=123:def] = 4
   '^^^^^^^^^^^^^ variable.other.asp - punctuation - keyword - constant
    
    Randomize Timer
   '^^^^^^^^^^^^^^^ storage.type.asp
    a=Abs(-6)*Rnd()*2+Fix(2.75) + Int(3.14)/Sin(30)+Cos(20)+Tan(40)+Sqr(4)+Log(6)+Sgn(3)
    ' ^^^ support.function.vb.asp
    '         ^^^ support.function.vb.asp
    '                 ^^^ support.function.vb.asp
    '                             ^^^ support.function.vb.asp
    '                                       ^^^ support.function.vb.asp
    '                                               ^^^ support.function.vb.asp
    '                                                       ^^^ support.function.vb.asp
    '                                                               ^^^ support.function.vb.asp
    '                                                                      ^^^ support.function.vb.asp
    '                                                                             ^^^ support.function.vb.asp
    
    b=Oct(12) & Hex(12)
    ' ^^^ support.function.vb.asp
    '           ^^^ support.function.vb.asp
    c = CSng("123") + CDbl("123") + CInt("123")+CLng("123")
    '   ^^^^ support.function.vb.asp
    '                 ^^^^ support.function.vb.asp
    '                               ^^^^ support.function.vb.asp
    '                                           ^^^^ support.function.vb.asp
    d = Time
    '   ^^^^ support.function.vb.asp
    e = Date()
    '   ^^^^ support.function.vb.asp
    
    submit = 2
   '^^^^^^ variable.other.asp - storage.type.function.asp
    function_hello = 2
   '^^^^^^^^^^^^^^ variable.other.asp - storage.type.function.asp
    
    %>
   '^^ punctuation.section.embedded.end.asp
   '  ^ - source.asp.embedded.html
    This file was last modified on: <%response.write(modified)
   '^ - source.asp.embedded.html
   '                                ^^ punctuation.section.embedded.begin.asp
   '                                  ^ source.asp.embedded.html
   '                                  ^^^^^^^^ support.class.asp
   '                                           ^^^^^ support.function.asp
    %>
    
    <p>foobar</p>
   '^^^ text.html.asp meta.tag.block.any.html - source.asp.embedded.html
    <%='test %>
   '^^^ punctuation.section.embedded.begin.asp - source.asp
   '   ^^^^^^ comment.line.apostrophe.asp
   '         ^^ punctuation.section.embedded.end.asp - comment.line.apostrophe.asp
   '         ^^^ - source.asp.embedded.html
    <%= "this" + Chr(32) + "that" %>
   '^^^ punctuation.section.embedded.begin.asp - source.asp.embedded.html
   '   ^^^^^^^^^^^^^^^^^^^^^^^^^^^ source.asp.embedded.html
   '    ^^^^^^ string.quoted.double.asp
   '           ^ keyword.operator.asp
   '             ^^^ support.function.vb.asp
   '                              ^^ punctuation.section.embedded.end.asp - source.asp
<%

Class ClassContainingMethodsWithReservedWords
    Sub [Select]()
   '^^^ storage.type.function.asp
   '    ^^^^^^^^ entity.name.function.asp
   '            ^ - entity.name.function.asp
        
    End Sub
    
    Sub [End]()
   '^^^ storage.type.function.asp
   '    ^^^^^ entity.name.function.asp
   '         ^ - entity.name.function.asp
        
    End Sub
End Class
Set ccmwrw = new ClassContainingMethodsWithReservedWords
ccmwrw.Select()
'     ^ punctuation.accessor.asp
'      ^^^^^^ variable.other.asp - invalid.illegal - keyword
ccmwrw. Select() ' spaces are not allowed after the .
'     ^ punctuation.accessor.asp
'      ^ - invalid.illegal.unexpected_token.asp
'       ^^^^^^ variable.other.asp - invalid.illegal - keyword
ccmwrw.End
'     ^ punctuation.accessor.asp
'      ^^^ variable.other.asp - invalid.illegal - keyword
'         ^ - invalid.illegal
ccmwrw .End
'     ^ invalid.illegal.unexpected-token.asp
'      ^ punctuation.accessor.asp
'       ^^^ variable.other.asp - invalid.illegal - keyword
'          ^ - invalid.illegal

_
     'a line continuation char can be followed by just a comment on the next non blank line
   ' ^ punctuation.definition.comment.asp
   ' ^^^^^^ comment.line.apostrophe.asp

t = [unclosed variable identifier
'   ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ variable.other.asp
Dim test
'<- storage.modifier.asp - variable.other.asp
 rem the syntax on the next line will cause an unterminated string constant error
'^^^^^^ comment.line.rem.asp
test = "hello%>
'      ^^^^^^ source.asp.embedded.html string.quoted.double.asp
'            ^^ punctuation.section.embedded.end.asp - string.quoted.double.asp
'              ^ - source.asp.embedded.html

<footer>
'^^^^^^ text.html.asp meta.tag.block.any.html entity.name.tag.block.any.html
    <% With abc %>
    '  ^^^^ keyword.control.flow.asp
    '           ^^ text.html.asp punctuation.section.embedded.end.inside-block.asp
        <p>Some conditional content in the footer</p>
        '<- text.html.asp meta.tag.block.any.html punctuation.definition.tag.begin.html
    <% End With %>
   '^^ punctuation.section.embedded.begin.inside-block.asp
   '   ^^^^^^^^ keyword.control.flow.asp
   '            ^^ punctuation.section.embedded.end.asp - meta.with.block.asp
   '              ^ - source.asp.embedded.html
    <% If abc = "def" Then %>
    '                     ^ meta.if.block.asp - meta.if.line.asp
        <span id="span1">Text here</span>
        '     ^^ text.html.asp meta.tag.inline.any.html meta.attribute-with-value.id.html entity.other.attribute-name.id.html
        '                         ^^ text.html.asp meta.tag.inline.any.html punctuation.definition.tag.begin.html
    <% End If %>
    ' ^ meta.if.block.asp
    '  ^^^^^^ keyword.control.flow.asp
    '        ^^^^ - meta.if.block.asp
   
</footer><% [%><br />
'            ^^ punctuation.section.embedded.end.asp
'               ^^ text.html.asp meta.tag.inline.any.html entity.name.tag.inline.any.html
<% Sub InventiveMethodNameHere(list) %>
'  ^^^ meta.method.identifier.asp storage.type.function.asp
'                                   ^ text.html.asp source.asp.embedded.html meta.method.asp meta.method.body.asp
'                                    ^^ text.html.asp punctuation.section.embedded.end.inside-block.asp
  <ul>
<%
        for each item in list
       '^^^^^^^^ text.html.asp source.asp.embedded.html meta.method.asp meta.method.body.asp meta.for.block.asp keyword.control.flow.asp
       '              ^^ text.html.asp source.asp.embedded.html meta.method.asp meta.method.body.asp meta.for.block.asp keyword.control.flow.asp
            %><li><%= item %></li><%
                    '^^^^^^ text.html.asp source.asp.embedded.html meta.method.asp meta.method.body.asp meta.for.block.asp
           '  ^ meta.tag.inline.any.html punctuation.definition.tag.begin.html
           '      ^^^ punctuation.section.embedded.begin.inside-block.asp
           '               ^^ punctuation.section.embedded.end.inside-block.asp
        Next
       '^^^^ text.html.asp source.asp.embedded.html meta.method.asp meta.method.body.asp keyword.control.flow.asp
       '    ^ - meta.for.block.asp
%></ul>
<% End Sub %>
'  ^^^^^^^ text.html.asp source.asp.embedded.html meta.method.asp storage.type.function.end.asp
'         ^ - meta.method.asp
 <p class="<% If True Then %>test<% End If %>"></p>
'^^^^^^^^^^^^^^^^^^^^^^^^^^ meta.tag.block.any.html
'                                         ^^^^^^^^^ meta.tag.block.any.html
'   ^^^^^ meta.attribute-with-value.class.html entity.other.attribute-name.class.html
'         ^^^ meta.attribute-with-value.class.html string.quoted.double.html
'                                   ^^^^^^^^^^ meta.attribute-with-value.class.html string.quoted.double.html
'                                             ^ - string.quoted.double.html
'          ^^^^^^^^^^^^^^^^ meta.class-name.html
'                                  ^^^^^^^^^^ meta.class-name.html
'          ^^ punctuation.section.embedded.begin.asp
'             ^^^^^^^^^^^^^ source.asp.embedded.html
'             ^^ keyword.control.flow.asp
'                ^^^^ meta.between-if-and-then.asp storage.type.asp
'                     ^^^^ keyword.control.flow.asp
'                         ^ meta.if.block.asp
'                                  ^ meta.if.block.asp
'                          ^^ punctuation.section.embedded.end.inside-block.asp
'                                ^^ punctuation.section.embedded.begin.inside-block.asp
'                                   ^^^^^^ keyword.control.flow.asp
'                                          ^^ punctuation.section.embedded.end.asp - meta.if.block.asp
'                                             ^ punctuation.definition.tag.end.html

 <p <%= "class=""test""" %> id="test1"></p>
'^^^ meta.tag.block.any.html
'                          ^^^^^^^^^^^^^^^^ meta.tag.block.any.html
'^ punctuation.definition.tag.begin.html
' ^ entity.name.tag.block.any.html
'   ^^^ punctuation.section.embedded.begin.asp
'       ^^^^^^^^^^^^^^^^ string.quoted.double.asp
'                        ^^ punctuation.section.embedded.end.asp
'                           ^^ meta.attribute-with-value.id.html entity.other.attribute-name.id.html
'                               ^^^^^ meta.attribute-with-value.id.html string.quoted.double.html meta.toc-list.id.html
'                                     ^ punctuation.definition.tag.end.html
'                                          ^ - meta.tag.block.any.html
<% If True Then
   Do
 %>
        <span <%= "class=""test""" %> id="test2"></span>
       '^^^^^^ meta.tag.inline.any.html
       '^ punctuation.definition.tag.begin.html
       ' ^^^^ entity.name.tag.inline.any.html
       '      ^^^ punctuation.section.embedded.begin.inside-block.asp
       '          ^^^^^^^^^^^^^^^^ string.quoted.double.asp
       '                           ^^ punctuation.section.embedded.end.inside-block.asp
       '                             ^^^^^^^^^^^^ meta.tag
       '                              ^^ meta.attribute-with-value.id.html entity.other.attribute-name.id.html
       '                                  ^^^^^ meta.attribute-with-value.id.html string.quoted.double.html meta.toc-list.id.html
       '                                        ^ punctuation.definition.tag.end.html
       '                                         ^^^^^^^ meta.tag.inline.any.html - meta.tag.after-embedded-asp.any.html
       '                                         ^^ punctuation.definition.tag.begin.html
       '                                           ^^^^ entity.name.tag.inline.any.html
       '                                               ^ punctuation.definition.tag.end.html
<% Loop
   End If %>
  '^^^^^^ keyword.control.flow.asp

 <span <% If False Then %> class="test" <% End If %> id="test3"></span>
'^^^^^^^^^^^^^^^^^^^^^^^ meta.tag.inline.any.html
'                                         ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ meta.tag.inline.any.html
'^ punctuation.definition.tag.begin.html
' ^^^^ entity.name.tag.inline.any.html
'      ^^ punctuation.section.embedded.begin.asp
'         ^^ keyword.control.flow.asp
'            ^^^^^ meta.between-if-and-then.asp storage.type.asp
'                  ^^^^ keyword.control.flow.asp
'                       ^^ punctuation.section.embedded.end.inside-block.asp
'                          ^^^^^^^^^^^^ meta.attribute-with-value.class.html
'                          ^^^^^ entity.other.attribute-name.class.html
'                               ^ punctuation.separator.key-value.html
'                                ^ string.quoted.double.html punctuation.definition.string.begin.html
'                                 ^^^^ string.quoted.double.html meta.class-name.html
'                                     ^ string.quoted.double.html punctuation.definition.string.end.html
'                                       ^^ punctuation.section.embedded.begin.inside-block.asp
'                                          ^^^^^^ keyword.control.flow.asp
'                                                 ^^ punctuation.section.embedded.end.asp
'                                                    ^^ meta.attribute-with-value.id.html entity.other.attribute-name.id.html
'                                                        ^^^^^ meta.attribute-with-value.id.html string.quoted.double.html meta.toc-list.id.html
'                                                              ^ punctuation.definition.tag.end.html
 <span <% If True Then %>
  class="test"
'^^^^^^^^^^^^^^ meta.tag
' ^^^^^^^^^^^^ meta.attribute-with-value.class.html
' ^^^^^ entity.other.attribute-name.class.html
'      ^ punctuation.separator.key-value.html
'       ^ string.quoted.double.html punctuation.definition.string.begin.html
'        ^^^^ string.quoted.double.html meta.class-name.html
'            ^ string.quoted.double.html punctuation.definition.string.end.html
 <% End If %>
'^^ punctuation.section.embedded.begin.inside-block.asp
'   ^^^^^^ keyword.control.flow.asp
'          ^^ punctuation.section.embedded.end.asp
 id="test4"></span>
'^^^^^^^^^^^ meta.tag
'^^ meta.attribute-with-value.id.html entity.other.attribute-name.id.html
'    ^^^^^ meta.attribute-with-value.id.html string.quoted.double.html meta.toc-list.id.html
'          ^ punctuation.definition.tag.end.html
'           ^^^^^^^ meta.tag.inline.any.html - meta.tag.after-embedded-asp.any.html
'           ^^ punctuation.definition.tag.begin.html
'             ^^^^ entity.name.tag.inline.any.html
'                 ^ punctuation.definition.tag.end.html
 <% If True Then %>
'^^ punctuation.section.embedded.begin.asp
'                ^^ punctuation.section.embedded.end.inside-block.asp
 <span class="<%= "test" %>" id="test5"></span>
'^^^^^^^^^^^^^ meta.tag.inline.any.html
'            ^ string.quoted.double.html punctuation.definition.string.begin.html
'             ^^^ punctuation.section.embedded.begin.inside-block.asp
'                        ^^ punctuation.section.embedded.end.inside-block.asp
'                          ^^^^^^^^^^^^^ meta.tag
'                          ^ string.quoted.double.html punctuation.definition.string.end.html
'                            ^^ meta.attribute-with-value.id.html entity.other.attribute-name.id.html
'                                ^^^^^ meta.attribute-with-value.id.html string.quoted.double.html meta.toc-list.id.html
'                                      ^ punctuation.definition.tag.end.html
'                                       ^^^^^^^ meta.tag.inline.any.html - meta.tag.after-embedded-asp.any.html
'                                       ^^ punctuation.definition.tag.begin.html
'                                         ^^^^ entity.name.tag.inline.any.html
'                                             ^ punctuation.definition.tag.end.html
 <% End If %>
'^^ punctuation.section.embedded.begin.inside-block.asp
'   ^^^^^^ keyword.control.flow.asp
'          ^^ punctuation.section.embedded.end.asp
 <%
   = hi = 2 ' will output a boolean expression - True or False %>
'  ^ punctuation.section.embedded.begin.asp
'   ^^^^^^^^^^ source.asp.embedded.html
'    ^^ variable.other.asp
'       ^ keyword.operator.asp
'         ^ constant.numeric.asp
'           ^ comment

'<- - comment - source.asp.embedded.html
 </body>
'^^^^^^^ meta.tag.structure.any.html
<script type="text/javascript">
<% If True Then %>var hello = "world";<% End If %>
</script>
</html>
