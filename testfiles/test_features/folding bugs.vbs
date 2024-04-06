' Сворачиваем все блоки

If blabla ' единсвенная строка, где остаётся выражение
'blabla
End If

If blabla
'blabla
Else
'blabla
End If

For i = 0 To 1
'blabla
Next

For Each i in blabla
'blabla
Next

Select Case blabla
'blabla
End Select

With blabla
'blabla
End With

Do bla-bla
'blabla
Loop

Public Property set blabla()

  Get ' нет фолдинга
	 dd
	 dd
  End Get

  Set(blabla) ' нет фолдинга
	 dd
	 dd
  End Set

End Property


Sub Main()
'blabla
End Sub

Public Sub blabla
'blabla
End Sub

Private Sub blabla
'blabla
End Sub

Protected [Overloads/Overrides/Overridable/NotOverridable/MustOverride/Shadows/Shared] Sub blabla()
'blabla
End Sub

Public [Overloads/Overrides/Overridable/NotOverridable/MustOverride/Shadows/Shared] Sub blabla()
'blabla
End Sub

Protected [Overloads/Overrides/Overridable/NotOverridable/MustOverride/Shadows/Shared] Function blabla()
'blabla
End Function

Public [Overloads/Overrides/Overridable/NotOverridable/MustOverride/Shadows/Shared] Function blabla()
'blabla
End Function

Static Sub blabla
'blabla
End Sub

Static Function blabla
'blabla
End Function

Friend Sub blabla
'blabla
End Sub

Protected Friend Sub blabla
'blabla
End Sub

Function blabla
'blabla
End Function



' Далее вообще нет фолдинга:

While blabla
'blabla
Wend

While blabla
'blabla
End While

Enum blabla
'blabla
End Enum

Class blabla
'blabla
End Class

Public Class blabla
'blabla
End Class

Try
  Catch exception1 When expression1
  Catch exception2 When expression2 Exit Try
  Finally Statements
End Try

Module blabla
'blabla
End Module
