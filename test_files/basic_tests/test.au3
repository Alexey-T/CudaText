  dim $t[3][2] = [["test;1;2;3", 123], _
    [';', Asc(';')], _ ; This comment
    ["", 0]]           ; comment
  #region		Begin region 
	;	#endregion
	If @error Then
		If @error = 2 Then $iMinIndex = $i
	Else
		Exit
	EndIf 
  #endregion	End region

 #comments-start test
  If SetIcon ($btnIcon, $sModulesPath & "\" & GUICtrlRead ($cmbSFXModule) & ",0") Then _
	$bIsDefaultIcon = True
  #cs test
    test
  #ce
 #comments-end test

#include
#include-once
#NoTrayIcon
#RequireAdmin

Func Palitra_()
  /*$zzz = 0 */
  While $zzz <= 2
    //$xxx = 0
    While $xxx <= 5
      $yyy = 0
      While $yyy <= 5
        $PalitraColor = "0x"&$ColorArray[$zzz]&$ColorArray[$xxx]&$ColorArray[$yyy]
        GUICtrlCreateLabel( "", ($zzz)*$PalitraSize*6+$xxx*$PalitraSize+264, $yyy*$PalitraSize+368, $PalitraSize ,$PalitraSize)
        GUICtrlSetBkColor ( -1, $PalitraColor)
      WEnd
    $xxx = $xxx + 1
    WEnd
  $zzz = $zzz +1
  Wend
EndFunc
