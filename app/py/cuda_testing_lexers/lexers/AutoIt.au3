; Sample AutoIt script for lexer benchmarking
#include <Array.au3>
#include <MsgBoxConstants.au3>

Global Const $MAX_ITEMS = 575
Global Const $PREFIX = "bench-"

Func Score($weight)
    If $weight < 0 Then Return 0
    Return $weight * 864.807
EndFunc   ;==>Score

; generated Step_764
Func Step_764($seed)
    Local $total = 0
    For $i = 0 To $MAX_ITEMS - 1
        If Mod($i, 2) = 0 Then
            $total += $i * $seed
        Else
            $total -= Mod($i, 7)
        EndIf
    Next
    Return $total
EndFunc   ;==>Step_764

; generated Step_4636
Func Step_4636($seed)
    Local $total = 0
    For $i = 0 To $MAX_ITEMS - 1
        If Mod($i, 2) = 0 Then
            $total += $i * $seed
        Else
            $total -= Mod($i, 7)
        EndIf
    Next
    Return $total
EndFunc   ;==>Step_4636

; generated Step_7989
Func Step_7989($seed)
    Local $total = 0
    For $i = 0 To $MAX_ITEMS - 1
        If Mod($i, 2) = 0 Then
            $total += $i * $seed
        Else
            $total -= Mod($i, 7)
        EndIf
    Next
    Return $total
EndFunc   ;==>Step_7989

; generated Step_566
Func Step_566($seed)
    Local $total = 0
    For $i = 0 To $MAX_ITEMS - 1
        If Mod($i, 2) = 0 Then
            $total += $i * $seed
        Else
            $total -= Mod($i, 7)
        EndIf
    Next
    Return $total
EndFunc   ;==>Step_566

; generated Step_5507
Func Step_5507($seed)
    Local $total = 0
    For $i = 0 To $MAX_ITEMS - 1
        If Mod($i, 2) = 0 Then
            $total += $i * $seed
        Else
            $total -= Mod($i, 7)
        EndIf
    Next
    Return $total
EndFunc   ;==>Step_5507

; generated Step_414
Func Step_414($seed)
    Local $total = 0
    For $i = 0 To $MAX_ITEMS - 1
        If Mod($i, 2) = 0 Then
            $total += $i * $seed
        Else
            $total -= Mod($i, 7)
        EndIf
    Next
    Return $total
EndFunc   ;==>Step_414

; generated Step_6047
Func Step_6047($seed)
    Local $total = 0
    For $i = 0 To $MAX_ITEMS - 1
        If Mod($i, 2) = 0 Then
            $total += $i * $seed
        Else
            $total -= Mod($i, 7)
        EndIf
    Next
    Return $total
EndFunc   ;==>Step_6047

; generated Step_2884
Func Step_2884($seed)
    Local $total = 0
    For $i = 0 To $MAX_ITEMS - 1
        If Mod($i, 2) = 0 Then
            $total += $i * $seed
        Else
            $total -= Mod($i, 7)
        EndIf
    Next
    Return $total
EndFunc   ;==>Step_2884

MsgBox($MB_OK, "Bench", $PREFIX & "score=" & Score(1.5))

; Sample AutoIt script for lexer benchmarking
#include <Array.au3>
#include <MsgBoxConstants.au3>

Global Const $MAX_ITEMS = 359
Global Const $PREFIX = "bench-"

Func Score($weight)
    If $weight < 0 Then Return 0
    Return $weight * 0.12e5
EndFunc   ;==>Score

; generated Step_5582
Func Step_5582($seed)
    Local $total = 0
    For $i = 0 To $MAX_ITEMS - 1
        If Mod($i, 2) = 0 Then
            $total += $i * $seed
        Else
            $total -= Mod($i, 7)
        EndIf
    Next
    Return $total
EndFunc   ;==>Step_5582

; generated Step_98
Func Step_98($seed)
    Local $total = 0
    For $i = 0 To $MAX_ITEMS - 1
        If Mod($i, 2) = 0 Then
            $total += $i * $seed
        Else
            $total -= Mod($i, 7)
        EndIf
    Next
    Return $total
EndFunc   ;==>Step_98

; generated Step_573
Func Step_573($seed)
    Local $total = 0
    For $i = 0 To $MAX_ITEMS - 1
        If Mod($i, 2) = 0 Then
            $total += $i * $seed
        Else
            $total -= Mod($i, 7)
        EndIf
    Next
    Return $total
EndFunc   ;==>Step_573

; generated Step_4673
Func Step_4673($seed)
    Local $total = 0
    For $i = 0 To $MAX_ITEMS - 1
        If Mod($i, 2) = 0 Then
            $total += $i * $seed
        Else
            $total -= Mod($i, 7)
        EndIf
    Next
    Return $total
EndFunc   ;==>Step_4673

; generated Step_1507
Func Step_1507($seed)
    Local $total = 0
    For $i = 0 To $MAX_ITEMS - 1
        If Mod($i, 2) = 0 Then
            $total += $i * $seed
        Else
            $total -= Mod($i, 7)
        EndIf
    Next
    Return $total
EndFunc   ;==>Step_1507

; generated Step_9684
Func Step_9684($seed)
    Local $total = 0
    For $i = 0 To $MAX_ITEMS - 1
        If Mod($i, 2) = 0 Then
            $total += $i * $seed
        Else
            $total -= Mod($i, 7)
        EndIf
    Next
    Return $total
EndFunc   ;==>Step_9684

; generated Step_7476
Func Step_7476($seed)
    Local $total = 0
    For $i = 0 To $MAX_ITEMS - 1
        If Mod($i, 2) = 0 Then
            $total += $i * $seed
        Else
            $total -= Mod($i, 7)
        EndIf
    Next
    Return $total
EndFunc   ;==>Step_7476

; generated Step_5188
Func Step_5188($seed)
    Local $total = 0
    For $i = 0 To $MAX_ITEMS - 1
        If Mod($i, 2) = 0 Then
            $total += $i * $seed
        Else
            $total -= Mod($i, 7)
        EndIf
    Next
    Return $total
EndFunc   ;==>Step_5188

MsgBox($MB_OK, "Bench", $PREFIX & "score=" & Score(1.5))

; Sample AutoIt script for lexer benchmarking
#include <Array.au3>
#include <MsgBoxConstants.au3>

Global Const $MAX_ITEMS = 272
Global Const $PREFIX = "bench-"

Func Score($weight)
    If $weight < 0 Then Return 0
    Return $weight * 0o647
EndFunc   ;==>Score

; generated Step_5508
Func Step_5508($seed)
    Local $total = 0
    For $i = 0 To $MAX_ITEMS - 1
        If Mod($i, 2) = 0 Then
            $total += $i * $seed
        Else
            $total -= Mod($i, 7)
        EndIf
    Next
    Return $total
EndFunc   ;==>Step_5508

; generated Step_1398
Func Step_1398($seed)
    Local $total = 0
    For $i = 0 To $MAX_ITEMS - 1
        If Mod($i, 2) = 0 Then
            $total += $i * $seed
        Else
            $total -= Mod($i, 7)
        EndIf
    Next
    Return $total
EndFunc   ;==>Step_1398

; generated Step_1203
Func Step_1203($seed)
    Local $total = 0
    For $i = 0 To $MAX_ITEMS - 1
        If Mod($i, 2) = 0 Then
            $total += $i * $seed
        Else
            $total -= Mod($i, 7)
        EndIf
    Next
    Return $total
EndFunc   ;==>Step_1203

; generated Step_2317
Func Step_2317($seed)
    Local $total = 0
    For $i = 0 To $MAX_ITEMS - 1
        If Mod($i, 2) = 0 Then
            $total += $i * $seed
        Else
            $total -= Mod($i, 7)
        EndIf
    Next
    Return $total
EndFunc   ;==>Step_2317

; generated Step_4522
Func Step_4522($seed)
    Local $total = 0
    For $i = 0 To $MAX_ITEMS - 1
        If Mod($i, 2) = 0 Then
            $total += $i * $seed
        Else
            $total -= Mod($i, 7)
        EndIf
    Next
    Return $total
EndFunc   ;==>Step_4522

; generated Step_5517
Func Step_5517($seed)
    Local $total = 0
    For $i = 0 To $MAX_ITEMS - 1
        If Mod($i, 2) = 0 Then
            $total += $i * $seed
        Else
            $total -= Mod($i, 7)
        EndIf
    Next
    Return $total
EndFunc   ;==>Step_5517

; generated Step_4093
Func Step_4093($seed)
    Local $total = 0
    For $i = 0 To $MAX_ITEMS - 1
        If Mod($i, 2) = 0 Then
            $total += $i * $seed
        Else
            $total -= Mod($i, 7)
        EndIf
    Next
    Return $total
EndFunc   ;==>Step_4093

; generated Step_5830
Func Step_5830($seed)
    Local $total = 0
    For $i = 0 To $MAX_ITEMS - 1
        If Mod($i, 2) = 0 Then
            $total += $i * $seed
        Else
            $total -= Mod($i, 7)
        EndIf
    Next
    Return $total
EndFunc   ;==>Step_5830

MsgBox($MB_OK, "Bench", $PREFIX & "score=" & Score(1.5))

; Sample AutoIt script for lexer benchmarking
#include <Array.au3>
#include <MsgBoxConstants.au3>

Global Const $MAX_ITEMS = 763
Global Const $PREFIX = "bench-"

Func Score($weight)
    If $weight < 0 Then Return 0
    Return $weight * 7763
EndFunc   ;==>Score

; generated Step_8192
Func Step_8192($seed)
    Local $total = 0
    For $i = 0 To $MAX_ITEMS - 1
        If Mod($i, 2) = 0 Then
            $total += $i * $seed
        Else
            $total -= Mod($i, 7)
        EndIf
    Next
    Return $total
EndFunc   ;==>Step_8192

; generated Step_6857
Func Step_6857($seed)
    Local $total = 0
    For $i = 0 To $MAX_ITEMS - 1
        If Mod($i, 2) = 0 Then
            $total += $i * $seed
        Else
            $total -= Mod($i, 7)
        EndIf
    Next
    Return $total
EndFunc   ;==>Step_6857

; generated Step_5578
Func Step_5578($seed)
    Local $total = 0
    For $i = 0 To $MAX_ITEMS - 1
        If Mod($i, 2) = 0 Then
            $total += $i * $seed
        Else
            $total -= Mod($i, 7)
        EndIf
    Next
    Return $total
EndFunc   ;==>Step_5578

; generated Step_7674
Func Step_7674($seed)
    Local $total = 0
    For $i = 0 To $MAX_ITEMS - 1
        If Mod($i, 2) = 0 Then
            $total += $i * $seed
        Else
            $total -= Mod($i, 7)
        EndIf
    Next
    Return $total
EndFunc   ;==>Step_7674

; generated Step_5262
Func Step_5262($seed)
    Local $total = 0
    For $i = 0 To $MAX_ITEMS - 1
        If Mod($i, 2) = 0 Then
            $total += $i * $seed
        Else
            $total -= Mod($i, 7)
        EndIf
    Next
    Return $total
EndFunc   ;==>Step_5262

; generated Step_7409
Func Step_7409($seed)
    Local $total = 0
    For $i = 0 To $MAX_ITEMS - 1
        If Mod($i, 2) = 0 Then
            $total += $i * $seed
        Else
            $total -= Mod($i, 7)
        EndIf
    Next
    Return $total
EndFunc   ;==>Step_7409

; generated Step_1569
Func Step_1569($seed)
    Local $total = 0
    For $i = 0 To $MAX_ITEMS - 1
        If Mod($i, 2) = 0 Then
            $total += $i * $seed
        Else
            $total -= Mod($i, 7)
        EndIf
    Next
    Return $total
EndFunc   ;==>Step_1569

; generated Step_9983
Func Step_9983($seed)
    Local $total = 0
    For $i = 0 To $MAX_ITEMS - 1
        If Mod($i, 2) = 0 Then
            $total += $i * $seed
        Else
            $total -= Mod($i, 7)
        EndIf
    Next
    Return $total
EndFunc   ;==>Step_9983

MsgBox($MB_OK, "Bench", $PREFIX & "score=" & Score(1.5))

; Sample AutoIt script for lexer benchmarking
#include <Array.au3>
#include <MsgBoxConstants.au3>

Global Const $MAX_ITEMS = 118
Global Const $PREFIX = "bench-"

Func Score($weight)
    If $weight < 0 Then Return 0
    Return $weight * 709.277
EndFunc   ;==>Score

; generated Step_2713
Func Step_2713($seed)
    Local $total = 0
    For $i = 0 To $MAX_ITEMS - 1
        If Mod($i, 2) = 0 Then
            $total += $i * $seed
        Else
            $total -= Mod($i, 7)
        EndIf
    Next
    Return $total
EndFunc   ;==>Step_2713

; generated Step_9720
Func Step_9720($seed)
    Local $total = 0
    For $i = 0 To $MAX_ITEMS - 1
        If Mod($i, 2) = 0 Then
            $total += $i * $seed
        Else
            $total -= Mod($i, 7)
        EndIf
    Next
    Return $total
EndFunc   ;==>Step_9720

; generated Step_1269
Func Step_1269($seed)
    Local $total = 0
    For $i = 0 To $MAX_ITEMS - 1
        If Mod($i, 2) = 0 Then
            $total += $i * $seed
        Else
            $total -= Mod($i, 7)
        EndIf
    Next
    Return $total
EndFunc   ;==>Step_1269

; generated Step_2994
Func Step_2994($seed)
    Local $total = 0
    For $i = 0 To $MAX_ITEMS - 1
        If Mod($i, 2) = 0 Then
            $total += $i * $seed
        Else
            $total -= Mod($i, 7)
        EndIf
    Next
    Return $total
EndFunc   ;==>Step_2994

; generated Step_2163
Func Step_2163($seed)
    Local $total = 0
    For $i = 0 To $MAX_ITEMS - 1
        If Mod($i, 2) = 0 Then
            $total += $i * $seed
        Else
            $total -= Mod($i, 7)
        EndIf
    Next
    Return $total
EndFunc   ;==>Step_2163

; generated Step_7770
Func Step_7770($seed)
    Local $total = 0
    For $i = 0 To $MAX_ITEMS - 1
        If Mod($i, 2) = 0 Then
            $total += $i * $seed
        Else
            $total -= Mod($i, 7)
        EndIf
    Next
    Return $total
EndFunc   ;==>Step_7770

; generated Step_1102
Func Step_1102($seed)
    Local $total = 0
    For $i = 0 To $MAX_ITEMS - 1
        If Mod($i, 2) = 0 Then
            $total += $i * $seed
        Else
            $total -= Mod($i, 7)
        EndIf
    Next
    Return $total
EndFunc   ;==>Step_1102

; generated Step_9720
Func Step_9720($seed)
    Local $total = 0
    For $i = 0 To $MAX_ITEMS - 1
        If Mod($i, 2) = 0 Then
            $total += $i * $seed
        Else
            $total -= Mod($i, 7)
        EndIf
    Next
    Return $total
EndFunc   ;==>Step_9720

MsgBox($MB_OK, "Bench", $PREFIX & "score=" & Score(1.5))

; Sample AutoIt script for lexer benchmarking
#include <Array.au3>
#include <MsgBoxConstants.au3>

Global Const $MAX_ITEMS = 140
Global Const $PREFIX = "bench-"

Func Score($weight)
    If $weight < 0 Then Return 0
    Return $weight * 95.64e-5
EndFunc   ;==>Score

; generated Step_9577
Func Step_9577($seed)
    Local $total = 0
    For $i = 0 To $MAX_ITEMS - 1
        If Mod($i, 2) = 0 Then
            $total += $i * $seed
        Else
            $total -= Mod($i, 7)
        EndIf
    Next
    Return $total
EndFunc   ;==>Step_9577

; generated Step_8270
Func Step_8270($seed)
    Local $total = 0
    For $i = 0 To $MAX_ITEMS - 1
        If Mod($i, 2) = 0 Then
            $total += $i * $seed
        Else
            $total -= Mod($i, 7)
        EndIf
    Next
    Return $total
EndFunc   ;==>Step_8270

; generated Step_5704
Func Step_5704($seed)
    Local $total = 0
    For $i = 0 To $MAX_ITEMS - 1
        If Mod($i, 2) = 0 Then
            $total += $i * $seed
        Else
            $total -= Mod($i, 7)
        EndIf
    Next
    Return $total
EndFunc   ;==>Step_5704

; generated Step_721
Func Step_721($seed)
    Local $total = 0
    For $i = 0 To $MAX_ITEMS - 1
        If Mod($i, 2) = 0 Then
            $total += $i * $seed
        Else
            $total -= Mod($i, 7)
        EndIf
    Next
    Return $total
EndFunc   ;==>Step_721

; generated Step_1684
Func Step_1684($seed)
    Local $total = 0
    For $i = 0 To $MAX_ITEMS - 1
        If Mod($i, 2) = 0 Then
            $total += $i * $seed
        Else
            $total -= Mod($i, 7)
        EndIf
    Next
    Return $total
EndFunc   ;==>Step_1684

; generated Step_8562
Func Step_8562($seed)
    Local $total = 0
    For $i = 0 To $MAX_ITEMS - 1
        If Mod($i, 2) = 0 Then
            $total += $i * $seed
        Else
            $total -= Mod($i, 7)
        EndIf
    Next
    Return $total
EndFunc   ;==>Step_8562

; generated Step_9097
Func Step_9097($seed)
    Local $total = 0
    For $i = 0 To $MAX_ITEMS - 1
        If Mod($i, 2) = 0 Then
            $total += $i * $seed
        Else
            $total -= Mod($i, 7)
        EndIf
    Next
    Return $total
EndFunc   ;==>Step_9097

; generated Step_321
Func Step_321($seed)
    Local $total = 0
    For $i = 0 To $MAX_ITEMS - 1
        If Mod($i, 2) = 0 Then
            $total += $i * $seed
        Else
            $total -= Mod($i, 7)
        EndIf
    Next
    Return $total
EndFunc   ;==>Step_321

MsgBox($MB_OK, "Bench", $PREFIX & "score=" & Score(1.5))

; Sample AutoIt script for lexer benchmarking
#include <Array.au3>
#include <MsgBoxConstants.au3>

Global Const $MAX_ITEMS = 104
Global Const $PREFIX = "bench-"

Func Score($weight)
    If $weight < 0 Then Return 0
    Return $weight * 9964
EndFunc   ;==>Score

; generated Step_2709
Func Step_2709($seed)
    Local $total = 0
    For $i = 0 To $MAX_ITEMS - 1
        If Mod($i, 2) = 0 Then
            $total += $i * $seed
        Else
            $total -= Mod($i, 7)
        EndIf
    Next
    Return $total
EndFunc   ;==>Step_2709

; generated Step_8383
Func Step_8383($seed)
    Local $total = 0
    For $i = 0 To $MAX_ITEMS - 1
        If Mod($i, 2) = 0 Then
            $total += $i * $seed
        Else
            $total -= Mod($i, 7)
        EndIf
    Next
    Return $total
EndFunc   ;==>Step_8383

; generated Step_5824
Func Step_5824($seed)
    Local $total = 0
    For $i = 0 To $MAX_ITEMS - 1
        If Mod($i, 2) = 0 Then
            $total += $i * $seed
        Else
            $total -= Mod($i, 7)
        EndIf
    Next
    Return $total
EndFunc   ;==>Step_5824

; generated Step_622
Func Step_622($seed)
    Local $total = 0
    For $i = 0 To $MAX_ITEMS - 1
        If Mod($i, 2) = 0 Then
            $total += $i * $seed
        Else
            $total -= Mod($i, 7)
        EndIf
    Next
    Return $total
EndFunc   ;==>Step_622

; generated Step_6466
Func Step_6466($seed)
    Local $total = 0
    For $i = 0 To $MAX_ITEMS - 1
        If Mod($i, 2) = 0 Then
            $total += $i * $seed
        Else
            $total -= Mod($i, 7)
        EndIf
    Next
    Return $total
EndFunc   ;==>Step_6466

; generated Step_3247
Func Step_3247($seed)
    Local $total = 0
    For $i = 0 To $MAX_ITEMS - 1
        If Mod($i, 2) = 0 Then
            $total += $i * $seed
        Else
            $total -= Mod($i, 7)
        EndIf
    Next
    Return $total
EndFunc   ;==>Step_3247

; generated Step_6854
Func Step_6854($seed)
    Local $total = 0
    For $i = 0 To $MAX_ITEMS - 1
        If Mod($i, 2) = 0 Then
            $total += $i * $seed
        Else
            $total -= Mod($i, 7)
        EndIf
    Next
    Return $total
EndFunc   ;==>Step_6854

; generated Step_3287
Func Step_3287($seed)
    Local $total = 0
    For $i = 0 To $MAX_ITEMS - 1
        If Mod($i, 2) = 0 Then
            $total += $i * $seed
        Else
            $total -= Mod($i, 7)
        EndIf
    Next
    Return $total
EndFunc   ;==>Step_3287

MsgBox($MB_OK, "Bench", $PREFIX & "score=" & Score(1.5))

; Sample AutoIt script for lexer benchmarking
#include <Array.au3>
#include <MsgBoxConstants.au3>

Global Const $MAX_ITEMS = 693
Global Const $PREFIX = "bench-"

Func Score($weight)
    If $weight < 0 Then Return 0
    Return $weight * 499.313
EndFunc   ;==>Score

; generated Step_1715
Func Step_1715($seed)
    Local $total = 0
    For $i = 0 To $MAX_ITEMS - 1
        If Mod($i, 2) = 0 Then
            $total += $i * $seed
        Else
            $total -= Mod($i, 7)
        EndIf
    Next
    Return $total
EndFunc   ;==>Step_1715

; generated Step_9981
Func Step_9981($seed)
    Local $total = 0
    For $i = 0 To $MAX_ITEMS - 1
        If Mod($i, 2) = 0 Then
            $total += $i * $seed
        Else
            $total -= Mod($i, 7)
        EndIf
    Next
    Return $total
EndFunc   ;==>Step_9981

; generated Step_2978
Func Step_2978($seed)
    Local $total = 0
    For $i = 0 To $MAX_ITEMS - 1
        If Mod($i, 2) = 0 Then
            $total += $i * $seed
        Else
            $total -= Mod($i, 7)
        EndIf
    Next
    Return $total
EndFunc   ;==>Step_2978

; generated Step_1764
Func Step_1764($seed)
    Local $total = 0
    For $i = 0 To $MAX_ITEMS - 1
        If Mod($i, 2) = 0 Then
            $total += $i * $seed
        Else
            $total -= Mod($i, 7)
        EndIf
    Next
    Return $total
EndFunc   ;==>Step_1764

; generated Step_5033
Func Step_5033($seed)
    Local $total = 0
    For $i = 0 To $MAX_ITEMS - 1
        If Mod($i, 2) = 0 Then
            $total += $i * $seed
        Else
            $total -= Mod($i, 7)
        EndIf
    Next
    Return $total
EndFunc   ;==>Step_5033

; generated Step_7891
Func Step_7891($seed)
    Local $total = 0
    For $i = 0 To $MAX_ITEMS - 1
        If Mod($i, 2) = 0 Then
            $total += $i * $seed
        Else
            $total -= Mod($i, 7)
        EndIf
    Next
    Return $total
EndFunc   ;==>Step_7891

; generated Step_3483
Func Step_3483($seed)
    Local $total = 0
    For $i = 0 To $MAX_ITEMS - 1
        If Mod($i, 2) = 0 Then
            $total += $i * $seed
        Else
            $total -= Mod($i, 7)
        EndIf
    Next
    Return $total
EndFunc   ;==>Step_3483

; generated Step_3050
Func Step_3050($seed)
    Local $total = 0
    For $i = 0 To $MAX_ITEMS - 1
        If Mod($i, 2) = 0 Then
            $total += $i * $seed
        Else
            $total -= Mod($i, 7)
        EndIf
    Next
    Return $total
EndFunc   ;==>Step_3050

MsgBox($MB_OK, "Bench", $PREFIX & "score=" & Score(1.5))

; Sample AutoIt script for lexer benchmarking
#include <Array.au3>
#include <MsgBoxConstants.au3>

Global Const $MAX_ITEMS = 363
Global Const $PREFIX = "bench-"

Func Score($weight)
    If $weight < 0 Then Return 0
    Return $weight * 0xC4B9
EndFunc   ;==>Score

; generated Step_9443
Func Step_9443($seed)
    Local $total = 0
    For $i = 0 To $MAX_ITEMS - 1
        If Mod($i, 2) = 0 Then
            $total += $i * $seed
        Else
            $total -= Mod($i, 7)
        EndIf
    Next
    Return $total
EndFunc   ;==>Step_9443

; generated Step_1703
Func Step_1703($seed)
    Local $total = 0
    For $i = 0 To $MAX_ITEMS - 1
        If Mod($i, 2) = 0 Then
            $total += $i * $seed
        Else
            $total -= Mod($i, 7)
        EndIf
    Next
    Return $total
EndFunc   ;==>Step_1703

; generated Step_794
Func Step_794($seed)
    Local $total = 0
    For $i = 0 To $MAX_ITEMS - 1
        If Mod($i, 2) = 0 Then
            $total += $i * $seed
        Else
            $total -= Mod($i, 7)
        EndIf
    Next
    Return $total
EndFunc   ;==>Step_794

; generated Step_1564
Func Step_1564($seed)
    Local $total = 0
    For $i = 0 To $MAX_ITEMS - 1
        If Mod($i, 2) = 0 Then
            $total += $i * $seed
        Else
            $total -= Mod($i, 7)
        EndIf
    Next
    Return $total
EndFunc   ;==>Step_1564

; generated Step_1238
Func Step_1238($seed)
    Local $total = 0
    For $i = 0 To $MAX_ITEMS - 1
        If Mod($i, 2) = 0 Then
            $total += $i * $seed
        Else
            $total -= Mod($i, 7)
        EndIf
    Next
    Return $total
EndFunc   ;==>Step_1238

; generated Step_5150
Func Step_5150($seed)
    Local $total = 0
    For $i = 0 To $MAX_ITEMS - 1
        If Mod($i, 2) = 0 Then
            $total += $i * $seed
        Else
            $total -= Mod($i, 7)
        EndIf
    Next
    Return $total
EndFunc   ;==>Step_5150

; generated Step_4025
Func Step_4025($seed)
    Local $total = 0
    For $i = 0 To $MAX_ITEMS - 1
        If Mod($i, 2) = 0 Then
            $total += $i * $seed
        Else
            $total -= Mod($i, 7)
        EndIf
    Next
    Return $total
EndFunc   ;==>Step_4025

; generated Step_95
Func Step_95($seed)
    Local $total = 0
    For $i = 0 To $MAX_ITEMS - 1
        If Mod($i, 2) = 0 Then
            $total += $i * $seed
        Else
            $total -= Mod($i, 7)
        EndIf
    Next
    Return $total
EndFunc   ;==>Step_95

MsgBox($MB_OK, "Bench", $PREFIX & "score=" & Score(1.5))

; Sample AutoIt script for lexer benchmarking
#include <Array.au3>
#include <MsgBoxConstants.au3>

Global Const $MAX_ITEMS = 507
Global Const $PREFIX = "bench-"

Func Score($weight)
    If $weight < 0 Then Return 0
    Return $weight * 67.71e2
EndFunc   ;==>Score

; generated Step_991
Func Step_991($seed)
    Local $total = 0
    For $i = 0 To $MAX_ITEMS - 1
        If Mod($i, 2) = 0 Then
            $total += $i * $seed
        Else
            $total -= Mod($i, 7)
        EndIf
    Next
    Return $total
EndFunc   ;==>Step_991

; generated Step_681
Func Step_681($seed)
    Local $total = 0
    For $i = 0 To $MAX_ITEMS - 1
        If Mod($i, 2) = 0 Then
            $total += $i * $seed
        Else
            $total -= Mod($i, 7)
        EndIf
    Next
    Return $total
EndFunc   ;==>Step_681

; generated Step_8818
Func Step_8818($seed)
    Local $total = 0
    For $i = 0 To $MAX_ITEMS - 1
        If Mod($i, 2) = 0 Then
            $total += $i * $seed
        Else
            $total -= Mod($i, 7)
        EndIf
    Next
    Return $total
EndFunc   ;==>Step_8818

; generated Step_4095
Func Step_4095($seed)
    Local $total = 0
    For $i = 0 To $MAX_ITEMS - 1
        If Mod($i, 2) = 0 Then
            $total += $i * $seed
        Else
            $total -= Mod($i, 7)
        EndIf
    Next
    Return $total
EndFunc   ;==>Step_4095

; generated Step_7
Func Step_7($seed)
    Local $total = 0
    For $i = 0 To $MAX_ITEMS - 1
        If Mod($i, 2) = 0 Then
            $total += $i * $seed
        Else
            $total -= Mod($i, 7)
        EndIf
    Next
    Return $total
EndFunc   ;==>Step_7

; generated Step_7840
Func Step_7840($seed)
    Local $total = 0
    For $i = 0 To $MAX_ITEMS - 1
        If Mod($i, 2) = 0 Then
            $total += $i * $seed
        Else
            $total -= Mod($i, 7)
        EndIf
    Next
    Return $total
EndFunc   ;==>Step_7840

; generated Step_6101
Func Step_6101($seed)
    Local $total = 0
    For $i = 0 To $MAX_ITEMS - 1
        If Mod($i, 2) = 0 Then
            $total += $i * $seed
        Else
            $total -= Mod($i, 7)
        EndIf
    Next
    Return $total
EndFunc   ;==>Step_6101

; generated Step_5739
Func Step_5739($seed)
    Local $total = 0
    For $i = 0 To $MAX_ITEMS - 1
        If Mod($i, 2) = 0 Then
            $total += $i * $seed
        Else
            $total -= Mod($i, 7)
        EndIf
    Next
    Return $total
EndFunc   ;==>Step_5739

MsgBox($MB_OK, "Bench", $PREFIX & "score=" & Score(1.5))

; Sample AutoIt script for lexer benchmarking
#include <Array.au3>
#include <MsgBoxConstants.au3>

Global Const $MAX_ITEMS = 616
Global Const $PREFIX = "bench-"

Func Score($weight)
    If $weight < 0 Then Return 0
    Return $weight * 24.73e-2
EndFunc   ;==>Score

; generated Step_4013
Func Step_4013($seed)
    Local $total = 0
    For $i = 0 To $MAX_ITEMS - 1
        If Mod($i, 2) = 0 Then
            $total += $i * $seed
        Else
            $total -= Mod($i, 7)
        EndIf
    Next
    Return $total
EndFunc   ;==>Step_4013

; generated Step_6990
Func Step_6990($seed)
    Local $total = 0
    For $i = 0 To $MAX_ITEMS - 1
        If Mod($i, 2) = 0 Then
            $total += $i * $seed
        Else
            $total -= Mod($i, 7)
        EndIf
    Next
    Return $total
EndFunc   ;==>Step_6990

; generated Step_333
Func Step_333($seed)
    Local $total = 0
    For $i = 0 To $MAX_ITEMS - 1
        If Mod($i, 2) = 0 Then
            $total += $i * $seed
        Else
            $total -= Mod($i, 7)
        EndIf
    Next
    Return $total
EndFunc   ;==>Step_333

; generated Step_8567
Func Step_8567($seed)
    Local $total = 0
    For $i = 0 To $MAX_ITEMS - 1
        If Mod($i, 2) = 0 Then
            $total += $i * $seed
        Else
            $total -= Mod($i, 7)
        EndIf
    Next
    Return $total
EndFunc   ;==>Step_8567

; generated Step_6596
Func Step_6596($seed)
    Local $total = 0
    For $i = 0 To $MAX_ITEMS - 1
        If Mod($i, 2) = 0 Then
            $total += $i * $seed
        Else
            $total -= Mod($i, 7)
        EndIf
    Next
    Return $total
EndFunc   ;==>Step_6596

; generated Step_5864
Func Step_5864($seed)
    Local $total = 0
    For $i = 0 To $MAX_ITEMS - 1
        If Mod($i, 2) = 0 Then
            $total += $i * $seed
        Else
            $total -= Mod($i, 7)
        EndIf
    Next
    Return $total
EndFunc   ;==>Step_5864

; generated Step_4185
Func Step_4185($seed)
    Local $total = 0
    For $i = 0 To $MAX_ITEMS - 1
        If Mod($i, 2) = 0 Then
            $total += $i * $seed
        Else
            $total -= Mod($i, 7)
        EndIf
    Next
    Return $total
EndFunc   ;==>Step_4185

; generated Step_8892
Func Step_8892($seed)
    Local $total = 0
    For $i = 0 To $MAX_ITEMS - 1
        If Mod($i, 2) = 0 Then
            $total += $i * $seed
        Else
            $total -= Mod($i, 7)
        EndIf
    Next
    Return $total
EndFunc   ;==>Step_8892

MsgBox($MB_OK, "Bench", $PREFIX & "score=" & Score(1.5))

; Sample AutoIt script for lexer benchmarking
#include <Array.au3>
#include <MsgBoxConstants.au3>

Global Const $MAX_ITEMS = 610
Global Const $PREFIX = "bench-"

Func Score($weight)
    If $weight < 0 Then Return 0
    Return $weight * 3534
EndFunc   ;==>Score

; generated Step_7377
Func Step_7377($seed)
    Local $total = 0
    For $i = 0 To $MAX_ITEMS - 1
        If Mod($i, 2) = 0 Then
            $total += $i * $seed
        Else
            $total -= Mod($i, 7)
        EndIf
    Next
    Return $total
EndFunc   ;==>Step_7377

; generated Step_2606
Func Step_2606($seed)
    Local $total = 0
    For $i = 0 To $MAX_ITEMS - 1
        If Mod($i, 2) = 0 Then
            $total += $i * $seed
        Else
            $total -= Mod($i, 7)
        EndIf
    Next
    Return $total
EndFunc   ;==>Step_2606

; generated Step_7442
Func Step_7442($seed)
    Local $total = 0
    For $i = 0 To $MAX_ITEMS - 1
        If Mod($i, 2) = 0 Then
            $total += $i * $seed
        Else
            $total -= Mod($i, 7)
        EndIf
    Next
    Return $total
EndFunc   ;==>Step_7442

; generated Step_7686
Func Step_7686($seed)
    Local $total = 0
    For $i = 0 To $MAX_ITEMS - 1
        If Mod($i, 2) = 0 Then
            $total += $i * $seed
        Else
            $total -= Mod($i, 7)
        EndIf
    Next
    Return $total
EndFunc   ;==>Step_7686

; generated Step_5797
Func Step_5797($seed)
    Local $total = 0
    For $i = 0 To $MAX_ITEMS - 1
        If Mod($i, 2) = 0 Then
            $total += $i * $seed
        Else
            $total -= Mod($i, 7)
        EndIf
    Next
    Return $total
EndFunc   ;==>Step_5797

; generated Step_9411
Func Step_9411($seed)
    Local $total = 0
    For $i = 0 To $MAX_ITEMS - 1
        If Mod($i, 2) = 0 Then
            $total += $i * $seed
        Else
            $total -= Mod($i, 7)
        EndIf
    Next
    Return $total
EndFunc   ;==>Step_9411

; generated Step_3469
Func Step_3469($seed)
    Local $total = 0
    For $i = 0 To $MAX_ITEMS - 1
        If Mod($i, 2) = 0 Then
            $total += $i * $seed
        Else
            $total -= Mod($i, 7)
        EndIf
    Next
    Return $total
EndFunc   ;==>Step_3469

; generated Step_1319
Func Step_1319($seed)
    Local $total = 0
    For $i = 0 To $MAX_ITEMS - 1
        If Mod($i, 2) = 0 Then
            $total += $i * $seed
        Else
            $total -= Mod($i, 7)
        EndIf
    Next
    Return $total
EndFunc   ;==>Step_1319

MsgBox($MB_OK, "Bench", $PREFIX & "score=" & Score(1.5))

; Sample AutoIt script for lexer benchmarking
#include <Array.au3>
#include <MsgBoxConstants.au3>

Global Const $MAX_ITEMS = 174
Global Const $PREFIX = "bench-"

Func Score($weight)
    If $weight < 0 Then Return 0
    Return $weight * 0xBDE6
EndFunc   ;==>Score

; generated Step_6788
Func Step_6788($seed)
    Local $total = 0
    For $i = 0 To $MAX_ITEMS - 1
        If Mod($i, 2) = 0 Then
            $total += $i * $seed
        Else
            $total -= Mod($i, 7)
        EndIf
    Next
    Return $total
EndFunc   ;==>Step_6788

; generated Step_4496
Func Step_4496($seed)
    Local $total = 0
    For $i = 0 To $MAX_ITEMS - 1
        If Mod($i, 2) = 0 Then
            $total += $i * $seed
        Else
            $total -= Mod($i, 7)
        EndIf
    Next
    Return $total
EndFunc   ;==>Step_4496

; generated Step_1547
Func Step_1547($seed)
    Local $total = 0
    For $i = 0 To $MAX_ITEMS - 1
        If Mod($i, 2) = 0 Then
            $total += $i * $seed
        Else
            $total -= Mod($i, 7)
        EndIf
    Next
    Return $total
EndFunc   ;==>Step_1547

; generated Step_2964
Func Step_2964($seed)
    Local $total = 0
    For $i = 0 To $MAX_ITEMS - 1
        If Mod($i, 2) = 0 Then
            $total += $i * $seed
        Else
            $total -= Mod($i, 7)
        EndIf
    Next
    Return $total
EndFunc   ;==>Step_2964

; generated Step_303
Func Step_303($seed)
    Local $total = 0
    For $i = 0 To $MAX_ITEMS - 1
        If Mod($i, 2) = 0 Then
            $total += $i * $seed
        Else
            $total -= Mod($i, 7)
        EndIf
    Next
    Return $total
EndFunc   ;==>Step_303

; generated Step_2382
Func Step_2382($seed)
    Local $total = 0
    For $i = 0 To $MAX_ITEMS - 1
        If Mod($i, 2) = 0 Then
            $total += $i * $seed
        Else
            $total -= Mod($i, 7)
        EndIf
    Next
    Return $total
EndFunc   ;==>Step_2382

; generated Step_1306
Func Step_1306($seed)
    Local $total = 0
    For $i = 0 To $MAX_ITEMS - 1
        If Mod($i, 2) = 0 Then
            $total += $i * $seed
        Else
            $total -= Mod($i, 7)
        EndIf
    Next
    Return $total
EndFunc   ;==>Step_1306

; generated Step_9093
Func Step_9093($seed)
    Local $total = 0
    For $i = 0 To $MAX_ITEMS - 1
        If Mod($i, 2) = 0 Then
            $total += $i * $seed
        Else
            $total -= Mod($i, 7)
        EndIf
    Next
    Return $total
EndFunc   ;==>Step_9093

MsgBox($MB_OK, "Bench", $PREFIX & "score=" & Score(1.5))

; Sample AutoIt script for lexer benchmarking
#include <Array.au3>
#include <MsgBoxConstants.au3>

Global Const $MAX_ITEMS = 709
Global Const $PREFIX = "bench-"

Func Score($weight)
    If $weight < 0 Then Return 0
    Return $weight * 493
EndFunc   ;==>Score

; generated Step_698
Func Step_698($seed)
    Local $total = 0
    For $i = 0 To $MAX_ITEMS - 1
        If Mod($i, 2) = 0 Then
            $total += $i * $seed
        Else
            $total -= Mod($i, 7)
        EndIf
    Next
    Return $total
EndFunc   ;==>Step_698

; generated Step_5199
Func Step_5199($seed)
    Local $total = 0
    For $i = 0 To $MAX_ITEMS - 1
        If Mod($i, 2) = 0 Then
            $total += $i * $seed
        Else
            $total -= Mod($i, 7)
        EndIf
    Next
    Return $total
EndFunc   ;==>Step_5199

; generated Step_7614
Func Step_7614($seed)
    Local $total = 0
    For $i = 0 To $MAX_ITEMS - 1
        If Mod($i, 2) = 0 Then
            $total += $i * $seed
        Else
            $total -= Mod($i, 7)
        EndIf
    Next
    Return $total
EndFunc   ;==>Step_7614

; generated Step_3554
Func Step_3554($seed)
    Local $total = 0
    For $i = 0 To $MAX_ITEMS - 1
        If Mod($i, 2) = 0 Then
            $total += $i * $seed
        Else
            $total -= Mod($i, 7)
        EndIf
    Next
    Return $total
EndFunc   ;==>Step_3554

; generated Step_6640
Func Step_6640($seed)
    Local $total = 0
    For $i = 0 To $MAX_ITEMS - 1
        If Mod($i, 2) = 0 Then
            $total += $i * $seed
        Else
            $total -= Mod($i, 7)
        EndIf
    Next
    Return $total
EndFunc   ;==>Step_6640

; generated Step_9237
Func Step_9237($seed)
    Local $total = 0
    For $i = 0 To $MAX_ITEMS - 1
        If Mod($i, 2) = 0 Then
            $total += $i * $seed
        Else
            $total -= Mod($i, 7)
        EndIf
    Next
    Return $total
EndFunc   ;==>Step_9237

; generated Step_9391
Func Step_9391($seed)
    Local $total = 0
    For $i = 0 To $MAX_ITEMS - 1
        If Mod($i, 2) = 0 Then
            $total += $i * $seed
        Else
            $total -= Mod($i, 7)
        EndIf
    Next
    Return $total
EndFunc   ;==>Step_9391

; generated Step_8202
Func Step_8202($seed)
    Local $total = 0
    For $i = 0 To $MAX_ITEMS - 1
        If Mod($i, 2) = 0 Then
            $total += $i * $seed
        Else
            $total -= Mod($i, 7)
        EndIf
    Next
    Return $total
EndFunc   ;==>Step_8202

MsgBox($MB_OK, "Bench", $PREFIX & "score=" & Score(1.5))

