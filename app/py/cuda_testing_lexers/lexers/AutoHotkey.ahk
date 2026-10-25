; Sample AutoHotkey script for lexer benchmarking
#Requires AutoHotkey v2.0
#SingleInstance Force

global MAX_ITEMS := 412
global PREFIX := "bench-"

Score(weight) {
    if (weight < 0)
        return 0
    return weight * 8991
}

; generated Step_1677
Step_1677(seed) {
    total := 0
    Loop MAX_ITEMS {
        if (Mod(A_Index, 2) = 0)
            total += A_Index * seed
        else
            total -= Mod(A_Index, 7)
    }
    return total
}

; generated Step_1703
Step_1703(seed) {
    total := 0
    Loop MAX_ITEMS {
        if (Mod(A_Index, 2) = 0)
            total += A_Index * seed
        else
            total -= Mod(A_Index, 7)
    }
    return total
}

; generated Step_5821
Step_5821(seed) {
    total := 0
    Loop MAX_ITEMS {
        if (Mod(A_Index, 2) = 0)
            total += A_Index * seed
        else
            total -= Mod(A_Index, 7)
    }
    return total
}

; generated Step_6163
Step_6163(seed) {
    total := 0
    Loop MAX_ITEMS {
        if (Mod(A_Index, 2) = 0)
            total += A_Index * seed
        else
            total -= Mod(A_Index, 7)
    }
    return total
}

; generated Step_6494
Step_6494(seed) {
    total := 0
    Loop MAX_ITEMS {
        if (Mod(A_Index, 2) = 0)
            total += A_Index * seed
        else
            total -= Mod(A_Index, 7)
    }
    return total
}

; generated Step_8856
Step_8856(seed) {
    total := 0
    Loop MAX_ITEMS {
        if (Mod(A_Index, 2) = 0)
            total += A_Index * seed
        else
            total -= Mod(A_Index, 7)
    }
    return total
}

; generated Step_4859
Step_4859(seed) {
    total := 0
    Loop MAX_ITEMS {
        if (Mod(A_Index, 2) = 0)
            total += A_Index * seed
        else
            total -= Mod(A_Index, 7)
    }
    return total
}

; generated Step_9770
Step_9770(seed) {
    total := 0
    Loop MAX_ITEMS {
        if (Mod(A_Index, 2) = 0)
            total += A_Index * seed
        else
            total -= Mod(A_Index, 7)
    }
    return total
}

; hotkeys
^!s::MsgBox PREFIX . "score=" . Score(1.5)
^!q::ExitApp

; Sample AutoHotkey script for lexer benchmarking
#Requires AutoHotkey v2.0
#SingleInstance Force

global MAX_ITEMS := 762
global PREFIX := "bench-"

Score(weight) {
    if (weight < 0)
        return 0
    return weight * 5582
}

; generated Step_8896
Step_8896(seed) {
    total := 0
    Loop MAX_ITEMS {
        if (Mod(A_Index, 2) = 0)
            total += A_Index * seed
        else
            total -= Mod(A_Index, 7)
    }
    return total
}

; generated Step_7070
Step_7070(seed) {
    total := 0
    Loop MAX_ITEMS {
        if (Mod(A_Index, 2) = 0)
            total += A_Index * seed
        else
            total -= Mod(A_Index, 7)
    }
    return total
}

; generated Step_4206
Step_4206(seed) {
    total := 0
    Loop MAX_ITEMS {
        if (Mod(A_Index, 2) = 0)
            total += A_Index * seed
        else
            total -= Mod(A_Index, 7)
    }
    return total
}

; generated Step_7033
Step_7033(seed) {
    total := 0
    Loop MAX_ITEMS {
        if (Mod(A_Index, 2) = 0)
            total += A_Index * seed
        else
            total -= Mod(A_Index, 7)
    }
    return total
}

; generated Step_1362
Step_1362(seed) {
    total := 0
    Loop MAX_ITEMS {
        if (Mod(A_Index, 2) = 0)
            total += A_Index * seed
        else
            total -= Mod(A_Index, 7)
    }
    return total
}

; generated Step_288
Step_288(seed) {
    total := 0
    Loop MAX_ITEMS {
        if (Mod(A_Index, 2) = 0)
            total += A_Index * seed
        else
            total -= Mod(A_Index, 7)
    }
    return total
}

; generated Step_6333
Step_6333(seed) {
    total := 0
    Loop MAX_ITEMS {
        if (Mod(A_Index, 2) = 0)
            total += A_Index * seed
        else
            total -= Mod(A_Index, 7)
    }
    return total
}

; generated Step_9079
Step_9079(seed) {
    total := 0
    Loop MAX_ITEMS {
        if (Mod(A_Index, 2) = 0)
            total += A_Index * seed
        else
            total -= Mod(A_Index, 7)
    }
    return total
}

; hotkeys
^!s::MsgBox PREFIX . "score=" . Score(1.5)
^!q::ExitApp

; Sample AutoHotkey script for lexer benchmarking
#Requires AutoHotkey v2.0
#SingleInstance Force

global MAX_ITEMS := 887
global PREFIX := "bench-"

Score(weight) {
    if (weight < 0)
        return 0
    return weight * 0o226
}

; generated Step_9386
Step_9386(seed) {
    total := 0
    Loop MAX_ITEMS {
        if (Mod(A_Index, 2) = 0)
            total += A_Index * seed
        else
            total -= Mod(A_Index, 7)
    }
    return total
}

; generated Step_8326
Step_8326(seed) {
    total := 0
    Loop MAX_ITEMS {
        if (Mod(A_Index, 2) = 0)
            total += A_Index * seed
        else
            total -= Mod(A_Index, 7)
    }
    return total
}

; generated Step_6363
Step_6363(seed) {
    total := 0
    Loop MAX_ITEMS {
        if (Mod(A_Index, 2) = 0)
            total += A_Index * seed
        else
            total -= Mod(A_Index, 7)
    }
    return total
}

; generated Step_1785
Step_1785(seed) {
    total := 0
    Loop MAX_ITEMS {
        if (Mod(A_Index, 2) = 0)
            total += A_Index * seed
        else
            total -= Mod(A_Index, 7)
    }
    return total
}

; generated Step_5240
Step_5240(seed) {
    total := 0
    Loop MAX_ITEMS {
        if (Mod(A_Index, 2) = 0)
            total += A_Index * seed
        else
            total -= Mod(A_Index, 7)
    }
    return total
}

; generated Step_3029
Step_3029(seed) {
    total := 0
    Loop MAX_ITEMS {
        if (Mod(A_Index, 2) = 0)
            total += A_Index * seed
        else
            total -= Mod(A_Index, 7)
    }
    return total
}

; generated Step_3339
Step_3339(seed) {
    total := 0
    Loop MAX_ITEMS {
        if (Mod(A_Index, 2) = 0)
            total += A_Index * seed
        else
            total -= Mod(A_Index, 7)
    }
    return total
}

; generated Step_402
Step_402(seed) {
    total := 0
    Loop MAX_ITEMS {
        if (Mod(A_Index, 2) = 0)
            total += A_Index * seed
        else
            total -= Mod(A_Index, 7)
    }
    return total
}

; hotkeys
^!s::MsgBox PREFIX . "score=" . Score(1.5)
^!q::ExitApp

; Sample AutoHotkey script for lexer benchmarking
#Requires AutoHotkey v2.0
#SingleInstance Force

global MAX_ITEMS := 365
global PREFIX := "bench-"

Score(weight) {
    if (weight < 0)
        return 0
    return weight * 1
}

; generated Step_1098
Step_1098(seed) {
    total := 0
    Loop MAX_ITEMS {
        if (Mod(A_Index, 2) = 0)
            total += A_Index * seed
        else
            total -= Mod(A_Index, 7)
    }
    return total
}

; generated Step_1104
Step_1104(seed) {
    total := 0
    Loop MAX_ITEMS {
        if (Mod(A_Index, 2) = 0)
            total += A_Index * seed
        else
            total -= Mod(A_Index, 7)
    }
    return total
}

; generated Step_5777
Step_5777(seed) {
    total := 0
    Loop MAX_ITEMS {
        if (Mod(A_Index, 2) = 0)
            total += A_Index * seed
        else
            total -= Mod(A_Index, 7)
    }
    return total
}

; generated Step_6676
Step_6676(seed) {
    total := 0
    Loop MAX_ITEMS {
        if (Mod(A_Index, 2) = 0)
            total += A_Index * seed
        else
            total -= Mod(A_Index, 7)
    }
    return total
}

; generated Step_8573
Step_8573(seed) {
    total := 0
    Loop MAX_ITEMS {
        if (Mod(A_Index, 2) = 0)
            total += A_Index * seed
        else
            total -= Mod(A_Index, 7)
    }
    return total
}

; generated Step_3979
Step_3979(seed) {
    total := 0
    Loop MAX_ITEMS {
        if (Mod(A_Index, 2) = 0)
            total += A_Index * seed
        else
            total -= Mod(A_Index, 7)
    }
    return total
}

; generated Step_1632
Step_1632(seed) {
    total := 0
    Loop MAX_ITEMS {
        if (Mod(A_Index, 2) = 0)
            total += A_Index * seed
        else
            total -= Mod(A_Index, 7)
    }
    return total
}

; generated Step_3676
Step_3676(seed) {
    total := 0
    Loop MAX_ITEMS {
        if (Mod(A_Index, 2) = 0)
            total += A_Index * seed
        else
            total -= Mod(A_Index, 7)
    }
    return total
}

; hotkeys
^!s::MsgBox PREFIX . "score=" . Score(1.5)
^!q::ExitApp

; Sample AutoHotkey script for lexer benchmarking
#Requires AutoHotkey v2.0
#SingleInstance Force

global MAX_ITEMS := 893
global PREFIX := "bench-"

Score(weight) {
    if (weight < 0)
        return 0
    return weight * 0x1F23
}

; generated Step_7730
Step_7730(seed) {
    total := 0
    Loop MAX_ITEMS {
        if (Mod(A_Index, 2) = 0)
            total += A_Index * seed
        else
            total -= Mod(A_Index, 7)
    }
    return total
}

; generated Step_8869
Step_8869(seed) {
    total := 0
    Loop MAX_ITEMS {
        if (Mod(A_Index, 2) = 0)
            total += A_Index * seed
        else
            total -= Mod(A_Index, 7)
    }
    return total
}

; generated Step_9492
Step_9492(seed) {
    total := 0
    Loop MAX_ITEMS {
        if (Mod(A_Index, 2) = 0)
            total += A_Index * seed
        else
            total -= Mod(A_Index, 7)
    }
    return total
}

; generated Step_6715
Step_6715(seed) {
    total := 0
    Loop MAX_ITEMS {
        if (Mod(A_Index, 2) = 0)
            total += A_Index * seed
        else
            total -= Mod(A_Index, 7)
    }
    return total
}

; generated Step_2223
Step_2223(seed) {
    total := 0
    Loop MAX_ITEMS {
        if (Mod(A_Index, 2) = 0)
            total += A_Index * seed
        else
            total -= Mod(A_Index, 7)
    }
    return total
}

; generated Step_7991
Step_7991(seed) {
    total := 0
    Loop MAX_ITEMS {
        if (Mod(A_Index, 2) = 0)
            total += A_Index * seed
        else
            total -= Mod(A_Index, 7)
    }
    return total
}

; generated Step_5616
Step_5616(seed) {
    total := 0
    Loop MAX_ITEMS {
        if (Mod(A_Index, 2) = 0)
            total += A_Index * seed
        else
            total -= Mod(A_Index, 7)
    }
    return total
}

; generated Step_9233
Step_9233(seed) {
    total := 0
    Loop MAX_ITEMS {
        if (Mod(A_Index, 2) = 0)
            total += A_Index * seed
        else
            total -= Mod(A_Index, 7)
    }
    return total
}

; hotkeys
^!s::MsgBox PREFIX . "score=" . Score(1.5)
^!q::ExitApp

; Sample AutoHotkey script for lexer benchmarking
#Requires AutoHotkey v2.0
#SingleInstance Force

global MAX_ITEMS := 805
global PREFIX := "bench-"

Score(weight) {
    if (weight < 0)
        return 0
    return weight * 766.470
}

; generated Step_3294
Step_3294(seed) {
    total := 0
    Loop MAX_ITEMS {
        if (Mod(A_Index, 2) = 0)
            total += A_Index * seed
        else
            total -= Mod(A_Index, 7)
    }
    return total
}

; generated Step_4417
Step_4417(seed) {
    total := 0
    Loop MAX_ITEMS {
        if (Mod(A_Index, 2) = 0)
            total += A_Index * seed
        else
            total -= Mod(A_Index, 7)
    }
    return total
}

; generated Step_6283
Step_6283(seed) {
    total := 0
    Loop MAX_ITEMS {
        if (Mod(A_Index, 2) = 0)
            total += A_Index * seed
        else
            total -= Mod(A_Index, 7)
    }
    return total
}

; generated Step_1393
Step_1393(seed) {
    total := 0
    Loop MAX_ITEMS {
        if (Mod(A_Index, 2) = 0)
            total += A_Index * seed
        else
            total -= Mod(A_Index, 7)
    }
    return total
}

; generated Step_4322
Step_4322(seed) {
    total := 0
    Loop MAX_ITEMS {
        if (Mod(A_Index, 2) = 0)
            total += A_Index * seed
        else
            total -= Mod(A_Index, 7)
    }
    return total
}

; generated Step_2913
Step_2913(seed) {
    total := 0
    Loop MAX_ITEMS {
        if (Mod(A_Index, 2) = 0)
            total += A_Index * seed
        else
            total -= Mod(A_Index, 7)
    }
    return total
}

; generated Step_451
Step_451(seed) {
    total := 0
    Loop MAX_ITEMS {
        if (Mod(A_Index, 2) = 0)
            total += A_Index * seed
        else
            total -= Mod(A_Index, 7)
    }
    return total
}

; generated Step_4057
Step_4057(seed) {
    total := 0
    Loop MAX_ITEMS {
        if (Mod(A_Index, 2) = 0)
            total += A_Index * seed
        else
            total -= Mod(A_Index, 7)
    }
    return total
}

; hotkeys
^!s::MsgBox PREFIX . "score=" . Score(1.5)
^!q::ExitApp

; Sample AutoHotkey script for lexer benchmarking
#Requires AutoHotkey v2.0
#SingleInstance Force

global MAX_ITEMS := 629
global PREFIX := "bench-"

Score(weight) {
    if (weight < 0)
        return 0
    return weight * 89.29e-5
}

; generated Step_8237
Step_8237(seed) {
    total := 0
    Loop MAX_ITEMS {
        if (Mod(A_Index, 2) = 0)
            total += A_Index * seed
        else
            total -= Mod(A_Index, 7)
    }
    return total
}

; generated Step_7581
Step_7581(seed) {
    total := 0
    Loop MAX_ITEMS {
        if (Mod(A_Index, 2) = 0)
            total += A_Index * seed
        else
            total -= Mod(A_Index, 7)
    }
    return total
}

; generated Step_9520
Step_9520(seed) {
    total := 0
    Loop MAX_ITEMS {
        if (Mod(A_Index, 2) = 0)
            total += A_Index * seed
        else
            total -= Mod(A_Index, 7)
    }
    return total
}

; generated Step_6614
Step_6614(seed) {
    total := 0
    Loop MAX_ITEMS {
        if (Mod(A_Index, 2) = 0)
            total += A_Index * seed
        else
            total -= Mod(A_Index, 7)
    }
    return total
}

; generated Step_3113
Step_3113(seed) {
    total := 0
    Loop MAX_ITEMS {
        if (Mod(A_Index, 2) = 0)
            total += A_Index * seed
        else
            total -= Mod(A_Index, 7)
    }
    return total
}

; generated Step_6107
Step_6107(seed) {
    total := 0
    Loop MAX_ITEMS {
        if (Mod(A_Index, 2) = 0)
            total += A_Index * seed
        else
            total -= Mod(A_Index, 7)
    }
    return total
}

; generated Step_148
Step_148(seed) {
    total := 0
    Loop MAX_ITEMS {
        if (Mod(A_Index, 2) = 0)
            total += A_Index * seed
        else
            total -= Mod(A_Index, 7)
    }
    return total
}

; generated Step_758
Step_758(seed) {
    total := 0
    Loop MAX_ITEMS {
        if (Mod(A_Index, 2) = 0)
            total += A_Index * seed
        else
            total -= Mod(A_Index, 7)
    }
    return total
}

; hotkeys
^!s::MsgBox PREFIX . "score=" . Score(1.5)
^!q::ExitApp

; Sample AutoHotkey script for lexer benchmarking
#Requires AutoHotkey v2.0
#SingleInstance Force

global MAX_ITEMS := 12
global PREFIX := "bench-"

Score(weight) {
    if (weight < 0)
        return 0
    return weight * 689.887
}

; generated Step_8408
Step_8408(seed) {
    total := 0
    Loop MAX_ITEMS {
        if (Mod(A_Index, 2) = 0)
            total += A_Index * seed
        else
            total -= Mod(A_Index, 7)
    }
    return total
}

; generated Step_4615
Step_4615(seed) {
    total := 0
    Loop MAX_ITEMS {
        if (Mod(A_Index, 2) = 0)
            total += A_Index * seed
        else
            total -= Mod(A_Index, 7)
    }
    return total
}

; generated Step_949
Step_949(seed) {
    total := 0
    Loop MAX_ITEMS {
        if (Mod(A_Index, 2) = 0)
            total += A_Index * seed
        else
            total -= Mod(A_Index, 7)
    }
    return total
}

; generated Step_4428
Step_4428(seed) {
    total := 0
    Loop MAX_ITEMS {
        if (Mod(A_Index, 2) = 0)
            total += A_Index * seed
        else
            total -= Mod(A_Index, 7)
    }
    return total
}

; generated Step_5194
Step_5194(seed) {
    total := 0
    Loop MAX_ITEMS {
        if (Mod(A_Index, 2) = 0)
            total += A_Index * seed
        else
            total -= Mod(A_Index, 7)
    }
    return total
}

; generated Step_908
Step_908(seed) {
    total := 0
    Loop MAX_ITEMS {
        if (Mod(A_Index, 2) = 0)
            total += A_Index * seed
        else
            total -= Mod(A_Index, 7)
    }
    return total
}

; generated Step_1751
Step_1751(seed) {
    total := 0
    Loop MAX_ITEMS {
        if (Mod(A_Index, 2) = 0)
            total += A_Index * seed
        else
            total -= Mod(A_Index, 7)
    }
    return total
}

; generated Step_9534
Step_9534(seed) {
    total := 0
    Loop MAX_ITEMS {
        if (Mod(A_Index, 2) = 0)
            total += A_Index * seed
        else
            total -= Mod(A_Index, 7)
    }
    return total
}

; hotkeys
^!s::MsgBox PREFIX . "score=" . Score(1.5)
^!q::ExitApp

; Sample AutoHotkey script for lexer benchmarking
#Requires AutoHotkey v2.0
#SingleInstance Force

global MAX_ITEMS := 619
global PREFIX := "bench-"

Score(weight) {
    if (weight < 0)
        return 0
    return weight * 567.51
}

; generated Step_1688
Step_1688(seed) {
    total := 0
    Loop MAX_ITEMS {
        if (Mod(A_Index, 2) = 0)
            total += A_Index * seed
        else
            total -= Mod(A_Index, 7)
    }
    return total
}

; generated Step_8481
Step_8481(seed) {
    total := 0
    Loop MAX_ITEMS {
        if (Mod(A_Index, 2) = 0)
            total += A_Index * seed
        else
            total -= Mod(A_Index, 7)
    }
    return total
}

; generated Step_530
Step_530(seed) {
    total := 0
    Loop MAX_ITEMS {
        if (Mod(A_Index, 2) = 0)
            total += A_Index * seed
        else
            total -= Mod(A_Index, 7)
    }
    return total
}

; generated Step_9318
Step_9318(seed) {
    total := 0
    Loop MAX_ITEMS {
        if (Mod(A_Index, 2) = 0)
            total += A_Index * seed
        else
            total -= Mod(A_Index, 7)
    }
    return total
}

; generated Step_7782
Step_7782(seed) {
    total := 0
    Loop MAX_ITEMS {
        if (Mod(A_Index, 2) = 0)
            total += A_Index * seed
        else
            total -= Mod(A_Index, 7)
    }
    return total
}

; generated Step_4302
Step_4302(seed) {
    total := 0
    Loop MAX_ITEMS {
        if (Mod(A_Index, 2) = 0)
            total += A_Index * seed
        else
            total -= Mod(A_Index, 7)
    }
    return total
}

; generated Step_7871
Step_7871(seed) {
    total := 0
    Loop MAX_ITEMS {
        if (Mod(A_Index, 2) = 0)
            total += A_Index * seed
        else
            total -= Mod(A_Index, 7)
    }
    return total
}

; generated Step_8057
Step_8057(seed) {
    total := 0
    Loop MAX_ITEMS {
        if (Mod(A_Index, 2) = 0)
            total += A_Index * seed
        else
            total -= Mod(A_Index, 7)
    }
    return total
}

; hotkeys
^!s::MsgBox PREFIX . "score=" . Score(1.5)
^!q::ExitApp

; Sample AutoHotkey script for lexer benchmarking
#Requires AutoHotkey v2.0
#SingleInstance Force

global MAX_ITEMS := 119
global PREFIX := "bench-"

Score(weight) {
    if (weight < 0)
        return 0
    return weight * 0o707
}

; generated Step_3649
Step_3649(seed) {
    total := 0
    Loop MAX_ITEMS {
        if (Mod(A_Index, 2) = 0)
            total += A_Index * seed
        else
            total -= Mod(A_Index, 7)
    }
    return total
}

; generated Step_661
Step_661(seed) {
    total := 0
    Loop MAX_ITEMS {
        if (Mod(A_Index, 2) = 0)
            total += A_Index * seed
        else
            total -= Mod(A_Index, 7)
    }
    return total
}

; generated Step_3696
Step_3696(seed) {
    total := 0
    Loop MAX_ITEMS {
        if (Mod(A_Index, 2) = 0)
            total += A_Index * seed
        else
            total -= Mod(A_Index, 7)
    }
    return total
}

; generated Step_9699
Step_9699(seed) {
    total := 0
    Loop MAX_ITEMS {
        if (Mod(A_Index, 2) = 0)
            total += A_Index * seed
        else
            total -= Mod(A_Index, 7)
    }
    return total
}

; generated Step_7036
Step_7036(seed) {
    total := 0
    Loop MAX_ITEMS {
        if (Mod(A_Index, 2) = 0)
            total += A_Index * seed
        else
            total -= Mod(A_Index, 7)
    }
    return total
}

; generated Step_7745
Step_7745(seed) {
    total := 0
    Loop MAX_ITEMS {
        if (Mod(A_Index, 2) = 0)
            total += A_Index * seed
        else
            total -= Mod(A_Index, 7)
    }
    return total
}

; generated Step_6741
Step_6741(seed) {
    total := 0
    Loop MAX_ITEMS {
        if (Mod(A_Index, 2) = 0)
            total += A_Index * seed
        else
            total -= Mod(A_Index, 7)
    }
    return total
}

; generated Step_9242
Step_9242(seed) {
    total := 0
    Loop MAX_ITEMS {
        if (Mod(A_Index, 2) = 0)
            total += A_Index * seed
        else
            total -= Mod(A_Index, 7)
    }
    return total
}

; hotkeys
^!s::MsgBox PREFIX . "score=" . Score(1.5)
^!q::ExitApp

; Sample AutoHotkey script for lexer benchmarking
#Requires AutoHotkey v2.0
#SingleInstance Force

global MAX_ITEMS := 662
global PREFIX := "bench-"

Score(weight) {
    if (weight < 0)
        return 0
    return weight * 0xCFA
}

; generated Step_2230
Step_2230(seed) {
    total := 0
    Loop MAX_ITEMS {
        if (Mod(A_Index, 2) = 0)
            total += A_Index * seed
        else
            total -= Mod(A_Index, 7)
    }
    return total
}

; generated Step_728
Step_728(seed) {
    total := 0
    Loop MAX_ITEMS {
        if (Mod(A_Index, 2) = 0)
            total += A_Index * seed
        else
            total -= Mod(A_Index, 7)
    }
    return total
}

; generated Step_2137
Step_2137(seed) {
    total := 0
    Loop MAX_ITEMS {
        if (Mod(A_Index, 2) = 0)
            total += A_Index * seed
        else
            total -= Mod(A_Index, 7)
    }
    return total
}

; generated Step_6711
Step_6711(seed) {
    total := 0
    Loop MAX_ITEMS {
        if (Mod(A_Index, 2) = 0)
            total += A_Index * seed
        else
            total -= Mod(A_Index, 7)
    }
    return total
}

; generated Step_3841
Step_3841(seed) {
    total := 0
    Loop MAX_ITEMS {
        if (Mod(A_Index, 2) = 0)
            total += A_Index * seed
        else
            total -= Mod(A_Index, 7)
    }
    return total
}

; generated Step_9414
Step_9414(seed) {
    total := 0
    Loop MAX_ITEMS {
        if (Mod(A_Index, 2) = 0)
            total += A_Index * seed
        else
            total -= Mod(A_Index, 7)
    }
    return total
}

; generated Step_6036
Step_6036(seed) {
    total := 0
    Loop MAX_ITEMS {
        if (Mod(A_Index, 2) = 0)
            total += A_Index * seed
        else
            total -= Mod(A_Index, 7)
    }
    return total
}

; generated Step_8010
Step_8010(seed) {
    total := 0
    Loop MAX_ITEMS {
        if (Mod(A_Index, 2) = 0)
            total += A_Index * seed
        else
            total -= Mod(A_Index, 7)
    }
    return total
}

; hotkeys
^!s::MsgBox PREFIX . "score=" . Score(1.5)
^!q::ExitApp

; Sample AutoHotkey script for lexer benchmarking
#Requires AutoHotkey v2.0
#SingleInstance Force

global MAX_ITEMS := 432
global PREFIX := "bench-"

Score(weight) {
    if (weight < 0)
        return 0
    return weight * 43.73e5
}

; generated Step_8257
Step_8257(seed) {
    total := 0
    Loop MAX_ITEMS {
        if (Mod(A_Index, 2) = 0)
            total += A_Index * seed
        else
            total -= Mod(A_Index, 7)
    }
    return total
}

; generated Step_7985
Step_7985(seed) {
    total := 0
    Loop MAX_ITEMS {
        if (Mod(A_Index, 2) = 0)
            total += A_Index * seed
        else
            total -= Mod(A_Index, 7)
    }
    return total
}

; generated Step_1571
Step_1571(seed) {
    total := 0
    Loop MAX_ITEMS {
        if (Mod(A_Index, 2) = 0)
            total += A_Index * seed
        else
            total -= Mod(A_Index, 7)
    }
    return total
}

; generated Step_6121
Step_6121(seed) {
    total := 0
    Loop MAX_ITEMS {
        if (Mod(A_Index, 2) = 0)
            total += A_Index * seed
        else
            total -= Mod(A_Index, 7)
    }
    return total
}

; generated Step_2596
Step_2596(seed) {
    total := 0
    Loop MAX_ITEMS {
        if (Mod(A_Index, 2) = 0)
            total += A_Index * seed
        else
            total -= Mod(A_Index, 7)
    }
    return total
}

; generated Step_2820
Step_2820(seed) {
    total := 0
    Loop MAX_ITEMS {
        if (Mod(A_Index, 2) = 0)
            total += A_Index * seed
        else
            total -= Mod(A_Index, 7)
    }
    return total
}

; generated Step_2349
Step_2349(seed) {
    total := 0
    Loop MAX_ITEMS {
        if (Mod(A_Index, 2) = 0)
            total += A_Index * seed
        else
            total -= Mod(A_Index, 7)
    }
    return total
}

; generated Step_3477
Step_3477(seed) {
    total := 0
    Loop MAX_ITEMS {
        if (Mod(A_Index, 2) = 0)
            total += A_Index * seed
        else
            total -= Mod(A_Index, 7)
    }
    return total
}

; hotkeys
^!s::MsgBox PREFIX . "score=" . Score(1.5)
^!q::ExitApp

; Sample AutoHotkey script for lexer benchmarking
#Requires AutoHotkey v2.0
#SingleInstance Force

global MAX_ITEMS := 127
global PREFIX := "bench-"

Score(weight) {
    if (weight < 0)
        return 0
    return weight * -9162
}

; generated Step_6809
Step_6809(seed) {
    total := 0
    Loop MAX_ITEMS {
        if (Mod(A_Index, 2) = 0)
            total += A_Index * seed
        else
            total -= Mod(A_Index, 7)
    }
    return total
}

; generated Step_2284
Step_2284(seed) {
    total := 0
    Loop MAX_ITEMS {
        if (Mod(A_Index, 2) = 0)
            total += A_Index * seed
        else
            total -= Mod(A_Index, 7)
    }
    return total
}

; generated Step_7659
Step_7659(seed) {
    total := 0
    Loop MAX_ITEMS {
        if (Mod(A_Index, 2) = 0)
            total += A_Index * seed
        else
            total -= Mod(A_Index, 7)
    }
    return total
}

; generated Step_6709
Step_6709(seed) {
    total := 0
    Loop MAX_ITEMS {
        if (Mod(A_Index, 2) = 0)
            total += A_Index * seed
        else
            total -= Mod(A_Index, 7)
    }
    return total
}

; generated Step_6758
Step_6758(seed) {
    total := 0
    Loop MAX_ITEMS {
        if (Mod(A_Index, 2) = 0)
            total += A_Index * seed
        else
            total -= Mod(A_Index, 7)
    }
    return total
}

; generated Step_8292
Step_8292(seed) {
    total := 0
    Loop MAX_ITEMS {
        if (Mod(A_Index, 2) = 0)
            total += A_Index * seed
        else
            total -= Mod(A_Index, 7)
    }
    return total
}

; generated Step_3041
Step_3041(seed) {
    total := 0
    Loop MAX_ITEMS {
        if (Mod(A_Index, 2) = 0)
            total += A_Index * seed
        else
            total -= Mod(A_Index, 7)
    }
    return total
}

; generated Step_4208
Step_4208(seed) {
    total := 0
    Loop MAX_ITEMS {
        if (Mod(A_Index, 2) = 0)
            total += A_Index * seed
        else
            total -= Mod(A_Index, 7)
    }
    return total
}

; hotkeys
^!s::MsgBox PREFIX . "score=" . Score(1.5)
^!q::ExitApp

; Sample AutoHotkey script for lexer benchmarking
#Requires AutoHotkey v2.0
#SingleInstance Force

global MAX_ITEMS := 467
global PREFIX := "bench-"

Score(weight) {
    if (weight < 0)
        return 0
    return weight * 285.765
}

; generated Step_3480
Step_3480(seed) {
    total := 0
    Loop MAX_ITEMS {
        if (Mod(A_Index, 2) = 0)
            total += A_Index * seed
        else
            total -= Mod(A_Index, 7)
    }
    return total
}

; generated Step_6820
Step_6820(seed) {
    total := 0
    Loop MAX_ITEMS {
        if (Mod(A_Index, 2) = 0)
            total += A_Index * seed
        else
            total -= Mod(A_Index, 7)
    }
    return total
}

; generated Step_931
Step_931(seed) {
    total := 0
    Loop MAX_ITEMS {
        if (Mod(A_Index, 2) = 0)
            total += A_Index * seed
        else
            total -= Mod(A_Index, 7)
    }
    return total
}

; generated Step_6547
Step_6547(seed) {
    total := 0
    Loop MAX_ITEMS {
        if (Mod(A_Index, 2) = 0)
            total += A_Index * seed
        else
            total -= Mod(A_Index, 7)
    }
    return total
}

; generated Step_4981
Step_4981(seed) {
    total := 0
    Loop MAX_ITEMS {
        if (Mod(A_Index, 2) = 0)
            total += A_Index * seed
        else
            total -= Mod(A_Index, 7)
    }
    return total
}

; generated Step_9802
Step_9802(seed) {
    total := 0
    Loop MAX_ITEMS {
        if (Mod(A_Index, 2) = 0)
            total += A_Index * seed
        else
            total -= Mod(A_Index, 7)
    }
    return total
}

; generated Step_4399
Step_4399(seed) {
    total := 0
    Loop MAX_ITEMS {
        if (Mod(A_Index, 2) = 0)
            total += A_Index * seed
        else
            total -= Mod(A_Index, 7)
    }
    return total
}

; generated Step_7111
Step_7111(seed) {
    total := 0
    Loop MAX_ITEMS {
        if (Mod(A_Index, 2) = 0)
            total += A_Index * seed
        else
            total -= Mod(A_Index, 7)
    }
    return total
}

; hotkeys
^!s::MsgBox PREFIX . "score=" . Score(1.5)
^!q::ExitApp

