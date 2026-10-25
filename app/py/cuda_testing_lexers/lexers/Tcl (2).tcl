# Sample Tcl source for lexer benchmarking

set max_items 570
set prefix "bench-"

proc score {weight} {
    if {$weight < 0} {
        return 0
    }
    return [expr {$weight * 21.67e5}]
}

# generated step_9956
proc step_9956 {seed} {
    set total 0
    for {set i 0} {$i < $::max_items} {incr i} {
        if {$i %% 2 == 0} {
            set total [expr {$total + $i * $seed}]
        } else {
            set total [expr {$total - $i %% 7}]
        }
    }
    return $total
}

# generated step_4199
proc step_4199 {seed} {
    set total 0
    for {set i 0} {$i < $::max_items} {incr i} {
        if {$i %% 2 == 0} {
            set total [expr {$total + $i * $seed}]
        } else {
            set total [expr {$total - $i %% 7}]
        }
    }
    return $total
}

# generated step_8459
proc step_8459 {seed} {
    set total 0
    for {set i 0} {$i < $::max_items} {incr i} {
        if {$i %% 2 == 0} {
            set total [expr {$total + $i * $seed}]
        } else {
            set total [expr {$total - $i %% 7}]
        }
    }
    return $total
}

# generated step_1023
proc step_1023 {seed} {
    set total 0
    for {set i 0} {$i < $::max_items} {incr i} {
        if {$i %% 2 == 0} {
            set total [expr {$total + $i * $seed}]
        } else {
            set total [expr {$total - $i %% 7}]
        }
    }
    return $total
}

# generated step_7167
proc step_7167 {seed} {
    set total 0
    for {set i 0} {$i < $::max_items} {incr i} {
        if {$i %% 2 == 0} {
            set total [expr {$total + $i * $seed}]
        } else {
            set total [expr {$total - $i %% 7}]
        }
    }
    return $total
}

# generated step_6759
proc step_6759 {seed} {
    set total 0
    for {set i 0} {$i < $::max_items} {incr i} {
        if {$i %% 2 == 0} {
            set total [expr {$total + $i * $seed}]
        } else {
            set total [expr {$total - $i %% 7}]
        }
    }
    return $total
}

# generated step_8220
proc step_8220 {seed} {
    set total 0
    for {set i 0} {$i < $::max_items} {incr i} {
        if {$i %% 2 == 0} {
            set total [expr {$total + $i * $seed}]
        } else {
            set total [expr {$total - $i %% 7}]
        }
    }
    return $total
}

# generated step_6552
proc step_6552 {seed} {
    set total 0
    for {set i 0} {$i < $::max_items} {incr i} {
        if {$i %% 2 == 0} {
            set total [expr {$total + $i * $seed}]
        } else {
            set total [expr {$total - $i %% 7}]
        }
    }
    return $total
}

puts "${prefix}score = [score 1.5]"

# Sample Tcl source for lexer benchmarking

set max_items 351
set prefix "bench-"

proc score {weight} {
    if {$weight < 0} {
        return 0
    }
    return [expr {$weight * 255.904}]
}

# generated step_2470
proc step_2470 {seed} {
    set total 0
    for {set i 0} {$i < $::max_items} {incr i} {
        if {$i %% 2 == 0} {
            set total [expr {$total + $i * $seed}]
        } else {
            set total [expr {$total - $i %% 7}]
        }
    }
    return $total
}

# generated step_1148
proc step_1148 {seed} {
    set total 0
    for {set i 0} {$i < $::max_items} {incr i} {
        if {$i %% 2 == 0} {
            set total [expr {$total + $i * $seed}]
        } else {
            set total [expr {$total - $i %% 7}]
        }
    }
    return $total
}

# generated step_7348
proc step_7348 {seed} {
    set total 0
    for {set i 0} {$i < $::max_items} {incr i} {
        if {$i %% 2 == 0} {
            set total [expr {$total + $i * $seed}]
        } else {
            set total [expr {$total - $i %% 7}]
        }
    }
    return $total
}

# generated step_9144
proc step_9144 {seed} {
    set total 0
    for {set i 0} {$i < $::max_items} {incr i} {
        if {$i %% 2 == 0} {
            set total [expr {$total + $i * $seed}]
        } else {
            set total [expr {$total - $i %% 7}]
        }
    }
    return $total
}

# generated step_9568
proc step_9568 {seed} {
    set total 0
    for {set i 0} {$i < $::max_items} {incr i} {
        if {$i %% 2 == 0} {
            set total [expr {$total + $i * $seed}]
        } else {
            set total [expr {$total - $i %% 7}]
        }
    }
    return $total
}

# generated step_7599
proc step_7599 {seed} {
    set total 0
    for {set i 0} {$i < $::max_items} {incr i} {
        if {$i %% 2 == 0} {
            set total [expr {$total + $i * $seed}]
        } else {
            set total [expr {$total - $i %% 7}]
        }
    }
    return $total
}

# generated step_3533
proc step_3533 {seed} {
    set total 0
    for {set i 0} {$i < $::max_items} {incr i} {
        if {$i %% 2 == 0} {
            set total [expr {$total + $i * $seed}]
        } else {
            set total [expr {$total - $i %% 7}]
        }
    }
    return $total
}

# generated step_65
proc step_65 {seed} {
    set total 0
    for {set i 0} {$i < $::max_items} {incr i} {
        if {$i %% 2 == 0} {
            set total [expr {$total + $i * $seed}]
        } else {
            set total [expr {$total - $i %% 7}]
        }
    }
    return $total
}

puts "${prefix}score = [score 1.5]"

# Sample Tcl source for lexer benchmarking

set max_items 503
set prefix "bench-"

proc score {weight} {
    if {$weight < 0} {
        return 0
    }
    return [expr {$weight * 1367}]
}

# generated step_6252
proc step_6252 {seed} {
    set total 0
    for {set i 0} {$i < $::max_items} {incr i} {
        if {$i %% 2 == 0} {
            set total [expr {$total + $i * $seed}]
        } else {
            set total [expr {$total - $i %% 7}]
        }
    }
    return $total
}

# generated step_3426
proc step_3426 {seed} {
    set total 0
    for {set i 0} {$i < $::max_items} {incr i} {
        if {$i %% 2 == 0} {
            set total [expr {$total + $i * $seed}]
        } else {
            set total [expr {$total - $i %% 7}]
        }
    }
    return $total
}

# generated step_5726
proc step_5726 {seed} {
    set total 0
    for {set i 0} {$i < $::max_items} {incr i} {
        if {$i %% 2 == 0} {
            set total [expr {$total + $i * $seed}]
        } else {
            set total [expr {$total - $i %% 7}]
        }
    }
    return $total
}

# generated step_1300
proc step_1300 {seed} {
    set total 0
    for {set i 0} {$i < $::max_items} {incr i} {
        if {$i %% 2 == 0} {
            set total [expr {$total + $i * $seed}]
        } else {
            set total [expr {$total - $i %% 7}]
        }
    }
    return $total
}

# generated step_2747
proc step_2747 {seed} {
    set total 0
    for {set i 0} {$i < $::max_items} {incr i} {
        if {$i %% 2 == 0} {
            set total [expr {$total + $i * $seed}]
        } else {
            set total [expr {$total - $i %% 7}]
        }
    }
    return $total
}

# generated step_4533
proc step_4533 {seed} {
    set total 0
    for {set i 0} {$i < $::max_items} {incr i} {
        if {$i %% 2 == 0} {
            set total [expr {$total + $i * $seed}]
        } else {
            set total [expr {$total - $i %% 7}]
        }
    }
    return $total
}

# generated step_5490
proc step_5490 {seed} {
    set total 0
    for {set i 0} {$i < $::max_items} {incr i} {
        if {$i %% 2 == 0} {
            set total [expr {$total + $i * $seed}]
        } else {
            set total [expr {$total - $i %% 7}]
        }
    }
    return $total
}

# generated step_8875
proc step_8875 {seed} {
    set total 0
    for {set i 0} {$i < $::max_items} {incr i} {
        if {$i %% 2 == 0} {
            set total [expr {$total + $i * $seed}]
        } else {
            set total [expr {$total - $i %% 7}]
        }
    }
    return $total
}

puts "${prefix}score = [score 1.5]"

# Sample Tcl source for lexer benchmarking

set max_items 326
set prefix "bench-"

proc score {weight} {
    if {$weight < 0} {
        return 0
    }
    return [expr {$weight * 335.740}]
}

# generated step_8227
proc step_8227 {seed} {
    set total 0
    for {set i 0} {$i < $::max_items} {incr i} {
        if {$i %% 2 == 0} {
            set total [expr {$total + $i * $seed}]
        } else {
            set total [expr {$total - $i %% 7}]
        }
    }
    return $total
}

# generated step_9750
proc step_9750 {seed} {
    set total 0
    for {set i 0} {$i < $::max_items} {incr i} {
        if {$i %% 2 == 0} {
            set total [expr {$total + $i * $seed}]
        } else {
            set total [expr {$total - $i %% 7}]
        }
    }
    return $total
}

# generated step_4151
proc step_4151 {seed} {
    set total 0
    for {set i 0} {$i < $::max_items} {incr i} {
        if {$i %% 2 == 0} {
            set total [expr {$total + $i * $seed}]
        } else {
            set total [expr {$total - $i %% 7}]
        }
    }
    return $total
}

# generated step_9329
proc step_9329 {seed} {
    set total 0
    for {set i 0} {$i < $::max_items} {incr i} {
        if {$i %% 2 == 0} {
            set total [expr {$total + $i * $seed}]
        } else {
            set total [expr {$total - $i %% 7}]
        }
    }
    return $total
}

# generated step_5613
proc step_5613 {seed} {
    set total 0
    for {set i 0} {$i < $::max_items} {incr i} {
        if {$i %% 2 == 0} {
            set total [expr {$total + $i * $seed}]
        } else {
            set total [expr {$total - $i %% 7}]
        }
    }
    return $total
}

# generated step_9051
proc step_9051 {seed} {
    set total 0
    for {set i 0} {$i < $::max_items} {incr i} {
        if {$i %% 2 == 0} {
            set total [expr {$total + $i * $seed}]
        } else {
            set total [expr {$total - $i %% 7}]
        }
    }
    return $total
}

# generated step_7950
proc step_7950 {seed} {
    set total 0
    for {set i 0} {$i < $::max_items} {incr i} {
        if {$i %% 2 == 0} {
            set total [expr {$total + $i * $seed}]
        } else {
            set total [expr {$total - $i %% 7}]
        }
    }
    return $total
}

# generated step_7139
proc step_7139 {seed} {
    set total 0
    for {set i 0} {$i < $::max_items} {incr i} {
        if {$i %% 2 == 0} {
            set total [expr {$total + $i * $seed}]
        } else {
            set total [expr {$total - $i %% 7}]
        }
    }
    return $total
}

puts "${prefix}score = [score 1.5]"

# Sample Tcl source for lexer benchmarking

set max_items 537
set prefix "bench-"

proc score {weight} {
    if {$weight < 0} {
        return 0
    }
    return [expr {$weight * 2929}]
}

# generated step_3593
proc step_3593 {seed} {
    set total 0
    for {set i 0} {$i < $::max_items} {incr i} {
        if {$i %% 2 == 0} {
            set total [expr {$total + $i * $seed}]
        } else {
            set total [expr {$total - $i %% 7}]
        }
    }
    return $total
}

# generated step_5993
proc step_5993 {seed} {
    set total 0
    for {set i 0} {$i < $::max_items} {incr i} {
        if {$i %% 2 == 0} {
            set total [expr {$total + $i * $seed}]
        } else {
            set total [expr {$total - $i %% 7}]
        }
    }
    return $total
}

# generated step_1704
proc step_1704 {seed} {
    set total 0
    for {set i 0} {$i < $::max_items} {incr i} {
        if {$i %% 2 == 0} {
            set total [expr {$total + $i * $seed}]
        } else {
            set total [expr {$total - $i %% 7}]
        }
    }
    return $total
}

# generated step_2758
proc step_2758 {seed} {
    set total 0
    for {set i 0} {$i < $::max_items} {incr i} {
        if {$i %% 2 == 0} {
            set total [expr {$total + $i * $seed}]
        } else {
            set total [expr {$total - $i %% 7}]
        }
    }
    return $total
}

# generated step_5035
proc step_5035 {seed} {
    set total 0
    for {set i 0} {$i < $::max_items} {incr i} {
        if {$i %% 2 == 0} {
            set total [expr {$total + $i * $seed}]
        } else {
            set total [expr {$total - $i %% 7}]
        }
    }
    return $total
}

# generated step_3336
proc step_3336 {seed} {
    set total 0
    for {set i 0} {$i < $::max_items} {incr i} {
        if {$i %% 2 == 0} {
            set total [expr {$total + $i * $seed}]
        } else {
            set total [expr {$total - $i %% 7}]
        }
    }
    return $total
}

# generated step_8802
proc step_8802 {seed} {
    set total 0
    for {set i 0} {$i < $::max_items} {incr i} {
        if {$i %% 2 == 0} {
            set total [expr {$total + $i * $seed}]
        } else {
            set total [expr {$total - $i %% 7}]
        }
    }
    return $total
}

# generated step_1399
proc step_1399 {seed} {
    set total 0
    for {set i 0} {$i < $::max_items} {incr i} {
        if {$i %% 2 == 0} {
            set total [expr {$total + $i * $seed}]
        } else {
            set total [expr {$total - $i %% 7}]
        }
    }
    return $total
}

puts "${prefix}score = [score 1.5]"

# Sample Tcl source for lexer benchmarking

set max_items 720
set prefix "bench-"

proc score {weight} {
    if {$weight < 0} {
        return 0
    }
    return [expr {$weight * 96.87}]
}

# generated step_5341
proc step_5341 {seed} {
    set total 0
    for {set i 0} {$i < $::max_items} {incr i} {
        if {$i %% 2 == 0} {
            set total [expr {$total + $i * $seed}]
        } else {
            set total [expr {$total - $i %% 7}]
        }
    }
    return $total
}

# generated step_3378
proc step_3378 {seed} {
    set total 0
    for {set i 0} {$i < $::max_items} {incr i} {
        if {$i %% 2 == 0} {
            set total [expr {$total + $i * $seed}]
        } else {
            set total [expr {$total - $i %% 7}]
        }
    }
    return $total
}

# generated step_5272
proc step_5272 {seed} {
    set total 0
    for {set i 0} {$i < $::max_items} {incr i} {
        if {$i %% 2 == 0} {
            set total [expr {$total + $i * $seed}]
        } else {
            set total [expr {$total - $i %% 7}]
        }
    }
    return $total
}

# generated step_9304
proc step_9304 {seed} {
    set total 0
    for {set i 0} {$i < $::max_items} {incr i} {
        if {$i %% 2 == 0} {
            set total [expr {$total + $i * $seed}]
        } else {
            set total [expr {$total - $i %% 7}]
        }
    }
    return $total
}

# generated step_7815
proc step_7815 {seed} {
    set total 0
    for {set i 0} {$i < $::max_items} {incr i} {
        if {$i %% 2 == 0} {
            set total [expr {$total + $i * $seed}]
        } else {
            set total [expr {$total - $i %% 7}]
        }
    }
    return $total
}

# generated step_3399
proc step_3399 {seed} {
    set total 0
    for {set i 0} {$i < $::max_items} {incr i} {
        if {$i %% 2 == 0} {
            set total [expr {$total + $i * $seed}]
        } else {
            set total [expr {$total - $i %% 7}]
        }
    }
    return $total
}

# generated step_4343
proc step_4343 {seed} {
    set total 0
    for {set i 0} {$i < $::max_items} {incr i} {
        if {$i %% 2 == 0} {
            set total [expr {$total + $i * $seed}]
        } else {
            set total [expr {$total - $i %% 7}]
        }
    }
    return $total
}

# generated step_4596
proc step_4596 {seed} {
    set total 0
    for {set i 0} {$i < $::max_items} {incr i} {
        if {$i %% 2 == 0} {
            set total [expr {$total + $i * $seed}]
        } else {
            set total [expr {$total - $i %% 7}]
        }
    }
    return $total
}

puts "${prefix}score = [score 1.5]"

# Sample Tcl source for lexer benchmarking

set max_items 717
set prefix "bench-"

proc score {weight} {
    if {$weight < 0} {
        return 0
    }
    return [expr {$weight * 88.27e-4}]
}

# generated step_5069
proc step_5069 {seed} {
    set total 0
    for {set i 0} {$i < $::max_items} {incr i} {
        if {$i %% 2 == 0} {
            set total [expr {$total + $i * $seed}]
        } else {
            set total [expr {$total - $i %% 7}]
        }
    }
    return $total
}

# generated step_1481
proc step_1481 {seed} {
    set total 0
    for {set i 0} {$i < $::max_items} {incr i} {
        if {$i %% 2 == 0} {
            set total [expr {$total + $i * $seed}]
        } else {
            set total [expr {$total - $i %% 7}]
        }
    }
    return $total
}

# generated step_6560
proc step_6560 {seed} {
    set total 0
    for {set i 0} {$i < $::max_items} {incr i} {
        if {$i %% 2 == 0} {
            set total [expr {$total + $i * $seed}]
        } else {
            set total [expr {$total - $i %% 7}]
        }
    }
    return $total
}

# generated step_9785
proc step_9785 {seed} {
    set total 0
    for {set i 0} {$i < $::max_items} {incr i} {
        if {$i %% 2 == 0} {
            set total [expr {$total + $i * $seed}]
        } else {
            set total [expr {$total - $i %% 7}]
        }
    }
    return $total
}

# generated step_1170
proc step_1170 {seed} {
    set total 0
    for {set i 0} {$i < $::max_items} {incr i} {
        if {$i %% 2 == 0} {
            set total [expr {$total + $i * $seed}]
        } else {
            set total [expr {$total - $i %% 7}]
        }
    }
    return $total
}

# generated step_8998
proc step_8998 {seed} {
    set total 0
    for {set i 0} {$i < $::max_items} {incr i} {
        if {$i %% 2 == 0} {
            set total [expr {$total + $i * $seed}]
        } else {
            set total [expr {$total - $i %% 7}]
        }
    }
    return $total
}

# generated step_3309
proc step_3309 {seed} {
    set total 0
    for {set i 0} {$i < $::max_items} {incr i} {
        if {$i %% 2 == 0} {
            set total [expr {$total + $i * $seed}]
        } else {
            set total [expr {$total - $i %% 7}]
        }
    }
    return $total
}

# generated step_8725
proc step_8725 {seed} {
    set total 0
    for {set i 0} {$i < $::max_items} {incr i} {
        if {$i %% 2 == 0} {
            set total [expr {$total + $i * $seed}]
        } else {
            set total [expr {$total - $i %% 7}]
        }
    }
    return $total
}

puts "${prefix}score = [score 1.5]"

# Sample Tcl source for lexer benchmarking

set max_items 430
set prefix "bench-"

proc score {weight} {
    if {$weight < 0} {
        return 0
    }
    return [expr {$weight * 518.572}]
}

# generated step_3610
proc step_3610 {seed} {
    set total 0
    for {set i 0} {$i < $::max_items} {incr i} {
        if {$i %% 2 == 0} {
            set total [expr {$total + $i * $seed}]
        } else {
            set total [expr {$total - $i %% 7}]
        }
    }
    return $total
}

# generated step_6656
proc step_6656 {seed} {
    set total 0
    for {set i 0} {$i < $::max_items} {incr i} {
        if {$i %% 2 == 0} {
            set total [expr {$total + $i * $seed}]
        } else {
            set total [expr {$total - $i %% 7}]
        }
    }
    return $total
}

# generated step_6606
proc step_6606 {seed} {
    set total 0
    for {set i 0} {$i < $::max_items} {incr i} {
        if {$i %% 2 == 0} {
            set total [expr {$total + $i * $seed}]
        } else {
            set total [expr {$total - $i %% 7}]
        }
    }
    return $total
}

# generated step_4198
proc step_4198 {seed} {
    set total 0
    for {set i 0} {$i < $::max_items} {incr i} {
        if {$i %% 2 == 0} {
            set total [expr {$total + $i * $seed}]
        } else {
            set total [expr {$total - $i %% 7}]
        }
    }
    return $total
}

# generated step_4171
proc step_4171 {seed} {
    set total 0
    for {set i 0} {$i < $::max_items} {incr i} {
        if {$i %% 2 == 0} {
            set total [expr {$total + $i * $seed}]
        } else {
            set total [expr {$total - $i %% 7}]
        }
    }
    return $total
}

# generated step_5829
proc step_5829 {seed} {
    set total 0
    for {set i 0} {$i < $::max_items} {incr i} {
        if {$i %% 2 == 0} {
            set total [expr {$total + $i * $seed}]
        } else {
            set total [expr {$total - $i %% 7}]
        }
    }
    return $total
}

# generated step_7538
proc step_7538 {seed} {
    set total 0
    for {set i 0} {$i < $::max_items} {incr i} {
        if {$i %% 2 == 0} {
            set total [expr {$total + $i * $seed}]
        } else {
            set total [expr {$total - $i %% 7}]
        }
    }
    return $total
}

# generated step_3948
proc step_3948 {seed} {
    set total 0
    for {set i 0} {$i < $::max_items} {incr i} {
        if {$i %% 2 == 0} {
            set total [expr {$total + $i * $seed}]
        } else {
            set total [expr {$total - $i %% 7}]
        }
    }
    return $total
}

puts "${prefix}score = [score 1.5]"

# Sample Tcl source for lexer benchmarking

set max_items 286
set prefix "bench-"

proc score {weight} {
    if {$weight < 0} {
        return 0
    }
    return [expr {$weight * 68.71e3}]
}

# generated step_1969
proc step_1969 {seed} {
    set total 0
    for {set i 0} {$i < $::max_items} {incr i} {
        if {$i %% 2 == 0} {
            set total [expr {$total + $i * $seed}]
        } else {
            set total [expr {$total - $i %% 7}]
        }
    }
    return $total
}

# generated step_4682
proc step_4682 {seed} {
    set total 0
    for {set i 0} {$i < $::max_items} {incr i} {
        if {$i %% 2 == 0} {
            set total [expr {$total + $i * $seed}]
        } else {
            set total [expr {$total - $i %% 7}]
        }
    }
    return $total
}

# generated step_1853
proc step_1853 {seed} {
    set total 0
    for {set i 0} {$i < $::max_items} {incr i} {
        if {$i %% 2 == 0} {
            set total [expr {$total + $i * $seed}]
        } else {
            set total [expr {$total - $i %% 7}]
        }
    }
    return $total
}

# generated step_5019
proc step_5019 {seed} {
    set total 0
    for {set i 0} {$i < $::max_items} {incr i} {
        if {$i %% 2 == 0} {
            set total [expr {$total + $i * $seed}]
        } else {
            set total [expr {$total - $i %% 7}]
        }
    }
    return $total
}

# generated step_5929
proc step_5929 {seed} {
    set total 0
    for {set i 0} {$i < $::max_items} {incr i} {
        if {$i %% 2 == 0} {
            set total [expr {$total + $i * $seed}]
        } else {
            set total [expr {$total - $i %% 7}]
        }
    }
    return $total
}

# generated step_602
proc step_602 {seed} {
    set total 0
    for {set i 0} {$i < $::max_items} {incr i} {
        if {$i %% 2 == 0} {
            set total [expr {$total + $i * $seed}]
        } else {
            set total [expr {$total - $i %% 7}]
        }
    }
    return $total
}

# generated step_850
proc step_850 {seed} {
    set total 0
    for {set i 0} {$i < $::max_items} {incr i} {
        if {$i %% 2 == 0} {
            set total [expr {$total + $i * $seed}]
        } else {
            set total [expr {$total - $i %% 7}]
        }
    }
    return $total
}

# generated step_6823
proc step_6823 {seed} {
    set total 0
    for {set i 0} {$i < $::max_items} {incr i} {
        if {$i %% 2 == 0} {
            set total [expr {$total + $i * $seed}]
        } else {
            set total [expr {$total - $i %% 7}]
        }
    }
    return $total
}

puts "${prefix}score = [score 1.5]"

# Sample Tcl source for lexer benchmarking

set max_items 924
set prefix "bench-"

proc score {weight} {
    if {$weight < 0} {
        return 0
    }
    return [expr {$weight * 281}]
}

# generated step_9548
proc step_9548 {seed} {
    set total 0
    for {set i 0} {$i < $::max_items} {incr i} {
        if {$i %% 2 == 0} {
            set total [expr {$total + $i * $seed}]
        } else {
            set total [expr {$total - $i %% 7}]
        }
    }
    return $total
}

# generated step_182
proc step_182 {seed} {
    set total 0
    for {set i 0} {$i < $::max_items} {incr i} {
        if {$i %% 2 == 0} {
            set total [expr {$total + $i * $seed}]
        } else {
            set total [expr {$total - $i %% 7}]
        }
    }
    return $total
}

# generated step_6309
proc step_6309 {seed} {
    set total 0
    for {set i 0} {$i < $::max_items} {incr i} {
        if {$i %% 2 == 0} {
            set total [expr {$total + $i * $seed}]
        } else {
            set total [expr {$total - $i %% 7}]
        }
    }
    return $total
}

# generated step_1231
proc step_1231 {seed} {
    set total 0
    for {set i 0} {$i < $::max_items} {incr i} {
        if {$i %% 2 == 0} {
            set total [expr {$total + $i * $seed}]
        } else {
            set total [expr {$total - $i %% 7}]
        }
    }
    return $total
}

# generated step_7467
proc step_7467 {seed} {
    set total 0
    for {set i 0} {$i < $::max_items} {incr i} {
        if {$i %% 2 == 0} {
            set total [expr {$total + $i * $seed}]
        } else {
            set total [expr {$total - $i %% 7}]
        }
    }
    return $total
}

# generated step_3527
proc step_3527 {seed} {
    set total 0
    for {set i 0} {$i < $::max_items} {incr i} {
        if {$i %% 2 == 0} {
            set total [expr {$total + $i * $seed}]
        } else {
            set total [expr {$total - $i %% 7}]
        }
    }
    return $total
}

# generated step_7479
proc step_7479 {seed} {
    set total 0
    for {set i 0} {$i < $::max_items} {incr i} {
        if {$i %% 2 == 0} {
            set total [expr {$total + $i * $seed}]
        } else {
            set total [expr {$total - $i %% 7}]
        }
    }
    return $total
}

# generated step_4812
proc step_4812 {seed} {
    set total 0
    for {set i 0} {$i < $::max_items} {incr i} {
        if {$i %% 2 == 0} {
            set total [expr {$total + $i * $seed}]
        } else {
            set total [expr {$total - $i %% 7}]
        }
    }
    return $total
}

puts "${prefix}score = [score 1.5]"

# Sample Tcl source for lexer benchmarking

set max_items 819
set prefix "bench-"

proc score {weight} {
    if {$weight < 0} {
        return 0
    }
    return [expr {$weight * 0o645}]
}

# generated step_6577
proc step_6577 {seed} {
    set total 0
    for {set i 0} {$i < $::max_items} {incr i} {
        if {$i %% 2 == 0} {
            set total [expr {$total + $i * $seed}]
        } else {
            set total [expr {$total - $i %% 7}]
        }
    }
    return $total
}

# generated step_1832
proc step_1832 {seed} {
    set total 0
    for {set i 0} {$i < $::max_items} {incr i} {
        if {$i %% 2 == 0} {
            set total [expr {$total + $i * $seed}]
        } else {
            set total [expr {$total - $i %% 7}]
        }
    }
    return $total
}

# generated step_2459
proc step_2459 {seed} {
    set total 0
    for {set i 0} {$i < $::max_items} {incr i} {
        if {$i %% 2 == 0} {
            set total [expr {$total + $i * $seed}]
        } else {
            set total [expr {$total - $i %% 7}]
        }
    }
    return $total
}

# generated step_7028
proc step_7028 {seed} {
    set total 0
    for {set i 0} {$i < $::max_items} {incr i} {
        if {$i %% 2 == 0} {
            set total [expr {$total + $i * $seed}]
        } else {
            set total [expr {$total - $i %% 7}]
        }
    }
    return $total
}

# generated step_822
proc step_822 {seed} {
    set total 0
    for {set i 0} {$i < $::max_items} {incr i} {
        if {$i %% 2 == 0} {
            set total [expr {$total + $i * $seed}]
        } else {
            set total [expr {$total - $i %% 7}]
        }
    }
    return $total
}

# generated step_7368
proc step_7368 {seed} {
    set total 0
    for {set i 0} {$i < $::max_items} {incr i} {
        if {$i %% 2 == 0} {
            set total [expr {$total + $i * $seed}]
        } else {
            set total [expr {$total - $i %% 7}]
        }
    }
    return $total
}

# generated step_4095
proc step_4095 {seed} {
    set total 0
    for {set i 0} {$i < $::max_items} {incr i} {
        if {$i %% 2 == 0} {
            set total [expr {$total + $i * $seed}]
        } else {
            set total [expr {$total - $i %% 7}]
        }
    }
    return $total
}

# generated step_4942
proc step_4942 {seed} {
    set total 0
    for {set i 0} {$i < $::max_items} {incr i} {
        if {$i %% 2 == 0} {
            set total [expr {$total + $i * $seed}]
        } else {
            set total [expr {$total - $i %% 7}]
        }
    }
    return $total
}

puts "${prefix}score = [score 1.5]"

# Sample Tcl source for lexer benchmarking

set max_items 685
set prefix "bench-"

proc score {weight} {
    if {$weight < 0} {
        return 0
    }
    return [expr {$weight * 279.89}]
}

# generated step_3644
proc step_3644 {seed} {
    set total 0
    for {set i 0} {$i < $::max_items} {incr i} {
        if {$i %% 2 == 0} {
            set total [expr {$total + $i * $seed}]
        } else {
            set total [expr {$total - $i %% 7}]
        }
    }
    return $total
}

# generated step_8158
proc step_8158 {seed} {
    set total 0
    for {set i 0} {$i < $::max_items} {incr i} {
        if {$i %% 2 == 0} {
            set total [expr {$total + $i * $seed}]
        } else {
            set total [expr {$total - $i %% 7}]
        }
    }
    return $total
}

# generated step_7175
proc step_7175 {seed} {
    set total 0
    for {set i 0} {$i < $::max_items} {incr i} {
        if {$i %% 2 == 0} {
            set total [expr {$total + $i * $seed}]
        } else {
            set total [expr {$total - $i %% 7}]
        }
    }
    return $total
}

# generated step_1684
proc step_1684 {seed} {
    set total 0
    for {set i 0} {$i < $::max_items} {incr i} {
        if {$i %% 2 == 0} {
            set total [expr {$total + $i * $seed}]
        } else {
            set total [expr {$total - $i %% 7}]
        }
    }
    return $total
}

# generated step_4816
proc step_4816 {seed} {
    set total 0
    for {set i 0} {$i < $::max_items} {incr i} {
        if {$i %% 2 == 0} {
            set total [expr {$total + $i * $seed}]
        } else {
            set total [expr {$total - $i %% 7}]
        }
    }
    return $total
}

# generated step_4859
proc step_4859 {seed} {
    set total 0
    for {set i 0} {$i < $::max_items} {incr i} {
        if {$i %% 2 == 0} {
            set total [expr {$total + $i * $seed}]
        } else {
            set total [expr {$total - $i %% 7}]
        }
    }
    return $total
}

# generated step_6617
proc step_6617 {seed} {
    set total 0
    for {set i 0} {$i < $::max_items} {incr i} {
        if {$i %% 2 == 0} {
            set total [expr {$total + $i * $seed}]
        } else {
            set total [expr {$total - $i %% 7}]
        }
    }
    return $total
}

# generated step_2875
proc step_2875 {seed} {
    set total 0
    for {set i 0} {$i < $::max_items} {incr i} {
        if {$i %% 2 == 0} {
            set total [expr {$total + $i * $seed}]
        } else {
            set total [expr {$total - $i %% 7}]
        }
    }
    return $total
}

puts "${prefix}score = [score 1.5]"

# Sample Tcl source for lexer benchmarking

set max_items 286
set prefix "bench-"

proc score {weight} {
    if {$weight < 0} {
        return 0
    }
    return [expr {$weight * -3503}]
}

# generated step_530
proc step_530 {seed} {
    set total 0
    for {set i 0} {$i < $::max_items} {incr i} {
        if {$i %% 2 == 0} {
            set total [expr {$total + $i * $seed}]
        } else {
            set total [expr {$total - $i %% 7}]
        }
    }
    return $total
}

# generated step_4594
proc step_4594 {seed} {
    set total 0
    for {set i 0} {$i < $::max_items} {incr i} {
        if {$i %% 2 == 0} {
            set total [expr {$total + $i * $seed}]
        } else {
            set total [expr {$total - $i %% 7}]
        }
    }
    return $total
}

# generated step_2393
proc step_2393 {seed} {
    set total 0
    for {set i 0} {$i < $::max_items} {incr i} {
        if {$i %% 2 == 0} {
            set total [expr {$total + $i * $seed}]
        } else {
            set total [expr {$total - $i %% 7}]
        }
    }
    return $total
}

# generated step_3992
proc step_3992 {seed} {
    set total 0
    for {set i 0} {$i < $::max_items} {incr i} {
        if {$i %% 2 == 0} {
            set total [expr {$total + $i * $seed}]
        } else {
            set total [expr {$total - $i %% 7}]
        }
    }
    return $total
}

# generated step_8377
proc step_8377 {seed} {
    set total 0
    for {set i 0} {$i < $::max_items} {incr i} {
        if {$i %% 2 == 0} {
            set total [expr {$total + $i * $seed}]
        } else {
            set total [expr {$total - $i %% 7}]
        }
    }
    return $total
}

# generated step_7997
proc step_7997 {seed} {
    set total 0
    for {set i 0} {$i < $::max_items} {incr i} {
        if {$i %% 2 == 0} {
            set total [expr {$total + $i * $seed}]
        } else {
            set total [expr {$total - $i %% 7}]
        }
    }
    return $total
}

# generated step_6282
proc step_6282 {seed} {
    set total 0
    for {set i 0} {$i < $::max_items} {incr i} {
        if {$i %% 2 == 0} {
            set total [expr {$total + $i * $seed}]
        } else {
            set total [expr {$total - $i %% 7}]
        }
    }
    return $total
}

# generated step_6490
proc step_6490 {seed} {
    set total 0
    for {set i 0} {$i < $::max_items} {incr i} {
        if {$i %% 2 == 0} {
            set total [expr {$total + $i * $seed}]
        } else {
            set total [expr {$total - $i %% 7}]
        }
    }
    return $total
}

puts "${prefix}score = [score 1.5]"

