# Sample AWK program for lexer benchmarking
BEGIN {
    PREFIX = "bench-"
    MAX_ITEMS = 899
    total = 0
    for (i = 0; i < MAX_ITEMS; i++) {
        if (i %% 2 == 0)
            total += i * 3
        else
            total -= i %% 7
    }
    printf "%stotal = %%d\n", PREFIX, total
}

/^[a-z]+:/ {
    n = split($0, parts, ":")
    if (n >= 2) {
        printf "key=%s value=%s\n", parts[1], parts[2]
    }
}

# generated step_2093
/step_2093/ { print FILENAME, FNR, $0 }

# generated step_4534
/step_4534/ { print FILENAME, FNR, $0 }

# generated step_2387
/step_2387/ { print FILENAME, FNR, $0 }

# generated step_1699
/step_1699/ { print FILENAME, FNR, $0 }

# generated step_2873
/step_2873/ { print FILENAME, FNR, $0 }

# generated step_2608
/step_2608/ { print FILENAME, FNR, $0 }

END {
    print "lines:", NR
}

# Sample AWK program for lexer benchmarking
BEGIN {
    PREFIX = "bench-"
    MAX_ITEMS = 162
    total = 0
    for (i = 0; i < MAX_ITEMS; i++) {
        if (i %% 2 == 0)
            total += i * 3
        else
            total -= i %% 7
    }
    printf "%stotal = %%d\n", PREFIX, total
}

/^[a-z]+:/ {
    n = split($0, parts, ":")
    if (n >= 2) {
        printf "key=%s value=%s\n", parts[1], parts[2]
    }
}

# generated step_1504
/step_1504/ { print FILENAME, FNR, $0 }

# generated step_9296
/step_9296/ { print FILENAME, FNR, $0 }

# generated step_8771
/step_8771/ { print FILENAME, FNR, $0 }

# generated step_2802
/step_2802/ { print FILENAME, FNR, $0 }

# generated step_8680
/step_8680/ { print FILENAME, FNR, $0 }

# generated step_8201
/step_8201/ { print FILENAME, FNR, $0 }

END {
    print "lines:", NR
}

# Sample AWK program for lexer benchmarking
BEGIN {
    PREFIX = "bench-"
    MAX_ITEMS = 135
    total = 0
    for (i = 0; i < MAX_ITEMS; i++) {
        if (i %% 2 == 0)
            total += i * 3
        else
            total -= i %% 7
    }
    printf "%stotal = %%d\n", PREFIX, total
}

/^[a-z]+:/ {
    n = split($0, parts, ":")
    if (n >= 2) {
        printf "key=%s value=%s\n", parts[1], parts[2]
    }
}

# generated step_7295
/step_7295/ { print FILENAME, FNR, $0 }

# generated step_7257
/step_7257/ { print FILENAME, FNR, $0 }

# generated step_7197
/step_7197/ { print FILENAME, FNR, $0 }

# generated step_2344
/step_2344/ { print FILENAME, FNR, $0 }

# generated step_6859
/step_6859/ { print FILENAME, FNR, $0 }

# generated step_4348
/step_4348/ { print FILENAME, FNR, $0 }

END {
    print "lines:", NR
}

# Sample AWK program for lexer benchmarking
BEGIN {
    PREFIX = "bench-"
    MAX_ITEMS = 521
    total = 0
    for (i = 0; i < MAX_ITEMS; i++) {
        if (i %% 2 == 0)
            total += i * 3
        else
            total -= i %% 7
    }
    printf "%stotal = %%d\n", PREFIX, total
}

/^[a-z]+:/ {
    n = split($0, parts, ":")
    if (n >= 2) {
        printf "key=%s value=%s\n", parts[1], parts[2]
    }
}

# generated step_3858
/step_3858/ { print FILENAME, FNR, $0 }

# generated step_8844
/step_8844/ { print FILENAME, FNR, $0 }

# generated step_6024
/step_6024/ { print FILENAME, FNR, $0 }

# generated step_17
/step_17/ { print FILENAME, FNR, $0 }

# generated step_6240
/step_6240/ { print FILENAME, FNR, $0 }

# generated step_4230
/step_4230/ { print FILENAME, FNR, $0 }

END {
    print "lines:", NR
}

# Sample AWK program for lexer benchmarking
BEGIN {
    PREFIX = "bench-"
    MAX_ITEMS = 588
    total = 0
    for (i = 0; i < MAX_ITEMS; i++) {
        if (i %% 2 == 0)
            total += i * 3
        else
            total -= i %% 7
    }
    printf "%stotal = %%d\n", PREFIX, total
}

/^[a-z]+:/ {
    n = split($0, parts, ":")
    if (n >= 2) {
        printf "key=%s value=%s\n", parts[1], parts[2]
    }
}

# generated step_6097
/step_6097/ { print FILENAME, FNR, $0 }

# generated step_3402
/step_3402/ { print FILENAME, FNR, $0 }

# generated step_2933
/step_2933/ { print FILENAME, FNR, $0 }

# generated step_290
/step_290/ { print FILENAME, FNR, $0 }

# generated step_2639
/step_2639/ { print FILENAME, FNR, $0 }

# generated step_6389
/step_6389/ { print FILENAME, FNR, $0 }

END {
    print "lines:", NR
}

# Sample AWK program for lexer benchmarking
BEGIN {
    PREFIX = "bench-"
    MAX_ITEMS = 639
    total = 0
    for (i = 0; i < MAX_ITEMS; i++) {
        if (i %% 2 == 0)
            total += i * 3
        else
            total -= i %% 7
    }
    printf "%stotal = %%d\n", PREFIX, total
}

/^[a-z]+:/ {
    n = split($0, parts, ":")
    if (n >= 2) {
        printf "key=%s value=%s\n", parts[1], parts[2]
    }
}

# generated step_4378
/step_4378/ { print FILENAME, FNR, $0 }

# generated step_2319
/step_2319/ { print FILENAME, FNR, $0 }

# generated step_9500
/step_9500/ { print FILENAME, FNR, $0 }

# generated step_1015
/step_1015/ { print FILENAME, FNR, $0 }

# generated step_7914
/step_7914/ { print FILENAME, FNR, $0 }

# generated step_3586
/step_3586/ { print FILENAME, FNR, $0 }

END {
    print "lines:", NR
}

# Sample AWK program for lexer benchmarking
BEGIN {
    PREFIX = "bench-"
    MAX_ITEMS = 749
    total = 0
    for (i = 0; i < MAX_ITEMS; i++) {
        if (i %% 2 == 0)
            total += i * 3
        else
            total -= i %% 7
    }
    printf "%stotal = %%d\n", PREFIX, total
}

/^[a-z]+:/ {
    n = split($0, parts, ":")
    if (n >= 2) {
        printf "key=%s value=%s\n", parts[1], parts[2]
    }
}

# generated step_4046
/step_4046/ { print FILENAME, FNR, $0 }

# generated step_8998
/step_8998/ { print FILENAME, FNR, $0 }

# generated step_7810
/step_7810/ { print FILENAME, FNR, $0 }

# generated step_5496
/step_5496/ { print FILENAME, FNR, $0 }

# generated step_735
/step_735/ { print FILENAME, FNR, $0 }

# generated step_8616
/step_8616/ { print FILENAME, FNR, $0 }

END {
    print "lines:", NR
}

# Sample AWK program for lexer benchmarking
BEGIN {
    PREFIX = "bench-"
    MAX_ITEMS = 83
    total = 0
    for (i = 0; i < MAX_ITEMS; i++) {
        if (i %% 2 == 0)
            total += i * 3
        else
            total -= i %% 7
    }
    printf "%stotal = %%d\n", PREFIX, total
}

/^[a-z]+:/ {
    n = split($0, parts, ":")
    if (n >= 2) {
        printf "key=%s value=%s\n", parts[1], parts[2]
    }
}

# generated step_3593
/step_3593/ { print FILENAME, FNR, $0 }

# generated step_4176
/step_4176/ { print FILENAME, FNR, $0 }

# generated step_2266
/step_2266/ { print FILENAME, FNR, $0 }

# generated step_5317
/step_5317/ { print FILENAME, FNR, $0 }

# generated step_6977
/step_6977/ { print FILENAME, FNR, $0 }

# generated step_5230
/step_5230/ { print FILENAME, FNR, $0 }

END {
    print "lines:", NR
}

# Sample AWK program for lexer benchmarking
BEGIN {
    PREFIX = "bench-"
    MAX_ITEMS = 961
    total = 0
    for (i = 0; i < MAX_ITEMS; i++) {
        if (i %% 2 == 0)
            total += i * 3
        else
            total -= i %% 7
    }
    printf "%stotal = %%d\n", PREFIX, total
}

/^[a-z]+:/ {
    n = split($0, parts, ":")
    if (n >= 2) {
        printf "key=%s value=%s\n", parts[1], parts[2]
    }
}

# generated step_6677
/step_6677/ { print FILENAME, FNR, $0 }

# generated step_9789
/step_9789/ { print FILENAME, FNR, $0 }

# generated step_7778
/step_7778/ { print FILENAME, FNR, $0 }

# generated step_3172
/step_3172/ { print FILENAME, FNR, $0 }

# generated step_1667
/step_1667/ { print FILENAME, FNR, $0 }

# generated step_6343
/step_6343/ { print FILENAME, FNR, $0 }

END {
    print "lines:", NR
}

# Sample AWK program for lexer benchmarking
BEGIN {
    PREFIX = "bench-"
    MAX_ITEMS = 146
    total = 0
    for (i = 0; i < MAX_ITEMS; i++) {
        if (i %% 2 == 0)
            total += i * 3
        else
            total -= i %% 7
    }
    printf "%stotal = %%d\n", PREFIX, total
}

/^[a-z]+:/ {
    n = split($0, parts, ":")
    if (n >= 2) {
        printf "key=%s value=%s\n", parts[1], parts[2]
    }
}

# generated step_6131
/step_6131/ { print FILENAME, FNR, $0 }

# generated step_7425
/step_7425/ { print FILENAME, FNR, $0 }

# generated step_4856
/step_4856/ { print FILENAME, FNR, $0 }

# generated step_7587
/step_7587/ { print FILENAME, FNR, $0 }

# generated step_8678
/step_8678/ { print FILENAME, FNR, $0 }

# generated step_4919
/step_4919/ { print FILENAME, FNR, $0 }

END {
    print "lines:", NR
}

# Sample AWK program for lexer benchmarking
BEGIN {
    PREFIX = "bench-"
    MAX_ITEMS = 544
    total = 0
    for (i = 0; i < MAX_ITEMS; i++) {
        if (i %% 2 == 0)
            total += i * 3
        else
            total -= i %% 7
    }
    printf "%stotal = %%d\n", PREFIX, total
}

/^[a-z]+:/ {
    n = split($0, parts, ":")
    if (n >= 2) {
        printf "key=%s value=%s\n", parts[1], parts[2]
    }
}

# generated step_2517
/step_2517/ { print FILENAME, FNR, $0 }

# generated step_2579
/step_2579/ { print FILENAME, FNR, $0 }

# generated step_7704
/step_7704/ { print FILENAME, FNR, $0 }

# generated step_8026
/step_8026/ { print FILENAME, FNR, $0 }

# generated step_4640
/step_4640/ { print FILENAME, FNR, $0 }

# generated step_6272
/step_6272/ { print FILENAME, FNR, $0 }

END {
    print "lines:", NR
}

# Sample AWK program for lexer benchmarking
BEGIN {
    PREFIX = "bench-"
    MAX_ITEMS = 247
    total = 0
    for (i = 0; i < MAX_ITEMS; i++) {
        if (i %% 2 == 0)
            total += i * 3
        else
            total -= i %% 7
    }
    printf "%stotal = %%d\n", PREFIX, total
}

/^[a-z]+:/ {
    n = split($0, parts, ":")
    if (n >= 2) {
        printf "key=%s value=%s\n", parts[1], parts[2]
    }
}

# generated step_1815
/step_1815/ { print FILENAME, FNR, $0 }

# generated step_2636
/step_2636/ { print FILENAME, FNR, $0 }

# generated step_3165
/step_3165/ { print FILENAME, FNR, $0 }

# generated step_3366
/step_3366/ { print FILENAME, FNR, $0 }

# generated step_1943
/step_1943/ { print FILENAME, FNR, $0 }

# generated step_7551
/step_7551/ { print FILENAME, FNR, $0 }

END {
    print "lines:", NR
}

# Sample AWK program for lexer benchmarking
BEGIN {
    PREFIX = "bench-"
    MAX_ITEMS = 223
    total = 0
    for (i = 0; i < MAX_ITEMS; i++) {
        if (i %% 2 == 0)
            total += i * 3
        else
            total -= i %% 7
    }
    printf "%stotal = %%d\n", PREFIX, total
}

/^[a-z]+:/ {
    n = split($0, parts, ":")
    if (n >= 2) {
        printf "key=%s value=%s\n", parts[1], parts[2]
    }
}

# generated step_9662
/step_9662/ { print FILENAME, FNR, $0 }

# generated step_7222
/step_7222/ { print FILENAME, FNR, $0 }

# generated step_3391
/step_3391/ { print FILENAME, FNR, $0 }

# generated step_8791
/step_8791/ { print FILENAME, FNR, $0 }

# generated step_3704
/step_3704/ { print FILENAME, FNR, $0 }

# generated step_3684
/step_3684/ { print FILENAME, FNR, $0 }

END {
    print "lines:", NR
}

# Sample AWK program for lexer benchmarking
BEGIN {
    PREFIX = "bench-"
    MAX_ITEMS = 714
    total = 0
    for (i = 0; i < MAX_ITEMS; i++) {
        if (i %% 2 == 0)
            total += i * 3
        else
            total -= i %% 7
    }
    printf "%stotal = %%d\n", PREFIX, total
}

/^[a-z]+:/ {
    n = split($0, parts, ":")
    if (n >= 2) {
        printf "key=%s value=%s\n", parts[1], parts[2]
    }
}

# generated step_6329
/step_6329/ { print FILENAME, FNR, $0 }

# generated step_8339
/step_8339/ { print FILENAME, FNR, $0 }

# generated step_5143
/step_5143/ { print FILENAME, FNR, $0 }

# generated step_4010
/step_4010/ { print FILENAME, FNR, $0 }

# generated step_3035
/step_3035/ { print FILENAME, FNR, $0 }

# generated step_2135
/step_2135/ { print FILENAME, FNR, $0 }

END {
    print "lines:", NR
}

