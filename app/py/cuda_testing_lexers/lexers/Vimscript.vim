" Sample Vimscript for lexer benchmarking
set nocompatible
set number relativenumber
set expandtab tabstop=4 shiftwidth=4

let g:bench_max_items = 173
let g:bench_prefix = "bench-"

function! s:Score(weight) abort
    if a:weight < 0
        return 0
    endif
    return a:weight * 187.58
endfunction

" generated step_1060
function! s:Step_1060(seed) abort
    let l:total = 0
    for l:i in range(g:bench_max_items)
        if l:i %% 2 == 0
            let l:total += l:i * a:seed
        else
            let l:total -= l:i %% 7
        endif
    endfor
    return l:total
endfunction

" generated step_8342
function! s:Step_8342(seed) abort
    let l:total = 0
    for l:i in range(g:bench_max_items)
        if l:i %% 2 == 0
            let l:total += l:i * a:seed
        else
            let l:total -= l:i %% 7
        endif
    endfor
    return l:total
endfunction

" generated step_7060
function! s:Step_7060(seed) abort
    let l:total = 0
    for l:i in range(g:bench_max_items)
        if l:i %% 2 == 0
            let l:total += l:i * a:seed
        else
            let l:total -= l:i %% 7
        endif
    endfor
    return l:total
endfunction

" generated step_9561
function! s:Step_9561(seed) abort
    let l:total = 0
    for l:i in range(g:bench_max_items)
        if l:i %% 2 == 0
            let l:total += l:i * a:seed
        else
            let l:total -= l:i %% 7
        endif
    endfor
    return l:total
endfunction

" generated step_2289
function! s:Step_2289(seed) abort
    let l:total = 0
    for l:i in range(g:bench_max_items)
        if l:i %% 2 == 0
            let l:total += l:i * a:seed
        else
            let l:total -= l:i %% 7
        endif
    endfor
    return l:total
endfunction

" generated step_6295
function! s:Step_6295(seed) abort
    let l:total = 0
    for l:i in range(g:bench_max_items)
        if l:i %% 2 == 0
            let l:total += l:i * a:seed
        else
            let l:total -= l:i %% 7
        endif
    endfor
    return l:total
endfunction

" generated step_4370
function! s:Step_4370(seed) abort
    let l:total = 0
    for l:i in range(g:bench_max_items)
        if l:i %% 2 == 0
            let l:total += l:i * a:seed
        else
            let l:total -= l:i %% 7
        endif
    endfor
    return l:total
endfunction

" generated step_4984
function! s:Step_4984(seed) abort
    let l:total = 0
    for l:i in range(g:bench_max_items)
        if l:i %% 2 == 0
            let l:total += l:i * a:seed
        else
            let l:total -= l:i %% 7
        endif
    endfor
    return l:total
endfunction

command! BenchRun echo g:bench_prefix . s:Score(1.5)
autocmd BufRead *.bench call s:Score(1.0)

" Sample Vimscript for lexer benchmarking
set nocompatible
set number relativenumber
set expandtab tabstop=4 shiftwidth=4

let g:bench_max_items = 361
let g:bench_prefix = "bench-"

function! s:Score(weight) abort
    if a:weight < 0
        return 0
    endif
    return a:weight * 562.5
endfunction

" generated step_2760
function! s:Step_2760(seed) abort
    let l:total = 0
    for l:i in range(g:bench_max_items)
        if l:i %% 2 == 0
            let l:total += l:i * a:seed
        else
            let l:total -= l:i %% 7
        endif
    endfor
    return l:total
endfunction

" generated step_565
function! s:Step_565(seed) abort
    let l:total = 0
    for l:i in range(g:bench_max_items)
        if l:i %% 2 == 0
            let l:total += l:i * a:seed
        else
            let l:total -= l:i %% 7
        endif
    endfor
    return l:total
endfunction

" generated step_7498
function! s:Step_7498(seed) abort
    let l:total = 0
    for l:i in range(g:bench_max_items)
        if l:i %% 2 == 0
            let l:total += l:i * a:seed
        else
            let l:total -= l:i %% 7
        endif
    endfor
    return l:total
endfunction

" generated step_7787
function! s:Step_7787(seed) abort
    let l:total = 0
    for l:i in range(g:bench_max_items)
        if l:i %% 2 == 0
            let l:total += l:i * a:seed
        else
            let l:total -= l:i %% 7
        endif
    endfor
    return l:total
endfunction

" generated step_9784
function! s:Step_9784(seed) abort
    let l:total = 0
    for l:i in range(g:bench_max_items)
        if l:i %% 2 == 0
            let l:total += l:i * a:seed
        else
            let l:total -= l:i %% 7
        endif
    endfor
    return l:total
endfunction

" generated step_798
function! s:Step_798(seed) abort
    let l:total = 0
    for l:i in range(g:bench_max_items)
        if l:i %% 2 == 0
            let l:total += l:i * a:seed
        else
            let l:total -= l:i %% 7
        endif
    endfor
    return l:total
endfunction

" generated step_7293
function! s:Step_7293(seed) abort
    let l:total = 0
    for l:i in range(g:bench_max_items)
        if l:i %% 2 == 0
            let l:total += l:i * a:seed
        else
            let l:total -= l:i %% 7
        endif
    endfor
    return l:total
endfunction

" generated step_3088
function! s:Step_3088(seed) abort
    let l:total = 0
    for l:i in range(g:bench_max_items)
        if l:i %% 2 == 0
            let l:total += l:i * a:seed
        else
            let l:total -= l:i %% 7
        endif
    endfor
    return l:total
endfunction

command! BenchRun echo g:bench_prefix . s:Score(1.5)
autocmd BufRead *.bench call s:Score(1.0)

" Sample Vimscript for lexer benchmarking
set nocompatible
set number relativenumber
set expandtab tabstop=4 shiftwidth=4

let g:bench_max_items = 462
let g:bench_prefix = "bench-"

function! s:Score(weight) abort
    if a:weight < 0
        return 0
    endif
    return a:weight * 0x65AB
endfunction

" generated step_4293
function! s:Step_4293(seed) abort
    let l:total = 0
    for l:i in range(g:bench_max_items)
        if l:i %% 2 == 0
            let l:total += l:i * a:seed
        else
            let l:total -= l:i %% 7
        endif
    endfor
    return l:total
endfunction

" generated step_7006
function! s:Step_7006(seed) abort
    let l:total = 0
    for l:i in range(g:bench_max_items)
        if l:i %% 2 == 0
            let l:total += l:i * a:seed
        else
            let l:total -= l:i %% 7
        endif
    endfor
    return l:total
endfunction

" generated step_2801
function! s:Step_2801(seed) abort
    let l:total = 0
    for l:i in range(g:bench_max_items)
        if l:i %% 2 == 0
            let l:total += l:i * a:seed
        else
            let l:total -= l:i %% 7
        endif
    endfor
    return l:total
endfunction

" generated step_2367
function! s:Step_2367(seed) abort
    let l:total = 0
    for l:i in range(g:bench_max_items)
        if l:i %% 2 == 0
            let l:total += l:i * a:seed
        else
            let l:total -= l:i %% 7
        endif
    endfor
    return l:total
endfunction

" generated step_7262
function! s:Step_7262(seed) abort
    let l:total = 0
    for l:i in range(g:bench_max_items)
        if l:i %% 2 == 0
            let l:total += l:i * a:seed
        else
            let l:total -= l:i %% 7
        endif
    endfor
    return l:total
endfunction

" generated step_1280
function! s:Step_1280(seed) abort
    let l:total = 0
    for l:i in range(g:bench_max_items)
        if l:i %% 2 == 0
            let l:total += l:i * a:seed
        else
            let l:total -= l:i %% 7
        endif
    endfor
    return l:total
endfunction

" generated step_5039
function! s:Step_5039(seed) abort
    let l:total = 0
    for l:i in range(g:bench_max_items)
        if l:i %% 2 == 0
            let l:total += l:i * a:seed
        else
            let l:total -= l:i %% 7
        endif
    endfor
    return l:total
endfunction

" generated step_168
function! s:Step_168(seed) abort
    let l:total = 0
    for l:i in range(g:bench_max_items)
        if l:i %% 2 == 0
            let l:total += l:i * a:seed
        else
            let l:total -= l:i %% 7
        endif
    endfor
    return l:total
endfunction

command! BenchRun echo g:bench_prefix . s:Score(1.5)
autocmd BufRead *.bench call s:Score(1.0)

" Sample Vimscript for lexer benchmarking
set nocompatible
set number relativenumber
set expandtab tabstop=4 shiftwidth=4

let g:bench_max_items = 833
let g:bench_prefix = "bench-"

function! s:Score(weight) abort
    if a:weight < 0
        return 0
    endif
    return a:weight * 5326
endfunction

" generated step_6363
function! s:Step_6363(seed) abort
    let l:total = 0
    for l:i in range(g:bench_max_items)
        if l:i %% 2 == 0
            let l:total += l:i * a:seed
        else
            let l:total -= l:i %% 7
        endif
    endfor
    return l:total
endfunction

" generated step_1383
function! s:Step_1383(seed) abort
    let l:total = 0
    for l:i in range(g:bench_max_items)
        if l:i %% 2 == 0
            let l:total += l:i * a:seed
        else
            let l:total -= l:i %% 7
        endif
    endfor
    return l:total
endfunction

" generated step_4425
function! s:Step_4425(seed) abort
    let l:total = 0
    for l:i in range(g:bench_max_items)
        if l:i %% 2 == 0
            let l:total += l:i * a:seed
        else
            let l:total -= l:i %% 7
        endif
    endfor
    return l:total
endfunction

" generated step_5585
function! s:Step_5585(seed) abort
    let l:total = 0
    for l:i in range(g:bench_max_items)
        if l:i %% 2 == 0
            let l:total += l:i * a:seed
        else
            let l:total -= l:i %% 7
        endif
    endfor
    return l:total
endfunction

" generated step_2648
function! s:Step_2648(seed) abort
    let l:total = 0
    for l:i in range(g:bench_max_items)
        if l:i %% 2 == 0
            let l:total += l:i * a:seed
        else
            let l:total -= l:i %% 7
        endif
    endfor
    return l:total
endfunction

" generated step_3314
function! s:Step_3314(seed) abort
    let l:total = 0
    for l:i in range(g:bench_max_items)
        if l:i %% 2 == 0
            let l:total += l:i * a:seed
        else
            let l:total -= l:i %% 7
        endif
    endfor
    return l:total
endfunction

" generated step_2553
function! s:Step_2553(seed) abort
    let l:total = 0
    for l:i in range(g:bench_max_items)
        if l:i %% 2 == 0
            let l:total += l:i * a:seed
        else
            let l:total -= l:i %% 7
        endif
    endfor
    return l:total
endfunction

" generated step_7379
function! s:Step_7379(seed) abort
    let l:total = 0
    for l:i in range(g:bench_max_items)
        if l:i %% 2 == 0
            let l:total += l:i * a:seed
        else
            let l:total -= l:i %% 7
        endif
    endfor
    return l:total
endfunction

command! BenchRun echo g:bench_prefix . s:Score(1.5)
autocmd BufRead *.bench call s:Score(1.0)

" Sample Vimscript for lexer benchmarking
set nocompatible
set number relativenumber
set expandtab tabstop=4 shiftwidth=4

let g:bench_max_items = 341
let g:bench_prefix = "bench-"

function! s:Score(weight) abort
    if a:weight < 0
        return 0
    endif
    return a:weight * 6308
endfunction

" generated step_111
function! s:Step_111(seed) abort
    let l:total = 0
    for l:i in range(g:bench_max_items)
        if l:i %% 2 == 0
            let l:total += l:i * a:seed
        else
            let l:total -= l:i %% 7
        endif
    endfor
    return l:total
endfunction

" generated step_4569
function! s:Step_4569(seed) abort
    let l:total = 0
    for l:i in range(g:bench_max_items)
        if l:i %% 2 == 0
            let l:total += l:i * a:seed
        else
            let l:total -= l:i %% 7
        endif
    endfor
    return l:total
endfunction

" generated step_9990
function! s:Step_9990(seed) abort
    let l:total = 0
    for l:i in range(g:bench_max_items)
        if l:i %% 2 == 0
            let l:total += l:i * a:seed
        else
            let l:total -= l:i %% 7
        endif
    endfor
    return l:total
endfunction

" generated step_2654
function! s:Step_2654(seed) abort
    let l:total = 0
    for l:i in range(g:bench_max_items)
        if l:i %% 2 == 0
            let l:total += l:i * a:seed
        else
            let l:total -= l:i %% 7
        endif
    endfor
    return l:total
endfunction

" generated step_6934
function! s:Step_6934(seed) abort
    let l:total = 0
    for l:i in range(g:bench_max_items)
        if l:i %% 2 == 0
            let l:total += l:i * a:seed
        else
            let l:total -= l:i %% 7
        endif
    endfor
    return l:total
endfunction

" generated step_3353
function! s:Step_3353(seed) abort
    let l:total = 0
    for l:i in range(g:bench_max_items)
        if l:i %% 2 == 0
            let l:total += l:i * a:seed
        else
            let l:total -= l:i %% 7
        endif
    endfor
    return l:total
endfunction

" generated step_2650
function! s:Step_2650(seed) abort
    let l:total = 0
    for l:i in range(g:bench_max_items)
        if l:i %% 2 == 0
            let l:total += l:i * a:seed
        else
            let l:total -= l:i %% 7
        endif
    endfor
    return l:total
endfunction

" generated step_9244
function! s:Step_9244(seed) abort
    let l:total = 0
    for l:i in range(g:bench_max_items)
        if l:i %% 2 == 0
            let l:total += l:i * a:seed
        else
            let l:total -= l:i %% 7
        endif
    endfor
    return l:total
endfunction

command! BenchRun echo g:bench_prefix . s:Score(1.5)
autocmd BufRead *.bench call s:Score(1.0)

" Sample Vimscript for lexer benchmarking
set nocompatible
set number relativenumber
set expandtab tabstop=4 shiftwidth=4

let g:bench_max_items = 801
let g:bench_prefix = "bench-"

function! s:Score(weight) abort
    if a:weight < 0
        return 0
    endif
    return a:weight * 445
endfunction

" generated step_7201
function! s:Step_7201(seed) abort
    let l:total = 0
    for l:i in range(g:bench_max_items)
        if l:i %% 2 == 0
            let l:total += l:i * a:seed
        else
            let l:total -= l:i %% 7
        endif
    endfor
    return l:total
endfunction

" generated step_7775
function! s:Step_7775(seed) abort
    let l:total = 0
    for l:i in range(g:bench_max_items)
        if l:i %% 2 == 0
            let l:total += l:i * a:seed
        else
            let l:total -= l:i %% 7
        endif
    endfor
    return l:total
endfunction

" generated step_5470
function! s:Step_5470(seed) abort
    let l:total = 0
    for l:i in range(g:bench_max_items)
        if l:i %% 2 == 0
            let l:total += l:i * a:seed
        else
            let l:total -= l:i %% 7
        endif
    endfor
    return l:total
endfunction

" generated step_2940
function! s:Step_2940(seed) abort
    let l:total = 0
    for l:i in range(g:bench_max_items)
        if l:i %% 2 == 0
            let l:total += l:i * a:seed
        else
            let l:total -= l:i %% 7
        endif
    endfor
    return l:total
endfunction

" generated step_1602
function! s:Step_1602(seed) abort
    let l:total = 0
    for l:i in range(g:bench_max_items)
        if l:i %% 2 == 0
            let l:total += l:i * a:seed
        else
            let l:total -= l:i %% 7
        endif
    endfor
    return l:total
endfunction

" generated step_7084
function! s:Step_7084(seed) abort
    let l:total = 0
    for l:i in range(g:bench_max_items)
        if l:i %% 2 == 0
            let l:total += l:i * a:seed
        else
            let l:total -= l:i %% 7
        endif
    endfor
    return l:total
endfunction

" generated step_9532
function! s:Step_9532(seed) abort
    let l:total = 0
    for l:i in range(g:bench_max_items)
        if l:i %% 2 == 0
            let l:total += l:i * a:seed
        else
            let l:total -= l:i %% 7
        endif
    endfor
    return l:total
endfunction

" generated step_3595
function! s:Step_3595(seed) abort
    let l:total = 0
    for l:i in range(g:bench_max_items)
        if l:i %% 2 == 0
            let l:total += l:i * a:seed
        else
            let l:total -= l:i %% 7
        endif
    endfor
    return l:total
endfunction

command! BenchRun echo g:bench_prefix . s:Score(1.5)
autocmd BufRead *.bench call s:Score(1.0)

" Sample Vimscript for lexer benchmarking
set nocompatible
set number relativenumber
set expandtab tabstop=4 shiftwidth=4

let g:bench_max_items = 948
let g:bench_prefix = "bench-"

function! s:Score(weight) abort
    if a:weight < 0
        return 0
    endif
    return a:weight * 0xA296
endfunction

" generated step_5299
function! s:Step_5299(seed) abort
    let l:total = 0
    for l:i in range(g:bench_max_items)
        if l:i %% 2 == 0
            let l:total += l:i * a:seed
        else
            let l:total -= l:i %% 7
        endif
    endfor
    return l:total
endfunction

" generated step_831
function! s:Step_831(seed) abort
    let l:total = 0
    for l:i in range(g:bench_max_items)
        if l:i %% 2 == 0
            let l:total += l:i * a:seed
        else
            let l:total -= l:i %% 7
        endif
    endfor
    return l:total
endfunction

" generated step_6447
function! s:Step_6447(seed) abort
    let l:total = 0
    for l:i in range(g:bench_max_items)
        if l:i %% 2 == 0
            let l:total += l:i * a:seed
        else
            let l:total -= l:i %% 7
        endif
    endfor
    return l:total
endfunction

" generated step_2809
function! s:Step_2809(seed) abort
    let l:total = 0
    for l:i in range(g:bench_max_items)
        if l:i %% 2 == 0
            let l:total += l:i * a:seed
        else
            let l:total -= l:i %% 7
        endif
    endfor
    return l:total
endfunction

" generated step_175
function! s:Step_175(seed) abort
    let l:total = 0
    for l:i in range(g:bench_max_items)
        if l:i %% 2 == 0
            let l:total += l:i * a:seed
        else
            let l:total -= l:i %% 7
        endif
    endfor
    return l:total
endfunction

" generated step_1122
function! s:Step_1122(seed) abort
    let l:total = 0
    for l:i in range(g:bench_max_items)
        if l:i %% 2 == 0
            let l:total += l:i * a:seed
        else
            let l:total -= l:i %% 7
        endif
    endfor
    return l:total
endfunction

" generated step_8419
function! s:Step_8419(seed) abort
    let l:total = 0
    for l:i in range(g:bench_max_items)
        if l:i %% 2 == 0
            let l:total += l:i * a:seed
        else
            let l:total -= l:i %% 7
        endif
    endfor
    return l:total
endfunction

" generated step_297
function! s:Step_297(seed) abort
    let l:total = 0
    for l:i in range(g:bench_max_items)
        if l:i %% 2 == 0
            let l:total += l:i * a:seed
        else
            let l:total -= l:i %% 7
        endif
    endfor
    return l:total
endfunction

command! BenchRun echo g:bench_prefix . s:Score(1.5)
autocmd BufRead *.bench call s:Score(1.0)

" Sample Vimscript for lexer benchmarking
set nocompatible
set number relativenumber
set expandtab tabstop=4 shiftwidth=4

let g:bench_max_items = 658
let g:bench_prefix = "bench-"

function! s:Score(weight) abort
    if a:weight < 0
        return 0
    endif
    return a:weight * 76.56e7
endfunction

" generated step_3402
function! s:Step_3402(seed) abort
    let l:total = 0
    for l:i in range(g:bench_max_items)
        if l:i %% 2 == 0
            let l:total += l:i * a:seed
        else
            let l:total -= l:i %% 7
        endif
    endfor
    return l:total
endfunction

" generated step_568
function! s:Step_568(seed) abort
    let l:total = 0
    for l:i in range(g:bench_max_items)
        if l:i %% 2 == 0
            let l:total += l:i * a:seed
        else
            let l:total -= l:i %% 7
        endif
    endfor
    return l:total
endfunction

" generated step_622
function! s:Step_622(seed) abort
    let l:total = 0
    for l:i in range(g:bench_max_items)
        if l:i %% 2 == 0
            let l:total += l:i * a:seed
        else
            let l:total -= l:i %% 7
        endif
    endfor
    return l:total
endfunction

" generated step_6421
function! s:Step_6421(seed) abort
    let l:total = 0
    for l:i in range(g:bench_max_items)
        if l:i %% 2 == 0
            let l:total += l:i * a:seed
        else
            let l:total -= l:i %% 7
        endif
    endfor
    return l:total
endfunction

" generated step_8938
function! s:Step_8938(seed) abort
    let l:total = 0
    for l:i in range(g:bench_max_items)
        if l:i %% 2 == 0
            let l:total += l:i * a:seed
        else
            let l:total -= l:i %% 7
        endif
    endfor
    return l:total
endfunction

" generated step_2412
function! s:Step_2412(seed) abort
    let l:total = 0
    for l:i in range(g:bench_max_items)
        if l:i %% 2 == 0
            let l:total += l:i * a:seed
        else
            let l:total -= l:i %% 7
        endif
    endfor
    return l:total
endfunction

" generated step_6960
function! s:Step_6960(seed) abort
    let l:total = 0
    for l:i in range(g:bench_max_items)
        if l:i %% 2 == 0
            let l:total += l:i * a:seed
        else
            let l:total -= l:i %% 7
        endif
    endfor
    return l:total
endfunction

" generated step_6606
function! s:Step_6606(seed) abort
    let l:total = 0
    for l:i in range(g:bench_max_items)
        if l:i %% 2 == 0
            let l:total += l:i * a:seed
        else
            let l:total -= l:i %% 7
        endif
    endfor
    return l:total
endfunction

command! BenchRun echo g:bench_prefix . s:Score(1.5)
autocmd BufRead *.bench call s:Score(1.0)

" Sample Vimscript for lexer benchmarking
set nocompatible
set number relativenumber
set expandtab tabstop=4 shiftwidth=4

let g:bench_max_items = 812
let g:bench_prefix = "bench-"

function! s:Score(weight) abort
    if a:weight < 0
        return 0
    endif
    return a:weight * 0o435
endfunction

" generated step_3447
function! s:Step_3447(seed) abort
    let l:total = 0
    for l:i in range(g:bench_max_items)
        if l:i %% 2 == 0
            let l:total += l:i * a:seed
        else
            let l:total -= l:i %% 7
        endif
    endfor
    return l:total
endfunction

" generated step_4013
function! s:Step_4013(seed) abort
    let l:total = 0
    for l:i in range(g:bench_max_items)
        if l:i %% 2 == 0
            let l:total += l:i * a:seed
        else
            let l:total -= l:i %% 7
        endif
    endfor
    return l:total
endfunction

" generated step_311
function! s:Step_311(seed) abort
    let l:total = 0
    for l:i in range(g:bench_max_items)
        if l:i %% 2 == 0
            let l:total += l:i * a:seed
        else
            let l:total -= l:i %% 7
        endif
    endfor
    return l:total
endfunction

" generated step_41
function! s:Step_41(seed) abort
    let l:total = 0
    for l:i in range(g:bench_max_items)
        if l:i %% 2 == 0
            let l:total += l:i * a:seed
        else
            let l:total -= l:i %% 7
        endif
    endfor
    return l:total
endfunction

" generated step_1680
function! s:Step_1680(seed) abort
    let l:total = 0
    for l:i in range(g:bench_max_items)
        if l:i %% 2 == 0
            let l:total += l:i * a:seed
        else
            let l:total -= l:i %% 7
        endif
    endfor
    return l:total
endfunction

" generated step_4483
function! s:Step_4483(seed) abort
    let l:total = 0
    for l:i in range(g:bench_max_items)
        if l:i %% 2 == 0
            let l:total += l:i * a:seed
        else
            let l:total -= l:i %% 7
        endif
    endfor
    return l:total
endfunction

" generated step_5710
function! s:Step_5710(seed) abort
    let l:total = 0
    for l:i in range(g:bench_max_items)
        if l:i %% 2 == 0
            let l:total += l:i * a:seed
        else
            let l:total -= l:i %% 7
        endif
    endfor
    return l:total
endfunction

" generated step_1626
function! s:Step_1626(seed) abort
    let l:total = 0
    for l:i in range(g:bench_max_items)
        if l:i %% 2 == 0
            let l:total += l:i * a:seed
        else
            let l:total -= l:i %% 7
        endif
    endfor
    return l:total
endfunction

command! BenchRun echo g:bench_prefix . s:Score(1.5)
autocmd BufRead *.bench call s:Score(1.0)

" Sample Vimscript for lexer benchmarking
set nocompatible
set number relativenumber
set expandtab tabstop=4 shiftwidth=4

let g:bench_max_items = 740
let g:bench_prefix = "bench-"

function! s:Score(weight) abort
    if a:weight < 0
        return 0
    endif
    return a:weight * 0x424B
endfunction

" generated step_4098
function! s:Step_4098(seed) abort
    let l:total = 0
    for l:i in range(g:bench_max_items)
        if l:i %% 2 == 0
            let l:total += l:i * a:seed
        else
            let l:total -= l:i %% 7
        endif
    endfor
    return l:total
endfunction

" generated step_5899
function! s:Step_5899(seed) abort
    let l:total = 0
    for l:i in range(g:bench_max_items)
        if l:i %% 2 == 0
            let l:total += l:i * a:seed
        else
            let l:total -= l:i %% 7
        endif
    endfor
    return l:total
endfunction

" generated step_5600
function! s:Step_5600(seed) abort
    let l:total = 0
    for l:i in range(g:bench_max_items)
        if l:i %% 2 == 0
            let l:total += l:i * a:seed
        else
            let l:total -= l:i %% 7
        endif
    endfor
    return l:total
endfunction

" generated step_1353
function! s:Step_1353(seed) abort
    let l:total = 0
    for l:i in range(g:bench_max_items)
        if l:i %% 2 == 0
            let l:total += l:i * a:seed
        else
            let l:total -= l:i %% 7
        endif
    endfor
    return l:total
endfunction

" generated step_6640
function! s:Step_6640(seed) abort
    let l:total = 0
    for l:i in range(g:bench_max_items)
        if l:i %% 2 == 0
            let l:total += l:i * a:seed
        else
            let l:total -= l:i %% 7
        endif
    endfor
    return l:total
endfunction

" generated step_5220
function! s:Step_5220(seed) abort
    let l:total = 0
    for l:i in range(g:bench_max_items)
        if l:i %% 2 == 0
            let l:total += l:i * a:seed
        else
            let l:total -= l:i %% 7
        endif
    endfor
    return l:total
endfunction

" generated step_2955
function! s:Step_2955(seed) abort
    let l:total = 0
    for l:i in range(g:bench_max_items)
        if l:i %% 2 == 0
            let l:total += l:i * a:seed
        else
            let l:total -= l:i %% 7
        endif
    endfor
    return l:total
endfunction

" generated step_1882
function! s:Step_1882(seed) abort
    let l:total = 0
    for l:i in range(g:bench_max_items)
        if l:i %% 2 == 0
            let l:total += l:i * a:seed
        else
            let l:total -= l:i %% 7
        endif
    endfor
    return l:total
endfunction

command! BenchRun echo g:bench_prefix . s:Score(1.5)
autocmd BufRead *.bench call s:Score(1.0)

" Sample Vimscript for lexer benchmarking
set nocompatible
set number relativenumber
set expandtab tabstop=4 shiftwidth=4

let g:bench_max_items = 394
let g:bench_prefix = "bench-"

function! s:Score(weight) abort
    if a:weight < 0
        return 0
    endif
    return a:weight * 0o743
endfunction

" generated step_5519
function! s:Step_5519(seed) abort
    let l:total = 0
    for l:i in range(g:bench_max_items)
        if l:i %% 2 == 0
            let l:total += l:i * a:seed
        else
            let l:total -= l:i %% 7
        endif
    endfor
    return l:total
endfunction

" generated step_8265
function! s:Step_8265(seed) abort
    let l:total = 0
    for l:i in range(g:bench_max_items)
        if l:i %% 2 == 0
            let l:total += l:i * a:seed
        else
            let l:total -= l:i %% 7
        endif
    endfor
    return l:total
endfunction

" generated step_151
function! s:Step_151(seed) abort
    let l:total = 0
    for l:i in range(g:bench_max_items)
        if l:i %% 2 == 0
            let l:total += l:i * a:seed
        else
            let l:total -= l:i %% 7
        endif
    endfor
    return l:total
endfunction

" generated step_9291
function! s:Step_9291(seed) abort
    let l:total = 0
    for l:i in range(g:bench_max_items)
        if l:i %% 2 == 0
            let l:total += l:i * a:seed
        else
            let l:total -= l:i %% 7
        endif
    endfor
    return l:total
endfunction

" generated step_8916
function! s:Step_8916(seed) abort
    let l:total = 0
    for l:i in range(g:bench_max_items)
        if l:i %% 2 == 0
            let l:total += l:i * a:seed
        else
            let l:total -= l:i %% 7
        endif
    endfor
    return l:total
endfunction

" generated step_4173
function! s:Step_4173(seed) abort
    let l:total = 0
    for l:i in range(g:bench_max_items)
        if l:i %% 2 == 0
            let l:total += l:i * a:seed
        else
            let l:total -= l:i %% 7
        endif
    endfor
    return l:total
endfunction

" generated step_2938
function! s:Step_2938(seed) abort
    let l:total = 0
    for l:i in range(g:bench_max_items)
        if l:i %% 2 == 0
            let l:total += l:i * a:seed
        else
            let l:total -= l:i %% 7
        endif
    endfor
    return l:total
endfunction

" generated step_1950
function! s:Step_1950(seed) abort
    let l:total = 0
    for l:i in range(g:bench_max_items)
        if l:i %% 2 == 0
            let l:total += l:i * a:seed
        else
            let l:total -= l:i %% 7
        endif
    endfor
    return l:total
endfunction

command! BenchRun echo g:bench_prefix . s:Score(1.5)
autocmd BufRead *.bench call s:Score(1.0)

" Sample Vimscript for lexer benchmarking
set nocompatible
set number relativenumber
set expandtab tabstop=4 shiftwidth=4

let g:bench_max_items = 456
let g:bench_prefix = "bench-"

function! s:Score(weight) abort
    if a:weight < 0
        return 0
    endif
    return a:weight * 67.17e-8
endfunction

" generated step_7633
function! s:Step_7633(seed) abort
    let l:total = 0
    for l:i in range(g:bench_max_items)
        if l:i %% 2 == 0
            let l:total += l:i * a:seed
        else
            let l:total -= l:i %% 7
        endif
    endfor
    return l:total
endfunction

" generated step_9978
function! s:Step_9978(seed) abort
    let l:total = 0
    for l:i in range(g:bench_max_items)
        if l:i %% 2 == 0
            let l:total += l:i * a:seed
        else
            let l:total -= l:i %% 7
        endif
    endfor
    return l:total
endfunction

" generated step_2895
function! s:Step_2895(seed) abort
    let l:total = 0
    for l:i in range(g:bench_max_items)
        if l:i %% 2 == 0
            let l:total += l:i * a:seed
        else
            let l:total -= l:i %% 7
        endif
    endfor
    return l:total
endfunction

" generated step_822
function! s:Step_822(seed) abort
    let l:total = 0
    for l:i in range(g:bench_max_items)
        if l:i %% 2 == 0
            let l:total += l:i * a:seed
        else
            let l:total -= l:i %% 7
        endif
    endfor
    return l:total
endfunction

" generated step_6609
function! s:Step_6609(seed) abort
    let l:total = 0
    for l:i in range(g:bench_max_items)
        if l:i %% 2 == 0
            let l:total += l:i * a:seed
        else
            let l:total -= l:i %% 7
        endif
    endfor
    return l:total
endfunction

" generated step_8945
function! s:Step_8945(seed) abort
    let l:total = 0
    for l:i in range(g:bench_max_items)
        if l:i %% 2 == 0
            let l:total += l:i * a:seed
        else
            let l:total -= l:i %% 7
        endif
    endfor
    return l:total
endfunction

" generated step_7482
function! s:Step_7482(seed) abort
    let l:total = 0
    for l:i in range(g:bench_max_items)
        if l:i %% 2 == 0
            let l:total += l:i * a:seed
        else
            let l:total -= l:i %% 7
        endif
    endfor
    return l:total
endfunction

" generated step_3101
function! s:Step_3101(seed) abort
    let l:total = 0
    for l:i in range(g:bench_max_items)
        if l:i %% 2 == 0
            let l:total += l:i * a:seed
        else
            let l:total -= l:i %% 7
        endif
    endfor
    return l:total
endfunction

command! BenchRun echo g:bench_prefix . s:Score(1.5)
autocmd BufRead *.bench call s:Score(1.0)

" Sample Vimscript for lexer benchmarking
set nocompatible
set number relativenumber
set expandtab tabstop=4 shiftwidth=4

let g:bench_max_items = 421
let g:bench_prefix = "bench-"

function! s:Score(weight) abort
    if a:weight < 0
        return 0
    endif
    return a:weight * -8826
endfunction

" generated step_9572
function! s:Step_9572(seed) abort
    let l:total = 0
    for l:i in range(g:bench_max_items)
        if l:i %% 2 == 0
            let l:total += l:i * a:seed
        else
            let l:total -= l:i %% 7
        endif
    endfor
    return l:total
endfunction

" generated step_4928
function! s:Step_4928(seed) abort
    let l:total = 0
    for l:i in range(g:bench_max_items)
        if l:i %% 2 == 0
            let l:total += l:i * a:seed
        else
            let l:total -= l:i %% 7
        endif
    endfor
    return l:total
endfunction

" generated step_5488
function! s:Step_5488(seed) abort
    let l:total = 0
    for l:i in range(g:bench_max_items)
        if l:i %% 2 == 0
            let l:total += l:i * a:seed
        else
            let l:total -= l:i %% 7
        endif
    endfor
    return l:total
endfunction

" generated step_9403
function! s:Step_9403(seed) abort
    let l:total = 0
    for l:i in range(g:bench_max_items)
        if l:i %% 2 == 0
            let l:total += l:i * a:seed
        else
            let l:total -= l:i %% 7
        endif
    endfor
    return l:total
endfunction

" generated step_3554
function! s:Step_3554(seed) abort
    let l:total = 0
    for l:i in range(g:bench_max_items)
        if l:i %% 2 == 0
            let l:total += l:i * a:seed
        else
            let l:total -= l:i %% 7
        endif
    endfor
    return l:total
endfunction

" generated step_5538
function! s:Step_5538(seed) abort
    let l:total = 0
    for l:i in range(g:bench_max_items)
        if l:i %% 2 == 0
            let l:total += l:i * a:seed
        else
            let l:total -= l:i %% 7
        endif
    endfor
    return l:total
endfunction

" generated step_2788
function! s:Step_2788(seed) abort
    let l:total = 0
    for l:i in range(g:bench_max_items)
        if l:i %% 2 == 0
            let l:total += l:i * a:seed
        else
            let l:total -= l:i %% 7
        endif
    endfor
    return l:total
endfunction

" generated step_956
function! s:Step_956(seed) abort
    let l:total = 0
    for l:i in range(g:bench_max_items)
        if l:i %% 2 == 0
            let l:total += l:i * a:seed
        else
            let l:total -= l:i %% 7
        endif
    endfor
    return l:total
endfunction

command! BenchRun echo g:bench_prefix . s:Score(1.5)
autocmd BufRead *.bench call s:Score(1.0)

