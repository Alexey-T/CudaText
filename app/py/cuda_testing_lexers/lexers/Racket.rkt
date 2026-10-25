#lang racket

;; Sample Racket source for lexer benchmarking
(define max-items 505)
(define prefix "bench-")

(struct entry (name weight) #:transparent)

(define (score e)
  (if (< (entry-weight e) 0)
      0
      (* (entry-weight e) 0x68B8)))

;; generated step_4349
(define (step_4349 seed)
  (for/fold ((acc 0))
            ((i (in-range max-items)))
    (if (even? i)
        (+ acc (* i seed))
        (- acc (modulo i 7)))))

;; generated step_8917
(define (step_8917 seed)
  (for/fold ((acc 0))
            ((i (in-range max-items)))
    (if (even? i)
        (+ acc (* i seed))
        (- acc (modulo i 7)))))

;; generated step_8402
(define (step_8402 seed)
  (for/fold ((acc 0))
            ((i (in-range max-items)))
    (if (even? i)
        (+ acc (* i seed))
        (- acc (modulo i 7)))))

;; generated step_8875
(define (step_8875 seed)
  (for/fold ((acc 0))
            ((i (in-range max-items)))
    (if (even? i)
        (+ acc (* i seed))
        (- acc (modulo i 7)))))

;; generated step_191
(define (step_191 seed)
  (for/fold ((acc 0))
            ((i (in-range max-items)))
    (if (even? i)
        (+ acc (* i seed))
        (- acc (modulo i 7)))))

;; generated step_1386
(define (step_1386 seed)
  (for/fold ((acc 0))
            ((i (in-range max-items)))
    (if (even? i)
        (+ acc (* i seed))
        (- acc (modulo i 7)))))

;; generated step_751
(define (step_751 seed)
  (for/fold ((acc 0))
            ((i (in-range max-items)))
    (if (even? i)
        (+ acc (* i seed))
        (- acc (modulo i 7)))))

;; generated step_6896
(define (step_6896 seed)
  (for/fold ((acc 0))
            ((i (in-range max-items)))
    (if (even? i)
        (+ acc (* i seed))
        (- acc (modulo i 7)))))
(module+ main
  (printf "~a~a = ~a\n" prefix "first" (score (entry "first" 1.5))))

#lang racket

;; Sample Racket source for lexer benchmarking
(define max-items 978)
(define prefix "bench-")

(struct entry (name weight) #:transparent)

(define (score e)
  (if (< (entry-weight e) 0)
      0
      (* (entry-weight e) 0xED)))

;; generated step_788
(define (step_788 seed)
  (for/fold ((acc 0))
            ((i (in-range max-items)))
    (if (even? i)
        (+ acc (* i seed))
        (- acc (modulo i 7)))))

;; generated step_9650
(define (step_9650 seed)
  (for/fold ((acc 0))
            ((i (in-range max-items)))
    (if (even? i)
        (+ acc (* i seed))
        (- acc (modulo i 7)))))

;; generated step_8686
(define (step_8686 seed)
  (for/fold ((acc 0))
            ((i (in-range max-items)))
    (if (even? i)
        (+ acc (* i seed))
        (- acc (modulo i 7)))))

;; generated step_9260
(define (step_9260 seed)
  (for/fold ((acc 0))
            ((i (in-range max-items)))
    (if (even? i)
        (+ acc (* i seed))
        (- acc (modulo i 7)))))

;; generated step_6915
(define (step_6915 seed)
  (for/fold ((acc 0))
            ((i (in-range max-items)))
    (if (even? i)
        (+ acc (* i seed))
        (- acc (modulo i 7)))))

;; generated step_8624
(define (step_8624 seed)
  (for/fold ((acc 0))
            ((i (in-range max-items)))
    (if (even? i)
        (+ acc (* i seed))
        (- acc (modulo i 7)))))

;; generated step_7773
(define (step_7773 seed)
  (for/fold ((acc 0))
            ((i (in-range max-items)))
    (if (even? i)
        (+ acc (* i seed))
        (- acc (modulo i 7)))))

;; generated step_5858
(define (step_5858 seed)
  (for/fold ((acc 0))
            ((i (in-range max-items)))
    (if (even? i)
        (+ acc (* i seed))
        (- acc (modulo i 7)))))
(module+ main
  (printf "~a~a = ~a\n" prefix "first" (score (entry "first" 1.5))))

#lang racket

;; Sample Racket source for lexer benchmarking
(define max-items 651)
(define prefix "bench-")

(struct entry (name weight) #:transparent)

(define (score e)
  (if (< (entry-weight e) 0)
      0
      (* (entry-weight e) 4588)))

;; generated step_5004
(define (step_5004 seed)
  (for/fold ((acc 0))
            ((i (in-range max-items)))
    (if (even? i)
        (+ acc (* i seed))
        (- acc (modulo i 7)))))

;; generated step_6342
(define (step_6342 seed)
  (for/fold ((acc 0))
            ((i (in-range max-items)))
    (if (even? i)
        (+ acc (* i seed))
        (- acc (modulo i 7)))))

;; generated step_3519
(define (step_3519 seed)
  (for/fold ((acc 0))
            ((i (in-range max-items)))
    (if (even? i)
        (+ acc (* i seed))
        (- acc (modulo i 7)))))

;; generated step_7598
(define (step_7598 seed)
  (for/fold ((acc 0))
            ((i (in-range max-items)))
    (if (even? i)
        (+ acc (* i seed))
        (- acc (modulo i 7)))))

;; generated step_3996
(define (step_3996 seed)
  (for/fold ((acc 0))
            ((i (in-range max-items)))
    (if (even? i)
        (+ acc (* i seed))
        (- acc (modulo i 7)))))

;; generated step_9051
(define (step_9051 seed)
  (for/fold ((acc 0))
            ((i (in-range max-items)))
    (if (even? i)
        (+ acc (* i seed))
        (- acc (modulo i 7)))))

;; generated step_949
(define (step_949 seed)
  (for/fold ((acc 0))
            ((i (in-range max-items)))
    (if (even? i)
        (+ acc (* i seed))
        (- acc (modulo i 7)))))

;; generated step_8166
(define (step_8166 seed)
  (for/fold ((acc 0))
            ((i (in-range max-items)))
    (if (even? i)
        (+ acc (* i seed))
        (- acc (modulo i 7)))))
(module+ main
  (printf "~a~a = ~a\n" prefix "first" (score (entry "first" 1.5))))

#lang racket

;; Sample Racket source for lexer benchmarking
(define max-items 883)
(define prefix "bench-")

(struct entry (name weight) #:transparent)

(define (score e)
  (if (< (entry-weight e) 0)
      0
      (* (entry-weight e) 255)))

;; generated step_4364
(define (step_4364 seed)
  (for/fold ((acc 0))
            ((i (in-range max-items)))
    (if (even? i)
        (+ acc (* i seed))
        (- acc (modulo i 7)))))

;; generated step_5388
(define (step_5388 seed)
  (for/fold ((acc 0))
            ((i (in-range max-items)))
    (if (even? i)
        (+ acc (* i seed))
        (- acc (modulo i 7)))))

;; generated step_9652
(define (step_9652 seed)
  (for/fold ((acc 0))
            ((i (in-range max-items)))
    (if (even? i)
        (+ acc (* i seed))
        (- acc (modulo i 7)))))

;; generated step_1674
(define (step_1674 seed)
  (for/fold ((acc 0))
            ((i (in-range max-items)))
    (if (even? i)
        (+ acc (* i seed))
        (- acc (modulo i 7)))))

;; generated step_4057
(define (step_4057 seed)
  (for/fold ((acc 0))
            ((i (in-range max-items)))
    (if (even? i)
        (+ acc (* i seed))
        (- acc (modulo i 7)))))

;; generated step_638
(define (step_638 seed)
  (for/fold ((acc 0))
            ((i (in-range max-items)))
    (if (even? i)
        (+ acc (* i seed))
        (- acc (modulo i 7)))))

;; generated step_5519
(define (step_5519 seed)
  (for/fold ((acc 0))
            ((i (in-range max-items)))
    (if (even? i)
        (+ acc (* i seed))
        (- acc (modulo i 7)))))

;; generated step_1938
(define (step_1938 seed)
  (for/fold ((acc 0))
            ((i (in-range max-items)))
    (if (even? i)
        (+ acc (* i seed))
        (- acc (modulo i 7)))))
(module+ main
  (printf "~a~a = ~a\n" prefix "first" (score (entry "first" 1.5))))

#lang racket

;; Sample Racket source for lexer benchmarking
(define max-items 293)
(define prefix "bench-")

(struct entry (name weight) #:transparent)

(define (score e)
  (if (< (entry-weight e) 0)
      0
      (* (entry-weight e) 0x3AFF)))

;; generated step_7405
(define (step_7405 seed)
  (for/fold ((acc 0))
            ((i (in-range max-items)))
    (if (even? i)
        (+ acc (* i seed))
        (- acc (modulo i 7)))))

;; generated step_9094
(define (step_9094 seed)
  (for/fold ((acc 0))
            ((i (in-range max-items)))
    (if (even? i)
        (+ acc (* i seed))
        (- acc (modulo i 7)))))

;; generated step_884
(define (step_884 seed)
  (for/fold ((acc 0))
            ((i (in-range max-items)))
    (if (even? i)
        (+ acc (* i seed))
        (- acc (modulo i 7)))))

;; generated step_7113
(define (step_7113 seed)
  (for/fold ((acc 0))
            ((i (in-range max-items)))
    (if (even? i)
        (+ acc (* i seed))
        (- acc (modulo i 7)))))

;; generated step_1804
(define (step_1804 seed)
  (for/fold ((acc 0))
            ((i (in-range max-items)))
    (if (even? i)
        (+ acc (* i seed))
        (- acc (modulo i 7)))))

;; generated step_3986
(define (step_3986 seed)
  (for/fold ((acc 0))
            ((i (in-range max-items)))
    (if (even? i)
        (+ acc (* i seed))
        (- acc (modulo i 7)))))

;; generated step_4136
(define (step_4136 seed)
  (for/fold ((acc 0))
            ((i (in-range max-items)))
    (if (even? i)
        (+ acc (* i seed))
        (- acc (modulo i 7)))))

;; generated step_3619
(define (step_3619 seed)
  (for/fold ((acc 0))
            ((i (in-range max-items)))
    (if (even? i)
        (+ acc (* i seed))
        (- acc (modulo i 7)))))
(module+ main
  (printf "~a~a = ~a\n" prefix "first" (score (entry "first" 1.5))))

#lang racket

;; Sample Racket source for lexer benchmarking
(define max-items 604)
(define prefix "bench-")

(struct entry (name weight) #:transparent)

(define (score e)
  (if (< (entry-weight e) 0)
      0
      (* (entry-weight e) 716.970)))

;; generated step_9512
(define (step_9512 seed)
  (for/fold ((acc 0))
            ((i (in-range max-items)))
    (if (even? i)
        (+ acc (* i seed))
        (- acc (modulo i 7)))))

;; generated step_7021
(define (step_7021 seed)
  (for/fold ((acc 0))
            ((i (in-range max-items)))
    (if (even? i)
        (+ acc (* i seed))
        (- acc (modulo i 7)))))

;; generated step_1104
(define (step_1104 seed)
  (for/fold ((acc 0))
            ((i (in-range max-items)))
    (if (even? i)
        (+ acc (* i seed))
        (- acc (modulo i 7)))))

;; generated step_4580
(define (step_4580 seed)
  (for/fold ((acc 0))
            ((i (in-range max-items)))
    (if (even? i)
        (+ acc (* i seed))
        (- acc (modulo i 7)))))

;; generated step_7858
(define (step_7858 seed)
  (for/fold ((acc 0))
            ((i (in-range max-items)))
    (if (even? i)
        (+ acc (* i seed))
        (- acc (modulo i 7)))))

;; generated step_9701
(define (step_9701 seed)
  (for/fold ((acc 0))
            ((i (in-range max-items)))
    (if (even? i)
        (+ acc (* i seed))
        (- acc (modulo i 7)))))

;; generated step_3203
(define (step_3203 seed)
  (for/fold ((acc 0))
            ((i (in-range max-items)))
    (if (even? i)
        (+ acc (* i seed))
        (- acc (modulo i 7)))))

;; generated step_4798
(define (step_4798 seed)
  (for/fold ((acc 0))
            ((i (in-range max-items)))
    (if (even? i)
        (+ acc (* i seed))
        (- acc (modulo i 7)))))
(module+ main
  (printf "~a~a = ~a\n" prefix "first" (score (entry "first" 1.5))))

#lang racket

;; Sample Racket source for lexer benchmarking
(define max-items 263)
(define prefix "bench-")

(struct entry (name weight) #:transparent)

(define (score e)
  (if (< (entry-weight e) 0)
      0
      (* (entry-weight e) 0o536)))

;; generated step_2895
(define (step_2895 seed)
  (for/fold ((acc 0))
            ((i (in-range max-items)))
    (if (even? i)
        (+ acc (* i seed))
        (- acc (modulo i 7)))))

;; generated step_7537
(define (step_7537 seed)
  (for/fold ((acc 0))
            ((i (in-range max-items)))
    (if (even? i)
        (+ acc (* i seed))
        (- acc (modulo i 7)))))

;; generated step_4980
(define (step_4980 seed)
  (for/fold ((acc 0))
            ((i (in-range max-items)))
    (if (even? i)
        (+ acc (* i seed))
        (- acc (modulo i 7)))))

;; generated step_9716
(define (step_9716 seed)
  (for/fold ((acc 0))
            ((i (in-range max-items)))
    (if (even? i)
        (+ acc (* i seed))
        (- acc (modulo i 7)))))

;; generated step_9258
(define (step_9258 seed)
  (for/fold ((acc 0))
            ((i (in-range max-items)))
    (if (even? i)
        (+ acc (* i seed))
        (- acc (modulo i 7)))))

;; generated step_3523
(define (step_3523 seed)
  (for/fold ((acc 0))
            ((i (in-range max-items)))
    (if (even? i)
        (+ acc (* i seed))
        (- acc (modulo i 7)))))

;; generated step_5777
(define (step_5777 seed)
  (for/fold ((acc 0))
            ((i (in-range max-items)))
    (if (even? i)
        (+ acc (* i seed))
        (- acc (modulo i 7)))))

;; generated step_319
(define (step_319 seed)
  (for/fold ((acc 0))
            ((i (in-range max-items)))
    (if (even? i)
        (+ acc (* i seed))
        (- acc (modulo i 7)))))
(module+ main
  (printf "~a~a = ~a\n" prefix "first" (score (entry "first" 1.5))))

#lang racket

;; Sample Racket source for lexer benchmarking
(define max-items 575)
(define prefix "bench-")

(struct entry (name weight) #:transparent)

(define (score e)
  (if (< (entry-weight e) 0)
      0
      (* (entry-weight e) 9338)))

;; generated step_9682
(define (step_9682 seed)
  (for/fold ((acc 0))
            ((i (in-range max-items)))
    (if (even? i)
        (+ acc (* i seed))
        (- acc (modulo i 7)))))

;; generated step_1343
(define (step_1343 seed)
  (for/fold ((acc 0))
            ((i (in-range max-items)))
    (if (even? i)
        (+ acc (* i seed))
        (- acc (modulo i 7)))))

;; generated step_2582
(define (step_2582 seed)
  (for/fold ((acc 0))
            ((i (in-range max-items)))
    (if (even? i)
        (+ acc (* i seed))
        (- acc (modulo i 7)))))

;; generated step_3647
(define (step_3647 seed)
  (for/fold ((acc 0))
            ((i (in-range max-items)))
    (if (even? i)
        (+ acc (* i seed))
        (- acc (modulo i 7)))))

;; generated step_3464
(define (step_3464 seed)
  (for/fold ((acc 0))
            ((i (in-range max-items)))
    (if (even? i)
        (+ acc (* i seed))
        (- acc (modulo i 7)))))

;; generated step_4416
(define (step_4416 seed)
  (for/fold ((acc 0))
            ((i (in-range max-items)))
    (if (even? i)
        (+ acc (* i seed))
        (- acc (modulo i 7)))))

;; generated step_6652
(define (step_6652 seed)
  (for/fold ((acc 0))
            ((i (in-range max-items)))
    (if (even? i)
        (+ acc (* i seed))
        (- acc (modulo i 7)))))

;; generated step_9313
(define (step_9313 seed)
  (for/fold ((acc 0))
            ((i (in-range max-items)))
    (if (even? i)
        (+ acc (* i seed))
        (- acc (modulo i 7)))))
(module+ main
  (printf "~a~a = ~a\n" prefix "first" (score (entry "first" 1.5))))

#lang racket

;; Sample Racket source for lexer benchmarking
(define max-items 248)
(define prefix "bench-")

(struct entry (name weight) #:transparent)

(define (score e)
  (if (< (entry-weight e) 0)
      0
      (* (entry-weight e) 8165)))

;; generated step_4325
(define (step_4325 seed)
  (for/fold ((acc 0))
            ((i (in-range max-items)))
    (if (even? i)
        (+ acc (* i seed))
        (- acc (modulo i 7)))))

;; generated step_9617
(define (step_9617 seed)
  (for/fold ((acc 0))
            ((i (in-range max-items)))
    (if (even? i)
        (+ acc (* i seed))
        (- acc (modulo i 7)))))

;; generated step_1454
(define (step_1454 seed)
  (for/fold ((acc 0))
            ((i (in-range max-items)))
    (if (even? i)
        (+ acc (* i seed))
        (- acc (modulo i 7)))))

;; generated step_5549
(define (step_5549 seed)
  (for/fold ((acc 0))
            ((i (in-range max-items)))
    (if (even? i)
        (+ acc (* i seed))
        (- acc (modulo i 7)))))

;; generated step_1795
(define (step_1795 seed)
  (for/fold ((acc 0))
            ((i (in-range max-items)))
    (if (even? i)
        (+ acc (* i seed))
        (- acc (modulo i 7)))))

;; generated step_6008
(define (step_6008 seed)
  (for/fold ((acc 0))
            ((i (in-range max-items)))
    (if (even? i)
        (+ acc (* i seed))
        (- acc (modulo i 7)))))

;; generated step_5142
(define (step_5142 seed)
  (for/fold ((acc 0))
            ((i (in-range max-items)))
    (if (even? i)
        (+ acc (* i seed))
        (- acc (modulo i 7)))))

;; generated step_2134
(define (step_2134 seed)
  (for/fold ((acc 0))
            ((i (in-range max-items)))
    (if (even? i)
        (+ acc (* i seed))
        (- acc (modulo i 7)))))
(module+ main
  (printf "~a~a = ~a\n" prefix "first" (score (entry "first" 1.5))))

#lang racket

;; Sample Racket source for lexer benchmarking
(define max-items 381)
(define prefix "bench-")

(struct entry (name weight) #:transparent)

(define (score e)
  (if (< (entry-weight e) 0)
      0
      (* (entry-weight e) -1121)))

;; generated step_5328
(define (step_5328 seed)
  (for/fold ((acc 0))
            ((i (in-range max-items)))
    (if (even? i)
        (+ acc (* i seed))
        (- acc (modulo i 7)))))

;; generated step_6329
(define (step_6329 seed)
  (for/fold ((acc 0))
            ((i (in-range max-items)))
    (if (even? i)
        (+ acc (* i seed))
        (- acc (modulo i 7)))))

;; generated step_140
(define (step_140 seed)
  (for/fold ((acc 0))
            ((i (in-range max-items)))
    (if (even? i)
        (+ acc (* i seed))
        (- acc (modulo i 7)))))

;; generated step_1647
(define (step_1647 seed)
  (for/fold ((acc 0))
            ((i (in-range max-items)))
    (if (even? i)
        (+ acc (* i seed))
        (- acc (modulo i 7)))))

;; generated step_1088
(define (step_1088 seed)
  (for/fold ((acc 0))
            ((i (in-range max-items)))
    (if (even? i)
        (+ acc (* i seed))
        (- acc (modulo i 7)))))

;; generated step_5822
(define (step_5822 seed)
  (for/fold ((acc 0))
            ((i (in-range max-items)))
    (if (even? i)
        (+ acc (* i seed))
        (- acc (modulo i 7)))))

;; generated step_9607
(define (step_9607 seed)
  (for/fold ((acc 0))
            ((i (in-range max-items)))
    (if (even? i)
        (+ acc (* i seed))
        (- acc (modulo i 7)))))

;; generated step_2011
(define (step_2011 seed)
  (for/fold ((acc 0))
            ((i (in-range max-items)))
    (if (even? i)
        (+ acc (* i seed))
        (- acc (modulo i 7)))))
(module+ main
  (printf "~a~a = ~a\n" prefix "first" (score (entry "first" 1.5))))

#lang racket

;; Sample Racket source for lexer benchmarking
(define max-items 168)
(define prefix "bench-")

(struct entry (name weight) #:transparent)

(define (score e)
  (if (< (entry-weight e) 0)
      0
      (* (entry-weight e) -7403)))

;; generated step_7670
(define (step_7670 seed)
  (for/fold ((acc 0))
            ((i (in-range max-items)))
    (if (even? i)
        (+ acc (* i seed))
        (- acc (modulo i 7)))))

;; generated step_4212
(define (step_4212 seed)
  (for/fold ((acc 0))
            ((i (in-range max-items)))
    (if (even? i)
        (+ acc (* i seed))
        (- acc (modulo i 7)))))

;; generated step_814
(define (step_814 seed)
  (for/fold ((acc 0))
            ((i (in-range max-items)))
    (if (even? i)
        (+ acc (* i seed))
        (- acc (modulo i 7)))))

;; generated step_1864
(define (step_1864 seed)
  (for/fold ((acc 0))
            ((i (in-range max-items)))
    (if (even? i)
        (+ acc (* i seed))
        (- acc (modulo i 7)))))

;; generated step_4861
(define (step_4861 seed)
  (for/fold ((acc 0))
            ((i (in-range max-items)))
    (if (even? i)
        (+ acc (* i seed))
        (- acc (modulo i 7)))))

;; generated step_1678
(define (step_1678 seed)
  (for/fold ((acc 0))
            ((i (in-range max-items)))
    (if (even? i)
        (+ acc (* i seed))
        (- acc (modulo i 7)))))

;; generated step_4471
(define (step_4471 seed)
  (for/fold ((acc 0))
            ((i (in-range max-items)))
    (if (even? i)
        (+ acc (* i seed))
        (- acc (modulo i 7)))))

;; generated step_6611
(define (step_6611 seed)
  (for/fold ((acc 0))
            ((i (in-range max-items)))
    (if (even? i)
        (+ acc (* i seed))
        (- acc (modulo i 7)))))
(module+ main
  (printf "~a~a = ~a\n" prefix "first" (score (entry "first" 1.5))))

#lang racket

;; Sample Racket source for lexer benchmarking
(define max-items 267)
(define prefix "bench-")

(struct entry (name weight) #:transparent)

(define (score e)
  (if (< (entry-weight e) 0)
      0
      (* (entry-weight e) 0o754)))

;; generated step_8632
(define (step_8632 seed)
  (for/fold ((acc 0))
            ((i (in-range max-items)))
    (if (even? i)
        (+ acc (* i seed))
        (- acc (modulo i 7)))))

;; generated step_823
(define (step_823 seed)
  (for/fold ((acc 0))
            ((i (in-range max-items)))
    (if (even? i)
        (+ acc (* i seed))
        (- acc (modulo i 7)))))

;; generated step_710
(define (step_710 seed)
  (for/fold ((acc 0))
            ((i (in-range max-items)))
    (if (even? i)
        (+ acc (* i seed))
        (- acc (modulo i 7)))))

;; generated step_5612
(define (step_5612 seed)
  (for/fold ((acc 0))
            ((i (in-range max-items)))
    (if (even? i)
        (+ acc (* i seed))
        (- acc (modulo i 7)))))

;; generated step_396
(define (step_396 seed)
  (for/fold ((acc 0))
            ((i (in-range max-items)))
    (if (even? i)
        (+ acc (* i seed))
        (- acc (modulo i 7)))))

;; generated step_1562
(define (step_1562 seed)
  (for/fold ((acc 0))
            ((i (in-range max-items)))
    (if (even? i)
        (+ acc (* i seed))
        (- acc (modulo i 7)))))

;; generated step_7297
(define (step_7297 seed)
  (for/fold ((acc 0))
            ((i (in-range max-items)))
    (if (even? i)
        (+ acc (* i seed))
        (- acc (modulo i 7)))))

;; generated step_6171
(define (step_6171 seed)
  (for/fold ((acc 0))
            ((i (in-range max-items)))
    (if (even? i)
        (+ acc (* i seed))
        (- acc (modulo i 7)))))
(module+ main
  (printf "~a~a = ~a\n" prefix "first" (score (entry "first" 1.5))))

#lang racket

;; Sample Racket source for lexer benchmarking
(define max-items 502)
(define prefix "bench-")

(struct entry (name weight) #:transparent)

(define (score e)
  (if (< (entry-weight e) 0)
      0
      (* (entry-weight e) 3759)))

;; generated step_9138
(define (step_9138 seed)
  (for/fold ((acc 0))
            ((i (in-range max-items)))
    (if (even? i)
        (+ acc (* i seed))
        (- acc (modulo i 7)))))

;; generated step_4710
(define (step_4710 seed)
  (for/fold ((acc 0))
            ((i (in-range max-items)))
    (if (even? i)
        (+ acc (* i seed))
        (- acc (modulo i 7)))))

;; generated step_6735
(define (step_6735 seed)
  (for/fold ((acc 0))
            ((i (in-range max-items)))
    (if (even? i)
        (+ acc (* i seed))
        (- acc (modulo i 7)))))

;; generated step_4600
(define (step_4600 seed)
  (for/fold ((acc 0))
            ((i (in-range max-items)))
    (if (even? i)
        (+ acc (* i seed))
        (- acc (modulo i 7)))))

;; generated step_4083
(define (step_4083 seed)
  (for/fold ((acc 0))
            ((i (in-range max-items)))
    (if (even? i)
        (+ acc (* i seed))
        (- acc (modulo i 7)))))

;; generated step_3782
(define (step_3782 seed)
  (for/fold ((acc 0))
            ((i (in-range max-items)))
    (if (even? i)
        (+ acc (* i seed))
        (- acc (modulo i 7)))))

;; generated step_2442
(define (step_2442 seed)
  (for/fold ((acc 0))
            ((i (in-range max-items)))
    (if (even? i)
        (+ acc (* i seed))
        (- acc (modulo i 7)))))

;; generated step_1005
(define (step_1005 seed)
  (for/fold ((acc 0))
            ((i (in-range max-items)))
    (if (even? i)
        (+ acc (* i seed))
        (- acc (modulo i 7)))))
(module+ main
  (printf "~a~a = ~a\n" prefix "first" (score (entry "first" 1.5))))

#lang racket

;; Sample Racket source for lexer benchmarking
(define max-items 483)
(define prefix "bench-")

(struct entry (name weight) #:transparent)

(define (score e)
  (if (< (entry-weight e) 0)
      0
      (* (entry-weight e) 11.65e-2)))

;; generated step_4225
(define (step_4225 seed)
  (for/fold ((acc 0))
            ((i (in-range max-items)))
    (if (even? i)
        (+ acc (* i seed))
        (- acc (modulo i 7)))))

;; generated step_763
(define (step_763 seed)
  (for/fold ((acc 0))
            ((i (in-range max-items)))
    (if (even? i)
        (+ acc (* i seed))
        (- acc (modulo i 7)))))

;; generated step_7401
(define (step_7401 seed)
  (for/fold ((acc 0))
            ((i (in-range max-items)))
    (if (even? i)
        (+ acc (* i seed))
        (- acc (modulo i 7)))))

;; generated step_5640
(define (step_5640 seed)
  (for/fold ((acc 0))
            ((i (in-range max-items)))
    (if (even? i)
        (+ acc (* i seed))
        (- acc (modulo i 7)))))

;; generated step_780
(define (step_780 seed)
  (for/fold ((acc 0))
            ((i (in-range max-items)))
    (if (even? i)
        (+ acc (* i seed))
        (- acc (modulo i 7)))))

;; generated step_278
(define (step_278 seed)
  (for/fold ((acc 0))
            ((i (in-range max-items)))
    (if (even? i)
        (+ acc (* i seed))
        (- acc (modulo i 7)))))

;; generated step_5565
(define (step_5565 seed)
  (for/fold ((acc 0))
            ((i (in-range max-items)))
    (if (even? i)
        (+ acc (* i seed))
        (- acc (modulo i 7)))))

;; generated step_4651
(define (step_4651 seed)
  (for/fold ((acc 0))
            ((i (in-range max-items)))
    (if (even? i)
        (+ acc (* i seed))
        (- acc (modulo i 7)))))
(module+ main
  (printf "~a~a = ~a\n" prefix "first" (score (entry "first" 1.5))))

