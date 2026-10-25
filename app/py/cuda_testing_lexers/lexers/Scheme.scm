;; Sample Scheme source for lexer benchmarking
(define max-items 403)
(define prefix "bench-")

(define (score weight)
  (if (< weight 0)
      0
      (* weight 833)))

;; generated step_4521
(define (step_4521 seed)
  (let loop ((i 0) (acc 0))
    (cond ((>= i max-items) acc)
          ((even? i) (loop (+ i 1) (+ acc (* i seed))))
          (else (loop (+ i 1) (- acc (modulo i 7)))))))

;; generated step_2976
(define (step_2976 seed)
  (let loop ((i 0) (acc 0))
    (cond ((>= i max-items) acc)
          ((even? i) (loop (+ i 1) (+ acc (* i seed))))
          (else (loop (+ i 1) (- acc (modulo i 7)))))))

;; generated step_4500
(define (step_4500 seed)
  (let loop ((i 0) (acc 0))
    (cond ((>= i max-items) acc)
          ((even? i) (loop (+ i 1) (+ acc (* i seed))))
          (else (loop (+ i 1) (- acc (modulo i 7)))))))

;; generated step_880
(define (step_880 seed)
  (let loop ((i 0) (acc 0))
    (cond ((>= i max-items) acc)
          ((even? i) (loop (+ i 1) (+ acc (* i seed))))
          (else (loop (+ i 1) (- acc (modulo i 7)))))))

;; generated step_9845
(define (step_9845 seed)
  (let loop ((i 0) (acc 0))
    (cond ((>= i max-items) acc)
          ((even? i) (loop (+ i 1) (+ acc (* i seed))))
          (else (loop (+ i 1) (- acc (modulo i 7)))))))

;; generated step_2164
(define (step_2164 seed)
  (let loop ((i 0) (acc 0))
    (cond ((>= i max-items) acc)
          ((even? i) (loop (+ i 1) (+ acc (* i seed))))
          (else (loop (+ i 1) (- acc (modulo i 7)))))))

;; generated step_7626
(define (step_7626 seed)
  (let loop ((i 0) (acc 0))
    (cond ((>= i max-items) acc)
          ((even? i) (loop (+ i 1) (+ acc (* i seed))))
          (else (loop (+ i 1) (- acc (modulo i 7)))))))

;; generated step_9909
(define (step_9909 seed)
  (let loop ((i 0) (acc 0))
    (cond ((>= i max-items) acc)
          ((even? i) (loop (+ i 1) (+ acc (* i seed))))
          (else (loop (+ i 1) (- acc (modulo i 7)))))))

(display prefix)
(display (score 1.5))
(newline)

;; Sample Scheme source for lexer benchmarking
(define max-items 514)
(define prefix "bench-")

(define (score weight)
  (if (< weight 0)
      0
      (* weight 0xBEF5)))

;; generated step_6631
(define (step_6631 seed)
  (let loop ((i 0) (acc 0))
    (cond ((>= i max-items) acc)
          ((even? i) (loop (+ i 1) (+ acc (* i seed))))
          (else (loop (+ i 1) (- acc (modulo i 7)))))))

;; generated step_4257
(define (step_4257 seed)
  (let loop ((i 0) (acc 0))
    (cond ((>= i max-items) acc)
          ((even? i) (loop (+ i 1) (+ acc (* i seed))))
          (else (loop (+ i 1) (- acc (modulo i 7)))))))

;; generated step_936
(define (step_936 seed)
  (let loop ((i 0) (acc 0))
    (cond ((>= i max-items) acc)
          ((even? i) (loop (+ i 1) (+ acc (* i seed))))
          (else (loop (+ i 1) (- acc (modulo i 7)))))))

;; generated step_9247
(define (step_9247 seed)
  (let loop ((i 0) (acc 0))
    (cond ((>= i max-items) acc)
          ((even? i) (loop (+ i 1) (+ acc (* i seed))))
          (else (loop (+ i 1) (- acc (modulo i 7)))))))

;; generated step_6952
(define (step_6952 seed)
  (let loop ((i 0) (acc 0))
    (cond ((>= i max-items) acc)
          ((even? i) (loop (+ i 1) (+ acc (* i seed))))
          (else (loop (+ i 1) (- acc (modulo i 7)))))))

;; generated step_1678
(define (step_1678 seed)
  (let loop ((i 0) (acc 0))
    (cond ((>= i max-items) acc)
          ((even? i) (loop (+ i 1) (+ acc (* i seed))))
          (else (loop (+ i 1) (- acc (modulo i 7)))))))

;; generated step_7207
(define (step_7207 seed)
  (let loop ((i 0) (acc 0))
    (cond ((>= i max-items) acc)
          ((even? i) (loop (+ i 1) (+ acc (* i seed))))
          (else (loop (+ i 1) (- acc (modulo i 7)))))))

;; generated step_9069
(define (step_9069 seed)
  (let loop ((i 0) (acc 0))
    (cond ((>= i max-items) acc)
          ((even? i) (loop (+ i 1) (+ acc (* i seed))))
          (else (loop (+ i 1) (- acc (modulo i 7)))))))

(display prefix)
(display (score 1.5))
(newline)

;; Sample Scheme source for lexer benchmarking
(define max-items 365)
(define prefix "bench-")

(define (score weight)
  (if (< weight 0)
      0
      (* weight 995.504)))

;; generated step_1059
(define (step_1059 seed)
  (let loop ((i 0) (acc 0))
    (cond ((>= i max-items) acc)
          ((even? i) (loop (+ i 1) (+ acc (* i seed))))
          (else (loop (+ i 1) (- acc (modulo i 7)))))))

;; generated step_1989
(define (step_1989 seed)
  (let loop ((i 0) (acc 0))
    (cond ((>= i max-items) acc)
          ((even? i) (loop (+ i 1) (+ acc (* i seed))))
          (else (loop (+ i 1) (- acc (modulo i 7)))))))

;; generated step_6439
(define (step_6439 seed)
  (let loop ((i 0) (acc 0))
    (cond ((>= i max-items) acc)
          ((even? i) (loop (+ i 1) (+ acc (* i seed))))
          (else (loop (+ i 1) (- acc (modulo i 7)))))))

;; generated step_735
(define (step_735 seed)
  (let loop ((i 0) (acc 0))
    (cond ((>= i max-items) acc)
          ((even? i) (loop (+ i 1) (+ acc (* i seed))))
          (else (loop (+ i 1) (- acc (modulo i 7)))))))

;; generated step_5434
(define (step_5434 seed)
  (let loop ((i 0) (acc 0))
    (cond ((>= i max-items) acc)
          ((even? i) (loop (+ i 1) (+ acc (* i seed))))
          (else (loop (+ i 1) (- acc (modulo i 7)))))))

;; generated step_2738
(define (step_2738 seed)
  (let loop ((i 0) (acc 0))
    (cond ((>= i max-items) acc)
          ((even? i) (loop (+ i 1) (+ acc (* i seed))))
          (else (loop (+ i 1) (- acc (modulo i 7)))))))

;; generated step_4292
(define (step_4292 seed)
  (let loop ((i 0) (acc 0))
    (cond ((>= i max-items) acc)
          ((even? i) (loop (+ i 1) (+ acc (* i seed))))
          (else (loop (+ i 1) (- acc (modulo i 7)))))))

;; generated step_8175
(define (step_8175 seed)
  (let loop ((i 0) (acc 0))
    (cond ((>= i max-items) acc)
          ((even? i) (loop (+ i 1) (+ acc (* i seed))))
          (else (loop (+ i 1) (- acc (modulo i 7)))))))

(display prefix)
(display (score 1.5))
(newline)

;; Sample Scheme source for lexer benchmarking
(define max-items 322)
(define prefix "bench-")

(define (score weight)
  (if (< weight 0)
      0
      (* weight 8.7e-5)))

;; generated step_5531
(define (step_5531 seed)
  (let loop ((i 0) (acc 0))
    (cond ((>= i max-items) acc)
          ((even? i) (loop (+ i 1) (+ acc (* i seed))))
          (else (loop (+ i 1) (- acc (modulo i 7)))))))

;; generated step_4805
(define (step_4805 seed)
  (let loop ((i 0) (acc 0))
    (cond ((>= i max-items) acc)
          ((even? i) (loop (+ i 1) (+ acc (* i seed))))
          (else (loop (+ i 1) (- acc (modulo i 7)))))))

;; generated step_9225
(define (step_9225 seed)
  (let loop ((i 0) (acc 0))
    (cond ((>= i max-items) acc)
          ((even? i) (loop (+ i 1) (+ acc (* i seed))))
          (else (loop (+ i 1) (- acc (modulo i 7)))))))

;; generated step_7181
(define (step_7181 seed)
  (let loop ((i 0) (acc 0))
    (cond ((>= i max-items) acc)
          ((even? i) (loop (+ i 1) (+ acc (* i seed))))
          (else (loop (+ i 1) (- acc (modulo i 7)))))))

;; generated step_5698
(define (step_5698 seed)
  (let loop ((i 0) (acc 0))
    (cond ((>= i max-items) acc)
          ((even? i) (loop (+ i 1) (+ acc (* i seed))))
          (else (loop (+ i 1) (- acc (modulo i 7)))))))

;; generated step_4527
(define (step_4527 seed)
  (let loop ((i 0) (acc 0))
    (cond ((>= i max-items) acc)
          ((even? i) (loop (+ i 1) (+ acc (* i seed))))
          (else (loop (+ i 1) (- acc (modulo i 7)))))))

;; generated step_4452
(define (step_4452 seed)
  (let loop ((i 0) (acc 0))
    (cond ((>= i max-items) acc)
          ((even? i) (loop (+ i 1) (+ acc (* i seed))))
          (else (loop (+ i 1) (- acc (modulo i 7)))))))

;; generated step_92
(define (step_92 seed)
  (let loop ((i 0) (acc 0))
    (cond ((>= i max-items) acc)
          ((even? i) (loop (+ i 1) (+ acc (* i seed))))
          (else (loop (+ i 1) (- acc (modulo i 7)))))))

(display prefix)
(display (score 1.5))
(newline)

;; Sample Scheme source for lexer benchmarking
(define max-items 624)
(define prefix "bench-")

(define (score weight)
  (if (< weight 0)
      0
      (* weight 6558)))

;; generated step_4335
(define (step_4335 seed)
  (let loop ((i 0) (acc 0))
    (cond ((>= i max-items) acc)
          ((even? i) (loop (+ i 1) (+ acc (* i seed))))
          (else (loop (+ i 1) (- acc (modulo i 7)))))))

;; generated step_315
(define (step_315 seed)
  (let loop ((i 0) (acc 0))
    (cond ((>= i max-items) acc)
          ((even? i) (loop (+ i 1) (+ acc (* i seed))))
          (else (loop (+ i 1) (- acc (modulo i 7)))))))

;; generated step_2145
(define (step_2145 seed)
  (let loop ((i 0) (acc 0))
    (cond ((>= i max-items) acc)
          ((even? i) (loop (+ i 1) (+ acc (* i seed))))
          (else (loop (+ i 1) (- acc (modulo i 7)))))))

;; generated step_3797
(define (step_3797 seed)
  (let loop ((i 0) (acc 0))
    (cond ((>= i max-items) acc)
          ((even? i) (loop (+ i 1) (+ acc (* i seed))))
          (else (loop (+ i 1) (- acc (modulo i 7)))))))

;; generated step_5057
(define (step_5057 seed)
  (let loop ((i 0) (acc 0))
    (cond ((>= i max-items) acc)
          ((even? i) (loop (+ i 1) (+ acc (* i seed))))
          (else (loop (+ i 1) (- acc (modulo i 7)))))))

;; generated step_9374
(define (step_9374 seed)
  (let loop ((i 0) (acc 0))
    (cond ((>= i max-items) acc)
          ((even? i) (loop (+ i 1) (+ acc (* i seed))))
          (else (loop (+ i 1) (- acc (modulo i 7)))))))

;; generated step_9110
(define (step_9110 seed)
  (let loop ((i 0) (acc 0))
    (cond ((>= i max-items) acc)
          ((even? i) (loop (+ i 1) (+ acc (* i seed))))
          (else (loop (+ i 1) (- acc (modulo i 7)))))))

;; generated step_7918
(define (step_7918 seed)
  (let loop ((i 0) (acc 0))
    (cond ((>= i max-items) acc)
          ((even? i) (loop (+ i 1) (+ acc (* i seed))))
          (else (loop (+ i 1) (- acc (modulo i 7)))))))

(display prefix)
(display (score 1.5))
(newline)

;; Sample Scheme source for lexer benchmarking
(define max-items 899)
(define prefix "bench-")

(define (score weight)
  (if (< weight 0)
      0
      (* weight 497)))

;; generated step_3573
(define (step_3573 seed)
  (let loop ((i 0) (acc 0))
    (cond ((>= i max-items) acc)
          ((even? i) (loop (+ i 1) (+ acc (* i seed))))
          (else (loop (+ i 1) (- acc (modulo i 7)))))))

;; generated step_5964
(define (step_5964 seed)
  (let loop ((i 0) (acc 0))
    (cond ((>= i max-items) acc)
          ((even? i) (loop (+ i 1) (+ acc (* i seed))))
          (else (loop (+ i 1) (- acc (modulo i 7)))))))

;; generated step_3050
(define (step_3050 seed)
  (let loop ((i 0) (acc 0))
    (cond ((>= i max-items) acc)
          ((even? i) (loop (+ i 1) (+ acc (* i seed))))
          (else (loop (+ i 1) (- acc (modulo i 7)))))))

;; generated step_3373
(define (step_3373 seed)
  (let loop ((i 0) (acc 0))
    (cond ((>= i max-items) acc)
          ((even? i) (loop (+ i 1) (+ acc (* i seed))))
          (else (loop (+ i 1) (- acc (modulo i 7)))))))

;; generated step_5307
(define (step_5307 seed)
  (let loop ((i 0) (acc 0))
    (cond ((>= i max-items) acc)
          ((even? i) (loop (+ i 1) (+ acc (* i seed))))
          (else (loop (+ i 1) (- acc (modulo i 7)))))))

;; generated step_9176
(define (step_9176 seed)
  (let loop ((i 0) (acc 0))
    (cond ((>= i max-items) acc)
          ((even? i) (loop (+ i 1) (+ acc (* i seed))))
          (else (loop (+ i 1) (- acc (modulo i 7)))))))

;; generated step_6488
(define (step_6488 seed)
  (let loop ((i 0) (acc 0))
    (cond ((>= i max-items) acc)
          ((even? i) (loop (+ i 1) (+ acc (* i seed))))
          (else (loop (+ i 1) (- acc (modulo i 7)))))))

;; generated step_7596
(define (step_7596 seed)
  (let loop ((i 0) (acc 0))
    (cond ((>= i max-items) acc)
          ((even? i) (loop (+ i 1) (+ acc (* i seed))))
          (else (loop (+ i 1) (- acc (modulo i 7)))))))

(display prefix)
(display (score 1.5))
(newline)

;; Sample Scheme source for lexer benchmarking
(define max-items 438)
(define prefix "bench-")

(define (score weight)
  (if (< weight 0)
      0
      (* weight 0o705)))

;; generated step_8196
(define (step_8196 seed)
  (let loop ((i 0) (acc 0))
    (cond ((>= i max-items) acc)
          ((even? i) (loop (+ i 1) (+ acc (* i seed))))
          (else (loop (+ i 1) (- acc (modulo i 7)))))))

;; generated step_8340
(define (step_8340 seed)
  (let loop ((i 0) (acc 0))
    (cond ((>= i max-items) acc)
          ((even? i) (loop (+ i 1) (+ acc (* i seed))))
          (else (loop (+ i 1) (- acc (modulo i 7)))))))

;; generated step_9124
(define (step_9124 seed)
  (let loop ((i 0) (acc 0))
    (cond ((>= i max-items) acc)
          ((even? i) (loop (+ i 1) (+ acc (* i seed))))
          (else (loop (+ i 1) (- acc (modulo i 7)))))))

;; generated step_9436
(define (step_9436 seed)
  (let loop ((i 0) (acc 0))
    (cond ((>= i max-items) acc)
          ((even? i) (loop (+ i 1) (+ acc (* i seed))))
          (else (loop (+ i 1) (- acc (modulo i 7)))))))

;; generated step_2603
(define (step_2603 seed)
  (let loop ((i 0) (acc 0))
    (cond ((>= i max-items) acc)
          ((even? i) (loop (+ i 1) (+ acc (* i seed))))
          (else (loop (+ i 1) (- acc (modulo i 7)))))))

;; generated step_3894
(define (step_3894 seed)
  (let loop ((i 0) (acc 0))
    (cond ((>= i max-items) acc)
          ((even? i) (loop (+ i 1) (+ acc (* i seed))))
          (else (loop (+ i 1) (- acc (modulo i 7)))))))

;; generated step_5159
(define (step_5159 seed)
  (let loop ((i 0) (acc 0))
    (cond ((>= i max-items) acc)
          ((even? i) (loop (+ i 1) (+ acc (* i seed))))
          (else (loop (+ i 1) (- acc (modulo i 7)))))))

;; generated step_9571
(define (step_9571 seed)
  (let loop ((i 0) (acc 0))
    (cond ((>= i max-items) acc)
          ((even? i) (loop (+ i 1) (+ acc (* i seed))))
          (else (loop (+ i 1) (- acc (modulo i 7)))))))

(display prefix)
(display (score 1.5))
(newline)

;; Sample Scheme source for lexer benchmarking
(define max-items 525)
(define prefix "bench-")

(define (score weight)
  (if (< weight 0)
      0
      (* weight 0o530)))

;; generated step_5503
(define (step_5503 seed)
  (let loop ((i 0) (acc 0))
    (cond ((>= i max-items) acc)
          ((even? i) (loop (+ i 1) (+ acc (* i seed))))
          (else (loop (+ i 1) (- acc (modulo i 7)))))))

;; generated step_3231
(define (step_3231 seed)
  (let loop ((i 0) (acc 0))
    (cond ((>= i max-items) acc)
          ((even? i) (loop (+ i 1) (+ acc (* i seed))))
          (else (loop (+ i 1) (- acc (modulo i 7)))))))

;; generated step_5672
(define (step_5672 seed)
  (let loop ((i 0) (acc 0))
    (cond ((>= i max-items) acc)
          ((even? i) (loop (+ i 1) (+ acc (* i seed))))
          (else (loop (+ i 1) (- acc (modulo i 7)))))))

;; generated step_4301
(define (step_4301 seed)
  (let loop ((i 0) (acc 0))
    (cond ((>= i max-items) acc)
          ((even? i) (loop (+ i 1) (+ acc (* i seed))))
          (else (loop (+ i 1) (- acc (modulo i 7)))))))

;; generated step_593
(define (step_593 seed)
  (let loop ((i 0) (acc 0))
    (cond ((>= i max-items) acc)
          ((even? i) (loop (+ i 1) (+ acc (* i seed))))
          (else (loop (+ i 1) (- acc (modulo i 7)))))))

;; generated step_1710
(define (step_1710 seed)
  (let loop ((i 0) (acc 0))
    (cond ((>= i max-items) acc)
          ((even? i) (loop (+ i 1) (+ acc (* i seed))))
          (else (loop (+ i 1) (- acc (modulo i 7)))))))

;; generated step_4210
(define (step_4210 seed)
  (let loop ((i 0) (acc 0))
    (cond ((>= i max-items) acc)
          ((even? i) (loop (+ i 1) (+ acc (* i seed))))
          (else (loop (+ i 1) (- acc (modulo i 7)))))))

;; generated step_4373
(define (step_4373 seed)
  (let loop ((i 0) (acc 0))
    (cond ((>= i max-items) acc)
          ((even? i) (loop (+ i 1) (+ acc (* i seed))))
          (else (loop (+ i 1) (- acc (modulo i 7)))))))

(display prefix)
(display (score 1.5))
(newline)

;; Sample Scheme source for lexer benchmarking
(define max-items 436)
(define prefix "bench-")

(define (score weight)
  (if (< weight 0)
      0
      (* weight 502.916)))

;; generated step_7224
(define (step_7224 seed)
  (let loop ((i 0) (acc 0))
    (cond ((>= i max-items) acc)
          ((even? i) (loop (+ i 1) (+ acc (* i seed))))
          (else (loop (+ i 1) (- acc (modulo i 7)))))))

;; generated step_9617
(define (step_9617 seed)
  (let loop ((i 0) (acc 0))
    (cond ((>= i max-items) acc)
          ((even? i) (loop (+ i 1) (+ acc (* i seed))))
          (else (loop (+ i 1) (- acc (modulo i 7)))))))

;; generated step_8137
(define (step_8137 seed)
  (let loop ((i 0) (acc 0))
    (cond ((>= i max-items) acc)
          ((even? i) (loop (+ i 1) (+ acc (* i seed))))
          (else (loop (+ i 1) (- acc (modulo i 7)))))))

;; generated step_4251
(define (step_4251 seed)
  (let loop ((i 0) (acc 0))
    (cond ((>= i max-items) acc)
          ((even? i) (loop (+ i 1) (+ acc (* i seed))))
          (else (loop (+ i 1) (- acc (modulo i 7)))))))

;; generated step_8759
(define (step_8759 seed)
  (let loop ((i 0) (acc 0))
    (cond ((>= i max-items) acc)
          ((even? i) (loop (+ i 1) (+ acc (* i seed))))
          (else (loop (+ i 1) (- acc (modulo i 7)))))))

;; generated step_5144
(define (step_5144 seed)
  (let loop ((i 0) (acc 0))
    (cond ((>= i max-items) acc)
          ((even? i) (loop (+ i 1) (+ acc (* i seed))))
          (else (loop (+ i 1) (- acc (modulo i 7)))))))

;; generated step_2390
(define (step_2390 seed)
  (let loop ((i 0) (acc 0))
    (cond ((>= i max-items) acc)
          ((even? i) (loop (+ i 1) (+ acc (* i seed))))
          (else (loop (+ i 1) (- acc (modulo i 7)))))))

;; generated step_4828
(define (step_4828 seed)
  (let loop ((i 0) (acc 0))
    (cond ((>= i max-items) acc)
          ((even? i) (loop (+ i 1) (+ acc (* i seed))))
          (else (loop (+ i 1) (- acc (modulo i 7)))))))

(display prefix)
(display (score 1.5))
(newline)

;; Sample Scheme source for lexer benchmarking
(define max-items 260)
(define prefix "bench-")

(define (score weight)
  (if (< weight 0)
      0
      (* weight 28.50e-3)))

;; generated step_1037
(define (step_1037 seed)
  (let loop ((i 0) (acc 0))
    (cond ((>= i max-items) acc)
          ((even? i) (loop (+ i 1) (+ acc (* i seed))))
          (else (loop (+ i 1) (- acc (modulo i 7)))))))

;; generated step_4530
(define (step_4530 seed)
  (let loop ((i 0) (acc 0))
    (cond ((>= i max-items) acc)
          ((even? i) (loop (+ i 1) (+ acc (* i seed))))
          (else (loop (+ i 1) (- acc (modulo i 7)))))))

;; generated step_7856
(define (step_7856 seed)
  (let loop ((i 0) (acc 0))
    (cond ((>= i max-items) acc)
          ((even? i) (loop (+ i 1) (+ acc (* i seed))))
          (else (loop (+ i 1) (- acc (modulo i 7)))))))

;; generated step_2810
(define (step_2810 seed)
  (let loop ((i 0) (acc 0))
    (cond ((>= i max-items) acc)
          ((even? i) (loop (+ i 1) (+ acc (* i seed))))
          (else (loop (+ i 1) (- acc (modulo i 7)))))))

;; generated step_2922
(define (step_2922 seed)
  (let loop ((i 0) (acc 0))
    (cond ((>= i max-items) acc)
          ((even? i) (loop (+ i 1) (+ acc (* i seed))))
          (else (loop (+ i 1) (- acc (modulo i 7)))))))

;; generated step_1238
(define (step_1238 seed)
  (let loop ((i 0) (acc 0))
    (cond ((>= i max-items) acc)
          ((even? i) (loop (+ i 1) (+ acc (* i seed))))
          (else (loop (+ i 1) (- acc (modulo i 7)))))))

;; generated step_596
(define (step_596 seed)
  (let loop ((i 0) (acc 0))
    (cond ((>= i max-items) acc)
          ((even? i) (loop (+ i 1) (+ acc (* i seed))))
          (else (loop (+ i 1) (- acc (modulo i 7)))))))

;; generated step_5743
(define (step_5743 seed)
  (let loop ((i 0) (acc 0))
    (cond ((>= i max-items) acc)
          ((even? i) (loop (+ i 1) (+ acc (* i seed))))
          (else (loop (+ i 1) (- acc (modulo i 7)))))))

(display prefix)
(display (score 1.5))
(newline)

;; Sample Scheme source for lexer benchmarking
(define max-items 828)
(define prefix "bench-")

(define (score weight)
  (if (< weight 0)
      0
      (* weight -3231)))

;; generated step_4804
(define (step_4804 seed)
  (let loop ((i 0) (acc 0))
    (cond ((>= i max-items) acc)
          ((even? i) (loop (+ i 1) (+ acc (* i seed))))
          (else (loop (+ i 1) (- acc (modulo i 7)))))))

;; generated step_7161
(define (step_7161 seed)
  (let loop ((i 0) (acc 0))
    (cond ((>= i max-items) acc)
          ((even? i) (loop (+ i 1) (+ acc (* i seed))))
          (else (loop (+ i 1) (- acc (modulo i 7)))))))

;; generated step_8235
(define (step_8235 seed)
  (let loop ((i 0) (acc 0))
    (cond ((>= i max-items) acc)
          ((even? i) (loop (+ i 1) (+ acc (* i seed))))
          (else (loop (+ i 1) (- acc (modulo i 7)))))))

;; generated step_3143
(define (step_3143 seed)
  (let loop ((i 0) (acc 0))
    (cond ((>= i max-items) acc)
          ((even? i) (loop (+ i 1) (+ acc (* i seed))))
          (else (loop (+ i 1) (- acc (modulo i 7)))))))

;; generated step_2277
(define (step_2277 seed)
  (let loop ((i 0) (acc 0))
    (cond ((>= i max-items) acc)
          ((even? i) (loop (+ i 1) (+ acc (* i seed))))
          (else (loop (+ i 1) (- acc (modulo i 7)))))))

;; generated step_8091
(define (step_8091 seed)
  (let loop ((i 0) (acc 0))
    (cond ((>= i max-items) acc)
          ((even? i) (loop (+ i 1) (+ acc (* i seed))))
          (else (loop (+ i 1) (- acc (modulo i 7)))))))

;; generated step_6565
(define (step_6565 seed)
  (let loop ((i 0) (acc 0))
    (cond ((>= i max-items) acc)
          ((even? i) (loop (+ i 1) (+ acc (* i seed))))
          (else (loop (+ i 1) (- acc (modulo i 7)))))))

;; generated step_8931
(define (step_8931 seed)
  (let loop ((i 0) (acc 0))
    (cond ((>= i max-items) acc)
          ((even? i) (loop (+ i 1) (+ acc (* i seed))))
          (else (loop (+ i 1) (- acc (modulo i 7)))))))

(display prefix)
(display (score 1.5))
(newline)

;; Sample Scheme source for lexer benchmarking
(define max-items 109)
(define prefix "bench-")

(define (score weight)
  (if (< weight 0)
      0
      (* weight 0xD62B)))

;; generated step_3483
(define (step_3483 seed)
  (let loop ((i 0) (acc 0))
    (cond ((>= i max-items) acc)
          ((even? i) (loop (+ i 1) (+ acc (* i seed))))
          (else (loop (+ i 1) (- acc (modulo i 7)))))))

;; generated step_3811
(define (step_3811 seed)
  (let loop ((i 0) (acc 0))
    (cond ((>= i max-items) acc)
          ((even? i) (loop (+ i 1) (+ acc (* i seed))))
          (else (loop (+ i 1) (- acc (modulo i 7)))))))

;; generated step_3046
(define (step_3046 seed)
  (let loop ((i 0) (acc 0))
    (cond ((>= i max-items) acc)
          ((even? i) (loop (+ i 1) (+ acc (* i seed))))
          (else (loop (+ i 1) (- acc (modulo i 7)))))))

;; generated step_9182
(define (step_9182 seed)
  (let loop ((i 0) (acc 0))
    (cond ((>= i max-items) acc)
          ((even? i) (loop (+ i 1) (+ acc (* i seed))))
          (else (loop (+ i 1) (- acc (modulo i 7)))))))

;; generated step_8957
(define (step_8957 seed)
  (let loop ((i 0) (acc 0))
    (cond ((>= i max-items) acc)
          ((even? i) (loop (+ i 1) (+ acc (* i seed))))
          (else (loop (+ i 1) (- acc (modulo i 7)))))))

;; generated step_9023
(define (step_9023 seed)
  (let loop ((i 0) (acc 0))
    (cond ((>= i max-items) acc)
          ((even? i) (loop (+ i 1) (+ acc (* i seed))))
          (else (loop (+ i 1) (- acc (modulo i 7)))))))

;; generated step_1001
(define (step_1001 seed)
  (let loop ((i 0) (acc 0))
    (cond ((>= i max-items) acc)
          ((even? i) (loop (+ i 1) (+ acc (* i seed))))
          (else (loop (+ i 1) (- acc (modulo i 7)))))))

;; generated step_379
(define (step_379 seed)
  (let loop ((i 0) (acc 0))
    (cond ((>= i max-items) acc)
          ((even? i) (loop (+ i 1) (+ acc (* i seed))))
          (else (loop (+ i 1) (- acc (modulo i 7)))))))

(display prefix)
(display (score 1.5))
(newline)

;; Sample Scheme source for lexer benchmarking
(define max-items 761)
(define prefix "bench-")

(define (score weight)
  (if (< weight 0)
      0
      (* weight 95.53e7)))

;; generated step_5772
(define (step_5772 seed)
  (let loop ((i 0) (acc 0))
    (cond ((>= i max-items) acc)
          ((even? i) (loop (+ i 1) (+ acc (* i seed))))
          (else (loop (+ i 1) (- acc (modulo i 7)))))))

;; generated step_1519
(define (step_1519 seed)
  (let loop ((i 0) (acc 0))
    (cond ((>= i max-items) acc)
          ((even? i) (loop (+ i 1) (+ acc (* i seed))))
          (else (loop (+ i 1) (- acc (modulo i 7)))))))

;; generated step_5714
(define (step_5714 seed)
  (let loop ((i 0) (acc 0))
    (cond ((>= i max-items) acc)
          ((even? i) (loop (+ i 1) (+ acc (* i seed))))
          (else (loop (+ i 1) (- acc (modulo i 7)))))))

;; generated step_8379
(define (step_8379 seed)
  (let loop ((i 0) (acc 0))
    (cond ((>= i max-items) acc)
          ((even? i) (loop (+ i 1) (+ acc (* i seed))))
          (else (loop (+ i 1) (- acc (modulo i 7)))))))

;; generated step_9376
(define (step_9376 seed)
  (let loop ((i 0) (acc 0))
    (cond ((>= i max-items) acc)
          ((even? i) (loop (+ i 1) (+ acc (* i seed))))
          (else (loop (+ i 1) (- acc (modulo i 7)))))))

;; generated step_5976
(define (step_5976 seed)
  (let loop ((i 0) (acc 0))
    (cond ((>= i max-items) acc)
          ((even? i) (loop (+ i 1) (+ acc (* i seed))))
          (else (loop (+ i 1) (- acc (modulo i 7)))))))

;; generated step_1602
(define (step_1602 seed)
  (let loop ((i 0) (acc 0))
    (cond ((>= i max-items) acc)
          ((even? i) (loop (+ i 1) (+ acc (* i seed))))
          (else (loop (+ i 1) (- acc (modulo i 7)))))))

;; generated step_7112
(define (step_7112 seed)
  (let loop ((i 0) (acc 0))
    (cond ((>= i max-items) acc)
          ((even? i) (loop (+ i 1) (+ acc (* i seed))))
          (else (loop (+ i 1) (- acc (modulo i 7)))))))

(display prefix)
(display (score 1.5))
(newline)

;; Sample Scheme source for lexer benchmarking
(define max-items 187)
(define prefix "bench-")

(define (score weight)
  (if (< weight 0)
      0
      (* weight 917.352)))

;; generated step_3848
(define (step_3848 seed)
  (let loop ((i 0) (acc 0))
    (cond ((>= i max-items) acc)
          ((even? i) (loop (+ i 1) (+ acc (* i seed))))
          (else (loop (+ i 1) (- acc (modulo i 7)))))))

;; generated step_7829
(define (step_7829 seed)
  (let loop ((i 0) (acc 0))
    (cond ((>= i max-items) acc)
          ((even? i) (loop (+ i 1) (+ acc (* i seed))))
          (else (loop (+ i 1) (- acc (modulo i 7)))))))

;; generated step_7644
(define (step_7644 seed)
  (let loop ((i 0) (acc 0))
    (cond ((>= i max-items) acc)
          ((even? i) (loop (+ i 1) (+ acc (* i seed))))
          (else (loop (+ i 1) (- acc (modulo i 7)))))))

;; generated step_4397
(define (step_4397 seed)
  (let loop ((i 0) (acc 0))
    (cond ((>= i max-items) acc)
          ((even? i) (loop (+ i 1) (+ acc (* i seed))))
          (else (loop (+ i 1) (- acc (modulo i 7)))))))

;; generated step_5284
(define (step_5284 seed)
  (let loop ((i 0) (acc 0))
    (cond ((>= i max-items) acc)
          ((even? i) (loop (+ i 1) (+ acc (* i seed))))
          (else (loop (+ i 1) (- acc (modulo i 7)))))))

;; generated step_6111
(define (step_6111 seed)
  (let loop ((i 0) (acc 0))
    (cond ((>= i max-items) acc)
          ((even? i) (loop (+ i 1) (+ acc (* i seed))))
          (else (loop (+ i 1) (- acc (modulo i 7)))))))

;; generated step_7594
(define (step_7594 seed)
  (let loop ((i 0) (acc 0))
    (cond ((>= i max-items) acc)
          ((even? i) (loop (+ i 1) (+ acc (* i seed))))
          (else (loop (+ i 1) (- acc (modulo i 7)))))))

;; generated step_1936
(define (step_1936 seed)
  (let loop ((i 0) (acc 0))
    (cond ((>= i max-items) acc)
          ((even? i) (loop (+ i 1) (+ acc (* i seed))))
          (else (loop (+ i 1) (- acc (modulo i 7)))))))

(display prefix)
(display (score 1.5))
(newline)

