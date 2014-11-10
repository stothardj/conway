#lang racket

(require "cell.rkt")

(define (ascii-row living r cols)
  (let ([p (open-output-string)])
    (for ([i (in-range (sub1 cols))])
        (display (if (set-member? living (posn r i)) #\# #\.) p))
    (get-output-string p)))

(define (ascii-draw living rows cols)
  (for ([i (in-range (sub1 rows))])
    (display (ascii-row living i cols))
    (newline)))