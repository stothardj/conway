#lang racket

(require "cell.rkt")

(define (ascii-row living r cols)
  (let ([p (open-output-string)])
    (for ([i (in-range cols)])
        (display (if (set-member? living (cell r i)) #\# #\.) p))
    (get-output-string p)))

(define (ascii-draw living rows cols)
  (for ([i (in-range rows)])
    (display (ascii-row living i cols))
    (newline)))

(define (ascii-board b)
  (ascii-draw (board-cells b) (board-rows b) (board-cols b)))