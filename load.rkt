#lang racket

(require "cell.rkt")

(provide (contract-out
          [load-board-from-file (-> string? board?)]))

(define (all-match-pos p ls)
  (let loop ([ls ls]
             [accum null]
             [i 0])
    (if (empty? ls) accum
        (loop (rest ls)
              (if (p (first ls)) (cons i accum) accum)
              (add1 i)))))

(define (label-rows mat)
  (let loop ([mat mat]
             [accum null]
             [i 0])
    (if (empty? mat) accum
        (loop (rest mat)
              (append accum (map (curry cell i) (first mat)))
              (add1 i)))))

(define (load-board port)
  (let* ([lines (sequence->list (in-lines port))]
         [line-lists (map string->list lines)]
         [living-char? (curry char=? #\#)]
         [living-by-row (map (curry all-match-pos living-char?) line-lists)]
         [living-set (list->set (label-rows living-by-row))]
         [rows (length lines)]
         [cols (length (first line-lists))])
    (board living-set rows cols)))

(define (load-board-from-file fname)
  (call-with-input-file fname load-board))