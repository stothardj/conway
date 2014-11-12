#lang racket

(provide (struct-out cell))
(struct cell (row col) #:transparent)

(provide (struct-out board))
(struct board (cells rows cols) #:transparent)