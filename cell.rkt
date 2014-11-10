#lang racket

(provide (struct-out cell))
(struct cell (row col) #:transparent)