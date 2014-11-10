#lang racket

(provide (struct-out posn))
(struct posn (row col) #:transparent)