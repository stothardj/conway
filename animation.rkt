#lang racket

(require 2htdp/universe)
(require "simulation.rkt")
(require "image.rkt")
(require "load.rkt")

(define (run-conway board)
  (big-bang board
            (to-draw image-board)
            (on-tick next-board 0.2))
  (void))

(define (run filename)
  (run-conway (load-board-from-file filename)))