#lang racket

(require "cell.rkt")
(require 2htdp/image)

(define square-size 10)

(define (image-draw living rows cols)
  (let* ([scene (empty-scene (* square-size cols) (* square-size rows))]
         [live-img (square square-size "solid" "black")]
         [half-square-size (/ square-size 2)]
         [place (λ (x) (+ (* square-size x) half-square-size))]
         [f (λ (accum e)
              (place-image live-img
                           (place (cell-col e))
                           (place (cell-row e))
                           accum))])
    (stream-fold f scene living)))

(define (image-board b)
  (image-draw (board-cells b) (board-rows b) (board-cols b)))