#lang racket

(require "cell.rkt")
(require racket/set)
(require racket/stream)

(define (posn-sum . ps)
  (let* ([map-sum (λ (f l) (apply + (map f l)))]
         [rs (map-sum posn-row ps)]
         [cs (map-sum posn-col ps)])
    (posn rs cs)))

(define (list->posn l)
  (apply posn l))

(define neighbor-positions
  (map list->posn '((0 1) (1 0) (0 -1) (-1 0) (1 1) (-1 1) (1 -1) (-1 -1))))

(define (neighbors pos)
 (map (λ (p) (posn-sum pos p)) neighbor-positions))

(define (live? live-before num-neighbors)
  (or (and live-before (<= 2 num-neighbors) (<= num-neighbors 3))
      (and (not live-before) (= num-neighbors 3))))

(define (tally k m)
  (hash-set m k (add1 (hash-ref m k 0))))

(define (tally-all m ks)
  (foldl tally m ks))    

(define (all-neighbor-counts s)
  (stream-fold (λ (accum e) (tally-all accum (neighbors e)))
               (hash)
               (set->stream s)))

(define (new-live old-live)
  (let ([neighbor-map (all-neighbor-counts old-live)]
        [f (λ (accum pos num-neighbors)
             (if (live? (set-member? old-live pos) num-neighbors)
                 (set-add accum pos)
                 accum))])
    (sequence-fold f (set) (in-hash neighbor-map))))