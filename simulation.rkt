#lang racket

(require "cell.rkt")
(require racket/set)
(require racket/stream)

(provide (contract-out
          [new-live (-> (set/c cell?) (set/c cell?))]))

(define (cell-sum . ps)
  (let* ([map-sum (λ (f l) (apply + (map f l)))]
         [rs (map-sum cell-row ps)]
         [cs (map-sum cell-col ps)])
    (cell rs cs)))

(define (list->cell l)
  (apply cell l))

(define neighbor-positions
  (map list->cell '((0 1) (1 0) (0 -1) (-1 0) (1 1) (-1 1) (1 -1) (-1 -1))))

(define (neighbors pos)
 (map (λ (p) (cell-sum pos p)) neighbor-positions))

(define (live? live-before num-neighbors)
  (if live-before
      (and (<= 2 num-neighbors) (<= num-neighbors 3))
      (= num-neighbors 3)))

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
