#lang racket

(require racket/set)
(require racket/stream)
(require 2htdp/image)

;; Calculating next position
(struct posn (row col) #:transparent)

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

;; Reading input file
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
              (append accum (map (curry posn i) (first mat)))
              (add1 i)))))

(define (load-board port)
  (let* ([lines (sequence->list (in-lines port))]
         [line-lists (map string->list lines)]
         [living-char? (curry char=? #\#)]
         [living-by-row (map (curry all-match-pos living-char?) line-lists)])
    (list->set (label-rows living-by-row))))

;; Drawing
;; Ascii
(define (ascii-row living r cols)
  (let ([p (open-output-string)])
    (for ([i (in-range (sub1 cols))])
        (display (if (set-member? living (posn r i)) #\# #\.) p))
    (get-output-string p)))

(define (ascii-draw living rows cols)
  (for ([i (in-range (sub1 rows))])
    (display (ascii-row living i cols))
    (newline)))

;; Image
(define square-size 10)

(define (image-draw living rows cols)
  (let* ([scene (empty-scene (* square-size cols) (* square-size rows))]
         [live-img (square square-size "solid" "black")]
         [half-square-size (/ square-size 2)]
         [place (λ (x) (+ (* square-size x) half-square-size))]
         [f (λ (accum e)
              (place-image live-img
                           (place (posn-col e))
                           (place (posn-row e))
                           accum))])
    (stream-fold f scene living)))

;; Repl testing
(define s (apply set (map list->posn '((1 2) (1 1) (2 2) (1 4) (1 5)))))

(define board (call-with-input-file "board.txt" load-board))