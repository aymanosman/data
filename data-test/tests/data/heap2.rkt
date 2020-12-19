#lang racket
(require rackunit
         data/heap
         (submod data/heap test-util))

(struct node (name val))

(define (node<=? a b)
  (<= (node-val a) (node-val b)))

(test-equal? "heap-remove-eq!"
  (let ()
    (define h (make-heap node<=?))
    (define nds (map (match-lambda [(cons a b) (node a b)])
                     '((a . 0) (b . 0) (c . 0) (d . 0) (e . 0) (f . 0))))
    (heap-add-all! h nds)
    (vector-map node-name (heap->vector h)))
  #(b c d e f a))

