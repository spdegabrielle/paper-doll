#lang racket/base
(require racket/class
         racket/draw)

(provide with-state)

(define-syntax-rule (with-state dc x y body ...)
  (let ([p (send dc get-pen)]
        [b (send dc get-brush)]
        [t (send dc get-transformation)])
    (send dc translate x y)
    body ...
    (send dc set-pen p)
    (send dc set-brush b)
    (send dc set-transformation t)))

