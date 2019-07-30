#lang slideshow
(require "main.rkt")

(slide
 (hc-append
  gap-size

  (body (head #:hair 'swoop
              #:color 'cartoon)
        (torso #:sleeves 'short
               #:dress? #t
               #:shirt-color 'hawaiian
               #:collar? #t
               #:polo? #t)
        (legs #:style 'stockings
              #:shoe-color 'black))

  (body (head #:hair 'yellow-along-top
              #:color 'cartoon
              #:eyes 'rolled)
        (torso #:sleeves 'long
               #:shirt-color 'blue
               #:collar? #t
               #:undershirt? #t)
        (legs #:style 'jeans))

  (body (head #:hair 'black-parted
              #:color 'cartoon
              #:eyes 'angry
              #:glasses 'patch)
        (torso #:sleeves 'short
               #:t-shirt? #t
               #:shirt-color 'yellow)
        (legs))

  (body (head #:hair 'brown-short-spiky
              #:color 'cartoon
              #:eyes 'cry)
        (torso #:sleeves 'short
               #:shirt-color 'paisley
               #:collar? #t)
        (legs #:style 'jeans
              #:shoe-color 'white))

  (body (head #:hair 'brown-middle-parted
              #:color 'cartoon
              #:eyes 'worried)
        (torso #:sleeves 'short
               #:shirt-color 'olive-striped
               #:collar? #t
               #:polo? #t)
        (legs))

  (body (head #:hair 'brown-curly
              #:color 'cartoon)
        (torso #:sleeves 'long
               #:shirt-color 'maroon
               #:cardigan? #t)
        (legs #:style 'jeans))))




