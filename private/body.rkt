#lang racket/base
(require racket/class
         racket/draw
         racket/math
         pict
         "with-state.rkt"
         "pen+brush.rkt")

(provide torso
         legs
         body)

(define shirt-path
  (let ([p (new dc-path%)])
    (send p move-to 20 15)
    (send p curve-to 20 5  25 0  35 0)
    (send p line-to 85 0)
    (send p curve-to 95 0  100 5  100 15)
    (send p line-to 100 115)
    (send p curve-to 100 120  100 120  95 120)
    (send p line-to 25 120)
    (send p curve-to 20 120  20 120  20 115)
    (send p close)
    p))

(define-values (long-sleeve-path short-sleeve-path)
  (let ([mk (lambda (bottom)
              (let ([p (new dc-path%)])
                (send p move-to 0 30)
                (send p curve-to 0 8  8 0  15 0)
                (send p curve-to 22 0  30 8  30 30)
                (send p line-to 30 bottom)
                (send p line-to 0 bottom)
                (send p close)
                p))])
    (values (mk 90)
            (mk 40))))

(define cuff-path
  (let ([p (new dc-path%)])
    (send p rectangle 0 75 30 15)
    p))

(define arm-path
  (let ([p (new dc-path%)])
    (send p move-to 2 90)
    (send p curve-to 2 100  8 110  15 110)
    (send p curve-to 22 110  28 100  28 90)
    (send p line-to 28 20)
    (send p line-to 2 20)
    (send p close)
    p))

(define collar-path
  (let ([p (new dc-path%)])
    (send p move-to 40 0)
    (send p line-to 40 25)
    (send p line-to 55 18)
    (send p close)
    (send p move-to 80 0)
    (send p line-to 80 25)
    (send p line-to 65 18)
    (send p close)
    p))

(define neck-path
  (let ([p (new dc-path%)])
    (send p move-to 40 0)
    (send p line-to 55 18)
    (send p line-to 65 18)
    (send p line-to 80 0)
    (send p close)
    p))

(define pants-path
  (let ([p (new dc-path%)])
    (send p move-to 5 0)
    (send p line-to 65 0)
    (send p curve-to 70 0  70 0  70 5)
    (send p line-to 70 120)
    (send p line-to 40 120)
    (send p line-to 40 40)
    (send p arc 30 30 10 10 0 pi)
    (send p line-to 30 120)
    (send p line-to 0 120)
    (send p line-to 0 5)
    (send p curve-to 0 0  0 0  5 0)
    (send p close)
    p))

(define shoe-path
  (let ([p (new dc-path%)])
    (send p move-to -3 122)
    (send p arc -3 104 36 36 0 pi)
    (send p close)
    p))

(define paisley-path
  (let ([p (new dc-path%)])
    (send p move-to 0 50)
    (send p arc -10 25 20 25 (* -1/2 pi) (* 1/2 pi))
    (send p arc -15 0 30 25 (* -1/2 pi) (* 1/2 pi) #f)
    (send p arc -15 0 30 30 (* 1/2 pi) (* 1/4 pi) #f)
    (define d (* (sin (* 1/4 pi)) 15))
    (send p curve-to (+ d 7) (+ (- 15 d) 7)  17 20  17 30)
    (send p curve-to 17 40  10 50  0 50)
    (send p close)
    (send p translate 0 -12)
    p))

(define paisley-inner-path
  (let ([p (new dc-path%)])
    (send p move-to 14 30)
    (send p curve-to 14 30  5 20  1 20)
    (send p arc -10 4 22 16 (* -1/2 pi) (* 1/2 pi) #f)
    (send p arc -10 4 22 16 (* 1/2 pi) (* 1/4 pi) #f)
    (define dx (+ 0 (* (sin (* 1/4 pi)) 11)))
    (define dy (- 12 (* (sin (* 1/4 pi)) 8)))
    (send p curve-to (+ dx 5) (+ dy 5) 14 20  14 30)
    (send p close)
    (send p translate 0 -12)
    p))

(define paisley-dot-path
  (let ([p (new dc-path%)])
    (send p ellipse -5 -5 10 10)
    p))

(define paisley-dot2-path
  (let ([p (new dc-path%)])
    (send p ellipse -2 -2 10 10)
    p))

(define flower-path
  (let ([p (new dc-path%)])
    (send p ellipse -2 -2 20 10)

    ;; NW petal
    (send p move-to -2 -4)
    (send p curve-to -4 -20 -4 -10 -15 -20)
    (send p curve-to -12 -8 -12 -8 -2 -4)
    (send p close)

    ;; N petal
    (send p move-to 9 -3)
    (send p curve-to 18 -5 18 -10 9 -18)
    (send p curve-to 0 -10 0 -5 8 -5)
    (send p close)

    ;; NE petal
    (send p move-to 20 -4)
    (send p curve-to 24 -20 30 -30 35 -20)
    (send p curve-to 28 -2 24 -2 20 -4)
    (send p close)

    ;; E petal
    (send p move-to 20 2)
    (send p curve-to 27 12 27 18 35 2)
    (send p close)

    ;; W petal
    (send p move-to -4 2)
    (send p curve-to -8 12 -8 18 -16 2)
    (send p close)

    ;; S petal
    (send p move-to -2 10)
    (send p curve-to -4 20 -4 10 -15 20)
    (send p curve-to -5 30  15 30  20 20)
    (send p line-to 16 10)
    (send p close)

    (send p translate -8 -4)

    p))

(define (path->region path [o-x 0] [o-y 0] [dx 0] [dy 0] [rot 0])
  (let ([r (new region%)]
        [p (new dc-path%)])
    (send p append path)
    (send p translate dx dy)
    (send p rotate rot)
    (send p translate (- dx) (- dy))
    (send r set-path p (+ o-x dx) (+ o-y dy) 'winding)
    r))

(define (region-union . rs)
  (let ([n-r (new region%)])
    (for ([r (in-list rs)])
      (send n-r union r))
    n-r))

(define (call-with-clipping-region dc r draw)
  (define old-region (send dc get-clipping-region))
  (let ([r (cond
             [(not old-region) r]
             [else
              (define new-r (make-object region%))
              (define-values (l t w h) (send old-region get-bounding-box))
              (send new-r set-rectangle l t w h)
              (send new-r intersect r)
              new-r])])
    (send dc set-clipping-region r))

  (draw dc)

  (send dc set-clipping-region old-region))

(define (paisley-region dc)
  (send dc set-brush blue-brush)
  (send dc draw-rectangle -10 0 210 200)

  (for ([x (in-range 20 120 30)]
        [i (in-naturals)]
        #:when #t
        [y (in-range 20 150 30)]
        [j (in-naturals)])
    (with-state dc x y
      (send dc scale 0.5 0.5)
      (when (odd? (+ i j))
        (send dc rotate pi))
      (send dc set-pen no-pen)
      (send dc set-brush bluer-brush)
      (send dc draw-path paisley-path 0 0)
      (send dc set-brush white-brush)
      (send dc draw-path paisley-inner-path 0 0)
      (send dc set-brush red-brush)
      (send dc draw-path paisley-dot-path 0 0)
      (send dc set-brush green-brush)
      (send dc draw-path paisley-dot2-path 0 0))))

(define (hawaiian-region dc)
  (send dc set-brush red-brush)
  (send dc draw-rectangle -10 0 210 200)

  (send dc set-pen no-pen)
  (send dc set-brush white-brush)
  (for ([x (in-range 20 120 40)]
        [i (in-naturals)]
        #:when #t
        [y (in-range (+ 10 (if (odd? i) 15 0)) 160 40)]
        [j (in-naturals)])
    (with-state dc x y
      (send dc scale 0.5 0.5)
      (when (odd? (+ i j))
        (send dc rotate pi))
      (send dc draw-path flower-path 0 0))))

(define ((make-striped-region shirt-brush) dc)
  (send dc set-pen no-pen)
  (send dc set-brush shirt-brush)
  (send dc draw-rectangle 0 0 200 200)
  (send dc set-brush white-brush)
  (for ([y (in-range 20 150 30)])
    (send dc draw-rectangle 0 y 200 5)))

(define (make-tiedye-region color)
  (define tiedye-brush
    (make-brush #:gradient (case color
                             [(tiedye)
                              (make-object radial-gradient%
                                           120 120 5 100 200 200
                                           (list
                                            (list 0.0 (make-color 255 100 100))
                                            (list 0.05 (make-color 255 0 0))
                                            (list 0.25 (make-color 255 255 255))
                                            (list 0.5 (make-color 255 255 0))
                                            (list 1.0 (make-color 255 0 255))))]
                             [(tiedye2)
                              (make-object radial-gradient%
                                           0 60 5 100 250 250
                                           (list
                                            (list 0.0 (make-color 100 100 255))
                                            (list 0.05 (make-color 0 0 255))
                                            (list 0.25 (make-color 255 255 255))
                                            (list 0.5 (make-color 0 255 255))
                                            (list 0.57 (make-color 0 255 255))
                                            (list 1.0 (make-color 0 100 255))))]
                             [(tiedye3)
                              (make-object radial-gradient%
                                           50 100 5 100 200 200
                                           (list
                                            (list 0.0 (make-color 255 100 0))
                                            (list 0.25 (make-color 200 200 50))
                                            (list 0.5 (make-color 255 100 0))
                                            (list 1.0 (make-color 100 255 50))))]
                             [(tiedye4)
                              (make-object linear-gradient%
                                           0 0 100 130
                                           (list
                                            (list 0.0 (make-color 255 100 0))
                                            (list 0.3 (make-color 255 255 0))
                                            (list 0.45 (make-color 255 255 255))
                                            (list 0.6 (make-color 255 100 100))
                                            (list 0.7 (make-color 255 0 100))
                                            (list 1.0 (make-color 0 255 100))))]
                             [else (error "bad tiedye color")])))
  (lambda (dc)
    (send dc set-brush tiedye-brush)
    (send dc draw-rectangle 0 0 200 200)))

(define (make-dress-path #:bend [bend 0])
  (let ([p (new dc-path%)])
    (define (b n) (- n bend))
    (define (b/2 n) (- n (/ bend 2)))
    (send p move-to (b 0) 150)
    (send p curve-to (b 8) 150 (b/2 10) 100 (b/2 15) 75)
    (send p curve-to 8 0 30 0 40 0)
    (send p line-to 60 0)
    (send p curve-to 70 0 90 0 85 75)
    (send p curve-to 95 150 (b 92) 150 (b 100) 150)
    (send p close)
    (send p scale 1.2 1)
    p))

(define dress-path (make-dress-path))

(define (torso #:sleeves [sleeves 'long]
               #:shirt-color [shirt-color 'pale]
               #:collar? [collar? #f]
               #:polo? [polo? #f]
               #:dress? [dress? #f]
               #:undershirt? [undershirt? #f]
               #:t-shirt? [t-shirt? #f]
               #:zipper? [zipper? #f]
               #:cardigan? [cardigan? #f]
               #:logo [logo #f])
  (define shirt-region
    (case shirt-color
      [(paisley) paisley-region]
      [(hawaiian) hawaiian-region]
      [(tiedye tiedye2 tiedye3 tiedye4) (make-tiedye-region shirt-color)]
      [(olive-striped) (make-striped-region olive-brush)]
      [(black-striped) (make-striped-region black-brush)]
      [else #f]))
  (dc (lambda (dc x y)
        (with-state dc x y
          (define body-pen (case shirt-color
                             [(black dark-blue) gray-thin-pen]
                             [else almost-black-pen]))
          (define body-brush cartoon-brush)
          (define shirt-brush
            (case shirt-color
              [(white) white-brush]
              [(pale) blue-brush]
              [(blue) med-blue-brush]
              [(dark-blue) dark-blue-brush]
              [(green) green-brush]
              [(red) red-brush]
              [(maroon) maroon-brush]
              [(orange) orange-brush]
              [(yellow) yellow-brush]
              [(olive) olive-brush]
              [(black) black-brush]
              [(dark-blue) dark-blue-brush]
              [(gray) gray-brush]
              [(paisley hawaiian olive-striped black-striped) no-brush]
              [(tiedye tiedye2 tiedye3 tiedye4) no-brush]
              [else (error "bad shirt color")]))

          (define sleeve-path
            (case sleeves
              [(short) short-sleeve-path]
              [(long) long-sleeve-path]
              [else (error "bad sleeves")]))
          (define rotate-amt 0.07)

          (define torso-path (if dress? dress-path shirt-path))

          (define (draw-once #:arm-brush [arm-brush body-brush]
                             #:arm-pen [arm-pen body-pen]
                             #:torso-pen [torso-pen body-pen]
                             #:sleeve-pen [sleeve-pen body-pen]
                             #:collar-pen [collar-pen body-pen])
            (send dc set-brush shirt-brush)

            (send dc set-pen torso-pen)
            (send dc draw-path torso-path 0 0)
            (send dc set-pen body-pen)

            (when t-shirt?
              (send dc set-brush body-brush)
              (send dc set-pen no-pen)
              (send dc draw-arc 40 -10 40 20 (- pi) 0)
              (send dc set-brush no-brush)
              (send dc set-pen collar-pen)
              (send dc draw-arc 35 -15 50 30 (- pi) 0))
            (when cardigan?
              (send dc set-brush white-brush)
              (send dc draw-rectangle 50 0 20 120)
              (send dc set-pen no-pen)
              (send dc set-brush body-brush)
              (send dc draw-rectangle 40 -8 40 15)
              (send dc draw-arc 40 -10 40 30 (- pi) 0)
              (send dc set-brush no-brush)
              (send dc set-pen collar-pen)
              (send dc draw-line 35 -13 35 5)
              (send dc draw-line 85 -13 85 5)
              (send dc draw-arc 35 -15 50 40 (- pi) 0)
              (send dc set-pen body-pen)
              (send dc draw-line 45 20 45 120)
              (send dc draw-line 75 20 75 120))
            (when zipper?
              (send dc draw-line 58 0 58 120)
              (send dc draw-line 62 0 62 120))

            (when collar?
              (define neck-brush (if undershirt?
                                     (if (eq? shirt-color 'black)
                                         black-brush
                                         white-brush)
                                     body-brush))

              (send dc set-pen no-pen)
              (send dc set-brush neck-brush)
              (send dc draw-path neck-path 0 0)
              (send dc set-brush shirt-brush)
              (send dc set-pen collar-pen)

              (case shirt-color
                [(paisley)
                 (send dc set-brush blue-brush)
                 (send dc draw-path collar-path 0 0)
                 (send dc set-brush shirt-brush)]
                [(hawaiian)
                 (send dc set-brush red-brush)
                 (send dc draw-path collar-path 0 0)
                 (send dc set-brush shirt-brush)]
                [(olive-striped)
                 (send dc set-brush olive-brush)
                 (send dc draw-path collar-path 0 0)
                 (send dc set-brush shirt-brush)]
                [(black-striped)
                 (send dc set-brush black-brush)
                 (send dc draw-path collar-path 0 0)
                 (send dc set-brush shirt-brush)]
                [else
                 (send dc draw-path collar-path 0 0)])

              (send dc set-brush no-brush)
              (send dc draw-rectangle 55 18 10 (if polo? 30 102))
              (for ([y (in-range 25 (if polo? 50 110) 15)])
                (send dc draw-ellipse 58 y 4 4)))

            (define (sleeve+arm)
              (send dc set-brush arm-brush)
              (send dc set-pen arm-pen)
              (send dc draw-path arm-path -15 0)
              (send dc set-brush shirt-brush)
              (send dc set-pen sleeve-pen)
              (send dc draw-path sleeve-path -15 0)
              (case (and (not t-shirt?) sleeves)
                [(long)
                 (send dc draw-path cuff-path -15 0)]))

            (with-state dc 28 3
              (send dc rotate (* pi (- rotate-amt)))
              (sleeve+arm))
            (with-state dc 92 3
              (send dc rotate (* pi rotate-amt))
              (sleeve+arm)))

          (define (paint-shirt paint-region)
            (call-with-clipping-region dc (path->region torso-path) paint-region)
            (draw-once #:collar-pen no-pen
                       #:sleeve-pen no-pen)
            (call-with-clipping-region dc              
                                       (region-union 
                                        (path->region sleeve-path 28 3 -15 0 (* pi (- rotate-amt)))
                                        (path->region sleeve-path 92 3 -15 0 (* pi rotate-amt)))
                                       paint-region)
            (draw-once #:arm-brush no-brush
                       #:arm-pen no-pen
                       #:torso-pen no-pen))
          (cond
            [shirt-region (paint-shirt shirt-region)]
            [else (draw-once)])))
      120 120))

(define (legs #:style [style 'khakis]
              #:shoe-color [shoe-color 'brown])
  (dc (lambda (dc x y)
        (with-state dc x y
          (send dc set-pen almost-black-pen)
          (send dc set-brush (case style
                               [(khakis) tan-brush]
                               [(jeans) denim-brush]
                               [(stockings) white-brush]
                               [else (error "bad pants style")]))
          (send dc draw-path pants-path)
          (send dc set-brush (case shoe-color
                               [(white) white-brush]
                               [(brown) brown-brush]
                               [(black) black-brush]
                               [else (error "bad shoe color")]))
          (send dc draw-path shoe-path 0 0)
          (send dc draw-path shoe-path 40 0)))
      70 120))

(define (vc-append/reverse sep x y)
  (define gx (ghost x))
  (pin-over (vc-append sep gx y)
            gx lt-find x))

(define (body h b l)
  (vc-append/reverse -5
                     (vc-append/reverse 5 h b)
                     l))

(module+ main
  (require slideshow
           "head.rkt")
  (define h (head #:color 'cartoon))
  (slide (hc-append
          gap-size
          (body (head #:color 'cartoon #:mouth 'light-brown-beard)
                (torso #:sleeves 'short #:t-shirt? #t #:shirt-color 'tiedye2) (legs))
          (body h (torso #:sleeves 'short #:t-shirt? #t #:shirt-color 'tiedye) (legs))
          (body h (torso #:sleeves 'short #:t-shirt? #t #:shirt-color 'tiedye3) (legs))
          (body h (torso #:sleeves 'short #:t-shirt? #t #:shirt-color 'tiedye4) (legs))
          (body h (torso #:sleeves 'short #:collar? #t #:polo? #t #:shirt-color 'paisley) (legs))
          (body h (torso #:shirt-color 'maroon #:sleeves 'short #:t-shirt? #t) (legs))
          (body h (torso #:sleeves 'long #:collar? #t #:undershirt? #t #:shirt-color 'blue) (legs #:style 'jeans))
          (body h (torso #:sleeves 'short #:collar? #t) (legs)))))

