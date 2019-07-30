#lang racket/base
(require racket/class
         racket/draw
         racket/math
         pict
         "with-state.rkt"
         "pen+brush.rkt")

(provide head)

(define spike
  (let ([p (new dc-path%)])
    (send p move-to 0 20)
    (send p line-to 5 0)
    (send p line-to 10 20)
    (send p close)
    p))

(define short-spike
  (let ([p (new dc-path%)])
    (send p move-to 0 16)
    (send p line-to 5 5)
    (send p line-to 10 16)
    (send p close)
    p))

(define curly-q
  (let ([p (new dc-path%)])
    (send p move-to 0 20)
    (send p curve-to -10 20 -10 0 0 0)
    (send p curve-to 20 10 20 30 0 30)
    (send p curve-to -10 30 -20 15 -10 15)
    (send p curve-to -10 15 -5 25 0 25)
    (send p curve-to 10 30 10 10 0 5)
    (send p curve-to -3 5 -3 10 -3 10)
    (send p close)
    (send p rotate (* pi 1/4))
    (send p translate -10 0)
    p))

(define swoop
  (let ([p (new dc-path%)])
    (send p move-to 50 0)
    (send p curve-to -10 0 -10 60 -10 80)
    (send p curve-to -10 90 -20 80 -20 75)
    (send p curve-to -30 80 -20 90 -10 90)
    (send p curve-to 20 90 15 60 50 50)
    (send p close)
    (let ([p2 (new dc-path%)])
      (send p2 append p)
      (send p2 scale -1 1)
      (send p2 translate 100 0)
      (send p append p2))
    p))

(define parted
  (let ([p (new dc-path%)])
    (send p arc 25 8 50 10 0 pi)
    (send p curve-to 30 20 60 20 60 13)
    (send p curve-to 65 20 70 20 75 12)
    (send p close)
    p))

(define parted-low
  (let ([p (new dc-path%)])
    (send p arc 25 8 50 10 0 pi)
    (send p line-to 23 24)
    (send p curve-to 25 30 60 30 60 20)
    (send p curve-to 65 20 70 30 75 12)
    (send p close)
    p))

(define parted-left
  (let ([p (new dc-path%)])
    (send p arc 25 8 50 10 0 pi)
    (send p curve-to 30 15 35 15 40 13)
    (send p curve-to 40 20 70 20 75 12)
    (send p close)
    p))

(define middle-parted
  (let ([p (new dc-path%)])
    (send p arc 25 8 50 10 0 pi)
    (send p curve-to 30 20 40 20 50 13)
    (send p curve-to 60 20 70 20 75 12)
    (send p close)
    p))

(define puff
  (let ([p (new dc-path%)])
    (send p arc 25 8 50 10 0 pi)
    (send p curve-to 30 20 40 20 40 13)
    (send p curve-to 45 20 60 20 60 13)
    (send p curve-to 65 20 70 20 75 12)
    (send p close)
    p))

(define along-top
  (let ([p (new dc-path%)])
    (send p arc 25 8 50 10 0 pi)
    (send p arc 30 12 40 8 pi 0 #f)
    (send p close)
    p))

(define wide-along-top
  (let ([p (new dc-path%)])
    (define extra 0.3)
    (send p arc 17 8 66 16 (- extra) (+ pi extra))
    (send p arc 22 12 56 12 pi 0 #f)
    (send p close)
    p))

(define vee
  (let ([p (new dc-path%)])
    (send p arc 25 8 50 10 0 pi)
    (send p curve-to 30 15 45 15 50 20)
    (send p curve-to 55 15 70 15 75 12)
    (send p close)
    p))

(define chef-hat
  (let ([p (new dc-path%)])
    (send p move-to 0 35)
    (send p curve-to 25 45 25 45 50 35)
    (send p line-to 50 0)
    (send p curve-to 60 0 70 -30 50 -20)
    (send p curve-to 60 -40 20 -30 25 -20)
    (send p curve-to 30 -30 -10 -40 0 -20)
    (send p curve-to -20 -30 -10 0 0 0)
    (send p close)
    p))

(define chef-hat-line
  (let ([p (new dc-path%)])
    (send p move-to 0 15)
    (send p curve-to 25 25 25 25 50 15)
    p))
    
(define wizard-hat
  (let ([p (new dc-path%)])
    (send p move-to 50 -30)
    (send p line-to 20 20)
    (send p curve-to 40 30 60 30 80 20)
    (send p close)
    p))

(define star
  (let ([p (new dc-path%)])
    (define delta (/ pi 5))
    (define in 1/3)
    (send p move-to 1 0)
    (for/fold ([angle delta])
              ([point (in-range 5)])
      (send p line-to (* in (cos angle)) (* in (sin angle)))
      (define new-angle (+ angle delta))
      (send p line-to (cos new-angle) (sin new-angle))
      (+ new-angle delta))
    (send p close)
    (send p rotate (/ pi 10))
    (send p scale (/ 30 2) (/ 30 2))
    (send p translate 50 0)
    p))

(define spiral
  (let ([p (new dc-path%)])
    (send p move-to 0 0)
    (send p arc 0 -2 4 4 pi (* 2 pi))
    (send p arc -4 -4 8 8 0 pi)
    (send p arc -4 -6 12 12 pi (* 2 pi))
    (send p arc -8 -8 16 16 0 (* 5/4 pi))
    p))

(define grin
  (let ([p (new dc-path%)])
    (send p arc 0 -10 24 20 (* pi -7/8) (* pi -1/8))
    (send p arc 0 -5 24 10 (* pi -1/8) (* pi -7/8) #f)
    (send p close)
    p))

(define big-grin
  (let ([p (new dc-path%)])
    (send p append grin)
    (send p scale 1.6 1.6)
    p))

(define goatee
  (let ([p (new dc-path%)])
    (send p arc 0 0 40 20 0 pi)
    (send p line-to 20 40)
    (send p close)
    (send p ellipse 5 5 30 10)
    p))

(define beard1
  (let ([p (new dc-path%)])
    (send p arc 0 0 40 20 0 pi)
    (send p line-to 10 30)
    (send p line-to 30 30)
    (send p close)
    (send p ellipse 5 5 30 10)
    p))

(define beard
  (let ([p (new dc-path%)])
    (send p arc 0 0 40 20 0 pi)
    (send p arc -22 -13 84 40 pi 0)
    (send p close)
    (send p ellipse 5 5 30 10)
    p))

(define (make-eyelid from to)
  (let ([p (new dc-path%)])
    (send p arc 0 0 25 20 (* pi from) (* pi to) #f)
    (send p close) 
    p))

(define eyelid (make-eyelid 7/8 1/8))
(define high-eyelid (make-eyelid 6/8 2/8))
(define low-eyelid (make-eyelid 9/8 -1/8))
(define pos-eyelid (make-eyelid 1 1/8))
(define neg-eyelid (make-eyelid 7/8 0))
(define high-pos-eyelid (make-eyelid 7/8 2/8))
(define high-neg-eyelid (make-eyelid 6/8 1/8))
(define bottom-neg-eyelid (make-eyelid 14/8 9/8))
(define bottom-pos-eyelid (make-eyelid 15/8 10/8))

(define (head #:hair [hair #f]
              #:bear? [bear? #f]
              #:eyes [eyes 'normal]
              #:mouth [mouth 'normal]
              #:glasses [glasses #f]
              #:hair-brush [hair-brush red-brown-brush]
              #:bolder-features? [bolder-features? #f]
              #:color [face-color #f])
  (define face-brush (case face-color
                       [(cartoon) cartoon-brush]
                       [else (if bear? brown-brush peach-brush)]))
  (define face-pen dark-gray-pen)
  (dc (lambda (dc x y)
        (with-state dc
          x y
          (case hair
            [(swoop)
             (send dc set-pen no-pen)
             (send dc set-brush hair-brush)
             (send dc draw-path swoop 0 0)])
          (send dc set-pen face-pen)
          (send dc set-brush face-brush)
          (when bear?
            (send dc draw-ellipse 0 0 30 30)
            (send dc draw-ellipse 70 0 30 30))
          (send dc draw-ellipse 0 10 100 75)
          (send dc set-pen no-pen)
          (send dc set-brush white-brush)
          (send dc draw-ellipse 20 30 25 20)
          (send dc draw-ellipse 55 30 25 20)
          (case glasses
            [(glasses)
             (send dc set-pen yellow-pen)
             (send dc set-brush no-brush)
             (send dc draw-ellipse 17 27 31 26)
             (send dc draw-ellipse 52 27 31 26)
             (send dc draw-arc 37 35 30 30 (* 0.4 pi) (* 0.6 pi))]
            [(sunglasses)
             (send dc set-brush black-brush)
             (send dc draw-ellipse 19 29 27 22)
             (send dc draw-ellipse 54 29 27 22)]
            [(patch)
             (send dc set-brush white-brush)
             (send dc draw-ellipse 20 30 25 20)
             (send dc set-brush black-brush)
             (send dc draw-ellipse 54 29 27 22)])
          (send dc set-brush black-brush)
          (case eyes
            [(rolled)
             (send dc draw-ellipse 28 30 10 10)
             (send dc draw-ellipse 62 30 10 10)]
            [(normal angry worried wide kinda-wide tired cry)
             (unless (eq? glasses 'sunglasses)
               (send dc draw-ellipse 28 35 10 10)
               (unless (eq? glasses 'patch)
                 (send dc draw-ellipse 62 35 10 10))
               (send dc set-pen face-pen)
               (send dc set-brush face-brush)
               (case eyes
                 [(normal)
                  (send dc draw-path eyelid 20 30)
                  (unless (eq? glasses 'patch)
                    (send dc draw-path eyelid 55 30))]
                 [(tired)
                  (send dc draw-path low-eyelid 20 30)
                  (send dc draw-path low-eyelid 55 30)]
                 [(kinda-wide)
                  (send dc draw-path high-eyelid 20 30)
                  (send dc draw-path high-eyelid 55 30)]
                 [(angry)
                  (send dc draw-path neg-eyelid 20 30)
                  (unless (eq? glasses 'patch)
                    (send dc draw-path pos-eyelid 55 30))]
                 [(worried)
                  (send dc draw-path high-pos-eyelid 20 30)
                  (unless (eq? glasses 'patch)
                    (send dc draw-path high-neg-eyelid 55 30))]
                 [(cry)
                  (send dc draw-path high-pos-eyelid 20 30)
                  (unless (eq? glasses 'patch)
                    (send dc draw-path high-neg-eyelid 55 30))
                  (send dc draw-path bottom-pos-eyelid 20 30)
                  (unless (eq? glasses 'patch)
                    (send dc draw-path bottom-neg-eyelid 55 30))]))]
            [(dazed)
             (send dc set-pen black-pen)
             (send dc set-brush no-brush)
             (send dc draw-path spiral 33 40)
             (send dc draw-path spiral 67 40)])
          (case glasses
            [(patch)
             (send dc set-pen thick-black-pen)
             (send dc draw-line 100 50 25 15)]
            [(sunglasses)
             (send dc set-pen black-pen)
             (send dc set-brush no-brush)
             (send dc draw-arc 37 35 30 30 (* 1/4 pi) (* 3/4 pi))])
          (send dc set-pen no-pen)
          (send dc set-brush black-brush)
          (when bear?
            (send dc draw-ellipse 38 50 24 20))
          (send dc set-pen (if bolder-features? black-pen face-pen))
          (send dc set-brush no-brush)
          (unless bear?
            (send dc draw-arc 46 40 8 20 (* pi -7/8) (* pi -1/8)))
          (case mouth
            [(normal yellow-beard light-brown-beard)
             (unless bear?
               (send dc draw-arc 38 50 24 20 (* pi -7/8) (* pi -1/8)))]
            [(frown)
             (when bear?
               (send dc set-pen thick-black-pen))
             (send dc draw-arc 38 (if bear? 72 64) 24 20 (* pi 1/8) (* pi 7/8))]
            [(straight)
             (when bear?
               (send dc set-pen thick-black-pen))
             (send dc draw-line 38 (if bear? 73 65) 62 (if bear? 73 65))])
          (send dc set-pen no-pen)
          (case mouth
            [(hot)
             (send dc set-brush white-brush)
             (send dc draw-ellipse 38 65 24 10)]
            [(grin)
             (send dc set-brush white-brush)
             (send dc draw-path grin 38 (if bear? 70 60))]
            [(big-grin)
             (send dc set-brush white-brush)
             (send dc draw-path big-grin 31 (if bear? 65 60))]
            [(goatee)
             (send dc set-brush black-brush)
             (send dc draw-path goatee 30 70)]
            [(yellow-beard light-brown-beard)
             (send dc set-brush (case mouth
                                  [(yellow-beard) yellow-brush]
                                  [(light-brown-beard) khaki-brush]))
             (send dc draw-path beard 30 60)])
          (case 'bigger ; hair-size
            [(bigger)
             (send dc translate -10 0)
             (send dc scale 1.2 1.2)])
          (case hair
            [(spiky brown-spiky short-spiky brown-short-spiky)
             (case hair
               [(brown-spiky brown-short-spiky) (send dc set-brush (if bear? med-brown-brush brown-brush))]
               [else (send dc set-brush black-brush)])
             (for ([i (in-range 30 61 5)])
               (case hair
                 [(short-spiky brown-short-spiky)
                  (send dc draw-path short-spike i (+ -3 (/ (abs (- 45 i)) 8)))]
                 [else
                  (send dc draw-path spike i 0)]))]
            [(yellow-parted brown-parted brown-parted-low brown-parted-left black-parted 
                            yellow-middle-parted brown-middle-parted
                            yellow-puff brown-puff black-puff
                            yellow-curly brown-curly black-curly
                            yellow-along-top brown-along-top black-along-top
                            yellow-wide-along-top brown-wide-along-top black-wide-along-top
                            brown-vee black-vee)
             (send dc set-brush (case hair
                                  [(yellow-parted yellow-middle-parted yellow-puff yellow-along-top yellow-wide-along-top yellow-curly) gold-brush]
                                  [(brown-parted brown-parted-low brown-parted-left brown-middle-parted brown-puff brown-along-top brown-vee brown-curly brown-wide-along-top)
                                   (if bear? med-brown-brush brown-brush)]
                                  [(black-parted black-puff black-along-top black-vee black-curly black-wide-along-top) black-brush]))
             (case hair
               [(yellow-parted brown-parted black-parted)
                (send dc draw-path parted)]
               [(brown-parted-left)
                (send dc draw-path parted-left)]
               [(brown-parted-low)
                (send dc draw-path parted-low)]
               [(yellow-middle-parted brown-middle-parted)
                (send dc draw-path middle-parted)]
               [(yellow-puff brown-puff black-puff)
                (send dc draw-path puff)]
               [(yellow-curly brown-curly black-curly)
                (for ([i 5])
                  (send dc draw-ellipse (+ 27 (* 8 i)) 5 10 10))
                (for ([i 3])
                  (send dc draw-ellipse (- 27 (* 8 i)) (+ 7 (* i 4)) 10 10)
                  (send dc draw-ellipse (+ 61 (* 8 i)) (+ 7 (* i 4)) 10 10))
                (for ([i 8])
                  (send dc draw-ellipse (- 3 (* 1 i)) (+ 19 (* i 6)) 10 10)
                  (send dc draw-ellipse (+ 85 (* 1 i)) (+ 19 (* i 6)) 10 10))]
               [(yellow-along-top brown-along-top black-along-top)
                (send dc draw-path along-top)]
               [(yellow-wide-along-top brown-wide-along-top black-wide-along-top)
                (send dc draw-path wide-along-top)]
               [(brown-vee black-vee)
                (send dc draw-path vee)])]
            [(swoop)
             (send dc set-brush hair-brush)
             (send dc draw-ellipse 20 5 60 15)]
            [(curly-q)
             (send dc set-brush light-brown-brush)
             (send dc draw-path curly-q 50 5)]
            [(chef)
             (send dc set-brush white-brush)
             (send dc set-pen dark-gray-pen)
             (send dc draw-path chef-hat 25 -20)
             (send dc set-brush no-brush)
             (send dc draw-path chef-hat-line 25 -20)]
            [(wizard wizard+star)
             (send dc set-brush dark-blue-brush)
             (send dc draw-path wizard-hat 0 0)
             (case hair
               [(wizard+star)
                (send dc set-brush gold-brush)
                (send dc draw-path star 0 0)])])))
      100
      75))

(module+ main
  (require slideshow)
  (slide (head)))
