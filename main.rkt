#lang racket/gui

(require math/matrix)

(define WIDTH 600)
(define HEIGHT 400)

(define frame (new frame%
                   [label "Slope Field"]
                   [width WIDTH]
                   [height (+ 100 HEIGHT)]))

(define canvas (new canvas%
                    [parent frame]
                    [min-width WIDTH]
                    [min-height HEIGHT]))

(define panel (new horizontal-panel%
                   [parent frame]
                   [min-width WIDTH]
                   [min-height 100]))

(define matrix-panel (new group-box-panel%
                          [label "Matrix"]
                          [parent panel]
                          [min-width 100]
                          [min-height 80]
                          [vert-margin 10]))

(define matrix-top (new horizontal-panel%
                        [parent matrix-panel]
                        [min-width 100]
                        [min-height 40]))

(define matrix-top-left (new text-field%
                             [label ""]
                             [parent matrix-top]
                             [init-value "1"]))

(define matrix-top-right (new text-field%
                              [label ""]
                              [parent matrix-top]
                              [init-value "0"]))


(define matrix-bottom (new horizontal-panel%
                           [parent matrix-panel]
                           [min-width 100]
                           [min-height 40]))

(define matrix-bottom-left (new text-field%
                                [label ""]
                                [parent matrix-bottom]
                                [init-value "0"]))

(define matrix-bottom-right (new text-field%
                                 [label ""]
                                 [parent matrix-bottom]
                                 [init-value "1"]))

(define dc (send canvas get-dc))


(define (draw-slope x y dx dy dc)
  (if (= dx 0)
      (send dc draw-line
            (+ x (/ WIDTH 2))
            (- (/ HEIGHT 2) y -2)
            (+ x (/ WIDTH 2))
            (- (/ HEIGHT 2) y 2))
      (begin
        (send dc draw-line
              (+ (- x 2) (/ WIDTH 2))
              (- (/ HEIGHT 2) (- y (* (/ dy dx)) 2))
              (+ (+ x 2) (/ WIDTH 2))
              (-  (/ HEIGHT 2) (+ y (* (/ dy dx) 2)))))))

(define (draw-slopes-from-matrix matrix dc)
  (define vectors (map (λ (x) (col-matrix [(first x) (second x)]))
                       (cartesian-product (range (- (/ WIDTH 2))
                                                 (/ WIDTH 2) 10)
                                          (range (- (/ HEIGHT 2))
                                                 (/ HEIGHT 2) 10))))
  (define pairs (map matrix->list vectors))
  (define dpairs (map matrix->list  (map (λ (v) (matrix* matrix v)) vectors)))
  (for-each (λ (p d) (draw-slope (first p) (second p) (first d) (second d) dc))
            pairs dpairs))

(define a (matrix [[1 0] [0 1]]))

(define (main)
  (send frame show #t)
  (send dc set-pen "black" 2 'solid)
  (send dc draw-line (/ WIDTH 2) 0 (/ WIDTH 2) HEIGHT)
  (send dc draw-line 0 (/ HEIGHT 2) WIDTH (/ HEIGHT 2))
  (send dc set-pen "black" 1 'solid)
  (send dc set-font (make-object font% 6 'swiss)))


(define (use-matrix _ __)
  (define tl (string->number (send matrix-top-left get-value)))
  (define tr (string->number (send matrix-top-right get-value)))
  (define bl (string->number (send matrix-bottom-left get-value)))
  (define br (string->number (send matrix-bottom-right get-value)))
  (send dc clear)
  
  (if (and tl tr bl br)
      (begin
        (main)
        (draw-slopes-from-matrix
         (matrix [[tl tr] [bl br]])
         dc))
      (send dc draw-text
            "The numbers given are not numbers man"
            (/ WIDTH 2)
            (/ HEIGHT 2))))

(define vector-panel (new group-box-panel%
                          [label "Vector"]
                          [parent panel]
                          [min-width 80]
                          [min-height 80]
                          [vert-margin 10]))

(define vector-top-panel (new horizontal-panel%
                              [parent vector-panel]
                              [min-width 80]
                              [min-height 40]))

(define vector-top (new text-field%
                        [label ""]
                        [parent vector-top-panel]
                        [init-value "0"]))

(define vector-bottom-panel (new horizontal-panel%
                                 [parent vector-panel]
                                 [min-width 80]
                                 [min-height 40]))

(define vector-bottom (new text-field%
                           [label ""]
                           [parent vector-bottom-panel]
                           [init-value "0"]))

(define (trajectory vector matrix iterations total-iterations sign step dc)
  (define next (matrix+ (matrix-map (λ (a) (/ (* a sign) step)) (matrix* matrix vector)) vector))
  (define next-pair (matrix->list next))
  (define vector-pair (matrix->list vector))
  
  (send dc draw-line
        (+ (first vector-pair) (/ WIDTH 2))
        (- (/ HEIGHT 2) (second vector-pair))
        (+ (first next-pair) (/ WIDTH 2))
        (- (/ HEIGHT 2) (second next-pair)))
  (when (= 0 (remainder iterations step))
    (send dc draw-text
          (number->string (* sign (quotient (- total-iterations iterations -1) step)))
          (+ (first vector-pair) (/ WIDTH 2))
          (-  (/ HEIGHT 2) (second vector-pair))))

  (unless (= iterations 0)
    (trajectory next matrix (sub1 iterations) total-iterations sign step dc)))

(define (plot-vector _ __)
  (define top (string->number (send vector-top get-value)))
  (define bottom (string->number (send vector-bottom get-value)))
  (define tl (string->number (send matrix-top-left get-value)))
  (define tr (string->number (send matrix-top-right get-value)))
  (define bl (string->number (send matrix-bottom-left get-value)))
  (define br (string->number (send matrix-bottom-right get-value)))

  (send dc set-pen
        (make-object color%
          (random 1 180)
          (random 1 180)
          (random 1 180))
        2 'solid)
  
  (if (and top bottom tl tr bl br)
      (begin
        (trajectory
         (col-matrix [top bottom])
         (matrix [[tl tr] [bl br]])
         (send slider get-value)
         (send slider get-value)
         1
         (send slider-2 get-value)
         dc)
        (trajectory
         (col-matrix [top bottom])
         (matrix [[tl tr] [bl br]])
         (send slider get-value)
         (send slider get-value)
         -1
         (send slider-2 get-value)
         dc))
      (send dc draw-text
            "The numbers given are not numbers man"
            (/ WIDTH 2)
            (/ HEIGHT 2))))

(define controls (new group-box-panel%
                      [label "Controls"]
                      [parent panel]
                      [min-width 200]
                      [min-height 80]
                      [vert-margin 10]
                      [alignment (list 'left 'center)]))

(define matrix-button (new button%
                           [label "Plot Matrix"]
                           [parent controls]
                           [callback use-matrix]))

(define vector-button (new button%
                           [label "Plot Vector Trajectory"]
                           [parent controls]
                           [callback plot-vector]))

(define slider (new slider%
                    [label "Trajectory Steps"]
                    [parent controls]
                    [min-value 1]
                    [max-value 5000]
                    [init-value 100]
                    [style (list 'horizontal 'vertical-label)]))

(define slider-2 (new slider%
                      [label "Step Length (over 1)"]
                      [parent controls]
                      [min-value 1]
                      [max-value 1000]
                      [init-value 1]
                      [style (list 'horizontal 'vertical-label)]))  

(send dc set-font
      (make-object font%
        20 'swiss))

(main)

