(require racket/draw)

(define target-img (make-bitmap 500 500))
(define dc (new bitmap-dc% [bitmap target-img]))

(define (make-picture seg-list)
  (lambda (rect dc)
    (for ([seg seg-list])
         (send dc draw-line (xcor (seg-start seg)) (ycor (seg-start seg))
               (xcor (seg-end seg)) (ycor (seg-end seg))))))

(define triangle-segments
  (list
    (make-segment (make-point 10 20) (make-point 400 300))
    (make-segment (make-point 400 300) (make-point 400 20))
    (make-segment (make-point 400 20) (make-point 10 20))))

(define triangle-pic (make-picture triangle-segments))

(triangle-pic
  (make-rect
    (make-point 0 0)
    (make-point 500 0)
    (make-point 0 500))
  dc)

(send target-img save-file "triangle.png" 'png)

;;
;; segment
;;
(define (make-segment pStart pEnd)
  (lambda (pick)
    (cond ((= pick 0) pStart)
          ((= pick 1) pEnd))))

(define (seg-map rect)
  (lambda (seg)
    (make-segment
       ((cor-map rect) (seg-start seg))
       ((cor-map rect) (seg-end seg)))))

(define (seg-start seg)
  (seg 0))

(define (seg-end seg)
  (seg 1))

;;
;; rect
;;
(define (make-rect origin horiz vert)
  (lambda (pick)
    (cond ((= pick 0) origin)
          ((= pick 1) horiz)
          ((= pick 2) vert))))

(define (cor-map rect)
  (lambda (point)
    (make-point
      (+ (xcor (origin rect)) (* (xcor point) (xcor horiz)))
      (+ (ycor (origin rect)) (* (ycor point) (ycor vert))))))

(define (origin rect)
  (rect 0))

(define (horiz rect)
  (rect 1))

(define (vert rect)
  (rect 2))

;;
;; point
;;
(define (make-point x y)
  (lambda (pick)
         (cond ((= pick 0) x)
               ((= pick 1) y))))

(define (xcor point)
  (point 0))

(define (ycor point)
  (point 1))
