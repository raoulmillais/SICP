(require racket/draw)

;;
;; picture
;;
(define (beside pic1 pic2 scale)
    (lambda (rect dc)
      (let ((left
            (make-rect
              (origin rect)
              (make-point (* (xcor (horiz rect)) scale) (ycor (horiz rect)))
              (vert rect)))
          (right
            (make-rect
              (make-point (* (xcor (horiz rect)) scale) (ycor (horiz rect)))
              (horiz rect)
              (vert rect))))
      (pic1 left dc)
      (pic2 right dc))))

(define (right-push pic count scale)
  (display count)
  (if (= count 0)
      pic
      (let ((smaller (right-push pic (- count 1) scale)))
       (beside pic smaller scale))))

(define (make-picture seg-list)
  (lambda (rect dc)
    (for ([seg seg-list])
         (let ((mapped-seg ((seg-map rect) seg)))
          (send dc draw-line (xcor (seg-start mapped-seg)) (ycor (seg-start mapped-seg))
               (xcor (seg-end mapped-seg)) (ycor (seg-end mapped-seg)))))))

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
       ((point-map rect) (seg-start seg))
       ((point-map rect) (seg-end seg)))))

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

(define (point-map rect)
  (lambda (point)
    (make-point
      (+
        (xcor (origin rect))
        (* (/ (xcor point) 1000) (- (xcor (horiz rect)) (xcor (origin rect)))))
      (+
        (ycor (origin rect))
        (* (/ (ycor point) 1000) (- (ycor (vert rect)) (ycor (origin rect))))))))


;;
;; draw
;;
(define target (make-bitmap 500 500))
(define dc (new bitmap-dc% [bitmap target]))

(define triangle-segments
  (list
    (make-segment (make-point 10 20) (make-point 400 300))
    (make-segment (make-point 400 300) (make-point 400 20))
    (make-segment (make-point 400 20) (make-point 10 20))))

(define triangle-pic (make-picture triangle-segments))

((right-push triangle-pic 2 0.5)
  (make-rect
    (make-point 0 0)
    (make-point 500 0)
    (make-point 0 500))
  dc)

(send target save-file "triangle.png" 'png)
