(defstruct point x y z)

(defun reflect-along-y (p)
  (if(point-p p)
   (setf (point-y p) (- (point-y p)))
   (print "not a valid point struct")))

(defun distance(p1 p2)
  (if(and (point-p p1) (point-p p2))
    (let* ((x1 (point-x p1))
      (y1 (point-y p1))
      (z1 (point-z p1))
      (x2 (point-x p2))
      (y2 (point-y p2))
      (z2 (point-z p2))
      (print (sqrt (+ (* x1 x2) (+ (* y1 y2) (* z1 z2)))))))
    (print "invalid input provided")))

(defclass point-class()
  (x y z))

(defun setf-point(p a b c)
  (with-slots (x y z) p 
    (setf x a) 
    (setf y b)
    (setf z c)))

(defun distance-from-origin(p)
  (with-slots (x y z) p 
    (sqrt (+ (* x x) (+ (* y y) (* z z))))))

;; :allocation :class combination creates a static value
(defclass daft-point()
 ((x :accessor daft-x :initarg :x)
  (y :accessor daft-y :initform 0)
  (z :accessor daft-z :allocation :class))) 

  


