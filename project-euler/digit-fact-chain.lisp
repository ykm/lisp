(defun fact(val)
  (if (zerop val) 1
      (* val (fact (1- val)))))

(defparameter factorials 
  (loop for i from 0 to 9 
     collect (fact i)))

(defun digits(num &optional (base 10))
  (labels ((rec(n b)
	     (multiple-value-bind (q r) (floor n b)
	       (if (and (zerop q) (zerop r)) nil
		   (cons r (rec q b))))))
    (rec num base)))


(defun next-digit-fact(num)
  (apply #'+ (mapcar #'(lambda(x) 
			 (nth x factorials)) 
		     (digits num))))

(defun digit-factorial-chain(num)
  (let ((chain ()))
    (loop for i = num then (next-digit-fact i)
       until (find i chain)
       do (push i chain))
    chain))
	 
(time (loop for i from 69 to 1000000 
	 when (eq 60 (length (digit-factorial-chain i)))
	 collect i))

;; Evaluation took:
;;   24.517 seconds of real time
;;   24.483330 seconds of total run time (24.336664 user, 0.146666 system)
;;   [ Run times consist of 1.073 seconds GC time, and 23.411 seconds non-GC time. ]
;;   99.86% CPU
;;   73,886,536,572 processor cycles
;;   4,947,746,032 bytes consed
