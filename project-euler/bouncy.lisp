(defun digits(num)
  (labels ((rec (num)
	     (multiple-value-bind (q r) (floor num 10)
	       (if (and (zerop q) (zerop r)) nil
		   (cons r (rec q))))))
    (nreverse (rec num))))

(defun analyse(num)
  (labels ((rec (lst func)
	     (if (null lst) T
		 (and (funcall func (car (car lst)) 
			       (cdr (car lst)))
		      (rec (cdr lst) func)))))
    (let* ((d (digits num))
	   (pairs (loop for (a b) on d by #'cdr 
		     when (and a b)
		     collect (cons a b))))
      (values (rec pairs #'<=)
	      (rec pairs #'>=)))))
