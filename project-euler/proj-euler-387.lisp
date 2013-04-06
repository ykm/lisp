(defun digits(num &optional (base 10))
  (multiple-value-bind (q r) (floor num base)
    (if (and (zerop q) (zerop r)) nil
	(cons r (digits q base)))))

(defun is-harshad-number(num)
  (zerop (mod num (apply #'+ (digits num)))))

(defun right-truncatable-harshad(number)
  (loop for dgt = (digits number) then (cdr dgt)
     while dgt 
     always (is-harshad-number (reverse-digits dgt))))
