(defun generate-sum(number)
  (let* ((dgt (digits number))
	 (rdgt (reverse dgt))
	 (rev (reverse-digits rdgt)))
    (values (+ number rev) (equal dgt rdgt))))

(defun check-number-aux(number iter)
  (when (> iter 0)
    (multiple-value-bind (sum is-pal) (generate-sum number)
      (if is-pal number
	  (check-number-aux sum (1- iter))))))

(defun check-number(number)
  (let ((final (if (is-num-palendrome number) (generate-sum number) number)))
    (check-number-aux final 50)))
