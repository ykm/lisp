(load "~/code/lisp/utilities.lisp")

(defun get-tuple(num)
  (let ((container ()))
    (loop for i from 1 to 9
       while (< (length container) 9)
       do (setq container (append (digits (* i num)) container)))
    container))

(time (loop for i from 9999 downto 1
	 for tuple = (get-tuple i) 
	 when (is-pandigital-p tuple)
	 maximize (reverse-digits tuple)))
