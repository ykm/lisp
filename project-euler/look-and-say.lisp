(load "~/code/lisp/l99.lisp")
(load "~/code/lisp/utilities.lisp")

(defun look-and-say(num)
  (loop for i in (runlength-encode (digits num))
     for e = 0 then (+ e 2)
     summing (* (expt 10 e) (+ (* 10 (cdr i)) (car i)))))

(defun look-and-say-index(index)
  (let ((num 1))
    (dotimes (i index)
      (setq num (look-and-say num)))
    num))

(defun count-digits(num digit)
  (loop for i = num then (floor i 10)
     for d = (mod i 10)
     while (> i 0)
     when (eql d digit)
     count i))

(defun solve-lns(value)
  (let ((val (look-and-say-index value)))
    (values
     (count-digits val 1)
     (count-digits val 2)
     (count-digits val 3))))

