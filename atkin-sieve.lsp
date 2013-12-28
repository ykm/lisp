(defpackage :cl-atkin-sieve
  (:use :common-lisp)
  (:export :sum-of-squares
	   :get-prime-candidates-map
	   :atkin-sieve
	   :get-primes))

(in-package :cl-atkin-sieve)

(defun sum-of-squares(x y &key (a 1) (b 1))
  (+ (* a x x) (* b y y)))

;; The macro has been specifically desgined to capture 
;; limit and candidates variables within the caller.
(defmacro get-sieve-function(a b mod-results)
  `(lambda(x y)
     (let* ((n (sum-of-squares x y :a ,a :b ,b))
	    (r (mod n 12)))
       (when (and (<= n limit) (not (null (member r ,mod-results))))
	 (setf (aref candidates n) 
	       (not (aref candidates n)))))))

(defun get-prime-candidates-map(limit)
  (let* ((lmt (isqrt limit))
	 (candidates (make-array (1+ limit) :initial-element nil))
	 (stage1 (get-sieve-function 4 1 '(1 5)))
	 (stage2 (get-sieve-function 3 1 '(7)))
	 (stage3 (get-sieve-function 3 -1 '(11))))
    (loop for x from 1 to lmt do
	 (loop for y from 1 to lmt do
	      (progn
		(funcall stage1 x y)
		(funcall stage2 x y)
		(when (> x y)
		  (funcall stage3 x y)))))
    candidates))

(defun atkin-sieve(candidates)
  (let ((len (length candidates)))
    (loop for i from 1 to (1- len)
       when (aref candidates i)
       do (loop for j from 1
	     for n = (* j i i)
	     while (< n len)
	     do (setf (aref candidates n) nil))))
  candidates)

(defun get-primes(limit)
  (let ((candidates (atkin-sieve (get-prime-candidates-map limit))))
    (loop for i from 0 to (1- (length candidates))
       when (aref candidates i)
       collect i)))

