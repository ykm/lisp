(defpackage :cl-prime-sieve
  (:use :common-lisp)
  (:export :sum-of-squares
	   :get-prime-candidates-map
	   :atkin-sieve
	   :get-primes))

(in-package :cl-prime-sieve)

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

(defun get-atkin-prime-candidates-map(limit)
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
  (setf (aref candidates 2) T)
  (setf (aref candidates 3) T)
  candidates)

(defun eratosthene-sieve(limit)
  (let ((candidates (make-array (1+ limit) :initial-element T)))
    (progn
      (setf (aref candidates 0) nil)
      (setf (aref candidates 1) nil))
    (loop for i from 2 to limit 
       when (aref candidates i) 
       do (loop for j from (+ i i) to limit by i
	     when (aref candidates j)
	     do (setf (aref candidates j) nil)))
    candidates))

(defun get-primes(limit &key (generator :eratosthene))
  (let ((candidates 
	 (case generator
	   (:atkin (atkin-sieve (get-atkin-prime-candidates-map limit)))
	   (:eratosthene (eratosthene-sieve limit))
	   (otherwise (error "Invalid generator type provided, valid type (:atkin :eratosthene)")))))
    (loop for i from 0 to (1- (length candidates))
       when (aref candidates i)
       collect i)))
