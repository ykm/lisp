(defun my-last(lst)
  (if (null lst) lst
      (if (null (cdr lst)) (car lst)
	  (my-last (cdr lst)))))

(defun element-at(lst k)
  (if (null lst) lst
      (if (zerop k) (car lst)
	  (element-at (cdr lst) (1- k)))))

(defun my-reverse(lst)
  (labels ((rec(lst acc)
	     (if (null lst) acc
		 (rec (cdr lst) 
		      (push (car lst) acc)))))
    (rec lst ())))

(defun my-length(lst)
  (if (null lst) 0
      (+ 1 (my-length (cdr lst)))))

(defun my-butlast(lst)
  (if (null (cdr lst)) nil
      (cons (car lst) 
	    (my-butlast (cdr lst)))))

(defun duplicate(lst count)
  (labels ((dup(val count)
	     (if (zerop count) nil
		 (cons val (dup val (1- count)))))
	   (rec (lst count)
	     (if(null lst) nil
		(append (dup (car lst) count) (rec (cdr lst) count)))))
    (rec lst count)))

(defun ncons-sublist(lst val &optional (index 0))
  (let ((tmp (nth index lst)))
    (setf (nth index lst) (cons val tmp))
    lst))

(defun pack(lst)
  (labels ((rec (lst sub packed prev)
	     (if (null lst) (push sub packed)
		 (rec (cdr lst) 
		      (if (eql prev (car lst))
			  (push (car lst) sub)
			  (progn 
			    (push sub packed)
			    (cons (car lst) nil)))
		      packed (car lst)))))
    (nreverse (rec lst () () (car lst)))))

(defun runlength-encode(lst)
  (let ((tmp (pack lst)))
    (loop for i in tmp collect 
	 (cons (car i) (length i)))))

(defun modified-runlength(lst)
  (let ((packed (pack lst)))
    (loop for i in packed collect
	 (if (eql 1 (length i))
	     (car i)
	     (cons (length i) (car i))))))

(defun decode-runlength(runlength)
  (let ((lst ()))
    (loop for i in runlength do
	 (if (listp i)
	     (dotimes (j (car i))
	       (push (cdr i) lst))
	     (push i lst)))
    (nreverse lst)))

(defun duplicate(lst &optional (times 2))
  (let ((dup ()))
    (loop for i in lst do 
	 (dotimes (j times)
	   (push i dup)))
    (nreverse dup)))

(defun drop(lst &optional (index 1))
  (labels ((rec (lst nlst count)
	     (if (null lst) nlst
		 (rec (cdr lst)
		      (if (eql count index)
			  nlst
			  (push (car lst) nlst))
		      (if (eql count index) 1 (1+ count))))))
    (nreverse (rec lst () 1))))

(defun split(lst count)
  (labels ((rec (lst count splt)
	     (if (zerop count) (list (nreverse splt) lst)
		 (rec (cdr lst) (1- count) (push (car lst) splt)))))
    (rec lst count ())))

(defun slice(lst start end)
  (labels ((in-range (index)
	     (and (>= index start) (<= index end)))			  
	   (rec (lst index slc)
	     (if (> index end) slc
		 (rec (cdr lst) (1+ index) 
		      (if (in-range index)
			  (push (car lst) slc)
			  slc)))))
    (nreverse (rec lst 1 ()))))

(defun rotate-left(lst places)
  (let ((tmp (split lst (if (< places 0) 
			    (+ (length lst) places) places))))
    (append (second tmp) (first tmp))))

(defun remove-nth(lst n)
  (labels ((rec (lst tmp index)
	     (if (eql index n) (append (nreverse tmp) (cdr lst))
		 (rec (cdr lst) (push (car lst) tmp) (1+ index)))))
    (rec lst () 0)))

(defun insert(lst pos val)
  (let ((splt (split lst pos)))
    (append (first splt) (push val (second splt)))))

(defun range(start end)
  (if (> start end) nil
      (cons start (range (1+ start) end))))

(defun range(start end)
  (if (> start end) nil
      (cons start (range (1+ start) end))))

(defun random-select(lst count)
  (loop for i from 1 to count 
     collect (nth (random (length lst)) lst)))

(defun lotto-select(len count)
  (let ((tmp (range 1 len)))
    (random-select tmp count)))

(defun random-permutation(lst)
  (random-select lst (length lst)))

(defun lsort(lst)
  (sort lst #'< :key #'length))

(defun is-prime(num)
  (let ((limit (1+ (isqrt num))))
    (loop for i from 2 to limit never (zerop (mod num i)))))

(defun gcd-new(a b)
  (let ((k (max a b))
	(m (min a b)))
    (loop until (eq m 0) do
	 (let ((r (mod k m)))
	   (setq k m)
	   (setq m r)))
    k))

(defun is-coprime(a b)
  (eql 1 (gcd-new a b)))

(defun totient(val)
  (loop for i from 1 to (1- val) when (is-coprime i val) collect i))

(defun totient-phi(val)
  (length (totient val)))

(defun totient-term(p m)
  (expt (* p (1- p)) (1- m)))

(defun totient-imp(num)
  (let ((tmp (runlength-encode (prime-factors num))))
    (loop for i in tmp summing (totient-term (car i) (cdr i)))))

(defun totient-ratio(val)
  (/ val (totient-imp val)))

(defun permutations(lst)
  (cond
    ((null lst) nil)
    ((null (cdr lst)) (list lst))
    (t (loop for element in lst
	  append (mapcar #'(lambda(l) (cons element l))
			 (permutations (remove element lst)))))))

;; A scheme-like function defination in common lisp
(defmacro def(lst &body body)
  (let ((func-name (car lst))
	(args (cdr lst)))
    `(defun ,func-name ,args
       ,@body)))

(defun prime-factors(num)
  (let ((factors ()))
    (loop for d from 2 while (> num 1) 
       do (loop while (zerop (mod num d)) 
	     do (progn 
		  (push d factors)
		  (setq num (/ num d)))))
    factors))

(defun consecutive-prime-factors(count)
  (macrolet ((push-return (val)
	       `(progn
		  (push ,val tmp)
		  (when (eq (length tmp) count) (return-from outer)))))
    (let ((tmp ()))
      (loop named outer for i from 2
	 do (if (= count (length (remove-duplicates (prime-factors i))))
		(if (null tmp)
		    (push-return i)
		    (if (> (- i (car tmp)) 1)
			(setq tmp nil)
			(push-return i)))
		(setq tmp nil)))
       tmp)))

(defun combinations(n r)
  (labels ((fact(val)
	     (if (zerop val) 1
		 (* val (fact (1- val))))))
    (/ (fact n) (* (fact r) (fact (- n r))))))

(defun coprimes(val)
  (loop for i from 1 to val 
     when (eq 1 (gcd val i)) 
     collect i)) 

(defun coprimes-count(val)
  (loop for i from 1 to val 
     when (eq 1 (gcd val i)) 
     count i))

(defun tot-ratio(n)
  (/ n (coprimes-count n)))
