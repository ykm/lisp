(ql:quickload 'cl-ppcre)

(defun primep(num)
  (when (oddp num)
    (loop for i from 3 to (1+ (isqrt num)) by 2
       never (zerop (mod num i)))))

(defun next-prime(num)
  (loop for i from (1+ num) 
     when (primep i) return i))

(defun fact(n)
  (if (zerop n) 1
      (* n (fact (1- n)))))

(defun equal-all(val &rest args)
  (reduce #'(lambda(x y) (and x y)) 
	  (mapcar #'(lambda(x) (equal x val)) args)))

(defun combinations(n r)
  (/ (fact n) (* (fact r) (fact (- n r)))))

(defun digits(num &optional (base 10))
  (labels ((rec(n b)
	     (multiple-value-bind (q r) (floor n b)
	       (if (and (zerop q) (zerop r)) nil
		   (cons r (rec q b))))))
    (rec num base)))

(defun reverse-digits(digit &optional (base 10))
  (labels ((rec (d b p)
	     (if (null d) 0
		 (+ (* (car d) p) 
		    (rec (cdr d) b (* p b))))))
    (rec digit base 1)))

(defun sorted-digits(number)
  (sort (digits number) #'<))

(defun hcf(n d)
  (loop for i from n downto 1 
     when (and (zerop (mod n i)) (zerop (mod d i)))
     return i))

(defun solve-ep-72()
  (time (loop for d from 1000000 downto 1 summing
	     (loop for n from d downto 1 
		when (and (< n d) (eq 1 (hcf n d))) counting n))))
  
(defun sum-digit-squares(start)
  (let ((total (apply #'+ 
		      (mapcar #'(lambda(x) (expt x 2)) 
			      (digits start)))))
    (if (or (eql total 1) (eql total 89)) total
	(sum-digit-squares total))))

(defun solve-ep-92()
  (time (loop for i from 1 to 10000000
	   when (eq 89 (sum-digit-squares i)) 
	   counting i)))

(defun next-goodstein(val k)
  (if (eq k 1) val
      (1- (reverse-digits (digits val k) (1+ k)))))

(defun get-goodstein-seq(num)
  (loop for k = 1 then (1+ k) 
     for i = num then (next-goodstein i k)
     while  (> i 0) count i))

(defun digit-permutations(number)
  (let* ((tmp (digits number))
	 (len (1- (length tmp))))
    (loop for i from 0 to len 
       for lst = tmp then (cons (car (last lst)) (butlast lst))
       collect (reverse-digits lst))))

(defun circular-prime-p(num)
  (loop for i in (digit-permutations num) always (primep i)))

(defun solve-ep-37()
  (loop for i = 2 then (next-prime i) 
     while (< i 1000000)
     when (circular-prime-p i) count i))

(defun is-pandigital-p(dgts &optional (start 1))
  (let ((end (- (length dgts) start)))
    (loop for i from start to end always (find i dgts))))

(defun solve(a b c)
  (let ((delta (sqrt (- (* b b) (* 4 a c))))
	(den (* 2 a))
	(neg (* -1 b)))
    (values (/ (+ neg delta) den)
	    (/ (- neg delta) den))))

(defun is-pentagonal(num)
  (multiple-value-bind (q r) 
      (floor (solve 3 -1 (* -2 num)))
    (declare (ignore q))
    (zerop r)))

(defun is-hexagonal(num)
  (multiple-value-bind (q r)
      (solve 2 -1 (* -1 num))
    (declare (ignore q))
    (zerop r)))

;; (loop for i from 286 
;;    as tri = (next-triangle i) 
;;    when (and (is-pentagonal tri) (is-hexagonal tri))
;;    return tri)
;; 29395278

(defun sort-push(lst n &key (test #'<) (key #'identity))
  (labels ((rec (lst n acc)
	     (if (null lst)
		 (reverse (cons n acc))
		 (if (funcall test (funcall key n) (funcall key (car lst)))
		     (append (reverse (cons n acc)) lst)
		     (rec (cdr lst) n (cons (car lst) acc))))))
    (rec lst n ())))


;; Integer to bytes converter and complement function
(defun bytes-to-integer(bytes)
  (let ((int 0))
    (loop for i = 0 then (+ i 8) 
       for j in bytes do
	 (setf (ldb (byte 8 i) int) j))
    int))

(defun integer-to-bytes(int)
  (loop for i = 0 then (+ i 8)
     as val = (ldb (byte 8 i) int)
     while (not (zerop val)) collect val))

(defun relative-primes(val)
  (loop for i from 1 to val 
     when (equal 1 (gcd i val))
     collect i))

(defmacro with-relative-primes((n var) &body body)
  `(loop for ,var from ,n downto 1 
      when (eq (gcd ,var ,n) 1)
      do ,@body))

(defun totient-ratio(num)
  (/ num (relative-primes num)))

(defun max-totient(count)
  (let ((val 0)
	(tmp 0))
    (loop for i from 1 to count
       for ratio = (totient-ratio i) 
       when (> ratio tmp)
       do (progn 
	    (setq val i)
	    (setq tmp ratio)))
    (values val tmp)))

(defun permutations(lst)
  (cond 
    ((null lst) nil)
    ((null (cdr lst)) (list lst))
    (t (loop for element in lst
	  append (mapcar #'(lambda(v) (cons element v))
			 (permutations (remove element lst)))))))

(defun read-matrix(filename)
  (with-open-file (file filename)
    (loop for line = (read-line file nil) 
       while line 
       collect (mapcar #'parse-integer (cl-ppcre:split "," line)))))

(defun maximum(lst)
  (cond
    ((null lst) (error "maximum on a empty list"))
    ((null (rest lst)) (car lst))
    (T (let ((val (maximum (rest lst))))
	 (if (> (first lst) val)
	     (first lst)
	     val)))))

(defmacro doprimes((var start condition) &body body)
  `(labels ((is-prime(val)
	      (loop for i from 2 to (isqrt val) 
		 never (zerop (mod val i)))))
     (loop for ,var from ,start while ,condition when (is-prime ,var) do
	  ,@body)))

(defun spiral-diagonal(side)
  (let ((start 1))
    (loop for i from 2 to (1- side) by 2 
       append (loop for j from 1 to 4 
		 collect (incf start i)))))

(defun spiral-prime-ratio(ratio)
  "pe-58 solution"
  (loop for i from 9 by 2
     for diagonal = (spiral-diagonal i)
     for prime-count = (count-if #'primep diagonal)
     when (< (/ prime-count (length diagonal)) ratio)
     do (return i)))
