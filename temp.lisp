(defun double(x)
  "computes double of x"
  (* 2 x))	

(defun negate(x)
  "negates the value of x"
  (- x))

(defun fact (n)
  "computes factorial of x"
  (if (= n 1) 1
      (* n (fact (- n 1)))))

(defun triangle (x)
  "computes nth value in a triangle series"
  (if(= x 1) 	1
     (+ x (triangle (- x 1)))))

(defun power(B E)
  "returns B^E"
  (if (= E 0)
      1
      (* B (power B (- E 1)))))

(defun fibonacci(n)
  "computes last value in a fibonacci series"
  (if (or (= n 0) (= n 1))
      1
      (+ (fibonacci (- n 1)) (fibonacci (- n 2)))))

(defun binomial(n r)
  "computes binomial function value"
  (if (or (= n r) (= r 0))
      1
      (+ (binomial (- n 1)  (- r 1)) (binomial (- n 1) r))))

(defun sum_list(val)
  "Computes sum of all numbers in a list"
  (if(null val)
     0
     (+ (first val) (sum_list (rest val)))))

(defun list-nth(N L)
  "return nth element in the list"
  (if (null L)
      nil
      (if(= N 0)
	 (first L)
	 (list-nth (- N 1) (rest L)))))

(defun list-last(L)
  "return the last element in the list as a list"
  (if (null L)
      nil
      (if(null (rest L))
	 L
	 (list-last (rest L)))))

(defun list-member(val L)
  "find whether val is a member of L"
  (if (null L)
      nil
      (if (eq val (first L))
	  L
	  (list-member val (rest L)))))

(defun list-butlast(L)
  "returns all the members in the list except the last one"
  (if (null L)
      nil
      (if (null (rest L))
	  nil
	  (cons (first L) (list-butlast (rest L))))))

(defun list-intersection(L1 L2)
  "returns the intersection between 2 sets a.k.a lists in lisp"
  (cond
    ((null L1) nil)
    ((member (first L1) L2) (cons (first L1) (list-intersection (rest L1) L2)))
    (T (list-intersection (rest L1) L2))))

(defun list-union(L1 L2)
  "create a union of 2 sets"
  (cond
    ((null L1) L2)
    ((not (member (first L1) L2)) 
     (cons (first L1) (list-union (rest L1) L2)))
    (T (list-union (rest L1) L2))))

(defun list-diff(L1 L2)
  "calculates difference between 2 sets"
  (cond
    ((null L1) nil)
    ((not (member (first L1) L2)) (cons (first L1) (list-diff (rest L1) L2)))
    (T nil)))

(defun list-reverse(L)
  "returns a reverse representation of a list"
  (if (null L)
      nil
      (append (list-reverse (rest L)) (list (first L)))))

"auxillary function and accumulator variables for better performance"

(defun list-rev-imp(L)
  "imporved list reversal function"
  (list-rev-imp-aux L nil))

(defun list-rev-imp-aux(L A)
  "imporved list reversal auxillary function
	A is the accumulator variable"
  (if (null L)
      A
      (list-rev-imp-aux (rest L) (cons (first L) A))))

(defun fact-imp(N)
  "imporved factorial function"
  (fact-imp-aux N 1))

(defun fact-imp-aux(N A)
  "imporved factorial function"
  (if (= N 1)
      A
      (fact-imp-aux (- N 1) (* N A))))

(defun triangle-imp (N)
  "imporved triangle series value calculator"
  (triangle-imp-aux N 0))

(defun triangle-imp-aux (N A)
  "triangle-imp auxillary function"
  (if (= N 1)
      A
      (triangle-imp-aux (- N 1) (+ A N))))

(defun power-imp(B E)
  "improved power function"
  (power-imp-aux B E 1))

(defun power-imp-aux(B E A)
  "improved power auxillary function"
  (if (= E 0)
      A
      (power-imp-aux B (- E 1) (* A B))))

"Functions as Function argument"
(defun repeat-func(F N X)
  "Call double function N times"
  (if (= N 0)
      X
      (repeat-func F (- N 1) (funcall F X))))
"example usage: (repeat-func (function double) 5 8)"


"
Exercise: Define a function (apply-func-list L X) so that, given a list L of functions and an object X, apply-func-list applies the functions in L to X in reversed order. For example, the following expression
	(apply-func-list (list #'double #'list-length #'rest) '(1 2 3))
is equivalent to
	(double (list-length (rest '(1 2 3))))
"

(defun apply-func-list(L X)
  "Higher order function, run each function in L on X"
  (if (null L)
      X
      (funcall (first L) (apply-func-list (rest L) X))))
"(apply-func-list (butlast L) (funcall (last L) X))"

(defun apply-func-list-new (L X)
  "Apply a list L of functions to object X."
  (if (null L)
      X
      (funcall (first L) (apply-func-list (rest L) X))))

(defun all-match(L P)
  "returns a list containing all the even numbers in the
	supplied list"
  (all-match-aux L P nil))

(defun all-match-aux(L P A)
  "auxillary"
  (cond
    ((null L) A)
    ((funcall P (first L)) (all-match-aux (rest L) P (cons (first L) A)))
    (T (all-match-aux (rest L) P A))))

"
Exercise:
sol. 1: (all-match '((1 2) (1 2 3 4) (1 2 3 4 5) (1 2 3)) #'(lambda(x)(>= (list-length x) 3)))
sol. 2: (all-match '((1 2) (1 2 3 4) (1 2 3 4 5) (1 2 3)) #'(lambda(x)(evenp (list-length x))))
sol. 3: (all-match '((1 2) (1 2 3 4) (1 2 3 4 5) (1 2 3)) #'(lambda(x)(zerop (rem (list-length x) 3))))
"

(defun remove-short-lists(L X)
  "L= list of lists, X=Minimum list length required
	Aim: to remove all lists with length < X
	"
  (remove-short-lists-aux L X nil))

(defun remove-short-lists-aux(L X A)
  "Auxillary function"
  (cond
    ((null L) A)
    ((< (list-length (first L)) X) (remove-short-lists-aux (rest L) X A))
    (T (remove-short-lists-aux (rest L) X (cons (first L) A)))))

(defun remove-even(L)
  "remove all the even numbers in a list"
  (remove-even-aux L nil))

(defun remove-even-aux (L A)
  "Auxillary function"
  (cond
    ((null L) A)
    ((not (zerop (rem (first L) 2))) (remove-even-aux (rest L) (cons (first L) A)))
    (T (remove-even-aux (rest L) A))))

(defun remove-if(L P)
  "Remove an element in L if it satisfies predicate P"
  (remove-is-aux L P nil))

(defun remove-is-aux(L P A)
  "Auxillary"
  (cond
    ((null L) A)
    ((funcall P (first L)) (remove-is-aux (rest L) P (cons (first L) A)))
    (T (remove-is-aux (rest L) P A))))

"
eg Usage: (remove-if '(1 0 9 2 9 0 9 0 8 0 0) #'(lambda(x)(not (zerop x))))
"

(defun list-min-max(L)
  "return the minimum and maximum value in the list"
  (if (null L)
      nil
      (values (reduce #'min L) (reduce #'max L))))

(defun bin-tree-member-p (B E)
  "Test if E is an element in binary tree B."
  (if (bin-tree-leaf-p B)
      (equal E (bin-tree-leaf-element B))
      (or (equal E (bin-tree-node-element B))
	  (bin-tree-member-p (bin-tree-node-left B) E)
	  (bin-tree-member-p (bin-tree-node-right B) E))))
