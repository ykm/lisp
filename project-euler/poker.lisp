(defclass suit()
  ((player :accessor player :initform nil)
   (cards :accessor suit-cards :initform ())
   (highest :accessor highest-card :initform nil)
   (pairs :accessor suit-pairs :initform ())
   (3kind :accessor three-kind-p :initform nil)
   (4kind :accessor four-kind-p :initform nil)
   (straight :accessor straight-p :initform nil)
   (flush :accessor flush-p :initform nil)
   (royal :accessor royal-flush-p :initform nil)))

(defun get-all-games(filename)
  (let ((file (open filename)))
    (loop for line = (read-line file nil)
       while line collect 
	 (cl-ppcre:split " " line))))

(defun get-card(card)
  (let* ((char1 (char card 0))
	 (char2 (char card 1))
	 (val (cond
		((eq char1 #\T) 10)
		((eq char1 #\J) 11)
		((eq char1 #\Q) 12)
		((eq char1 #\K) 13)
		((eq char1 #\A) 14)
		(T (digit-char-p char1))))
	 (type (cond 
		 ((eq char2 #\C) 'CLUB)
		 ((eq char2 #\D) 'DIAMOND)
		 ((eq char2 #\S) 'SPADE)
		 (T 'HEART))))
    (values val type)))

(defun get-suit(card)
  (let ((tmp (make-instance 'suits)))
    (setf (suit-cards tmp)
	  (loop for i in card collect
	       (get-card i)))
    tmp))

(defun sort-cards(cards)
  (sort cards #'(lambda(x y) (< (first x) (first y)))))

(defun is-flush-p(cards)
  (reduce #'(lambda (x y) (equal (second x) (second y))) 
	  (suit-cards cards)))