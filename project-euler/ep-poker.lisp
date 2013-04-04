(ql:quickload 'cl-ppcre)
(defclass suit()
  ((cards :accessor suit-cards :initform ())
   (player :accessor player-index :initform 0)
   (hc :accessor highest-card)
   (pairs :accessor suit-pairs :initform ())
   (straight :accessor straight-p :initform nil)
   (flush :accessor flush-p :initform nil)
   (3kind :accessor three-kind-p :initform nil)
   (4kind :accessor four-kind-p :initform nil)
   (royal :accessor royal-flush-p :initform nil)
   (full :accessor full-house-p :initform nil)
   (score :accessor player-score :initform 0)))

(defun get-card(card)
  (let* ((val (char card 0))
	 (typ (char card 1))
	 (value (cond 
		  ((eql val #\T) 10)
		  ((eql val #\J) 11)
		  ((eql val #\Q) 12)
		  ((eql val #\K) 13)
		  ((eql val #\A) 14)
		  (t (digit-char-p val))))
	 (type (cond 
		 ((eql typ #\S) 'SPADE)
		 ((eql typ #\H) 'HEART)
		 ((eql typ #\C) 'CLUB)
		 ((eql typ #\D) 'DIAMOND))))
    (cons value type)))

(defun sort-cards(suit)
  (let ((cards (suit-cards suit)))
    (setf (suit-cards suit) 
	  (sort cards #'(lambda(x y) (< (first x) (first y)))))))

(defun create-suit(cards)
  (let ((suit (make-instance 'suit)))
    (loop for i in cards do
	 (push (get-card i) (suit-cards suit)))
    (sort-cards suit)
    suit))

(defun consecutive-p(vals)
  (eq (1- (length vals)) 
      (apply #'+ (loop for (a b) on vals by #'cdr when (and a b) collect (- b a)))))

(defun duplicate-count(list)
  (remove-duplicates 
   (loop for i in list collect 
	(cons i (count i list))) 
   :test #'equal))

(defun analyse-suit(suit)
  (let* ((vals (loop for i in (suit-cards suit) collect (car i)))
	 (types (loop for i in (suit-cards suit) collect (cdr i)))
	 (vals-count (duplicate-count vals)))
    (progn
      (setf (royal-flush-p suit) (equal vals (list 10 11 12 13 14)))
      (setf (flush-p suit) (eq 1 (length (remove-duplicates types))))
      (setf (highest-card suit) (apply #'max vals))
      (setf (straight-p suit) (and (consecutive-p vals) (eq 1 (length (remove-duplicates types)))))
      (loop for i in vals-count
	 as value = (car i) 
	 as count = (cdr i) do
	   (cond
	     ((eq 4 count) (setf (four-kind-p suit) T))
	     ((eq 3 count) (setf (three-kind-p suit) T))
	     ((eq 2 count) (push value (suit-pairs suit)))
	     (t nil)))
      "the full-house-p, when present, denotes the pair value forming the full house" 
      (setf (full-house-p suit) (and (three-kind-p suit) (suit-pairs suit)))))) 

(defun score-player(suit)
  (let ((score 0))
    (progn 
      (analyse-suit suit)
      (when (or (royal-flush-p suit) (straight-p suit) (flush-p suit)) (incf score 400))
      (when (or (three-kind-p suit) (full-house-p suit)) (incf score 300))
      (when (four-kind-p suit) (incf score 200))
      (loop for i in (suit-pairs suit) do (incf score (* 10 i)))
      (incf score (* 5 (highest-card suit)))
      (setf (player-score suit) score))))

(defun player1-won-p(game)
  (let ((score1 (score-player (create-suit (subseq game 0 5))))
	(score2 (score-player (create-suit (subseq game 5 10)))))
    (list (> score1 score2) score1 score2)))

(defun lets-play(filename)
  (let ((file (open filename))
	(player1-count 0))
    (loop for line = (read-line file nil) while line do
	 (when (first (player1-won-p (cl-ppcre:split " " line))) (incf player1-count)))
    player1-count))
