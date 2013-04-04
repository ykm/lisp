;;BST Constructors
(defun create-empty-bst()
	"Create an empty set"
        nil
)

(defun bst-empty-p(B)
	"Whether set is empty"
	(null S)
)

;;
(defun bst-insert(B E)
	"Insert element E into set S"
	(
		if(null B)
			(list E)
		(adjoin E B :test #'equal)
	)
)

(defun bst-remove(B E)
	"Remove element E from S"
	(remove E B :test #'equal)
)

(defun bst-member-p(B E)
	"whether E is a member of S"
	(
		if(bst-empty-p B)
			nil
		(member E B :test #'equal)
	)
)


