;;Constructors
"Create a binary tree from an "
(defun make-bt-leaf(L)
  "Create a binary tree leaf from the given value"
  (list L))

(defun make-bt-node(N B1 B2)
  "Create a node from the passed value"
  (list N B1 B2))	

;;Selectors
(defun get-bt-leaf-element(L)
  "Get the value at the leaf Element L in the binary tree"(
  first L))

(defun get-bt-node-element(N)
  (first N))

(defun get-bt-node-left(N)
  (second N))

(defun get-bt-node-right(N)
  (third N))


;;Recognizers
(defun is-bt-node(N)
  "Determines whether the given elemnt is a node"
  (> (list-length N) 1))

(defun is-bt-leaf(L)
  "determines whether the supplied element is leaf node"
  (= (list-length L) 1))

;;Binary tree search
(defun bin-tree-member-p (B E)
  "Test if E is an element in binary tree B."
  (if (bin-tree-leaf-p B)
    (equal E (bin-tree-leaf-element B))
    (or (equal E (bin-tree-node-element B)) 
      (bin-tree-member-p (bin-tree-node-left B) E) 
      (bin-tree-member-p (bin-tree-node-right B) E))))

