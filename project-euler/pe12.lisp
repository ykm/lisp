(defun pe12(num)
  (labels ((factors-count(val)
	     (1+ (loop for i from 1 to (/ val 2) 
		    when (zerop (mod val i))
		    count i))))
    (loop for i from 1
       for tri = i then (+ i tri)
       when (>= (factors-count tri) num)
       return (cons i tri))))
