(defun find-subseqs(str count)
  (let ((len (length str)))
    (loop for i from 0 to len 
       until (> (+ count i) len)
       collect (subseq str i (+ count i)))))	

(defun number-permutations(num len)
  (labels ((rec (n d)
	     (if (< n 10) nil
		 (cons (mod n d) 
		       (rec (floor n 10) d)))))
    (nreverse (rec num (expt 10 len)))))
