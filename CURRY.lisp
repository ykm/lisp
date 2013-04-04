
(defmacro defcurry (name parms &body body)
"Define a currying function. Uses the same syntax as DEFUN, except that
the PARMS must be symbols. Any partial application of NAME will result
in a function that is able to take in the future applications and
eventually return a result. Inspired by the currying capability of
ML."

(when (or (not (every #'symbolp parms))
          (find 'cl:&rest parms) (find 'cl:&key parms))
  (error "Lambda list for (DEFCURRY ~a ...) should contain no special
directives (&REST, &KEY) and no sublists." name))

(with-gensyms (curry-parms curry-lambda)
  `(progn (defun ,name (&rest ,curry-parms)
            (let ((,curry-lambda (clambda ,parms ,@body)))
              (if (endp ,curry-parms)
                  ,curry-lambda
		  (apply ,curry-lambda ,curry-parms))))
	  ;; Have to manually take care of documentation
          ,(when (stringp (car body)) `(setf (documentation ',name 'function)
                                             ,(car body)))
          ',name)))

(defmacro clambda (parms &body body)
  "A currying version of LAMBDA. If the result of a call to
CLAMBDA is partially applied to some arguments, the result is another
function which can be applied to the rest of the arguments, i.e.,
currying."
  
  (cond ((endp parms) `(progn ,@body))
        (t (let ((p1 (car parms))
                 (rest-p (not (single parms)))
                 (rest (gensym "REST-"))
                 (rec-c (gensym "REC-C-")))
             `(lambda (,p1 ,@(if rest-p `(&rest ,rest) nil))
                (let ((,rec-c (clambda ,(cdr parms) ,@body)))
                  ,(if rest-p
                       `(if (endp ,rest) ,rec-c (apply ,rec-c ,rest))
		       `,rec-c)))))))