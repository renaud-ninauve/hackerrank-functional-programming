(defun read-list ()
    (let ((n (read *standard-input* nil)))
        (if (null n)
            nil
            (cons n (read-list)))))

(defun renaud-sort (list predicate)
  (let
    ((sorted `(,@list)))
    (sort sorted predicate)
  )
)

(defun renaud-gcd (a b)
  (if (= a b)
    a
    (destructuring-bind (min max) (renaud-sort `(,a ,b) '<)
      (renaud-gcd (- max min) min)
    )
  )
)

(defun split (str)
  (let
    ((pos (position #\Space str :test #'equal)))
    `(,(subseq str 0 pos) ,(subseq str (+ pos 1)))
  )
)

(format t "~D~%"
 (let
    ((args (read-list)))
    (renaud-gcd (first args) (second args))
  )
)
