(defun read-list ()
    (let ((n (read *standard-input* nil)))
        (if (null n)
            nil
            (cons n (read-list)))))

(defun ppcm (a b)
  (/ (* a b) (gcd a b))
)

(let
  ((args (rest (read-list))))
  (format t "~D" (reduce #'ppcm args))
)
