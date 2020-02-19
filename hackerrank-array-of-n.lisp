(defun array-of-n (value)
  (if (> value 0)
    `(,@(array-of-n (- value 1)) ,value)
    `()
  )
)

(defun read-list ()
    (let ((n (read *standard-input* nil)))
        (if (null n)
            nil
            (cons n (read-list)))))

(let ((n (read *standard-input* nil)))
  (array-of-n n)
)
