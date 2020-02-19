(defun read-list ()
    (let ((n (read *standard-input* nil)))
        (if (null n)
            nil
            (cons n (read-list)))))

(defun compute-length (acc value)
  (+ acc 1)
)

(let
  (
    (args (read-list))
  )
  (format t "~D~%" (reduce #'compute-length args :initial-value 0))
)
