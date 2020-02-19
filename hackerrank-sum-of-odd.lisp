(defun read-list ()
    (let ((n (read *standard-input* nil)))
        (if (null n)
            nil
            (cons n (read-list)))))

(defun sum-of-odd (acc value)
  (+ acc (if (oddp value) value 0))
)

(let
  (
    (args (read-list))
  )
  (format t "~D~%" (reduce #'sum-of-odd args :initial-value 0))
)
