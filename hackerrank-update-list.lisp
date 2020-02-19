(defun read-list ()
    (let ((n (read *standard-input* nil)))
        (if (null n)
            nil
            (cons n (read-list)))))

(defun print-list (values)
  (if (and values(typep values 'sequence))
    (progn
      (format t "~D~%" (first values))
      (print-list (rest values))
    )
  )
)

(let
  (
    (args (read-list))
  )
  (print-list
    (map 'list #'(lambda (value) (if (> value 0) value (- value))) args)
  )
)
