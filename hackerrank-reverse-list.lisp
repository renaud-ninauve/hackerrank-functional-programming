(defun reverse-list (values)

  (if (and values(typep values 'sequence))
    (append (reverse-list (rest values)) (list (first values)))
    `()
  )
)

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
    (result (reverse-list (read-list)))
  )
  (print-list result)
)
