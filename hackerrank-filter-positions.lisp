(defun filter-pos (values odd)
  (if values
    (if odd
      (filter-pos (rest values) nil)
      (progn

          (format t "~D~%" (first values))
          (filter-pos (rest values) t)
        
      )
    )
  )
)

(defun read-list ()
    (let ((n (read *standard-input* nil)))
        (if (null n)
            nil
            (cons n (read-list)))))

(let
  ((args (read-list)))
  (filter-pos args t)
)
