(defun filter (max values)  
  (if values
    (progn (let ((head (first values)))
      (if (< head max) (format t "~D~%" head))
      (filter max (rest values))
    )))
)

(defun read-list ()
    (let ((n (read *standard-input* nil)))
        (if (null n)
            nil
            (cons n (read-list)))))

(let
  ((args (read-list)))
  (filter (first args) (rest args))
)
