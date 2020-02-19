(defun read-list ()
    (let ((n (read *standard-input* nil)))
        (if (null n)
            nil
            (cons n (read-list)))))

(defun fibo (x)
  (if (= x 1)
    0
    (if (= x 2)
      1
      (+ (fibo (- x 1)) (fibo (- x 2)))
    )
  )
)

(format t "~D~%"
 (let
    ((args (read-list)))
    (fibo (first args))
  )
)
