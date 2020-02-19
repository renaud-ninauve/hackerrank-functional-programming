(defun read-list ()
    (let ((n (read *standard-input* nil)))
        (if (null n)
            nil
            (cons n (read-list)))))

(defun print-list (values)
  (if (and values(typep values 'sequence))
    (progn
      (format t "~,4f~%" (first values))
      (print-list (rest values))
    )
  )
)

(defparameter *factorial* '())
(defun factorial (value)
  (let
    ((memo (get-factorial value)))
    (if memo
      memo
    )
    (if (> value 1)
      (let
        ((result (* value (factorial (- value 1)))))
        (put-factorial value result)
        result
      )
      (progn
        (put-factorial value 1)
        1
      )
    )
  )
)

(defun get-factorial (value)
  (let
    ((memo (find-if #'(lambda (kv) (= (car kv) value)) *factorial*)))
    (if memo (cdr memo) nil)
  )
)

(defun put-factorial (value result)
  (push (cons value result) *factorial*)
)

(defun pow (value n)
  (if (= n 0)
    1.0d0
    (if (= n 1)
      value
      (* value (pow value (- n 1)))
    )
  )
)

(defun e10 (value)
  (reduce
    #'+
    (map
      'list
      #'(lambda (n) (/ (pow value n) (coerce (factorial n) 'double-float)))
      `(0 1 2 3 4 5 6 7 8 9)
    )
    :initial-value 0.0)
)

(let
    ((params (read-list)))
    (print-list (map
      'list
      #'e10
      (rest params)
    ))
)
