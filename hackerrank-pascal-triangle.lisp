(defmacro rlambda (argnames &body body)
  `(labels
      ((this (,@argnames)
         ,@body
         ))
    #'this
  )
)

(defun factorial (value)
  (if (= 0 value)
      1
      (* value (factorial (- value 1)))
  )
)

(defmacro curry-1 (fn arg)
  (let
    ((lambda-arg (gensym)))
    `(lambda (,lambda-arg) (funcall ,fn ,arg ,lambda-arg))
  )
)

(defun pascal (row col)
  (/ (factorial row) (* (factorial col) (factorial (- row col))))
)

(defun range-0 (max)
  (funcall
   (rlambda (col cols)
     (if (< col 0)
         cols
         (this (- col 1) (cons col cols))
     )
   )
   max
   '()
  )
)

(defun pascal-row (row)
  (map 'list (curry-1 #'pascal row) (range-0 row))
)

(defun pascal-triangle (max-row)
  (map 'list #'pascal-row (range-0 (- max-row 1)))
)

(defmacro lambda-with-loop-status (argnames &body body)
  `(let
      ((loop-status (list 'first t)))
    (lambda (,@argnames)
      ,@body
      (setf loop-status (list 'first nil))
    )
  )
)

(defun print-row (row)
 
  (map nil
       (lambda-with-loop-status (v)
         (if (getf loop-status 'first)
             (format t "~D" v)
             (format t " ~D" v)
         )
         )
       row
       )
  (format t "~%")
  )

(defun print-pascal-triangle (triangle)
  (map nil
       #'print-row
       triangle
   )
  )

(let
    ((triangle (pascal-triangle (read *standard-input* nil))))
  (print-pascal-triangle triangle)
)


