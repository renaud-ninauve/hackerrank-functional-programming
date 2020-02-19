(defun f (n list)
  (if list
    (progn (let
      ((head (first list)))
      (dotimes (i n) (format t "~D~%" head))
      (f n (rest list))
    )))
)

(f 3 (list 1 2 3))
