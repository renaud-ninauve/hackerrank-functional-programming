(defun read-list ()
    (let ((n (read *standard-input* nil)))
        (if (null n)
            nil
            (cons n (read-list)))))

(defun curry (fn &rest init-args)
  #'(lambda (&rest args) (apply fn (append init-args args)))
)

(defun to-digits (number)
  (let
    ((str (write-to-string number)))
    (map 'list #'digit-char-p str)
  )
)

(defun super-digit (n k)
  (labels
    ((kmultiply (other) (funcall (curry #'* k) other)))
    (let
      ((sum (reduce #'+ (map 'list #'kmultiply (to-digits n)) :initial-value 0)))
      (if (< sum 10)
        sum
        (super-digit sum 1)
      )
    )
  )
)

(let
  ((params (read-list)))
  (format t "~D" (super-digit (first params) (second params)))
)
