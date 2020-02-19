(defun read-list ()
    (let ((n (read *standard-input* nil)))
        (if (null n)
            nil
            (cons n (read-list)))))

(defmacro not-equal (a b)
  `(not (equal ,a ,b))
)

(defmacro str-first-rest (str)
  `(if ,str
      (if (length-ge 2 ,str)
        (list (elt ,str 0) (subseq ,str 1))
        (list (elt ,str 0) nil)
      )
      (list nil nil)
  )
)

(defun length-ge (length str)
  (let ((count 0) (end (- length 1)))
    (labels
      (
        (predicate (c)
          (if (< count end)
            (progn
              (incf count)
              nil
              )
              t
            )
        ))
      (find-if #'predicate str)
    )
  )
)

(defun suffix (count)
  (if (> count 1)
    (write-to-string count)
    ""
  )
)

(defun compress (str &key (acc "") last-char (last-count 0))
  (destructuring-bind (first rest) (str-first-rest str)
    (if first
      (if (equal first last-char)
        (compress rest :acc acc :last-char last-char :last-count (+ 1 last-count))
        (compress rest :acc (concatenate 'string acc (suffix last-count) `(,first)) :last-char first :last-count 1)
      )
    (concatenate 'string acc (suffix last-count))
    )
  )
)

(format t "~a" (compress (string-downcase (string (first (read-list))))))
