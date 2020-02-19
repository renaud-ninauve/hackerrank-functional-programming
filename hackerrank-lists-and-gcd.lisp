(defun read-list ()
    (let ((line (read-line *standard-input* nil)))
        (if (null line)
            nil
            (cons (read-from-string (concatenate 'string "(" line ")")) (read-list)))))

(defun group-by-2 (fn)
  (let
    ((first nil))
    #'(lambda (elt)
      (if first
        (progn
          (funcall fn first elt)
          (setf first nil)
        )
        (setf first elt)
      )
    )
  )
)

(defun list-intersection (predicate list1 list2)
  (loop for el in list1
      when (member-if (lambda (x) (funcall predicate el x)) list2)
      collect el
  )
)

(defun lists-intersection (predicate lists)
  (reduce
    (lambda (a b) (list-intersection predicate a b))
    lists
  )
)

(defun gcd2 (lists)

  (let
    (
      (divisor-count-min-pow (make-hash-table))
    )
    (loop for (divisor pow) in (reduce #'append lists)
      do
        (let
          ((stored (gethash divisor divisor-count-min-pow)))
          (if stored
            (setf (gethash divisor divisor-count-min-pow) (list (+ 1 (first stored)) (min pow (second stored)) ))
            (setf (gethash divisor divisor-count-min-pow) (list 1 pow))
          )
        )
    )

    (sort
      (loop for divisor being the hash-keys of divisor-count-min-pow
            using (hash-value count-min-pow)
            when (= (first count-min-pow) (length lists))
            collect (list divisor (second count-min-pow))
            ;do (format t "The value associated with the key ~S is ~S~%" divisor count-min-pow)
      )
      (lambda (a b) (< (first a) (first b)))
    )
  )
)

(defun group-by-2 (lst)
  (loop
      for el in lst
      with first
      when first
      collect (list first el)
      do (if first (setf first nil) (setf first el))
  )
)

;(print (gcd2 '( ((1 2) (9 2)) ((1 4) (9 4)) ((1 4) (9 4)) )))

(let
  ((args (rest (read-list))))
  (loop for (divisor pow) in (gcd2 (mapcar #'group-by-2 args))
    do (format t "~D ~D " divisor pow)
  )
)
