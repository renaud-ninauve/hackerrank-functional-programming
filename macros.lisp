;(defmacro do-list (var values &body body)
;  `(let ((,var (first ,values)))
;    (progn ,@body))
;)

;(defmacro do-list (var values &body body)
;  `(if ,values
;      (let ((,var (,first ,values)))
;        `(progn (,append ,@body ,(do-list var (rest values) body)))
;        )))

(defmacro unless2 (condition &body body)
  `(if (not ,condition) (progn ,@body))
)

(print (macroexpand-1 `(unless2 nil
  (format t "coucou"))))
(unless2 nil
  (format t "coucou"))

;(print (macroexpand-1 `(do-list toto (list "coucou1" "coucou2")
;  (format t "~S" toto))))
;
;(do-list toto (list "coucou1" "coucou2")
;  (format t "~S" toto))
