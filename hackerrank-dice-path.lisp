(defun read-list ()
    (let ((n (read *standard-input* nil)))
        (if (null n)
            nil
            (cons n (read-list)))))

(defvar *rolldown* (make-hash-table))
(defun roll-down (dice)
  (if (gethash dice *rolldown*)
    (gethash dice *rolldown*)
    (let
      ((result (list
        :top (getf dice :back)
        :front (getf dice :top)
        :left (getf dice :left)
        :bottom (getf dice :front)
        :back (getf dice :bottom)
        :right (getf dice :right)
      )))
      (setf (gethash dice *rolldown*) result)
      result
    )
  )
)

(defvar *rollright* (make-hash-table))
(defun roll-right (dice)
  (if (gethash dice *rollright*)
    (gethash dice *rollright*)
    (let
      ((result (list
        :top (getf dice :left)
        :front (getf dice :front)
        :left (getf dice :bottom)
        :bottom (getf dice :right)
        :back (getf dice :back)
        :right (getf dice :top)
      )))
      (setf (gethash dice *rollright*) result)
      result
    )
  )
)

(defun maxpath (maxrow maxcol dice)
    (let
      (
        (total (getf dice :top))
        (can-roll-down (> maxrow 1))
        (can-roll-right (> maxcol 1))
      )
      (if (and (not can-roll-down) (not can-roll-right))
        (progn
          (setf (gethash (list maxrow maxcol dice) *maxpath*) total)
          total
        )
        (if (and can-roll-down can-roll-right)
          (let
            (
              (down-total (maxpath (- maxrow 1) maxcol (roll-down dice)))
              (right-total (maxpath maxrow (- maxcol 1) (roll-right dice)))
            )
            (if (> down-total right-total)
              (+ total down-total)
              (+ total right-total)
            )
          )
          (if can-roll-down
            (+ total (maxpath (- maxrow 1) maxcol (roll-down dice)))
            (if can-roll-right
              (+ total (maxpath maxrow (- maxcol 1) (roll-right dice)))
            )
          )
        )
      )
    )  
)

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

(defun dice-init ()
  (list
      :top 1
      :front 2
      :left 3
      :bottom 6
      :back 5
      :right 4
  )
)

(maxpath 10 10 (dice-init))
(print (maxpath 3 3 (dice-init)  )) ; 19
(print (maxpath 20 20 (dice-init)  )) ; 19

;(let
;  ((args (rest (read-list))))
;  (map nil (group-by-2 #'(lambda (maxrow maxcol) (format t "~D~%" (maxpath maxrow maxcol (dice-init))))) args)
;)
