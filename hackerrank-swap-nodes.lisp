(defun read-list ()
    (let ((n (read *standard-input* nil)))
        (if (null n)
            nil
            (cons n (read-list)))))

(defun bin-tree-make-leaf (value)
  (list value)
)

(defun bin-tree-make-node (value left right)
  (list value left right)
)

(defun bin-tree-get-leaf-value (leaf)
  (first leaf)
)

(defun bin-tree-get-node-value (node)
  (first node)
)

(defun bin-tree-get-value (elt)
  (if (bin-tree-leaf-p elt)
      (bin-tree-get-leaf-value elt)
      (bin-tree-get-node-value elt)
  )
)

(defun bin-tree-get-node-left (node)
  (second node)
)

(defun bin-tree-get-node-right (node)
  (third node)
)

(defun bin-tree-leaf-p (el)
  (and (consp el) (= 1 (length el)))
)

(defun bin-tree-node-p (el)
  (and (consp el) (= 3 (length el)))
)

(defun bin-tree-walk (fn tree level)
  (if (bin-tree-leaf-p tree)
    (funcall fn (bin-tree-get-node-value tree) level)
    (let
      (
        (value (bin-tree-get-node-value tree))
        (left (bin-tree-get-node-left tree))
        (right (bin-tree-get-node-right tree))
      )
      (bin-tree-walk fn left (1+ level))
      (funcall fn value level)
      (bin-tree-walk fn right (1+ level))
    )
  )
)

(defun bin-tree-bfs (fn should-swap tree)
  (bin-tree-bfs-aux fn should-swap (list tree) 1)
)
(defun bin-tree-bfs-aux (fn should-swap trees level)
  (if trees
  (let
    ((tovisit (list)))
    (loop for tree in trees
      do
      (if (bin-tree-leaf-p tree)
        (funcall fn tree level)
        (let
          (           
            (left (bin-tree-get-node-left tree))
            (right (bin-tree-get-node-right tree))
          )
          (funcall fn tree level)
          (if (funcall should-swap level)
              (setf tovisit (append tovisit (list right left)))
              (setf tovisit (append tovisit (list left right)))
          )          
        )
      )
    )
    (bin-tree-bfs-aux fn should-swap tovisit (+ 1 level))
  )
  )
)

(defun bin-tree-copy (tree)
  (if (bin-tree-leaf-p tree)
    (bin-tree-copy-leaf tree)
    (bin-tree-copy-node tree)
  )
)

(defun bin-tree-copy-leaf (leaf)
  (bin-tree-make-leaf (bin-tree-get-leaf-value leaf))
)

(defun bin-tree-copy-node (node)
  (bin-tree-make-node
    (bin-tree-get-node-value node)
    (bin-tree-copy (bin-tree-get-node-left node))
    (bin-tree-copy (bin-tree-get-node-right node))
  )
)

(defun list-split (fn list)
  (list-split-aux fn list (list) (list))
)

(defun list-split-aux (fn values acc subacc)
  (if (null values)
    (append acc (list subacc))
    (let
      (
        (value (first values))
      )
      (if (funcall fn value)
        (list-split-aux fn (rest values) (append acc (list subacc)) (list value))
        (list-split-aux fn (rest values) acc (append subacc (list value)))
      )
    )
  )
)

(defun levelchangep ()
  (let
    (
      (index -1)
      (level 0)
      (next_split_index 1)
    )

    (lambda (value)
      (incf index)
      (if (= index next_split_index)
        (progn
          (incf level)
          (setf next_split_index (+ next_split_index (expt 2 level)))
          t
        )
      )
    )
  )
)

(defun bin-tree-make (values)
  (let
    (
      (levels
        (reverse
          (list-split
            (levelchangep)
            values
          )
        )
      )
    )
    (bin-tree-make-aux
      (rest levels)
      (map
        'list
        (lambda (value) (bin-tree-make-leaf value))
        (first levels)
      )
    )
  )
)

(defun bin-tree-make-aux (levels nodes)
  (if (null levels)
    (first nodes)
    (let
      (
        (remaining (first levels))
        (previous nil)
        (newnodes (list))
      )
      (map
        nil
        (lambda (node)
          (if previous
            (progn
              (setf newnodes (append newnodes (list (bin-tree-make-node (first remaining) previous node))))
              (setf previous nil)
              (setf remaining (rest remaining))
            )
            (setf previous node)
          )
        )
        nodes
      )
      (bin-tree-make-aux (rest levels) newnodes)
    )
  )
)
(print
  (swap-klevels 2
    (bin-tree-make '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15))
  )
)

(defun swap-klevels (swap-level tree)
  (let ((result (list)))
    (bin-tree-bfs
     (lambda (node level) (setf result (cons (bin-tree-get-value node) result)))   
     (lambda (level) (= 0 (mod swap-level level)))
  
     tree
     )
    (reverse result)
  )  
)

(defmacro rlambda (argnames &body body)
  ` (labels ((this (,@argnames) ,@body)) #'this)
)

;(bin-tree-walk
;  (lambda (value level) (format t "level=~D value=~D~%" level value))
;  (bin-tree-make-node 1 (bin-tree-make-node 2 (bin-tree-make-leaf 3) (bin-tree-make-leaf 4)) (bin-tree-make-leaf 5))
;  1
;)
