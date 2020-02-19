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

(defun bin-tree-bfs (fn tree)
  (bin-tree-bfs-aux fn (list tree))
)
(defun bin-tree-bfs-aux (fn trees)
  (if trees
  (let
    ((tovisit (list)))
    (loop for tree in trees
      do
      (if (bin-tree-leaf-p tree)
        (funcall fn (bin-tree-get-node-value tree))
        (let
          (
            (value (bin-tree-get-node-value tree))
            (left (bin-tree-get-node-left tree))
            (right (bin-tree-get-node-right tree))
          )
          (funcall fn value)
          (setf tovisit (append tovisit (list left right)))
        )
      )
    )
    (bin-tree-bfs-aux fn tovisit)
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

(bin-tree-walk
  (lambda (value level) (format t "level=~D value=~D~%" level value))
  (bin-tree-make-node 1 (bin-tree-make-node 2 (bin-tree-make-leaf 3) (bin-tree-make-leaf 4)) (bin-tree-make-leaf 5))
  1
)
