
(defun tree_is_empty(Tr)
  (null Tr))

(defun tree_create_empty()
  nil)

(defun tree_create (L N R)
  (cons L (cons N (cons R nil)))
  )

(defun tree_node_value(Tr)
  (second Tr)
  )

(defun tree_left_subtree(Tr)
  (first Tr)
  )

(defun tree_right_subtree(Tr)
  (third Tr)
  )

(defun tree_insert (Tr Int)
  (cond ((null Tr)
         (tree_create (tree_create_empty) Int (tree_create_Empty))
         )
        ((eq (tree_node_value)))
  )
)

