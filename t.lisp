
(defun list-randoms (N)
  (if (= N 0)
    nil
    (cons (random 1.0) (list-randoms(- N 1))))
  )

(let ((L '(1 2 3))) (princ L) (princ L) (princ L))

