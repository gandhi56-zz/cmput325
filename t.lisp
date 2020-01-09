(defun c (x)
  (if (null x)
    0
    (if (null (cdr x))
      0
      (+ 1 (c (cdr (cdr x))))
      )
    )
  )


(defun f (x)
  (car (car (cdr (car (car (cdr x)))))
  ))


