
(defun fib(n)
  (if (<= n 1)
    1
    (+ (fib (- n 1)) (fib (- n 2)))
    )
  )

(defun foo ()
  1
  )

(write (fib 40))

