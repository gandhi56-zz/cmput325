(defun absolute (L)
	(if (null L)
		nil
		(if (numberp (car L))
			(append (list (car L)) (absolute (cdr L)))
			nil
			;(append (list (- 0 (car L))) (absolute (cdr L)))
			)
		)
	)

