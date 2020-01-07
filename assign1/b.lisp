
;; compute sum of elements of a list, list 
;; must only contain numbers
(defun sum (list)
	(if (null list)
		0
		(+ (car list) (sum (cdr list))))
	)


;; print list
(defun print-list (list)
	(if (not (null list))
		(progn
			(princ (car list))
			(print-list (cdr list)))
		)
	)

;; recursively sum numbers in a list
;; list may contain lists
(defun sumnum (list)
	(cond 
		((null list)
				 0)
		
		((numberp (car list))
		 (+ (car list) (sumnum (cdr list))))

		((symbolp (car list))
		 (sumnum (cdr list)))

		(t ; else
			(+ (sumnum (car list)) (sumnum (cdr list))))
		)
	)

;; check if a list is a sublist of another
(defun sublist (l1 l2)
	
	(cond
		((null l1) t)
		
		(t
			(and (member (car l1) l2)
				(sublist (cdr l1) l2)))

		)
	)

;; compute length of a list
(defun len (L)
	(if (null L)
		0
		(+ 1 (len (cdr L)))
		)
	)

(defun selectnumbers (L)
	(if (null L)
		nil
		(if (numberp (car L))
			(append (list (car L)) (selectnumbers (cdr L)))
			(selectnumbers (cdr L))
			)
		)
	)


(defun test-case (ID Test Result)
	    (if (equal Test Result)
				        (format t "Test ~S OK~%" ID)
								        (format t "FAIL: Test ~S expected ~S got ~S~%" ID Result Test)
												    )
			)


