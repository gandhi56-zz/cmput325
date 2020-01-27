; Cmput 325 Winter 2020 Assignment 1
; Student ID 1523205 
; Student name Anshil Gandhi

;QUESTION 1 selectnumbers ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; input: a list L
;; output: a list containing all numbers in L
;;
;; approach: assuming the list L is nonempty, construct
;;  a new list with the first element of L if it is a number
;;  and the solution to the sublist without the first element

(defun selectnumbers (L)
	(if (null L)
		nil
		(if (numberp (car L))
			(append (list (car L)) (selectnumbers (cdr L)))
			(selectnumbers (cdr L))
			)
		)
	)