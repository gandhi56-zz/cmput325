; Cmput 325 Winter 2020 Assignment 1
; Student ID 1523205 Student name Anshil Gandhi

;QUESTION 1 selectnumbers ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
; input: a list L
; output: a list containing all numbers in L

(defun selectnumbers (L)
	(if (null L)
		nil
		(if (numberp (car L))
			(append (list (car L)) (selectnumbers (cdr L)))
			(selectnumbers (cdr L))
			)
		)
	)

;QUESTION 2 rselect ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
; recursively select numbers and create a new list

(defun rselect (N)
  (if (null N)
    nil
    (if (atom (car N))
      ; if N[0] is an atom
      (if (numberp (car N))
        (append (list (car N)) (rselect (cdr N)))
        (rselect (cdr N))
        )

      ; if N[0] is a list
      (if (null (rselect (car N)))
        nil
        (list (append (rselect (car N)) (rselect (cdr N))))
        )

      )
    )
  )


;QUESTION 3 absolute ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
; return a list of absolute values of each value in the input list

(defun absolutes (L)
  (if (null L)
    nil
    (if (>= (car L) 0)
      (append (list (car L)) (absolutes (cdr L)))
      (append (list (- 0 (car L))) (absolutes (cdr L)))
      )
    )
  )

;QUESTION 4 courses ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
; param:  n, an integer
;         L, a list of courses in the form of 
;      ((course-name-1 (s11 s12 ...)) (course-name-2 (s21 s22 ...)))

(defun course_count ( x crs )
  (if (null (car crs))
    0
    (if (member x (cdr (car crs)))
      (+ 1 (course_count x (cdr crs)))
      (course_count x (cdr crs))
      )
    )
  )


(format t "~a" (course_count 1 '( (c1 (1 2 3)) (c2 (2)) (c3 (1 3))  )))







