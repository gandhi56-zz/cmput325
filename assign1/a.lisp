; Cmput 326 Winter 2020 Assignment 1
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


; good
(defun crs_cnt (std i)
  (if (null std)
    0
    (if (eq i (car std))
      (+ 1 (crs_cnt (cdr std) i))
      (crs_cnt (cdr std) i)
      )
    )
  )

; good
(defun stds (crs)
  (if (null crs)
    nil
    (append (cadar crs) (stds (cdr crs)) )
    )
  )

#||
(defun bruh (n L AC)
  ;(crs_cnt L n)
  ;(crs_cnt (stds L) n)
  ;(stds L)

  (if (null L)
    AC
    (if (member (car L) AC)
      ; if already counted, recurse on the rest of the list
      (bruh n (cdr L) AC)

      (if (eq n (crs_cnt L (car L)))
        ; count car L and recurse on the rest of the list
        (bruh n (cdr L) (append (list (car L)) AC)) 

        ; recurse on the rest of the list
        (bruh n (cdr L) AC)
        )
      )
    )
  )
||#


(defun bruh (n L AC all)
  (if (null L)
    AC
    (if (member (car L) AC)
      (bruh n (cdr L) AC all)
      (if (eq n (crs_cnt all (car L)))
        (bruh n (cdr L) (append AC (list (car L))) all)
        (bruh n (cdr L) AC all)
        )
      )
    )
  )



(defun courses (n L)
  (bruh n (stds L) nil (stds L))
  )
















