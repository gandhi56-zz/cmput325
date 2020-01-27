; Cmput 326 Winter 2020 Assignment 1
; Student ID 1523205 
; Student name Anshil Gandhi

;QUESTION 3 absolute ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; return a list of absolute values of each value in the input list
;;
;; create a new list with the abs(first element of the input list)
;; and recursively add the absolute value of elements of the rest 
;; of the input list

(defun absolutes (L)
  (if (null L)
    nil
    (if (>= (car L) 0)
      (append (list (car L)) (absolutes (cdr L)))
      (append (list (- 0 (car L))) (absolutes (cdr L)))
      )
    )
  )