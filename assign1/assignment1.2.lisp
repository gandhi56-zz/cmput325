; Cmput 326 Winter 2020 Assignment 1
; Student ID 1523205 
; Student name Anshil Gandhi

;QUESTION 2 rselect ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; return list of numbers preserving list nestings as in the input list
;;
;; approach: follow the same approach as in assignment1.1.lisp,
;;  except if the first element is a list, obtain an answer to
;;  this list by applying the function before recursing on the rest
;;  of the list in question

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