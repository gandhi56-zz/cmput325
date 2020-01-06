; Cmput 325 Winter 2020 Assignment 1
; Student ID 1523205 Student name Anshil Gandhi

;QUESTION 1 selectnumbers ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
; input: a list L
; output: a list containing all numbers in L

(defun foo(x)
  (format t "foo ~a" x)
  )

(defun print_list (L)
  (write L)
  )

(defun selectnumbers (L)
  (let nums (list nil))
  (loop for x in L
        do (
          if (numberp x) (append nums (list x)) nil
          ; TODO append x to nums; ERROR append not working!
        )
  )
  (write nums)
)


;QUESTION 2 rselect ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
; recursively select numbers and create a new list


;QUESTION 2 absolute ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
; return a list of absolute values of each value in the input list

#||
(defun absolute (L)
  (loop for x in L
    do (  
    )
  )  
)
||#
