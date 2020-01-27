
; Cmput 326 Winter 2020 Assignment 1
; Student ID 1523205 
; Student name Anshil Gandhi

;QUESTION 4 courses ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Given a list of courses stored in a form as follows
;;      ((course-name-1 (s11 s12 ...)) (course-name-2 (s21 s22 ...)))
;; and an integer n, return a list of students that are enrolled in exactly n courses
;;
;; - obtain a list L of students, allowing duplicates
;; - for each student x in L, count the number of appearances of x in L
;;      - if x appears n times then put it into the output list

(defun crs_cnt (std i)
  (if (null std)
    0
    (if (eq i (car std))
      (+ 1 (crs_cnt (cdr std) i))
      (crs_cnt (cdr std) i)
      )
    )
  )

(defun stds (crs)
  (if (null crs)
    nil
    (append (cadar crs) (stds (cdr crs)) )
    )
  )

(defun get_stds (n L AC all)
  (if (null L)
    AC
    (if (member (car L) AC)
      (get_stds n (cdr L) AC all)
      (if (eq n (crs_cnt all (car L)))
        (get_stds n (cdr L) (append AC (list (car L))) all)
        (get_stds n (cdr L) AC all)
        )
      )
    )
  )

(defun courses (n L)
  (get_stds n (stds L) nil (stds L))
  )