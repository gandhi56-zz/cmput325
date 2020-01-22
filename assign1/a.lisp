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

;QUESTION 5 tictactoe ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
; check the number of 3 x's in a row, column or diagonal
; check the number of 3 o's in a row, column or diagonal
; determine winner or illegal position depending on the number of wins
; check if there is a blank cell
; if so, the game is ongoing else the game has ended in a draw

; return r-th row
(defun get_row (board r)
  (cond ((eq r 0) (car board))
        ((eq r 1) (cadr board))
        ((eq r 2) (caddr board))
    )
  )

; return c-th column
(defun get_col (board c)
  (cond ((eq c 2) (list (caddar board) (car (cddadr board)) (caddr (caddr board))) )
        ((eq c 0) (list (caar board) (caadr board) (caaddr board)))
        ((eq c 1) (list (cadar board) (cadadr board) (car (cdaddr board))))
        )
  )

; TODO test: return diagonal
(defun diag (board n)
  
  (cond ((eq c 1) (list (caddar board) (cadadr board) (caaddr board)) )
        ((eq c 0) (list (caar board) (cadadr board) (caddr (caddr board))))
        )
  )

; check winner
(defun check_win (L)
  (if (and (eq (first L) (second L)) (eq (first L) (third L)) )
    (first L)
    "?"
    )
  )

(defun tictactoe ()
  (princ (check_win '(o o o)))
  )







