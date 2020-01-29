
;QUESTION 5 tictactoe ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; output whether the given state of tictactoe is illegal, ongoing, draw or if either
;; player has won
;;
;; to determine if the state is legal
;;  - ensure the board is exactly 3 rows with 3 columns each
;;  - each element in the board must be one of '?, 'x, or 'o
;;  - the number of x's must be at most one more than the number
;;      of o's
;;  - count the number of wins by x and o for each row, column and diagonal
;;      x cannot win with two triplets on two different rows, for instance
;; if the board is legal, then determine if either player has won, the game is
;;  drawn or more moves can be played by counting the number of triplets
;;  and the number of ?s in the state

; obtained from assignment1.4.lisp
(defun cnt (std i)
  (if (null std)
    0
    (if (eq i (car std))
      (+ 1 (cnt (cdr std) i))
      (cnt (cdr std) i)
      )
    )
  )

; return r-th row
(defun get_row (board r)
  (cond ((= r 0) (car board))
        ((= r 1) (cadr board))
        ((= r 2) (caddr board))
    )
  )

; return c-th column
(defun get_col (board c)
  (cond 
        ((= c 0) (list (caar board) (caadr board) (caaddr board)))
        ((= c 1) (list (cadar board) (cadadr board) (car (cdaddr board))))
        ((= c 2) (list (caddar board) (car (cddadr board)) (caddr (caddr board))) )
        )
  )

; return diagonal in a list
(defun get_diag (board c)
  (cond 
        ((= c 0) (list (caar board) (cadadr board) (caddr (caddr board))))
        ((= c 1) (list (caaddr board) (cadadr board) (caddar board)))
        )
  )

; check winner
(defun check_win (L)
  (if (and (eq (first L) (second L)) (eq (first L) (third L)) )
    (first L)
    "?"
    )
  )

; count number of wins by a player on rows
(defun cnt_row_wins (board row c)
  (if (> row 2)
    0
    (if (eq c (check_win (get_row board row)))
      (+ 1 (cnt_row_wins board (+ row 1) c))
      (cnt_row_wins board (+ row 1) c)
      )
    )
  )

; count number of wins by a player on cols
(defun cnt_col_wins (board col c)
  (if (> col 2)
    0
    (if (eq c (check_win (get_col board col)))
      (+ 1 (cnt_col_wins board (+ col 1) c))
      (cnt_col_wins board (+ col 1) c)
      )
    )
  )

; count number of wins by a player on cols
(defun cnt_diag_wins (board diag c)
  (if (> diag 1)
    0
    (if (eq c (check_win (get_diag board diag)))
      (+ 1 (cnt_diag_wins board (+ diag 1) c))
      (cnt_diag_wins board (+ diag 1) c)
      )
    )
  )

; count the number of triplets containing c
(defun cnt_wins (board c)
  (+ (cnt_row_wins board 0 c) (cnt_col_wins board 0 c) (cnt_diag_wins board 0 c))
  )

; check if there is an empty cell
(defun has_empty (board row)
  (if (> row 2)
    nil
    (if (member '? (get_row board row))
      T
      (has_empty board (+ 1 row))
      )
    )
  )

; convert board into 1d
(defun cvt (board)
  (append (car board) (cadr board) (caddr board) )
  )

; count the number of c's
(defun cnt_sym (board c)
  (cnt (cvt board) c)
  )

; check if all symbols are valid
(defun ok_symb (L)
  (if (null L)
    T
    (if (member (car L) '(x o ?))
      (ok_symb (cdr L))
      nil
      )
    )
  )

; compute length of a list
(defun len (L)
  (if (null L)
    0
    (+ 1 (len (cdr L)))
    )
  )

; check if board dimensions are valid
(defun ok_dim (board)
  (if (atom board)
    nil
    ;
    (if (= 3 (len board))
      ; check each row
      (if (or (atom (car board))
               (atom (cadr board))
               (atom (caddr board)))
        nil 
        
        (if  (and (= 3 (len (get_row board 0))) 
                  (= 3 (len (get_row board 1))) 
                  (= 3 (len (get_row board 2)))
                )
            T
            nil
          )
        )
        nil
      )
    )
  )

; checks if the board admits a legal position
(defun legal_board (board)
  (if (and  (ok_dim board) 
            (ok_symb (cvt board))
            (or (= (- (cnt_sym board 'x) (cnt_sym board 'o)) 0)
                (= (- (cnt_sym board 'x) (cnt_sym board 'o)) 1)
                )
            (<= (cnt_row_wins board 0 'x) 1)
            (<= (cnt_row_wins board 0 'o) 1)
            (<= (cnt_col_wins board 0 'x) 1)
            (<= (cnt_col_wins board 0 'o) 1)
            (<= (cnt_diag_wins board 0 'x) 2)
            (<= (cnt_diag_wins board 0 'o) 2)
            )
    T
    nil
    )
  )

; main tictactoe driver program
(defun tictactoe (board)
  (if (legal_board board)
    (if (= 0 (cnt_wins board 'x))
      ; if x does not have a triple
      (if (= 0 (cnt_wins board 'o))
        (if (has_empty board 0)                         'ongoing  'draw         )
        (if (= (cnt_sym board 'x) (cnt_sym board 'o) ) 'o-win    'illegal      )
        )
      
      ; if x has at least 1 triple
      (if (= 0 (cnt_wins board 'o))

        ; x must occupy exactly one more than the number of o's
        (if (= (cnt_sym board 'x) (+ 1 (cnt_sym board 'o)) ) 'x-win    'illegal      )
        
        ; o cannot win if x has already won
        'illegal
        )
      )
    'illegal
    )
  )
