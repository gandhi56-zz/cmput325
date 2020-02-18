
; computes gcd(x, y)
(defun euclid (x y)
  (cond ((and (= x 0) (= y 0))  'GCD-ERROR        )
        ( (= y 0)               x                 )
        (t                  ( euclid y (mod x y)) )
        )
  )
; ------------------------------------------------------------------

; checks if E is a fraction
(defun isFrac (E)
  (if (and (integerp (car E)) (integerp (cdr E)) )
    t
    nil
    )
  )
; ------------------------------------------------------------------

; converts fraction to integer if possible
(defun fracToInt (F)
  (if (= (cdr F) 1)
    (car F)
    F
    )
  )
; ------------------------------------------------------------------

; converts integer to fraction
(defun intToFrac (N)
  (cons N 1)
  )
; ------------------------------------------------------------------

; divides numerator and denominator by their gcd
(defun sfHelper (F d)
  (cons (/ (car F) d) (/ (cdr F) d) )
  )

(defun simplifyFraction (F)
  (if (= (cdr F) 0)
    'ZERODIVIDE-ERROR
    (fracToInt (sfHelper F (euclid (car F) (cdr F))))
    )
  )
; ------------------------------------------------------------------

; ===================================
; || Binary expression evaluation  ||
; ===================================
;
; an integer is a binary expression
; (p . q) is a binary expression if p and q are integers
; if E1 and E2 are binary expressions, then (E1 + E2), (E1 - E2), (E1 * E2) and (E1 / E2) are binary expressions

; return the binary operation
(defun op (E) (cadr E))

(defun inv (F)
  (cons (cdr F) (car F))
  )

; add fractions
(defun addFrac (l r)
  (cons (+ (* (car l) (cdr r)) (* (car r) (cdr l)) ) (* (cdr l) (cdr r) ) )
  )

; subtract fractions
(defun subFrac (l r)
  (cons (- (* (car l) (cdr r)) (* (car r) (cdr l)) )( * (cdr l) (cdr r) ) )
  )

; multiply fractions
(defun mulFrac (l r)
  (cons (* (car l) (car r)) ( * (cdr l) (cdr r) ) )
  )

; divide fractions
(defun divFrac (l r)
  (mulFrac l (inv r))
  )

; checks for divide by zero error 
; for +, -, * on two binary expressions
(defun bad_denom (l r)
  (or (eq (cdr l) 0) (eq (cdr r) 0))
  )

; evaluate when l and r are known fractions
(defun evaluate (op l r)
  (cond 
        ((or (eq l 'ZERODIVIDE-ERROR) (eq r 'ZERODIVIDE-ERROR))
          'ZERODIVIDE-ERROR
          )
  
        ( (eq '+ op)
         (if (bad_denom l r)  'ZERODIVIDE-ERROR
            (sfHelper (addFrac l r) (euclid (car (addFrac l r)) (cdr (addFrac l r)) ) )
            )
         )
        ( (eq '- op)
         (if (bad_denom l r)  'ZERODIVIDE-ERROR
            (sfHelper (subFrac l r) (euclid (car (addFrac l r)) (cdr (addFrac l r)) ) )
            )
         )
        ( (eq '* op)
         (if (bad_denom l r)  'ZERODIVIDE-ERROR
            (sfHelper (mulFrac l r) (euclid (car (addFrac l r)) (cdr (addFrac l r)) ) )
            )
          )
        ( (eq '/ op)
         (if (bad_denom l (inv r))  'ZERODIVIDE-ERROR
            (sfHelper (divFrac l r) (euclid (car (addFrac l r)) (cdr (addFrac l r)) ) )
            )
          )
        )
  )

; checks if E already contains an error
(defun badExpr (E)
  (if (atom E)
    (if (eq E 'ZERODIVIDE-ERROR)
      t
      nil
      )

    (if (or (eq (first E) 'ZERODIVIDE-ERROR) (eq (third E) 'ZERODIVIDE-ERROR) )
      t
      nil
      )
    )
  )

; recursive evaluation of binary expression
; if E is an integer, then returns its equivalent fraction
; if E is a fraction, then reduce and return the fraction
; otherwise, evaluate the binary expression
(defun sbin(E)
  (cond
        ( (integerp E)   (intToFrac E)                           )
        ( (isFrac E )   (sfHelper E (euclid (car E) (cdr E)))   )
        ( t (evaluateExpr E))
      )
  )

; evaluate binary expression, helper for sbin
(defun evaluateExpr (E)
  (if (badExpr E)
    'ZERODIVIDE-ERROR
    (evaluate (op E) (sbin (car E)) (sbin (caddr E)))
    )
  )

; main driver function for binary expression evaluation
(defun simplifyBinary (E)
  (if (eq 'ZERODIVIDE-ERROR (sbin E))
    'ZERODIVIDE-ERROR
    (simplifyFraction (sbin E))
    )
  )

; given an infix expression E, return the binary expression
; - no simplification
; - no error checking
(defun binarize (E)
  nil
  )

; ------------------------------------------------------------------
(defun test-case (ID Test Result)
    (if (equal Test Result)
        (format t "Test ~S OK~%" ID)
        (format t "FAIL: Test ~S expected ~S got ~S~%" ID Result Test)
    )
  )



; ------------------------------------------------------------------



; Main driver
(defun main ()
  nil
  )
