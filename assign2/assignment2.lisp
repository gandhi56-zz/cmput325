; Cmput 326 Winter 2020 Assignment 1
; Student ID 1523205 Student name Anshil Gandhi

; computes gcd(x, y)
(defun euclidGCD (x y)
  (cond ((and (= x 0) (= y 0))  'GCD-ERROR        )
        ( (= y 0)               x                 )
        (t                  ( euclidGCD y (mod x y)) )
        )
  )

(defun euclid (x y)
  (if (or (= x 0) (= y 0))
    'GCD-ERROR
    (euclidGCD x y)
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
  (if (= (cdr F) 1) (car F) F )
  )
; ------------------------------------------------------------------

; converts integer to fraction
(defun intToFrac (N)
  (if (atom N)
    (cons N 1)
    N
    )
  )
; ------------------------------------------------------------------

; divides numerator and denominator by their gcd
(defun sfHelper (F d) (cons (/ (car F) d) (/ (cdr F) d) ) )

; compute gcd of numerator and denominator of a fraction
(defun euclidFrac (F)
  (euclidGCD (car F) (cdr F))
  )

(defun simplifyFraction (F)
  (if (= (cdr F) 0) 'ZERODIVIDE-ERROR
    (if (eq (euclidFrac F) 'GCD-ERROR )
      0
      (fracToInt (sfHelper F (euclidFrac F)))
      )
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
(defun divFrac (l r)  (mulFrac l (inv r)) )

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
            (intToFrac (simplifyFraction (addFrac l r)))
            )
         )
        ( (eq '- op)
         (if (bad_denom l r)  'ZERODIVIDE-ERROR
            (intToFrac (simplifyFraction (subFrac l r)))
            )
         )
        ( (eq '* op)
         (if (bad_denom l r)  'ZERODIVIDE-ERROR
            (intToFrac (simplifyFraction (mulFrac l r)))
            )
          )
        ( (eq '/ op)
         (if (bad_denom l (inv r))  'ZERODIVIDE-ERROR
            (intToFrac (simplifyFraction (divFrac l r)))
            )
          )
        )
  )

; checks if E already contains an error
(defun badExpr (E)
  (if (atom E)
    (if (eq E 'ZERODIVIDE-ERROR)  t nil )
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
        ( (isFrac E )
          (if (eq 'GCD-ERROR (euclidFrac E))
            0
            (sfHelper E (euclidFrac E))
            )
          )
        
        ( t  
          (if (badExpr E) 'ZERODIVIDE-ERROR
            (evaluate (op E) (sbin (car E)) (sbin (caddr E)))
            )
          )
      )
  )

; main driver function for binary expression evaluation
(defun simplifyBinary (E)
  (cond 
    ((eq 'ZERODIVIDE-ERROR  (sbin E)) 'ZERODIVIDE-ERROR)
    (t (simplifyFraction (sbin E)))
    )
  )

; ------------------------------------------------------------------

;; operator getters and identifiers
(defun big_op (op)      (if (or (eq op '*) (eq op '/))      t nil ) )
(defun small_op (op)    (if (or (eq op '+) (eq op '-))      t nil ) )
(defun is_op (op)       (if (or (big_op op) (small_op op))  t nil ) )
(defun leftmost_op (E)  (if (is_op (first E)) (first E) (second E)) )
(defun get_op1 (E)                                  (leftmost_op E) )
(defun get_op2 (E)                          (leftmost_op (cddr E))  )

;; converts an infix expression to a binary expression
(defun binarize (E)
  (cond

    
    ; E is an number, return E
    ( (or (integerp E) (isFrac E)) 
      E)

    ; unnecassary but just in case there is no operator in E
    ( (and (null (get_op1 E)) (null (get_op2 E)) )  
      E
      )
    
    ; E is already a binary expression,
    ; binarize the two operands and return
    ; the recursively binarized expression
    ( (null (get_op2 E))  
      (list (binarize (first E)) (second E) (binarize (third E)))  
      )

    ( t 
      (if (and (small_op (get_op1 E)) (big_op (get_op2 E)))
        ; if second operator has precedence over the first operator
        ; nest the expression containing op2
        ; binarize each operand and recurse
        (binarize (append (list (binarize (first E)) (second E) 
          (list (binarize (third E)) (fourth E) (binarize (car (cddddr E))))) 
            (cdr (cddddr E)) 
            ) 
          )

        ; otherwise, nest the expression around op1, binarize each operand
        ; and return the binarized expression
        (binarize (append (list 
          (list (binarize (first E)) (second E) (binarize (third E)))) (cdddr E)
            )
          )
        )
      )
    )
  )

;; simplify infix expression
(defun simplify (E) (simplifyBinary (binarize E)) )

;; if var is a constant return it
;; otherwise search for var in bindings
(defun getValue (bindings var)
  (cond
    ( (is_op var)  var)
    ( (integerp var) var)
    (t
      (if (null bindings)
        'UNKNOWN-VARIABLE  ; var does not exist in bindings
        (if (eq (caar bindings) var)
          (simplify (cadar bindings))
          (getValue (cdr bindings) var)
          )
        )
      )
    )
  )

;; recursive substitution of E with 
;; variables defined in bindings
;; store answer in AC
(defun subVar (bindings E AC)
  (if (null E)
    AC
    (cond
      ; if E is a number or an operator then push it into AC
      ; if E is a variable then query its value from AC
      ( (atom E)  (getValue bindings E) )

      ; if E is a list
      (t
        (if (isFrac E)
          E
          (subVar bindings (cdr E) (append AC (list (subVar bindings (car E) nil) )) )
          )
        )
      )
    )
  )

;; Bindings = ( (x1 I1) (x2 I2) ... (xn In) )
;; E is a varexpr
;; return an infix expression
(defun substitutevar (Bindings E) 
  (subVar Bindings E nil)
  )

;; given bindings and a varExpr E, evaluate E
;; according to the rules of arithmetic
(defun simplifyVar (Bindings E)
  (simplify (substitutevar Bindings E))
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

  ; euclid
(test-case 1.1 (euclid 4 5) 1)
(test-case 1.2 (euclid 4 50) 2)
(test-case 1.3 (euclid 0 5) 'GCD-ERROR)
(test-case 1.4 (euclid 0 0) 'GCD-ERROR)
(test-case 1.5 (euclid 1000000000000000000 0) 'GCD-ERROR)
(test-case 1.6 (euclid 12345 54321) 3)
(test-case 1.7 (euclid 2455451323 (* 188880871 188880871)) 188880871)
(test-case 1.8 (euclid 63245986 102334155) 1)
(test-case 1.9 (euclid (* 2 3 7 13 31) (* 3 3 5 7 11 13)) (* 3 7 13))

;; ; simplifyfraction
(test-case 2.1 (simplifyfraction '(5 . 1)) 5)
(test-case 2.2 (simplifyfraction '(15 . 5)) 3)
(test-case 2.3 (simplifyfraction '(-100 . -4)) 25)
(test-case 2.4 (simplifyfraction '(0 . 2)) 0)
(test-case 2.5 (simplifyfraction '(105 . 6)) '(35 . 2))
(test-case 2.6 (simplifyfraction '(4 . -8)) '(-1 . 2))
(test-case 2.7 (simplifyfraction '(102334155 . 63245986)) '(102334155 . 63245986))
(test-case 2.8 (simplifyfraction '(35675983429718641 . 2455451323)) '(188880871 . 13))
(test-case 2.9 (simplifyfraction '(10 . 0)) 'ZERODIVIDE-ERROR)
(test-case "2.10" (simplifyfraction '(0 . 0)) 'ZERODIVIDE-ERROR)

;; ; simplifybinary
(test-case 3.1 (simplifybinary 5) 5)
(test-case 3.2 (simplifybinary '(50 . -10)) -5)
(test-case 3.3 (simplifybinary '(60 . 8)) '(15 . 2))
(test-case 3.4 (simplifybinary '(3 + 5)) 8)
(test-case 3.5 (simplifybinary '(((1 + 1) * (1 + 1)) * ((1 + 1) * (1 + 1)))) 16)
(test-case 3.6 (simplifybinary '(3 / 0)) 'ZERODIVIDE-ERROR)
(test-case 3.7 (simplifybinary '(1 + (3 / (5 - 5)))) 'ZERODIVIDE-ERROR)
(test-case 3.8 (simplifybinary '((1 / 2) + (1 / 2))) 1)
(test-case 3.9 (simplifybinary '((1 / 2) + (1 / 3))) '(5 . 6))
(test-case "3.10" (simplifybinary '((1 . 2) + (1 . 3))) '(5 . 6))
(test-case 3.11 (simplifybinary '(1 / 3)) '(1 . 3))
(test-case 3.12 (simplifybinary '((1 . 3) / (5 . 7))) '(7 . 15))
(test-case 3.13 (simplifybinary '(60 . 8)) '(15 . 2))
(test-case 3.14 (simplifybinary '((2 + (3 * 5)) - (7 / 4))) '(61 . 4))
(test-case 3.15 (simplifybinary '(1 + (1 + ((1 / 2) + ((1 / (2 * 3)) + ((1 / ((2 * 3) * 4)) + (1 / (((2 * 3) * 4) * 5)))))))) '(163 . 60))
(test-case 3.16 (simplifybinary '(0 * (1 / 0))) 'ZERODIVIDE-ERROR)
(test-case 3.17 (simplifybinary '(0 / 0)) 'ZERODIVIDE-ERROR)
(test-case 3.18 (simplifybinary '(2 + (0 * (1 / (5 - 5))))) 'ZERODIVIDE-ERROR)

(test-case 4.1 (binarize 2) 2)
(test-case 4.2 (binarize '(2 + 3)) '(2 + 3))
(test-case 4.3 (binarize '(2 / 3 * 4)) '((2 / 3) * 4))
(test-case 4.4 (binarize '(2 + 3 + 5 - 7 + 4)) '((((2 + 3) + 5) - 7) + 4))
(test-case 4.5 (binarize '(2 + 3 * 5 - 7 / 4)) '((2 + (3 * 5)) - (7 / 4)))
(test-case 4.6 (binarize '(2 + 3 * 5 / 7 / 4)) '(2 + (((3 * 5) / 7) / 4)))
(test-case 4.7 (binarize '((1 . 2) + (3 . 5) + (7 . 4))) '(((1 . 2) + (3 . 5)) + (7 . 4)))
(test-case 4.8 (binarize '((2 + 3 + 5 - 7 + 4) * (2 + 3 + 5 - 7 + 4))) '(((((2 + 3) + 5) - 7) + 4) * ((((2 + 3) + 5) - 7) + 4)))
(test-case 4.9 (binarize '(0 / 0 / 0 / 0 / 0)) '((((0 / 0) / 0) / 0) / 0))
(test-case "4.10" (binarize '((1 + 2 + 3) * (4 + 5 * 6) + (7 * 8 / 9) / (10 - 11 / (12 - 13 * 14)))) '((((1 + 2) + 3) * (4 + (5 * 6))) + (((7 * 8) / 9) / (10 - (11 / (12 - (13 * 14)))))))
(test-case 4.11 (binarize '((1 + (2 . 5) + 3) * (4 + 5 * 6) + (7 * 8 / (9 . 2)) / (10 - 11 / (12 - 13 * (14 . 15))))) '((((1 + (2 . 5)) + 3) * (4 + (5 * 6))) + (((7 * 8) / (9 . 2)) / (10 - (11 / (12 - (13 * (14 . 15))))))))
(test-case 4.12 (binarize '(1 / (5 - 3 - 2))) '(1 / ((5 - 3) - 2)))

(test-case 5.1 (simplify 5) 5)
(test-case 5.2 (simplify '(50 . -10)) -5)
(test-case 5.3 (simplify '(60 . 8)) '(15 . 2))
(test-case 5.4 (simplify '(3 + 5)) 8)
(test-case 5.5 (simplify '(((1 + 1) * (1 + 1)) * ((1 + 1) * (1 + 1)))) 16)
(test-case 5.6 (simplify '(3 / 0)) 'ZERODIVIDE-ERROR)
(test-case 5.7 (simplify '(1 - 1 * 1 - 1 / 1)) -1)
(test-case 5.8 (simplify '(1 + 1 + 1 / 2 + 1 / (2 * 3) + 1 / (2 * 3 * 4) + 1 / (2 * 3 * 4 * 5))) '(163 . 60))
(test-case 5.9 (simplify '(0 / 0 / 0 / 0 / 0)) 'ZERODIVIDE-ERROR)
(test-case "5.10" (simplify '((2 + 3 + 5 - 7 + 4) * (2 + 3 + 5 - 7 + 4))) 49)
(test-case 5.11 (simplify '((1 . 2) + (3 . 5) + (7 . 4))) '(57 . 20))
(test-case 5.12 (simplify '((1 . 3) / (5 . 7))) '(7 . 15))
(test-case 5.13 (simplify '((1 . 3) / (5 . 7) * (30 . 7))) 2)
(test-case 5.14 (simplify '(1 + 1 + (1 . 2) + (1 . 6) + (1 . 24) + (1 . 120))) '(163 . 60))
(test-case 5.15 (simplify '((2 + (3 * 5)) - (7 / 4))) '(61 . 4))
(test-case 5.16 (simplify '(2 + 3 * 5 - 7 / 4)) '(61 . 4))
(test-case 5.17 (simplify '(2 + (((3 * 5) / 7) / 4))) '(71 . 28))
(test-case 5.18 (simplify '(2 + 3 * 5 / 7 / 4)) '(71 . 28))
(test-case 5.19 (simplify '((1 . 9) + ((-4 . 6) + (-4 . 6)) * ((-4 . 6) + (-4 . 6)))) '(17 . 9) )
(test-case "5.20" (simplify '(1 + 2 / 3)) '(5 . 3))
(test-case 5.21 (simplify '(1 / (5 - 3 - 2))) 'ZERODIVIDE-ERROR)
(test-case 5.22 (simplify '(1 / (1 / (1 / (1 / (5 - 3 - 2))) ))) 'ZERODIVIDE-ERROR)
(test-case 5.23 (simplify '(1 / (1 / (1 / (1 / (5 - 3 - (5 . 2)))) ))) '(-1 . 2))

; substitutevar
(test-case 6.1 (substitutevar nil '(1 + 2 / 3)) '(1 + 2 / 3))
(test-case 6.2 (substitutevar '((a 5)) 'a) 5)
(test-case 6.3 (substitutevar '((d 9)(b 6)(a 5)) 'a) 5)
(test-case 6.4 (substitutevar '((d 9)(b 6)(z 0)(a -5)) '(a + b + z + d)) '(-5 + 6 + 0 + 9))
(test-case 6.5 (substitutevar '((x 1)) '(2 + x)) '(2 + 1))
(test-case 6.6 (substitutevar '((x 1) (y (2 + 3))) '(x + x + y)) '(1 + 1 + 5))
(test-case 6.7 (substitutevar '((x 0)) '(x / x / x / x / x)) '(0 / 0 / 0 / 0 / 0))
(test-case 6.8 (substitutevar '((x 1)) '(((x + x) - (x + x)) * ((x + x) * (x / x)))) '(((1 + 1) - (1 + 1)) * ((1 + 1) * (1 / 1))))
(test-case 6.9 (substitutevar '((x (3 . 5)) (y (1 + 1 + (1 . 2) + (1 . 6) + (1 . 24) + (1 . 120)))) '(((x + y) - (y + x)) * ((y + y) * (x / (5. 3))))) '((((3 . 5) + (163 . 60)) - ((163 . 60) + (3 . 5))) * (((163 . 60) + (163 . 60)) * ((3 . 5) / (5. 3)))))
(test-case "6.10" (substitutevar '((x 5) (y 3)) '(1 / (x - y - 2))) '(1 / (5 - 3 - 2)))

; simplifyvar
(test-case 7.1 (simplifyvar '((x 1)) '(2 + x)) 3)
(test-case 7.2 (simplifyvar '((x 1) (y (2 + 3))) '(x + x + y)) 7)
(test-case 7.3 (simplifyvar nil '(1 + 2 / 3)) '(5 . 3))
(test-case 7.4 (simplifyvar '((a 5)) 'a) 5)
(test-case 7.5 (simplifyvar '((d 9)(b 6)(a 5)) 'a) 5)
(test-case 7.6 (simplifyvar '((d 9)(b 6)(z 0)(a -5)) '(a + b + z + d)) 10)
(test-case 7.7 (simplifyvar '((x 5) (y 3)) '(1 / (x - y - 2))) 'ZERODIVIDE-ERROR)
(test-case 7.8 (simplifyvar '((x 5) (y (11 . 3))) '(1 / (x - y - 2))) '(-3 . 2))
(test-case 7.9 (simplifyvar '((x 0)) '(x / x / x / x / x)) 'ZERODIVIDE-ERROR)
(test-case "7.10" (simplifyvar '((x 1)) '(((x + x) - (x + x)) * ((x + x) * (x / x)))) 0)
(test-case 7.11 (simplifyvar '((x (3 . 5)) (y (1 + 1 + (1 . 2) + (1 . 6) + (1 . 24) + (1 . 120)))) 'x) '(3 . 5))
(test-case 7.12 (simplifyvar '((x (3 . 5)) (y (1 + 1 + (1 . 2) + (1 . 6) + (1 . 24) + (1 . 120)))) 'y) '(163 . 60))
(test-case 7.13 (simplifyvar '((x (3 . 5)) (y (1 + 1 + (1 . 2) + (1 . 6) + (1 . 24) + (1 . 120)))) '(((x + y) - (y + x)) * ((y + y) * (x / (5 . 3))))) 0)
(test-case 7.14 (simplifyvar '((x (3 . 5)) (y (1 + 1 + (1 . 2) + (1 . 6) + (1 . 24) + (1 . 120)))) '(((x + y) - (y - x)) * ((y + y) * (x / (5 . 3))))) '(1467 . 625))
(test-case 7.15 (simplifyvar '((x 2)) '(1 / (1 / (1 / (1 / (5 - 3 - x))) ))) 'ZERODIVIDE-ERROR)
(test-case 7.16 (simplifyvar '((x (5 . 2))) '(1 / (1 / (1 / (1 / (5 - 3 - x))) ))) '(-1 . 2))





  ; simplifybinary
  (test-case "3.19" (simplifybinary '(0 * 0) ) 0)
  (test-case "3.20" (simplifybinary '((0 * 0) * (0 * 0))) 0)
  (test-case "3.21" (simplifybinary '(((0 * 0) * (0 * 0)) * ((0 * 0) * (0 * 0)))) 0)
  (test-case "3.22" (simplifybinary '((((0 * 0) * (0 * 0)) * ((0 * 0) * (0 * 0)))
                                      *
                                      (((0 * 0) * (0 * 0)) * ((0 * 0) * (0 * 0))))) 0)
  (test-case "3.23" (simplifybinary
                     '(((((0 * 0) * (0 * 0)) * ((0 * 0) * (0 * 0))) *
                        (((0 * 0) * (0 * 0)) * ((0 * 0) * (0 * 0))))
                       *
                       ((((0 * 0) * (0 * 0)) * ((0 * 0) * (0 * 0))) *
                        (((0 * 0) * (0 * 0)) * ((0 * 0) * (0 * 0)))))
                     ) 0)
  (test-case "3.24" (simplifybinary
                     '((((((0 * 0) * (0 * 0)) * ((0 * 0) * (0 * 0))) *
                         (((0 * 0) * (0 * 0)) * ((0 * 0) * (0 * 0))))
                        *
                        ((((0 * 0) * (0 * 0)) * ((0 * 0) * (0 * 0))) *
                         (((0 * 0) * (0 * 0)) * ((0 * 0) * (0 * 0)))))
                       *
                       (((((0 * 0) * (0 * 0)) * ((0 * 0) * (0 * 0))) *
                         (((0 * 0) * (0 * 0)) * ((0 * 0) * (0 * 0))))
                        *
                        ((((0 * 0) * (0 * 0)) * ((0 * 0) * (0 * 0))) *
                         (((0 * 0) * (0 * 0)) * ((0 * 0) * (0 * 0))))))
                     ) 0)
  (test-case "3.25" (simplifybinary
                     '((((((0 * 0) * (0 + 0)) * ((0 * 0) * (0 * 0))) *
                         (((0 * 0) * (0 * 0)) + ((0 * 0) * (0 - 0))))
                        *
                        ((((0 * 0) - (0 * 0)) * ((0 * 0) * (0 * 0))) *
                         (((0 * 0) * (0 * 0)) * ((0 * 0) + (0 * 0)))))
                       *
                       (((((0 * 0) * (0 * 0)) * ((0 * 0) * (0 * 0))) *
                         (((0 * 0) + (0 * 0)) + ((0 * 0) - (0 * 0))))
                        *
                        ((((0 * 0) * (0 * 0)) * ((0 * 0) * (0 * 0))) *
                         (((0 * 0) * (0 * 0)) - ((0 * 0) * (0 * 0))))))
                     ) 0)
  (test-case "3.26" (simplifybinary
                     '((((((0 * 0) * (0 + 0)) * ((0 * 0) * (0 * 0))) *
                         (((0 * 0) * (0 * 0)) + ((0 * 0) * (0 - 0))))
                        *
                        ((((0 * 0) - (0 * 0)) * ((0 * 0) * (0 * 0))) *
                         (((0 * 0) * (0 * 0)) * ((0 * 0) + (0 * 0)))))
                       *
                       (((((0 * 0) * (0 * 0)) * ((0 * 0) * (0 * 0))) *
                         (((0 * 0) + (0 * 0)) + ((0 * 0) - (0 * 0))))
                        *
                        ((((0 * 0) * (0 * 0)) * ((0 * 0) * (0 * 0))) *
                         (((0 * 0) * (0 * 0)) - ((0 * 0) * (0 / 0))))))
                     ) 'ZERODIVIDE-ERROR)
  (test-case "3.27" (simplifybinary
                     '((((((1 + 2) - (3 * 4)) / ((5 + 6) - (7 * 8))) /
                         (((9 + 10) - (11 * 12)) / ((13 + 14) - (15 * 16))))
                        /
                        ((((17 + 18) - (19 * 20)) / ((21 + 22) - (23 * 24))) /
                         (((25 + 26) - (27 * 28)) / ((29 + 30) - (31 * 32)))))
                       /
                       (((((33 + 34) - (35 * 36)) / ((37 + 38) - (39 * 40))) /
                         (((41 + 42) - (43 * 44)) / ((45 + 46) - (47 * 48))))
                        /
                        ((((49 + 50) - (51 * 52)) / ((53 + 54) - (55 * 56))) /
                         (((57 + 58) - (59 * 60)) / ((61 + 62) - (63 * 64))))))
                     ) '(131988593110978197 . 308085605269311125))
  (test-case "3.28" (simplifybinary
                     '((((((1 + 2) - (3 * 4)) / ((5 + 6) - (7 * 8))) /
                         (((9 + 10) - (11 * 12)) / ((13 + 14) - (15 * 16))))
                        /
                        ((((17 + 18) - (19 * 20)) / ((21 + 22) - (23 * 24))) /
                         (((25 + 26) - (27 * 28)) / ((29 + 30) - (31 * 32)))))
                       /
                       (((((33 + 34) - (35 * 36)) / ((37 + 38) - (39 * 40))) /
                         (((41 + 42) - (43 * 44)) / ((0 / 0) - (47 * 48))))
                        /
                        ((((49 + 50) - (51 * 52)) / ((53 + 54) - (55 * 56))) /
                         (((57 + 58) - (59 * 60)) / ((61 + 62) - (63 * 64))))))
                     ) 'ZERODIVIDE-ERROR)
  (test-case "3.29" (simplifybinary
                     '((((((1 . 2) + (3 . 4)) - ((5 . 6) * (7 . 8))) /
                         (((9 . 10) + (11 . 12)) - ((13 . 14) * (15 . 16))))
                        /
                        ((((17 . 18) + (19 . 20)) - ((21 . 22) * (23 . 24))) /
                         (((25 . 26) + (27 . 28)) - ((29 . 30) * (31 . 32)))))
                       /
                       (((((33 . 34) + (35 . 36)) - ((37 . 38) * (39 . 40))) /
                         (((41 . 42) + (43 . 44)) - ((45 . 46) * (47 . 48))))
                        /
                        ((((49 . 50) + (51 . 52)) - ((53 . 54) * (55 . 56))) /
                         (((57 . 58) + (59 . 60)) - ((61 . 62) * (63 . 64))))))
                     )
             '(61202529293797639408750 . 109941528488902164907853))
  (test-case "3.30" (simplifybinary
                     '((((((1 . 2) + 3) - (5 * (7 . 8))) /
                         (9 - ((13 . 14) * (15 . 16))))
                        /
                        ((((17 . 18) + (19 . 20)) - 21) /
                         25))
                       /
                       ((((33 + (35 . 36)) - ((37 . 38) * 39)) /
                         (((41 . 42) + (43 . 44)) - 45))
                        /
                        ((((49 . 50) + (51 . 52)) - ((53 . 54) * (55 . 56))) /
                         (((57 . 58) + (59 . 60)) - ((61 . 62) * (63 . 64))))))
                     )
             '(112098351017680000 . 74044505435119219))


  ;; binarize
  (test-case "4.13"
            (binarize
              '(0 * 0 * (0 * 0) * (0 * 0 * (0 * 0)) * (0 * 0 * (0 * 0) * (0 * 0 * (0 * 0))) *
                (0 * 0 * (0 * 0) * (0 * 0 * (0 * 0)) * (0 * 0 * (0 * 0) * (0 * 0 * (0 * 0))))
                *
                (0 * 0 * (0 * 0) * (0 * 0 * (0 * 0)) * (0 * 0 * (0 * 0) * (0 * 0 * (0 * 0))) *
                (0 * 0 * (0 * 0) * (0 * 0 * (0 * 0)) *
                  (0 * 0 * (0 * 0) * (0 * 0 * (0 * 0)))))))
            '((((((0 * 0) * (0 * 0)) * ((0 * 0) * (0 * 0))) *
                (((0 * 0) * (0 * 0)) * ((0 * 0) * (0 * 0))))
                *
                ((((0 * 0) * (0 * 0)) * ((0 * 0) * (0 * 0))) *
                (((0 * 0) * (0 * 0)) * ((0 * 0) * (0 * 0)))))
              *
              (((((0 * 0) * (0 * 0)) * ((0 * 0) * (0 * 0))) *
                (((0 * 0) * (0 * 0)) * ((0 * 0) * (0 * 0))))
                *
                ((((0 * 0) * (0 * 0)) * ((0 * 0) * (0 * 0))) *
                (((0 * 0) * (0 * 0)) * ((0 * 0) * (0 * 0))))))
            )
  (test-case "4.14"
            (binarize
              '(0 / 0 / (0 / 0) / (0 / 0 / (0 / 0)) / (0 / 0 / (0 / 0) / (0 / 0 / (0 / 0))) /
                (0 / 0 / (0 / 0) / (0 / 0 / (0 / 0)) / (0 / 0 / (0 / 0) / (0 / 0 / (0 / 0))))
                /
                (0 / 0 / (0 / 0) / (0 / 0 / (0 / 0)) / (0 / 0 / (0 / 0) / (0 / 0 / (0 / 0))) /
                (0 / 0 / (0 / 0) / (0 / 0 / (0 / 0)) /
                  (0 / 0 / (0 / 0) / (0 / 0 / (0 / 0)))))))
            '((((((0 / 0) / (0 / 0)) / ((0 / 0) / (0 / 0))) /
                (((0 / 0) / (0 / 0)) / ((0 / 0) / (0 / 0))))
                /
                ((((0 / 0) / (0 / 0)) / ((0 / 0) / (0 / 0))) /
                (((0 / 0) / (0 / 0)) / ((0 / 0) / (0 / 0)))))
              /
              (((((0 / 0) / (0 / 0)) / ((0 / 0) / (0 / 0))) /
                (((0 / 0) / (0 / 0)) / ((0 / 0) / (0 / 0))))
                /
                ((((0 / 0) / (0 / 0)) / ((0 / 0) / (0 / 0))) /
                (((0 / 0) / (0 / 0)) / ((0 / 0) / (0 / 0))))))
            )
  (test-case "4.15"
            (binarize
              '(0 + 0 + (0 + 0) + (0 + 0 + (0 + 0)) + (0 + 0 + (0 + 0) + (0 + 0 + (0 + 0))) +
                (0 + 0 + (0 + 0) + (0 + 0 + (0 + 0)) + (0 + 0 + (0 + 0) + (0 + 0 + (0 + 0))))
                +
                (0 + 0 + (0 + 0) + (0 + 0 + (0 + 0)) + (0 + 0 + (0 + 0) + (0 + 0 + (0 + 0))) +
                (0 + 0 + (0 + 0) + (0 + 0 + (0 + 0)) +
                  (0 + 0 + (0 + 0) + (0 + 0 + (0 + 0)))))))
            '((((((0 + 0) + (0 + 0)) + ((0 + 0) + (0 + 0))) +
                (((0 + 0) + (0 + 0)) + ((0 + 0) + (0 + 0))))
                +
                ((((0 + 0) + (0 + 0)) + ((0 + 0) + (0 + 0))) +
                (((0 + 0) + (0 + 0)) + ((0 + 0) + (0 + 0)))))
              +
              (((((0 + 0) + (0 + 0)) + ((0 + 0) + (0 + 0))) +
                (((0 + 0) + (0 + 0)) + ((0 + 0) + (0 + 0))))
                +
                ((((0 + 0) + (0 + 0)) + ((0 + 0) + (0 + 0))) +
                (((0 + 0) + (0 + 0)) + ((0 + 0) + (0 + 0))))))
            )
  (test-case "4.16"
            (binarize
              '(0 - 0 - (0 - 0) - (0 - 0 - (0 - 0)) - (0 - 0 - (0 - 0) - (0 - 0 - (0 - 0))) -
                (0 - 0 - (0 - 0) - (0 - 0 - (0 - 0)) - (0 - 0 - (0 - 0) - (0 - 0 - (0 - 0))))
                -
                (0 - 0 - (0 - 0) - (0 - 0 - (0 - 0)) - (0 - 0 - (0 - 0) - (0 - 0 - (0 - 0))) -
                (0 - 0 - (0 - 0) - (0 - 0 - (0 - 0)) -
                  (0 - 0 - (0 - 0) - (0 - 0 - (0 - 0)))))))
            '((((((0 - 0) - (0 - 0)) - ((0 - 0) - (0 - 0))) -
                (((0 - 0) - (0 - 0)) - ((0 - 0) - (0 - 0))))
                -
                ((((0 - 0) - (0 - 0)) - ((0 - 0) - (0 - 0))) -
                (((0 - 0) - (0 - 0)) - ((0 - 0) - (0 - 0)))))
              -
              (((((0 - 0) - (0 - 0)) - ((0 - 0) - (0 - 0))) -
                (((0 - 0) - (0 - 0)) - ((0 - 0) - (0 - 0))))
                -
                ((((0 - 0) - (0 - 0)) - ((0 - 0) - (0 - 0))) -
                (((0 - 0) - (0 - 0)) - ((0 - 0) - (0 - 0))))))
            )
  (test-case "4.17"
            (binarize
              '((1 . 2) * (3 . 4) * ((5 . 6) * (7 . 8)) * ((9 . 10) * (11 . 12) * ((13 . 14) * (15 . 16))) * ((17 . 18) * (19 . 20) * ((21 . 22) * (23 . 24)) * ((25 . 26) * (27 . 28) * ((29 . 30) * (31 . 32)))) *
                ((33 . 34) * (35 . 36) * ((37 . 38) * (39 . 40)) * ((41 . 42) * (43 . 44) * ((45 . 46) * (47 . 48))) * ((49 . 50) * (51 . 52) * ((53 . 54) * (55 . 56)) * ((57 . 58) * (59 . 60) * ((61 . 62) * (63 . 64)))))
                *
                ((65 . 66) * (67 . 68) * ((69 . 70) * (71 . 72)) * ((73 . 74) * (75 . 76) * ((77 . 78) * (79 . 80))) * ((81 . 82) * (83 . 84) * ((85 . 86) * (87 . 88)) * ((89 . 90) * (91 . 92) * ((93 . 94) * (95 . 96)))) *
                ((97 . 98) * (99 . 100) * ((101 . 102) * (103 . 104)) * ((105 . 106) * (107 . 108) * ((109 . 110) * (111 . 112))) *
                  ((113 . 114) * (115 . 116) * ((117 . 118) * (119 . 120)) * ((121 . 122) * (123 . 124) * ((125 . 126) * (127 . 128))))))))
            '(((((((1 . 2) * (3 . 4)) * ((5 . 6) * (7 . 8))) * (((9 . 10) * (11 . 12)) * ((13 . 14) * (15 . 16)))) *
                ((((17 . 18) * (19 . 20)) * ((21 . 22) * (23 . 24))) * (((25 . 26) * (27 . 28)) * ((29 . 30) * (31 . 32)))))
                *
                (((((33 . 34) * (35 . 36)) * ((37 . 38) * (39 . 40))) * (((41 . 42) * (43 . 44)) * ((45 . 46) * (47 . 48)))) *
                ((((49 . 50) * (51 . 52)) * ((53 . 54) * (55 . 56))) * (((57 . 58) * (59 . 60)) * ((61 . 62) * (63 . 64))))))
              *
              ((((((65 . 66) * (67 . 68)) * ((69 . 70) * (71 . 72))) * (((73 . 74) * (75 . 76)) * ((77 . 78) * (79 . 80)))) *
                ((((81 . 82) * (83 . 84)) * ((85 . 86) * (87 . 88))) * (((89 . 90) * (91 . 92)) * ((93 . 94) * (95 . 96)))))
                *
                (((((97 . 98) * (99 . 100)) * ((101 . 102) * (103 . 104))) * (((105 . 106) * (107 . 108)) * ((109 . 110) * (111 . 112)))) *
                ((((113 . 114) * (115 . 116)) * ((117 . 118) * (119 . 120))) * (((121 . 122) * (123 . 124)) * ((125 . 126) * (127 . 128)))))))
            )
  (test-case "4.18"
            (binarize
              '((1 . 2) / (3 . 4) / ((5 . 6) / (7 . 8)) / ((9 . 10) / (11 . 12) / ((13 . 14) / (15 . 16))) / ((17 . 18) / (19 . 20) / ((21 . 22) / (23 . 24)) / ((25 . 26) / (27 . 28) / ((29 . 30) / (31 . 32)))) /
                ((33 . 34) / (35 . 36) / ((37 . 38) / (39 . 40)) / ((41 . 42) / (43 . 44) / ((45 . 46) / (47 . 48))) / ((49 . 50) / (51 . 52) / ((53 . 54) / (55 . 56)) / ((57 . 58) / (59 . 60) / ((61 . 62) / (63 . 64)))))
                /
                ((65 . 66) / (67 . 68) / ((69 . 70) / (71 . 72)) / ((73 . 74) / (75 . 76) / ((77 . 78) / (79 . 80))) / ((81 . 82) / (83 . 84) / ((85 . 86) / (87 . 88)) / ((89 . 90) / (91 . 92) / ((93 . 94) / (95 . 96)))) /
                ((97 . 98) / (99 . 100) / ((101 . 102) / (103 . 104)) / ((105 . 106) / (107 . 108) / ((109 . 110) / (111 . 112))) /
                  ((113 . 114) / (115 . 116) / ((117 . 118) / (119 . 120)) / ((121 . 122) / (123 . 124) / ((125 . 126) / (127 . 128))))))))
            '(((((((1 . 2) / (3 . 4)) / ((5 . 6) / (7 . 8))) / (((9 . 10) / (11 . 12)) / ((13 . 14) / (15 . 16)))) /
                ((((17 . 18) / (19 . 20)) / ((21 . 22) / (23 . 24))) / (((25 . 26) / (27 . 28)) / ((29 . 30) / (31 . 32)))))
                /
                (((((33 . 34) / (35 . 36)) / ((37 . 38) / (39 . 40))) / (((41 . 42) / (43 . 44)) / ((45 . 46) / (47 . 48)))) /
                ((((49 . 50) / (51 . 52)) / ((53 . 54) / (55 . 56))) / (((57 . 58) / (59 . 60)) / ((61 . 62) / (63 . 64))))))
              /
              ((((((65 . 66) / (67 . 68)) / ((69 . 70) / (71 . 72))) / (((73 . 74) / (75 . 76)) / ((77 . 78) / (79 . 80)))) /
                ((((81 . 82) / (83 . 84)) / ((85 . 86) / (87 . 88))) / (((89 . 90) / (91 . 92)) / ((93 . 94) / (95 . 96)))))
                /
                (((((97 . 98) / (99 . 100)) / ((101 . 102) / (103 . 104))) / (((105 . 106) / (107 . 108)) / ((109 . 110) / (111 . 112)))) /
                ((((113 . 114) / (115 . 116)) / ((117 . 118) / (119 . 120))) / (((121 . 122) / (123 . 124)) / ((125 . 126) / (127 . 128)))))))
            )
  (test-case "4.19"
            (binarize
              '((1 . 2) / (3 . 4) / ((5 . 6) * (7 . 8)) / ((9 . 10) / (11 . 12) / ((13 . 14) / (15 . 16))) / ((17 . 18) / (19 . 20) / ((21 . 22) / (23 . 24)) / ((25 . 26) / (27 . 28) / ((29 . 30) / (31 . 32)))) /
                ((33 . 34) / (35 . 36) / ((37 . 38) / (39 . 40)) * ((41 . 42) / (43 . 44) / ((45 . 46) / (47 . 48))) / ((49 . 50) / (51 . 52) / ((53 . 54) / (55 . 56)) / ((57 . 58) / (59 . 60) / ((61 . 62) / (63 . 64)))))
                /
                ((65 . 66) / (67 . 68) / ((69 . 70) / (71 . 72)) / ((73 . 74) / (75 . 76) / ((77 . 78) / (79 . 80))) / ((81 . 82) / (83 . 84) / ((85 . 86) / (87 . 88)) / ((89 . 90) / (91 . 92) / ((93 . 94) / (95 . 96)))) /
                ((97 . 98) / (99 . 100) / ((101 . 102) / (103 . 104)) / ((105 . 106) / (107 . 108) / ((109 . 110) / (111 . 112))) /
                  ((113 . 114) / (115 . 116) / ((117 . 118) / (119 . 120)) / ((121 . 122) / (123 . 124) / ((125 . 126) / (127 . 128))))))))
            '(((((((1 . 2) / (3 . 4)) / ((5 . 6) * (7 . 8))) / (((9 . 10) / (11 . 12)) / ((13 . 14) / (15 . 16)))) /
                ((((17 . 18) / (19 . 20)) / ((21 . 22) / (23 . 24))) / (((25 . 26) / (27 . 28)) / ((29 . 30) / (31 . 32)))))
                /
                (((((33 . 34) / (35 . 36)) / ((37 . 38) / (39 . 40))) * (((41 . 42) / (43 . 44)) / ((45 . 46) / (47 . 48)))) /
                ((((49 . 50) / (51 . 52)) / ((53 . 54) / (55 . 56))) / (((57 . 58) / (59 . 60)) / ((61 . 62) / (63 . 64))))))
              /
              ((((((65 . 66) / (67 . 68)) / ((69 . 70) / (71 . 72))) / (((73 . 74) / (75 . 76)) / ((77 . 78) / (79 . 80)))) /
                ((((81 . 82) / (83 . 84)) / ((85 . 86) / (87 . 88))) / (((89 . 90) / (91 . 92)) / ((93 . 94) / (95 . 96)))))
                /
                (((((97 . 98) / (99 . 100)) / ((101 . 102) / (103 . 104))) / (((105 . 106) / (107 . 108)) / ((109 . 110) / (111 . 112)))) /
                ((((113 . 114) / (115 . 116)) / ((117 . 118) / (119 . 120))) / (((121 . 122) / (123 . 124)) / ((125 . 126) / (127 . 128)))))))
            )

  ;; simplify
  (test-case "5.24"
             (simplify
              '(0 * 0 * (0 * 0) * (0 * 0 * (0 * 0)) * (0 * 0 * (0 * 0) * (0 * 0 * (0 * 0))) *
                (0 * 0 * (0 * 0) * (0 * 0 * (0 * 0)) * (0 * 0 * (0 * 0) * (0 * 0 * (0 * 0))))
                *
                (0 * 0 * (0 * 0) * (0 * 0 * (0 * 0)) * (0 * 0 * (0 * 0) * (0 * 0 * (0 * 0))) *
                 (0 * 0 * (0 * 0) * (0 * 0 * (0 * 0)) *
                  (0 * 0 * (0 * 0) * (0 * 0 * (0 * 0)))))))
             0)
  (test-case "5.25"
             (simplify
              '(0 + 0 + (0 + 0) + (0 + 0 + (0 + 0)) + (0 + 0 + (0 + 0) + (0 + 0 + (0 + 0))) +
                (0 + 0 + (0 + 0) + (0 + 0 + (0 + 0)) + (0 + 0 + (0 + 0) + (0 + 0 + (0 + 0))))
                +
                (0 + 0 + (0 + 0) + (0 + 0 + (0 + 0)) + (0 + 0 + (0 + 0) + (0 + 0 + (0 + 0))) +
                 (0 + 0 + (0 + 0) + (0 + 0 + (0 + 0)) +
                  (0 + 0 + (0 + 0) + (0 + 0 + (0 + 0)))))))
             0)
  (test-case "5.26"
             (simplify
              '(0 - 0 - (0 - 0) - (0 - 0 - (0 - 0)) - (0 - 0 - (0 - 0) - (0 - 0 - (0 - 0))) -
                (0 - 0 - (0 - 0) - (0 - 0 - (0 - 0)) - (0 - 0 - (0 - 0) - (0 - 0 - (0 - 0))))
                -
                (0 - 0 - (0 - 0) - (0 - 0 - (0 - 0)) - (0 - 0 - (0 - 0) - (0 - 0 - (0 - 0))) -
                 (0 - 0 - (0 - 0) - (0 - 0 - (0 - 0)) -
                  (0 - 0 - (0 - 0) - (0 - 0 - (0 - 0)))))))
             0)
  (test-case "5.27"
             (simplify
              '(0 + 0 - (0 * 0) + (0 - 0 * (0 + 0)) - (0 * 0 + (0 - 0) * (0 + 0 - (0 * 0))) +
                (0 - 0 * (0 + 0) - (0 * 0 + (0 - 0)) * (0 + 0 - (0 * 0) + (0 - 0 * (0 + 0))))
                -
                (0 * 0 + (0 - 0) * (0 + 0 - (0 * 0)) + (0 - 0 * (0 + 0) - (0 * 0 + (0 - 0))) *
                 (0 + 0 - (0 * 0) + (0 - 0 * (0 + 0)) -
                  (0 * 0 + (0 - 0) * (0 * 0 - (0 * 0)))))))
             0)
  (test-case "5.28"
             (simplify
              '(0 / 0 / (0 / 0) / (0 / 0 / (0 / 0)) / (0 / 0 / (0 / 0) / (0 / 0 / (0 / 0))) /
                (0 / 0 / (0 / 0) / (0 / 0 / (0 / 0)) / (0 / 0 / (0 / 0) / (0 / 0 / (0 / 0))))
                *
                (0 / 0 / (0 / 0) / (0 / 0 / (0 / 0)) / (0 / 0 / (0 / 0) / (0 / 0 / (0 / 0))) /
                 (0 / 0 / (0 / 0) / (0 / 0 / (0 / 0)) /
                  (0 / 0 / (0 / 0) / (0 / 0 / (0 / 0)))))))
             'ZERODIVIDE-ERROR)
  (test-case "5.29"
             (simplify
              '(0 / 2 / (1 / 2) / (1 / 2 / (1 / 2)) / (1 / 2 / (1 / 2) / (1 / 2 / (1 / 2))) /
                (1 / 2 / (1 / 2) / (1 / 2 / (1 / 2)) / (1 / 2 / (1 / 2) / (1 / 2 / (1 / 2))))
                *
                (1 / 2 / (1 / 2) / (1 / 2 / (1 / 2)) / (1 / 2 / (1 / 2) / (1 / 2 / (1 / 2))) /
                 (1 / 2 / (1 / 2) / (1 / 2 / (1 / 2)) /
                  (1 / 2 / (1 / 2) / (1 / 2 / (1 / 2)))))))
             0)
  (test-case "5.30"
             (simplify
              '(1 / 2 / (1 / 2) / (1 / 2 / (1 / 2)) / (1 / 2 / (1 / 2) / (1 / 2 / (1 / 2))) /
                (1 / 2 / (1 / 2) / (1 / 2 / (1 / 2)) / (1 / 2 / (1 / 2) / (1 / 2 / (1 / 2))))
                *
                (1 / 2 / (1 / 2) / (1 / 2 / (1 / 2)) / (1 / 2 / (1 / 2) / (1 / 2 / (1 / 2))) /
                 (1 / 2 / (1 / 2) / (1 / 2 / (1 / 2)) /
                  (1 / 2 / (1 / 2) / (1 / 2 / (1 / 2)))))))
             1)
  (test-case "5.31"
             (simplify
              '(1 / 2 / (1 / 2) / (1 / 2 / (1 / 2)) / (1 / 2 / (1 / 2) / (1 / 2 / (1 / 2))) /
                (1 / 2 / (1 / 2) / (1 / 2 / (1 / 2)) / (1 / 2 / (1 / 2) / (1 / 2 / (1 / 2))))
                *
                (1 / 2 / (1 / 2) / (1 / 2 / (1 / 2)) / (1 / 2 / (1 / 2) / (1 / 2 / (1 / 2))) /
                 (1 / 2 / (1 / 2) / (1 / 2 / (1 / 2)) /
                  (1 / 2 / (1 / 2) / (1 / 2 / (1 / 2)))))))
             1)
  (test-case "5.32"
             (simplify
              '(1 / (2 . 3) / (1 / (2 . 3)) / (1 / (2 . 3) / (1 / (2 . 3))) / (1 / (2 . 3) / (1 / (2 . 3)) / (1 / (2 . 3) / (1 / (2 . 3)))) /
                (1 / (2 . 3) / (1 / (2 . 3)) / (1 / (2 . 3) / (1 / (2 . 3))) / (1 / (2 . 3) / (1 / (2 . 3)) / (1 / (2 . 3) / (1 / (2 . 3)))))
                /
                (1 / (2 . 3) / (1 / (2 . 3)) / (1 / (2 . 3) / (1 / (2 . 3))) / (1 / (2 . 3) / (1 / (2 . 3)) / (1 / (2 . 3) / (1 / (2 . 3)))) /
                 (1 / (2 . 3) / (1 / (2 . 3)) / (1 / (2 . 3) / (1 / (2 . 3))) /
                  (1 / (2 . 3) / (1 / (2 . 3)) / (1 / (2 . 3) / (1 / (2 . 3))))))))
             1)

  ;; ; substitutevar
  (test-case "6.11"
             (substitutevar
              '((a 1) (b 1) (c 1) (d 1) (e 1) (f 1) (g 1) (h 1) (i 1) (j 1) (k 1) (l 1) (m 1) (n 1) (o 1) (p 1) (q 1) (r 1) (s 1) (t 1) (u 1) (v 1) (w 1) (x 1) (y 1) (z 1))
              '(a + b + c + d + e + f + g + h + i + j + k + l + m + n + o + p + q + r + s + t + u + v + w + x + y + z))
             '(1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1))
  (test-case "6.12"
             (substitutevar
              '((a 2) (b 2) (c 2) (d 2) (e 2) (f 2) (g 2) (h 2) (i 2) (j 2) (k 2) (l 2) (m 2) (n 2) (o 2) (p 2) (q 2) (r 2) (s 2) (t 2) (u 2) (v 2) (w 2) (x 2) (y 2) (z 2))
              '(a + b + c + d + e + f + g + h + i + j + k + l + m + n + o + p + q + r + s + t + u + v + w + x + y + z))
             '(2 + 2 + 2 + 2 + 2 + 2 + 2 + 2 + 2 + 2 + 2 + 2 + 2 + 2 + 2 + 2 + 2 + 2 + 2 + 2 + 2 + 2 + 2 + 2 + 2 + 2))
  (test-case "6.13"
             (substitutevar
              '((a (2 . 3)) (b (2 . 3)) (c (2 . 3)) (d (2 . 3)) (e (2 . 3)) (f (2 . 3)) (g (2 . 3)) (h (2 . 3)) (i (2 . 3)) (j (2 . 3)) (k (2 . 3)) (l (2 . 3)) (m (2 . 3)) (n (2 . 3)) (o (2 . 3)) (p (2 . 3)) (q (2 . 3)) (r (2 . 3)) (s (2 . 3)) (t (2 . 3)) (u (2 . 3)) (v (2 . 3)) (w (2 . 3)) (x (2 . 3)) (y (2 . 3)) (z (2 . 3)))
              '(a + b + c + d + e + f + g + h + i + j + k + l + m + n + o + p + q + r + s + t + u + v + w + x + y + z))
             '((2 . 3) + (2 . 3) + (2 . 3) + (2 . 3) + (2 . 3) + (2 . 3) + (2 . 3) + (2 . 3) + (2 . 3) + (2 . 3) + (2 . 3) + (2 . 3) + (2 . 3) + (2 . 3) + (2 . 3) + (2 . 3) + (2 . 3) + (2 . 3) + (2 . 3) + (2 . 3) + (2 . 3) + (2 . 3) + (2 . 3) + (2 . 3) + (2 . 3) + (2 . 3)))
  (test-case "6.14"
             (substitutevar
              '((a 1) (b 2) (c 3) (d 4) (e 5) (f 6) (g 7) (h 8) (i 9) (j 10) (k 11) (l 12) (m 13) (n 14) (o 15) (p 16) (q 17) (r 18) (s 19) (t 20) (u 21) (v 22) (w 23) (x 24) (y 25) (z 26))
              '(a + b - c * d / e + f - g * h / i + j - k * l / m + n - o * p / q + r - s * t / u + v - w * x / y + z))
             '(1 + 2 - 3 * 4 / 5 + 6 - 7 * 8 / 9 + 10 - 11 * 12 / 13 + 14 - 15 * 16 / 17 + 18 - 19 * 20 / 21 + 22 - 23 * 24 / 25 + 26))
  (test-case "6.15"
             (substitutevar
              '((z 26) (y 25) (x 24) (w 23) (v 22) (u 21) (t 20) (s 19) (r 18) (q 17) (p 16)
                (o 15) (n 14) (m 13) (l 12) (k 11) (j 10) (i 9) (h 8) (g 7) (f 6) (e 5) (d 4)
                (c 3) (b 2) (a 1))
              '(a + b - c * d / e + f - g * h / i + j - k * l / m + n - o * p / q + r - s * t / u + v - w * x / y + z))
             '(1 + 2 - 3 * 4 / 5 + 6 - 7 * 8 / 9 + 10 - 11 * 12 / 13 + 14 - 15 * 16 / 17 + 18 - 19 * 20 / 21 + 22 - 23 * 24 / 25 + 26))
  (test-case "6.16"
             (substitutevar
              '((z 26) (y 25) (x 24) (w 23) (v 22) (u 21) (t 20) (s 19) (r 18) (q 17) (p 16)
                (o 15) (n 14) (m 13) (l 12) (k 11) (j 10) (i 9) (h 8) (g 7) (f 6) (e 5) (d 4)
                (c 3) (b 2) (a 1))
              '(a + a - a * a / a + a - a * a / a + a - a * a / a + a - a * a / a + a - a * a / a + a - a * a / a + a))
             '(1 + 1 - 1 * 1 / 1 + 1 - 1 * 1 / 1 + 1 - 1 * 1 / 1 + 1 - 1 * 1 / 1 + 1 - 1 * 1 / 1 + 1 - 1 * 1 / 1 + 1))
  (test-case "6.17"
             (substitutevar
              '((a 1) (b 2) (c 3) (d 4) (e 5) (f 6) (g 7) (h 8) (i 9) (j 10) (k 11) (l 12) (m 13) (n 14) (o 15) (p 16) (q 17) (r 18) (s 19) (t 20) (u 21) (v 22) (w 23) (x 24) (y 25) (z 26))
              '(a + (b - (c * (d / (e + (f - (g * (h / (i + (j - (k * (l / (m + (n - (o * (p / (q + (r - (s * (t / (u + (v - (w * (x / (y + z))))))))))))))))))))))))))
             '(1 + (2 - (3 * (4 / (5 + (6 - (7 * (8 / (9 + (10 - (11 * (12 / (13 + (14 - (15 * (16 / (17 + (18 - (19 * (20 / (21 + (22 - (23 * (24 / (25 + 26))))))))))))))))))))))))))

  ;; ; simplifyvar
  (test-case "7.17"
             (simplifyvar
              '((a 1) (b 2) (c 3) (d 4) (e 5) (f 6) (g 7) (h 8) (i 9) (j 10) (k 11) (l 12) (m 13) (n 14) (o 15) (p 16) (q 17) (r 18) (s 19) (t 20) (u 21) (v 22) (w 23) (x 24) (y 25) (z 26))
              '(a + b - c * d / e + f - g * h / i + j - k * l / m + n - o * p / q + r - s * t / u + v - w * x / y + z))
             '(9025949 . 348075))
  (test-case "7.18"
             (simplifyvar
              '((a 1) (b 2) (c 3) (d 4) (e 5) (f 6) (g 7) (h 8) (i 9) (j 10) (k 11) (l 12) (m 13) (n 14) (o 15) (p 16) (q 17) (r 18) (s 19) (t 20) (u 21) (v 22) (w 23) (x 24) (y 0) (z 26))
              '(a + b - c * d / e + f - g * h / i + j - k * l / m + n - o * p / q + r - s * t / u + v - w * x / y + z))
             'zerodivide-error)
  (test-case "7.19"
             (simplifyvar
              '((a (1 / 1))
                (b (1 + 1))
                (c (2 * 1 + 1))
                (d (2 * 2))
                (e (3 * 2 - 247 / 247))
                (f (37 - 37 + 2 * 3))
                (g ((-1 . 7) + (-1 . 7) + (-1 . 7) + (-1 . 7) + (-1 . 7) + (-1 . 7) + (-1 . 7) + 8))
                (h (2 * (2 * 2)))
                (i (27 / 3))
                (j ((2 * 5 / 10 * 1 + 1 * 10 / 2 * 2) - 1))
                (k (55 / 5 * 6 / 11 + 5))
                (l (2 * 2 * 3))
                (m (3 * 2 - 7 / 7 + 1 * 2 * 4))
                (n (((-1 . 7) + (-1 . 7) + (-1 . 7) + (-1 . 7) + (-1 . 7) + (-1 . 7) + (-1 . 7)) * (0 - 2 * 7)))
                (o (3 * (1 + 2 + 3) - 3))
                (p (2 * 2 * 2 * 2))
                (q (-1 * 7 + 1 * 2 * 3 * 4))
                (r (1 * 2 * 3 * 4 * 5 / 2 / 5 + 6))
                (s (1 * 2 + 3 * 4 + 5))
                (t (1 / 2 / (1 / 40)))
                (u (1 + 2 + 3 + 4 + 5 + 6))
                (v (55 / 11 * 4 + 2))
                (w (-1 - -2 * 3 * 4))
                (x (2 * 3 * 4))
                (y ((1 * 2 + 3) * 4 + 5))
                (z (13 * -1 - 26 * -1 + 13)))
              '(a + b - c * d / e + f - g * h / i + j - k * l / m + n - o * p / q + r - s * t / u + v - w * x / y + z))
             '(9025949 . 348075))

  )
