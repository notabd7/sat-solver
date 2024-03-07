#lang racket

(require rackunit)
(require rackunit/text-ui)





;The whole point of this code was to understand the basics of SAT solvers and apply recursion in order to understand the concept using racket.
;I was curious about SAT solvers and wanted to master recursion at leat grasp the concept completely

;I directly relate integers and boolean values.
;then use this ability to generate partial truth tables.
;which are inputs to the function.
;then I create the truth table outputs.
;which is finally used to solve a problems.

; Step1:

;Converingt an Integer to a List of true/false values.
;If we want to convert the integer 9 to a list of t/f values
;we start by finding out its remainder and quotient when divided by 2.
;(remainder 9 2) = 1
;(quotient 9 2) = 4
;This tells us the least significant bit is a 1
;We will represent 1 as #t, so the list is currently (#t)
;Next, we repeat the proccess with the quotient 4
;(remainder 4 2) = 0
;(quotient 4 2) = 2
;Zero is false, so we add this to the list (#f #t)
;Repeat with the quotient 2
;(remainder 2 2) = 0
;(quotient 2 2) = 1
;Zero is false, the list becomes (#f #f #t)
;(remainder 1 2) = 1
;(quotient 1 2) = 0
;One is true, the list because (#t #f #f #t)
;The quotient was zero meaning we can stop.

;I Implement the following function below
;Since we want to line up with standard binary 2^0 being the last value,
;It is easiest to make L a parameter and use a helper function.
;Note: 0 is not supported by this function.
;We will deal with it in the next part.

(define (int_to_bool n)
  (int_to_bool_h n '())
)


(define (int_to_bool_h n L)
  (if (= n 0)
      L
      (int_to_bool_h (quotient n 2)
                     (cons (if (= (remainder n 2) 0) #f #t) L))))


;end
;Tests:
; the commented ouput is the expected output
(display "Test Step 1 \n")
(display "output ' expected \n")
(display (int_to_bool 0))  '() 
(display(int_to_bool 1)) '(#t)
(display (int_to_bool 2)) '(#t #f)
(display (int_to_bool 3)) '(#t #t)

;Step 2
;Only significant binary digits are stored by the above function.
;But we would want every number to have the same bit length.
;so the function below pads #f onto the front of the list.

(define (pad num_bits bit_list)
  (if (= (length bit_list) num_bits)
      bit_list
      (pad num_bits (cons #f bit_list))
)
  )
;end
;Test
(display "Test Step 2 \n")
(display "output ' expected \n")
(display (pad 5 (int_to_bool 10))) '(#f #t #f #t #f)
(display (pad 5 (int_to_bool 11))) '(#f #t #f #t #t)
(display (pad 5 (int_to_bool 12))) '(#f #t #t #f #f)
(display (pad 5 (int_to_bool 13))) '(#f #t #t #f #t)
(display (pad 5 (int_to_bool 14))) '(#f #t #t #t #f)
(display (pad 5 (int_to_bool 15))) '(#f #t #t #t #t)


;Step 3
;Generating a Truth Table
;Given a number of variables n
;I generate a truth table will all variable settings.
;The truth table has rows with values starting at
;2^n-1 and ending at 0.
;For example, the truth tables for n=2 is
;( (#t #t) (#t #f) (#f #t) (#f #f) )




; Helper function for generating Truth Table

(define (tt_inputs n)
  (tt_inputs_h n (- (expt 2 n) 1)))

(define (tt_inputs_h n row_val)
  (if (< row_val 0)
      '()  ; Base case: return an empty list when row_val becomes negative
      (cons (int_to_bool_with_padding row_val n)
            (tt_inputs_h n (- row_val 1)))))

(define (int_to_bool_with_padding n length)
  (pad length (int_to_bool_h n '())))



(define (pad_to_length lst length)
  (if (< (length lst) length)
      (pad_to_length (cons #f lst) length)
      lst))

;end
;Tests
(display "Test Step 3 \n")
(display "output ' expected \n")

(display (tt_inputs 0))
                '(())
  
(display (tt_inputs 1))
                '( (#t) (#f)
  )
(display(tt_inputs 2))
                '( (#t #t)
                   (#t #f)
                   (#f #t)
                   (#f #f))
  
(display(tt_inputs 3))
                '( (#t #t #t)
                   (#t #t #f)
                   (#t #f #t)
                   (#t #f #f)
                   (#f #t #t)
                   (#f #t #f)
                   (#f #f #t)
                   (#f #f #f)
                   )
   
;Step 4
;The inputs we made above have the format '(#t #f #f #t).
;We need boolean expressions that work with this format.
;We will make function that take a list as input
;This function implements (A->B) i.e A implies B
(define (implies_example boolean_vars)
  (let (;Start of name list
        (a (list-ref boolean_vars 0));Pairs (name value)
        (b (list-ref boolean_vars 1))
      );End of name list
    (or (not a) b)
 );end of let
)

;Tests
(display "Test Step 4 \n")
(display "output ' expected \n")
(display(implies_example '(#t #t))) ' #t
(display (implies_example '(#t #f))) ' #f
(display (implies_example '(#f #t))) ' #t
(display (implies_example '(#f #f))) ' #t


;Helper functions these are being implemented here so I can test my SAT solver on them
; Function 1
;implementing ~(~a v (b v ~a) ) to be used later
(define (example_expr1 bool_vars)
  (not (or (not (first bool_vars)) (or (second bool_vars) (not (first bool_vars))))))
  

;end



; Function 2
;implementing (a and b) or (~a and ~b) to be used later
(define (example_expr2 bool_vars)
  (or (and (first bool_vars) (second bool_vars)) (and (not (first bool_vars)) (not (second bool_vars))))
  )


;end


;Function 3
;implementing (a and (not b) and c) to be used later
(define (example_expr3 bool_vars)
  (and (and (first bool_vars) (not (second bool_vars))) (third bool_vars))
)
;end


;Step 5
;function takes
;fun - a function that takes a list of boolean values and returns a boolean
;tt - a truth table (list of lists of T/F values)
;And returns a list of T/F values with results
;For example if fun computes (not a)
;and tt = ( (#t) (#f) )
;Then the return of
;(evaluate_tt fun tt) should be (#f #t)
(define (evaluate_tt fun tt)
  (map fun tt)
  )

;end




;A function that converts a list of T/F values
;back to an integer
;This function is the inverse of step 1
;The list is reversed for you in this function
;
(define (bool_to_int values)
  (bool_to_int_h (reverse values) 0)
)
;Implemented this helper function
;values - the list of #t/#f false
;exp - the current power of 2 you are on
(define (bool_to_int_h values exp)
  (if (null? values)
      0 
      (+ (if (car values)
             (expt 2 exp)
             0)  
         (bool_to_int_h (cdr values) (+ exp 1))

         )))  

;end


; FINALLY
;function that takes a function and the number of variables it has
;and determines which inputs make the function return true
;Display the inputs that make the function return true as integers
;For example (or a b) is true when '(#t #t) '(#t #f) '(#f #t)
;so the answer would be '(3 2 1) it is true for any of those integers
;in binary


(define (sat_solve func n)
  (define tt (tt_inputs n))
  
  (define sat-values (filter func tt))

  
  (map
   (lambda (row) (bool_to_int row))
   sat-values)) 


;Testing

(define-test-suite test_sat_solve
  (check-equal? (sat_solve implies_example 2) '(3 1 0))
  (check-equal? (sat_solve example_expr1 2) '(2))
  (check-equal? (sat_solve example_expr2 2) '(3 0))
  (check-equal? (sat_solve example_expr3 3) '(5))
  (check-equal? (sat_solve (lambda (X) (and (first X) (second X))) 2) '(3))
  (check-equal? (sat_solve (lambda (X) (or (first X) (second X))) 2) '(3 2 1))
)
(display "Results of SAT Solver\n")
(define tests_sat (- 12 (* 2 (run-tests test_sat_solve 'verbose))))
