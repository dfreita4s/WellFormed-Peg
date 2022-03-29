#lang racket
(require "verf-well-formed.rkt")
(require rackunit)
(require rackcheck)
(require peg-gen)

;(sample (gen:peg 1 2 3) 2) 

;; Testing if the generated PEG is Well-Formed

(define (testPEG e)
  ;(println e)
  (if (is-WF (getGrammar e) (getExpression e) '())
      (println #t)
      (begin
        (println e)
        (println #f)
        (exit 0))) ;; If isn't Well-Formed then we print the expression and stop
  )

;; Helper function to get Grammar and the Expression from randPEG

(define (getGrammar e)
  ;(display "Grammar: ")
  ;(println (car e))
  (car e)
  )

(define (getExpression e)
  ;(display "Expression: ")
  ;;(println (car (cdr e)))
  (car (cdr e))
  )


;; Test generation 

(define (testLoop n)
  (define listRandomNumber (sample (gen:one-of '(1 2 3)) 3)) 
  (if (> n 0) 
      (begin
        (testPEG (car (sample (gen:peg (car listRandomNumber)
                                       (cadr listRandomNumber)
                                       (last listRandomNumber)) 1)))
        (testLoop (- n 1)))
      (display "All tests passed successfully")
      )
  )


;(testPEG '(∅ (* ε) ())) ;; Grammar Expression ()?
;(testLoop 2)
;(testLoop 100)     ; oK
;(testLoop 1000)    ; oK
;(testLoop 10000)   ; oK
;(testLoop 100000)  ; oK
;(testLoop 1000000) ; oK

;; (is-WF '(K (/ (• ε 3) (• ε 2)) (C (• (/ 2 E) (! K)) (E (• (/ 1 3) (/ 3 C)) ∅))) '(* (• ε C)) '())

; FAZER pegar todas as gerações ;; (sample (gen:peg 3 2 3) 4)

;(check-property wellformed-ford)
;(check-property (make-config #:tests 20) wellformed-ford)
(define-property wellformed-ford ([peg  (gen:peg 3 5 2)])
    (check-equal?  (is-WF (getGrammar peg) (getExpression peg) '()) #t)
  )

(make-config #:tests 10)

#;(define (allTypesMatch g g1 )
   (andmap (lambda (t) (matchTypes t (assoc (car t) g) )) g1)
  )

#;(define-property type-checks([peg  (gen:peg 3 5 2)])
    (check-equal?  (testgen peg) #t)
  )

#;(define-property type-contexts-match([peg  (gen:peg 3 5 2)])
    (check-equal?  (allTypesMatch (solution2context (infer (peg2struct peg))) (last peg)) #t)
  )


