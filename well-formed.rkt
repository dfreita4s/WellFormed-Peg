#lang racket
(require "verf-well-formed.rkt")
(require rackcheck)
(require peg-gen)

;(sample (gen:peg 1 2 3) 2);; Testing if the generated PEG is Well-Formed

(define (testPEG e)
  (print e)
  (if (is-WF (getGrammar e) (getExpression e) '())
      (println #t)
      (begin
        (println e)
        (exit 0))) ;; If isn't Well-Formed then we print the expression and stop
  )

;; Helper function to get Grammar and the Expression from randPEG

(define (getGrammar e)
  (print "Grammar: ")
  (print (car e))
  (car e)
  )

(define (getExpression e)
  (print "Expression: ")
  (print (cdr e))
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
      (display "Fim")
      )
  )

(testLoop 2)
;(testLoop 100)