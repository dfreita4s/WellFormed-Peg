#lang racket
(require "./peg.rkt")
(require "./judgments.rkt")
(require redex) ; nao usar redex
(provide (all-defined-out))

(define-judgment-form Peg
  #:mode (WF? I O)
  #:contract (WF? state boolean)

  [(side-condition (is-WF (input-grammar state) (input-peg state) '()))
   -------------------------------
   (WF? state #t)]
  
  )

;; Helpers functions 
 ; If not consumed return false 

(define (choice e1 e2)
  (if (member (term 0) (list e1))
      #f
      #t)
  )

(define (sequence e1 e2) 
  (if (and (member (term 0) (list e1) ) (member (term 0) (list e2)))
      #f
      #t)
  )

(define (not-predicate e)
  (if (member (term f) (list e))
      #f
      #t
      )
  )

(define (kleene e)
  (if (or (member (term 0) (list e)) (member (term f) (list e)))
      #f
      #t
      )
  )


;; Analysis of Grammars
;; If e ⇀ 0, then e might succeed while consuming no input
;; If e ⇀ 1, then e might succeed while consuming at least one terminal
;; If e ⇀ f, then e might fail on some input
;; If e ⇀ s, then e outcome of either 0 or 1
(define (zero⇀? grammar e non-terminal) 
  (if (list? e)
      (let ((id (car e)))
        (cond [(eq? id '/)  (choice (zero⇀? grammar (cadr e) non-terminal) (zero⇀? grammar (caddr e) non-terminal))]
              [(eq? id '•)  (sequence (zero⇀? grammar (cadr e) non-terminal) (zero⇀? grammar (caddr e) non-terminal))]
              [(eq? id '!)  (not-predicate (zero⇀? grammar (cadr e) non-terminal))]
              [(eq? id '*)  (kleene (zero⇀? grammar (cadr e) non-terminal))]
              [else  #f] 
              )

        )
      (cond [(number? e) '(1 (term f))] 
            [(eq? e 'ε)  (term 0)]
            [(not (eq? grammar '∅)) (if (verifica-list-nonterminal grammar e non-terminal)
                                        (is-WF grammar (lookup-nt grammar e) (append non-terminal e)) 
                                        #f)] 
            [else  #f]
            )
      )
  )

(define nt '())
(define (verf-judg-nt grammar exp) 

  (define result (judgment-holds (lookup ,grammar ,exp R) R)) ;; trocar para uma funcao em racket
  
  (if (member (term ⊥) (judgment-holds (lookup ,grammar ,exp R) R));;
      #f
      (car result) ;ele sai do lookup como uma lista, ex.: '(ε), precisamos do termo puro, então fazemos o car
      )
  )
  

(define (lookup-nt grammar snt)
  (if (eq? grammar '∅)
      #f
      (let ([nt (car grammar)]
            [exp-nt (second grammar)]
            )
        (if (eq? nt snt)
            exp-nt
            (lookup-nt (third grammar) snt))
        )
      )
  )

(lookup-nt '(A (/ 0 B) (B A ∅)) 'A)

(define (verifica-list-nonterminal grammar exp non-terminal)
  ;(define result (judgment-holds (lookup ,grammar ,exp R) R))
  (println non-terminal)
  (if (> (length (list non-terminal)) 1)
      (if (check-duplicates non-terminal)
          #f
          #t)
      #t)
  )

(define (is-WF grammar e non-terminal) ; (grammar expression non-terminal)
  (if (list? e)
      (let ((id (car e)))
        (cond [(eq? id '/)  (and (is-WF grammar (cadr e) non-terminal) (is-WF grammar (caddr e) non-terminal))] ;; aqui teria que ser um or, nao?
              [(eq? id '•)  (and (is-WF grammar (cadr e) non-terminal)
                                 (or (zero⇀? grammar (cadr e)) ;; precisa desse or?
                                     (is-WF grammar (caddr e) non-terminal)))]
              [(eq? id '!)  (is-WF grammar (cadr e) non-terminal)]
              [(eq? id '*)  (and (is-WF grammar (cadr e) non-terminal)
                                 (zero⇀? grammar (cadr e) non-terminal))]
              [else  #f] 
              )

        )
      (cond [(number? e) #t]
            [(eq? e 'ε)  #t]
            [(not (eq? grammar '∅)) (if (verifica-list-nonterminal grammar e non-terminal)
                                        (is-WF grammar (lookup-nt grammar e) (cons non-terminal e)) 
                                        #f)] 
            [else  #f]
            )
      )
 
  )


(is-WF '(A (/ 0 B) (B 0 ∅)) '(* A) '())
;(is-WF '(A (/ 0 B) (B A) ∅) '(* A) '())
;(is-WF '∅ '(*(/ ε 1)) '())

; Start function
(define (test-WF e)
  (is-WF (car e) (list-ref (cdr e) 2) '())
  )

;(test-WF '((A (/ 0 1) (B 0)) A))
(define (getGrammar expL)
  (car expL)
  )

(define (getExp expL) 
  (list-ref (cdr expL) 2)
  (display "\n")
  (display (list-ref (cdr expL) 2))
  )
