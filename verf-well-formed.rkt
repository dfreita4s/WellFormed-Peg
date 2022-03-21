#lang racket
(require racket/match)
(require "./peg.rkt")
;(require "./judgments.rkt")
;(require redex) ; nao usar redex
(provide (all-defined-out))

#;(define-judgment-form Peg
    #:mode (WF? I O)
    #:contract (WF? state boolean)

    [(side-condition (is-WF (input-grammar state) (input-peg state) '()))
     -------------------------------
     (WF? state #t)]
  
    )

;; Helpers functions 
; If not consumed return false 

(define (choice e1 e2)
  (if (member '0 (list e1))
      #f
      #t)
  )

(define (sequence e1 e2) 
  (if (and (member '0 (list e1) ) (member '0 (list e2)))
      #f
      #t)
  )

(define (not-predicate e)
  (if (member 'f (list e))
      #f
      #t
      )
  )

(define (kleene e)
  (if (or (member '0 (list e)) (member  'f (list e)))
      #f
      #t
      )
  )


;; Analysis of Grammars
;; If e ⇀ 0, then e might succeed while consuming no input
;; If e ⇀ 1, then e might succeed while consuming at least one terminal
;; If e ⇀ f, then e might fail on some input
;; If e ⇀ s, then e outcome of either 0 or 1
(define (zero⇀? grammar e) 
  (if (list? e)
      (let ((id (car e)))
        (cond [(eq? id '/)  (choice (zero⇀? grammar (cadr e)) (zero⇀? grammar (caddr e)))]
              [(eq? id '•)  (sequence (zero⇀? grammar (cadr e)) (zero⇀? grammar (caddr e)))]
              [(eq? id '!)  (not-predicate (zero⇀? grammar (cadr e)))]
              [(eq? id '*)  (kleene (zero⇀? grammar (cadr e)))]
              [else  #f] 
              )

        )
      (cond [(number? e) '(1 'f)] 
            [(eq? e 'ε)  '0]
            [else (zero⇀? grammar (lookup-nt grammar e))] 
            
            )
      )
  )

(define (⇀0? xs)
  (member 0 xs))

(define (⇀f? xs)
  (member 'f xs))

(define (⇀s? xs)
  (or (member 0 xs) (member 1 xs)))


; (⇀ '(A ε ∅) 'A) fazer o resto * ! bolinha
; (/ 1 ε)
; (⇀ '∅ '1) ==> '(1 f)
; (⇀ '∅ 'ε) ==> '(0)
; (⇀ '∅ '(/ 1 ε)) ==> '(1 0)
(define (⇀ grammar e) 
  (match e
    ['ε '(0)]
    [(list '/ e1 e2) (let* ([r1 (⇀ grammar e1)]
                            [r2 (⇀ grammar e2)])
                       (if (⇀f? r1) 
                           (append r2 (remove 'f r1))
                           r1))]
    [(list '• e1 e2) (let* ([r1 (⇀ grammar e1)]
                            [r1 (⇀ grammar e2)])
                       (if ()))]   ;; inicio do sequence
    [(? number?) '(1 f)]
    [(? symbol?)  (⇀ grammar (lookup-nt grammar e))]
    )
  )
(define nt '())
#;(define (verf-judg-nt grammar exp) 

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
        (cond [(eq? id '/)  (and (is-WF grammar (cadr e) non-terminal) (is-WF grammar (caddr e) non-terminal))] 
              [(eq? id '•)  (and (is-WF grammar (cadr e) non-terminal)
                                 (or (zero⇀? grammar (cadr e)) ;; precisa desse or?
                                     (is-WF grammar (caddr e) non-terminal)))]
              [(eq? id '!)  (is-WF grammar (cadr e) non-terminal)]
              [(eq? id '*)  (and (is-WF grammar (cadr e) non-terminal)
                                 (zero⇀? grammar (cadr e)))]
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
