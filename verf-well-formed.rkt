#lang racket
(require "./peg.rkt")
(require "./judgments.rkt")
(require redex)
(provide (all-defined-out))

(define-judgment-form Peg
  #:mode (WF? I O)
  #:contract (WF? state boolean)

  [(side-condition (is-WF (input-grammar state) (input-peg state) '()))
   -------------------------------
   (WF? state #t)]
  
  )

(define (zero⇀? grammar exp) 
  (if (member 0 (judgment-holds (⇀ ,grammar ,exp D) D))
      #f
      #t
      )
  )

(define nt '())
(define (verf-judg-nt grammar exp non-terminal) 

  (define result (judgment-holds (lookup ,grammar ,exp R) R))
  
 
  (if (member (term ⊥) (judgment-holds (lookup ,grammar ,exp R) R));;
      #f
      (car result) ;ele sai do lookup como uma lista, ex.: '(ε), precisamos do termo puro, então fazemos o car
      ))
  



(define (verifica-list-nonterminal grammar exp non-terminal)
  (define result (judgment-holds (lookup ,grammar ,exp R) R))
  (if (not (null? (list non-terminal)))
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
                                 (or (zero⇀? grammar (cadr e))
                                     (is-WF grammar (caddr e) non-terminal)))]
              [(eq? id '!)  (is-WF grammar (cadr e) non-terminal)]
              [(eq? id '*)  (and (is-WF grammar (cadr e) non-terminal)
                                 ;verifica se a grammar é ∅, se n for, usa o resultado do verf-judg-nt pra verificar o judgment do *
                                 ;pra ele n usar o não terminal puro.
                                 (zero⇀? grammar (cadr e)))]; passar a grammar no verf-judg para nao precisar de verf a gramatica
              [else  #f] 
              )

        )
      (cond [(number? e) #t]
            [(eq? e 'ε)  #t]
            [(not (eq? grammar '∅)) (if (verifica-list-nonterminal grammar e non-terminal)
                                        (is-WF grammar (verf-judg-nt grammar e non-terminal) (cons e non-terminal)) 
                                        #f)] 
            [else  #f]
            )
      )
 
  )
; Start function
(define (test-WF e)
  (is-WF (car e) (list-ref (cdr e) 2) '())

  )

(define (getGrammar expL)
  (car expL)
  )

(define (getExp expL) 
  (list-ref (cdr expL) 2)
  (display "\n")
  (display (list-ref (cdr expL) 2))
  )
