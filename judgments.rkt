#lang racket
(require "./peg.rkt")
(require redex)
(provide (all-defined-out))

(define-extended-language WFevalPeg Grammar
  [E (e s)]
  [R e ⊥]
  [D S ⊥]
  [S 0 1])


; Judgment to look up for the value of some grammar
; Return the value (peg or fail)
(define-judgment-form WFevalPeg 
  #:mode (lookup I I O)
  #:contract (lookup G x R)
  
  [--------------------------------
   (lookup (x_1 e G) x_1 e)]

  [--------------------------------
   (lookup ∅ x ⊥)]

  [(lookup G x_2 R)
   (side-condition (diffs? x_1 x_2))
   --------------------------------
   (lookup (x_1 e_1 G) x_2 R)] 
  )

; Judgment to evaluate if a peg consumes a entry
; Return what left of the entry

; Judgment to verify if the grammar consumes a entry
; Return:
; 0: succed while consuming no input
; 1: succed while consuming at least one terminal
; ⊥: fail on some input
(define-judgment-form WFevalPeg 

  #:mode (⇀ I I O)
  #:contract (⇀ G e D)

  ;Empty
  [-------------------------------
   (⇀ G ε 0)]

  ;Terminal
  [-------------------------------
   (⇀ G natural 1)]

  [-------------------------------
   (⇀ G natural ⊥)]

  ;Non-Terminal
  [(lookup G x e)
   (⇀ G e D)
   -------------------------------
   (⇀ G x D)]

  #;[(lookup G x ⊥)
     -------------------------------
     (⇀ G x ⊥)]
 
  ;Sequence
  [(⇀ G e_1 0)
   (⇀ G e_2 0)
   -------------------------------
   (⇀ G (• e_1 e_2) 0)]

  [(⇀ G e_1 1)
   (⇀ G e_2 S)
   -------------------------------
   (⇀ G (• e_1 e_2) 1)]

  [(⇀ G e_1 0)
   (⇀ G e_2 1)
   -------------------------------
   (⇀ G (• e_1 e_2) 1)]

  [(⇀ G e_1 ⊥)
   -------------------------------
   (⇀ G (• e_1 e_2) ⊥)]
  
  [(⇀ G e_1 S)
   (⇀ G e_2 ⊥)
   -------------------------------
   (⇀ G (• e_1 e_2) ⊥)]

  ;Choice
  [(⇀ G e_1 S)
   -------------------------------
   (⇀ G (/ e_1 e_2) S)]
  
  [(⇀ G e_1 ⊥)
   (⇀ G e_2 D)
   -------------------------------
   (⇀ G (/ e_1 e_2) D)]

  ;Repetition
  [(⇀ G e 1)
   -------------------------------
   (⇀ G (* e) 1)]

  [(⇀ G e 0)
   -------------------------------
   (⇀ G (* e) 0)]

  ;Not
  [(⇀ G e S)
   -------------------------------
   (⇀ G (! e) ⊥)]

  [(⇀ G e ⊥)
   ;(side-condition (not-zero? e))
   -------------------------------
   (⇀ G (! e) 0)]
 
  [(⇀ G e ⊥)
   -------------------------------
   (⇀ G (! e) 1)]
  )