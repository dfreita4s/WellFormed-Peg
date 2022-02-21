#lang racket
(require rackcheck)
(require peg-gen)

(sample (gen:peg 1 2 3) 2)