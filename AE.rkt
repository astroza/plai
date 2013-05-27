#lang plai

(define-type AE
  [num (n number?)]
  [add (lhs AE?)
       (rhs AE?)]
  [sub (lhs AE?)
       (rhs AE?)]
  )

(define (parse program)
  (cond
    [(number? program) (num program)]
    [(list? program)
  (case (first program)
    [(+) (add (parse (second program))
              (parse (third program)))]
    [(-) (sub (parse (second program))
              (parse (third program)))]
    )]))

(define (interp an-ae)
  (type-case AE an-ae
    [num (n) n]
    [add (l r) (+ (interp l) (interp r))]
    [sub (l r) (- (interp l) (interp r))]
 ))

