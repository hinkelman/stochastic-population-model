#lang typed/racket

(require math)

(provide logmod-typed repeat-logmod-typed)

(: logmod-typed : Integer Flonum Flonum Flonum Flonum -> (Listof Flonum))
(define (logmod-typed t y r k thetasd)

  (: calc : Flonum -> Flonum)
  (define (calc y)
    (define theta (flvector-ref (flnormal-sample 0.0 thetasd 1) 0))
    (* y (- r (* r (/ y k))) (exp theta)))

  (: loop : Flonum Integer -> (Listof Flonum))
  (define (loop y i)
    (if (= i t)
        (list y)
        (cons y (loop (calc y) (add1 i)))))
  (loop y 1))

(: repeat-logmod-typed : Integer Integer Flonum Flonum Flonum Flonum -> (Listof (Listof Flonum)))
(define (repeat-logmod-typed reps t y r k thetasd)
  (for/list ([i (in-range reps)]) (logmod-typed t y r k thetasd)))


