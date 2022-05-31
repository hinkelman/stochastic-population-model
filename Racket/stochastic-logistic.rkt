#lang racket

(require math
         "stochastic-logistic-typed.rkt")

(define YINIT 1.0)        ; initial population size
(define R 1.4)            ; maximum population growth rate
(define K 20.0)           ; carrying capacity
(define THETASD 0.1)      ; standard deviation for adding noise to population
(define T 100)            ; number of years to run simulation
(define REPS 1000)        ; number of replications
(define T2 (* T REPS))    ; used to compare difference between long-running simulation and many calls to simulation
(define TIME-SAMPLES 100) ; number of samples to run when timing functions

(define (logmod t y r k thetasd)

  (define (calc y)
    (define theta (flvector-ref (flnormal-sample 0.0 thetasd 1) 0))
    (* y (- r (* r (/ y k))) (exp theta)))
    
  (define (loop y i)
    (if (= i t)
        (list y)
        (cons y (loop (calc y) (add1 i)))))
  (loop y 1))

;; literal translation of R code
(define (logmod-vec t y r k thetasd)
  (define y-vec (make-vector t))
  (vector-set! y-vec 0 y)
  (define theta-vec (flnormal-sample 0.0 thetasd t))
  (for ([i (in-range 1 t)])
    (define last-y (vector-ref y-vec (sub1 i)))
    (define theta (flvector-ref theta-vec i))
    (vector-set! y-vec i (* last-y (- r (* r (/ last-y k))) (exp theta))))
  y-vec)

(define (time-apply-cpu proc lst reps)
  (define out
    (for/list ([i (in-range reps)])
      (define-values (results cpu-time real-time gc-time) (time-apply proc lst))
      cpu-time))
  (displayln (string-append "min: " (number->string (apply min out))
                            " mean: " (number->string (round (mean out)))
                            " max: " (number->string (apply max out))
                            "    function: "
                            (symbol->string (object-name proc)))))

(time-apply-cpu logmod (list T2 YINIT R K THETASD) TIME-SAMPLES)
(time-apply-cpu logmod-typed (list T2 YINIT R K THETASD) TIME-SAMPLES)
(time-apply-cpu logmod-vec (list T2 YINIT R K THETASD) TIME-SAMPLES)

(define (repeat-logmod reps t y r k thetasd)
  (for/list ([i (in-range reps)]) (logmod t y r k thetasd)))
(define (repeat-logmod-vec reps t y r k thetasd)
  (for/list ([i (in-range reps)]) (logmod-vec t y r k thetasd)))

(time-apply-cpu repeat-logmod (list REPS T YINIT R K THETASD) TIME-SAMPLES)
(time-apply-cpu repeat-logmod-typed (list REPS T YINIT R K THETASD) TIME-SAMPLES)
(time-apply-cpu repeat-logmod-vec (list REPS T YINIT R K THETASD) TIME-SAMPLES)
