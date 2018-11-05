;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Exercise 1.2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(/ (+ 5
      (/ 1.0 2)
      (- 2
         (- 3
            (+ 6
               (/ 1.0 5)))))
   (* 3
      (- 6 2)
      (- 2 7)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Exercise 1.3
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (square x) (* x x))

(define (add-max2 a b c)
  (if (> a b)
    (+ (square a)
       (square (if (> b c) b c)))
    (if (> a c)
      (+ (square a) (square b))
      (+ (square b) (square c)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Exercise 1.4
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (a-plus-abs-b a b)      ; a + abs(b)
  ((if (> b 0) + -) a b))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Exercise 1.5
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; If applicative-order evaluation is used, then the test function will go into
; infinte recursion.
; But if the interpreter used normal-order evaluation, then zero would be
; returned, as (p) need not then be evaluated


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Exercise 1.6
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; With new-if, infinite recursion will happen, as both if-clause and
; else-clause will be evaluated so that their values can be passed to (new-if).
; The (sqrt-iter) in the else-clause will cause the infinite loop.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Exercise 1.7
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (sqrt2 x)
  (sqrt-recursive 1.0 x))

(define (sqrt-recursive guess x)
  (if (good-enough? guess x)
    guess
    (sqrt-recursive (improve guess x)
                    x)))

; This is the only change
(define (good-enough? guess x)
  (< (/ (abs (- guess (improve guess x))) guess) 0.001))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (square x)
  (* x x))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Exercise 1.8
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (cube-root x) (cube-root-recursive 1.0 x))

(define (cube-root-recursive guess x)
  (if (good-enough3? guess x)
    guess
    (cube-root-recursive (improve3 guess x) x)))

(define (good-enough3? guess x)
  (< (/ (abs (- guess (improve3 guess x))) guess) 0.0001))

(define (improve3 guess x)
  (average3 (/ x (square guess)) guess guess))

(define (average3 x y z)
  (/ (+ x y z) 3))

(define (cube x) ( * x x x))
