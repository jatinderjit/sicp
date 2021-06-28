;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Exercise 1.9
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define plus +)

(define (inc x) (plus x 1))
(define (dec x) (- x 1))

(define (+ a b)
  (if (= a 0)
    b
    (inc (+ (dec a) b))))
; This is a recursive process
; (+ 4 5)
; (inc (+ 3 5))
; (inc (inc (+ 2 5)))
; (inc (inc (inc (+ 1 5))))
; (inc (inc (inc (inc (+ 0 5)))))
; (inc (inc (inc (inc 5))))
; (inc (inc (inc 6)))
; (inc (inc 7))
; (inc 8)
; 9

(define (+ a b)
  (if (=a 0)
    b
    (+ (dec a) (inc b))))
; This is an iterative process
; (+ 4 5)
; (+ 3 6)
; (+ 2 7)
; (+ 1 8)
; (+ 0 9)
; 9


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Exercise 1.10
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))
(A 1 10)
; (A 1 n) evaluates to 2^n
; (A 0 (A 1 9))
; (A 0 (A 0 (A 1 8)))
; (A 0 (A 0 (A 0 (A 1 7))))
; (A 0 (A 0 (A 0 (A 0 (A 1 6)))))
; (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 5))))))
; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 4)))))))
; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 3))))))))
; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 2)))))))))
; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 1))))))))))
; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 2)))))))))
; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 4))))))))
; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 8)))))))
; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 16))))))
; (A 0 (A 0 (A 0 (A 0 (A 0 32)))))
; (A 0 (A 0 (A 0 (A 0 64))))
; (A 0 (A 0 (A 0 128)))
; (A 0 (A 0 256))
; (A 0 512)
; 1024

(A 2 4)
; (A 1 (A 2 3))
; (A 1 (A 1 (A 2 2)))
; (A 1 (A 1 (A 1 (A 2 1))))
; (A 1 (A 1 (A 1 2)))
; (A 1 (A 1 (A 0 (A 1 1))))
; (A 1 (A 1 (A 0 2)))
; (A 1 (A 1 4))
; (A 1 (A 0 (A 1 3)))
; (A 1 (A 0 (A 0 (A 1 2))))
; (A 1 (A 0 (A 0 (A 0 (A 1 1)))))
; (A 1 (A 0 (A 0 (A 0 2))))
; (A 1 (A 0 (A 0 4)))
; (A 1 (A 0 8))
; (A 1 16) ; This will evaluate to 2^16
; 65536

(A 3 3)
; (A 2 (A 3 2))
; (A 2 (A 2 (A 3 1)))
; (A 2 (A 2 2))
; (A 2 (A 1 (A 2 1)))
; (A 2 (A 1 2))
; (A 2 4) ; (A 1 2) evaluates to 4
; 65536

(define (f n) (A 0 n))
; (* 2 n)

(define (g n) (A 1 n))
; (A 0 (A 1 (- n 1)))
; equivalent to (* 2 (A 1 (- n 1)))
; which will evaluate to (* 2 (* 2 (... (n-2 times) (A 1 1))))
; (A 1 1) is 2
; So this evaluates to (^ 2 n)

(define (^ a b) 
    (define (power-iter a b current)
      (cond ((= b 0) current)
            (else (power-iter a (dec b) (* a current)))))
    (power-iter a b 1)
  )


(define (h n) (A 2 n))
; (A 1 (A 2 (- n 1)))
; equivalent to (^ 2 (A 2 (- n 1)))
; which will evaluate to (^ 2 (^ 2 (... (n-2 times) (A 2 1)))
; (A 2 1) is 2
; So this evaluates to 2^(2^(2^...(2))))
(define (recursive-two-power n)
  (if (= n 1)
     2
     (^ 2 (recursive-two-power (dec n)))))

(define (rtp n val)
  (if (= n 1)
    2
    (rtp (dec n) (^ val 2))
    ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Exercise 1.11
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; f(n) = f(n - 1) + 2f(n - 2) + 3f(n - 3)

; Recursive solution
(define (f n)
  (if (< n 3)
    n
    (+ (f (- n 1))
       (* 2 (f (- n 2)))
       (* 3 (f (- n 3))))))

; Iterative solution
(define (f2 n)

  (define (fx a b c)
    (+ a (* 2 b) (* 3 c)))

  (define (f-iter a b c n2)
    (if (= n2 0)
      (fx a b c)
      (f-iter (fx a b c) a b (- n2 1))))

  (if (< n 3)
    n
    (f-iter 2 1 0 (- n 3))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Exercise 1.12
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (pascal r c)
  (if (or (= c 1) (= r c))
    1
    (+ (pascal (- r 1) (- c 1))
       (pascal (- r 1) c))))
