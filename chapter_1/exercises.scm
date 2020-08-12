; Exercise 1
; 10 => 10
; (+ 5 3 4) => 12
; (- 9 1) => 8
; (/ 6 2) => 3
; (+ (* 2 4) (- 4 6)) => 6
; (define a 3) 
; (define b (+ a 1)) 
; (+ a b (* a b)) => (+ 3 4 (* 3 4)) => 19
; (= a b) => #f
; (if (and (> b a) (< b (* a b)))
;   b 
;   a) => b => 4
; (cond ((= a 4) 6)
;       ((= b 4) (+ 6 7 a))
;       (else 25)) => 16
; (+ 2 (if (> b a) b a)) => 6
; (* (cond ((> a b) a)
;          ((< a b) b)
;          (else -1))
;    (+ a 1)) => 16

; Exercise 1.2 
; Translate the following expression
; 5 + 4 + (2 - (3 - (6 + 4/5))) / 3(6 - 2)(2 - 7)

(/ (+ 5 4 (- 2 (- 3 (+ 6 4/5)))) 
   (* 3 (- 6 2) (- 2 7)))

; Exercise 1.3
; Define a procedure that takes 3 numbers as arguments and returns the sum of the squares of the two 
; larger numbers. 

(define (square x)
  (* x x))

(define (sum-of-squares a b)
  (+ (square a) (square b)))

(define (sum-squares-of-largest a b c)
  (cond ((and (>= a c) (>= b c)) (sum-of-squares a b))
	((and (>= b a) (>= c a)) (sum-of-squares b c))
	((and (>= a b) (>= c b)) (sum-of-squares a c))))

; Exercise 1.4
; If b is positive then adding it to a would give the correct result
; If b is negative then we would want to subtract it from a 
; because a - b when b is negative is equal to a - (- b) = a + b
; If b is 0, we subtract. It doesn't matter because a - 0 = a
; When b is positive (if (> b 0) + -) evaluates to + which is then
; applied to the arguments a and b. 
; When b is negative or 0 (if (> b 0) + -) evaluates to - which 
; is then applied to the arguments a and b
(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))

; Exercise 1.5
; Ben Bitdiddle has invented a test to determine whether the interpreter he is faced
; with is using applicative-order evaluation or normal-order evaluation. 
; He defines the following two procedures:
(define (p) (p))

(define (test x y)
  (if (= x 0)
      0
      y))

(test 0 (p))

; If it was applicative order, it would call (p) which would call p indefinitley, 
; because it evaluates the arguments first. 
; If it was normal order, it would return 0, because it would never actually evaluate (p) 
; (test 0 (p))
; (if (= 0 0)
;     0
;     (p)) => 0


(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (sqrt x)
  (sqrt-iter 1.0 x))

Exercise 1.6
(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
	(else else-clause)))

(define (ex-sqr-iter guess x)
  (new-if (good-enough? guess x)
	  guess
	  (ex-sqr-iter (improve guess x) x)))

; Since new-if is a function all of its arguments get evaluated, include the recursive call 
; to ex-sqrt-iter. Instead of computing the sqrt it gets stuck in an infinite loop 

; Exercise 1.8 

(define (cube-root x) 
  (cube-root-iter 1.0 x))

(define (cube-root-iter guess x)
  (if (good-enough-cube? guess x) 
      guess
      (cube-root-iter (improve-cube guess x) x)))

(define (good-enough-cube? guess x) 
  (< (abs (- (cube guess) x)) 0.001))

(define (improve-cube guess x) 
  (/ (+ (/ x (square guess))  (* 2 guess))
     3))

(define (square x) 
  (* x x))

(define (cube x) 
  (* x x x))


