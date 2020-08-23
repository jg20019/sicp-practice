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


; Exercise 1.9 
(define (plus-1 a b)
  (if (= a 0) 
      b
      (inc (plus-1 (dec a) b))))

(plus-1 3 4) 
(inc (plus-1 2 4))
(inc (inc (plus-1 1 4)))
(inc (inc (inc (plus-1 0 4))))
(inc (inc (inc 4)))
(inc (inc 5))
(inc 6)
7 

; This function is a recursive process because it defers the inc operations 
; until it reaches the base case. 

(define (plus-2 a b) 
  (if (= a 0) 
      b 
      (plus-2 (dec a) (inc b))))

(plus-2 3 4)
(plus-2 2 5)
(plus-2 1 6)
(plus-2 0 7)
7 
; This function is an iterative process. 

; Exercise 1.10 
; Ackermann's function 

(define (A x y)
  (cond ((= y 0) 0)
	((= x 0) (* 2 y))
	((= y 1) 2)
	(else (A (- x 1)
		 (A x (- y 1))))))





(A 1 10) 
(A 0 (A 1 9))
(A 0 (A 0 (A 1 8)))
(A 0 (A 0 (A 0 (A 1 7))))
(A 0 (A 0 (A 0 (A 0 (A 1 6)))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 1 5))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 4)))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 3))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 2)))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 1))))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 2)))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 4))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 8)))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 16))))))
(A 0 (A 0 (A 0 (A 0 (A 0 32)))))
(A 0 (A 0 (A 0 (A 0 64))))
(A 0 (A 0 (A 0 128)))
(A 0 (A 0 256))
(A 0 512)
1024 


(A 2 4)
(A 1 (A 2 3))
(A 1 (A 1 (A 2 2)))
(A 1 (A 1 (A 1 (A 2 1))))
(A 1 (A 1 (A 1 2)))
(A 1 (A 1 (A 0 (A 1 1))))
(A 1 (A 1 (A 0 2)))
(A 1 (A 1 4))
(A 1 (A 0 (A 1 3)))
(A 1 (A 0 (A 0 (A 1 2))))
(A 1 (A 0 (A 0 (A 0 (A 1 1)))))
(A 1 (A 0 (A 0 (A 0 2))))
(A 1 (A 0 (A 0 4)))
(A 1 (A 0 8))
(A 1 16)
(A 0 (A 1 15))
(A 0 (A 0 (A 1 14)))
(A 0 (A 0 (A 0 (A 1 13))))
(A 0 (A 0 (A 0 (A 0 (A 1 12)))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 1 11))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 10)))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 9))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 8)))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 7))))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 6)))))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 5))))))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 4)))))))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 3)))))))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 2))))))))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 1))))))))))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 2))))))))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 4))))))))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 16))))))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 32)))))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 64))))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 128)))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 256))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 512)))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 1024))))))
(A 0 (A 0 (A 0 (A 0 (A 0 2048)))))
(A 0 (A 0 (A 0 (A 0 4096))))
(A 0 (A 0 (A 0 8192)))
(A 0 (A 0 16384))
(A 0 32768)
65536 

(A 3 3)
(A 2 (A 3 2))
(A 2 (A 2 (A 3 1)))
(A 2 (A 2 2))
(A 2 (A 1 (A 2 1)))
(A 2 (A 1 2))
(A 2 (A 0 (A 1 1)))
(A 2 (A 0 2))
(A 2 4) => 65536

(define (f n) (A 0 n))
; f(n) = 2n 

(define (g n) (A 1 n))
; g(n) = 2^n 

(define (h n) (A 2 n))
; h(0) = 0
; h(1) = 2
; h(n) = 2^(h(n-1))

(define (h-prime n) 
  ; this is what h does. I don't know how else to represent this mathematically, 
  ; I could probably make this quicker by using iteration
  (cond ((= n 0) 0)
        ((= n 1) 2)
        (else (expt 2 (h-prime (- n 1))))))

(and (= (h 0)  (h-prime 0))
     (= (h 1) (h-prime 1)) 
     (= (h 2) (h-prime 2))
     (= (h 3) (h-prime 3)) 
     (= (h 4) (h-prime 4)))


(define (count-change amount)
  (cc amount 5))

(define (cc amount kinds-of-coins) 
  (cond ((= amount 0) 1)
	((or (< amount 0) (= kinds-of-coins 0)) 0)
	(else (+ (cc amount
		     (- kinds-of-coins 1))
		 (cc (- amount
			(first-denomination kinds-of-coins))
		     kinds-of-coins)))))

(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
	((= kinds-of-coins 2) 5)
	((= kinds-of-coins 3) 10)
	((= kinds-of-coins 4) 25)
	((= kinds-of-coins 5) 50)))

(count-change 100)

;; Exercise 1.11
(define (f n) 
  (if (< n 3) n
      (+ (f (- n 1)) 
	 (* 2 (f (- n 2)))
	 (* 3 (f (- n 3))))))


(f 0) ; 0
(f 1) ; 1
(f 2) ; 2
(f 3) ; 4
(f 4) ; 11
(f 5) ; 25 

(define (f-iter n) 
  (define (iter a b c n) 
    (if (< n 0) c
	(iter b 
	      c 
	      (+ c  (* 2 b) (* 3 a))
	      (- n 1))))
  (if (< n 3) 
      n 
      (iter 0 1 2 (- n 3))))

; Exercise 1.12 
(define (pascals-triangle r c) 
  (cond ((or (< r 0) (< c 0)) 0)
	((or (= c 0) (= r c)) 1)
	(else (+ (pascals-triangle (- r 1) (- c 1))
		 (pascals-triangle (- r 1) c)))))
; Exercise 1.13 
; Expects me to write a proof by induction which I currently do not know how to do so ... pass. 

; R(n) has an order of growth theta(f(n)) if there are positive constants k_1f(n) and k_2f(n) independent of n such that
; k1f(n) <= R(n) <= k2f(n) 

;; Exercise 1.4 Orders of Growth of Count Change 

  

  










