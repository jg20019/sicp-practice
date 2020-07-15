
;; these functions create are the constructor and the selectors. 
;; All code that is used to manipulate rational numbers are defined by these.
;; In fact, you could actually create the functions add-rat, sub-rat, mul-rat
;; div-rat, and equal-rat? based on the exisitence of these functions even 
;; before they have been designed. This strategy is called wishful thinking


(define (gcd x y)
  (cond ((> y x) (gcd y x))
	((= y 0) x)
	(else (gcd y (remainder x y)))))


(define (make-rat n d)
  (let ((g (gcd (abs n) (abs d))))
    (cond ((and (< n 0) (< d 0)) (cons (/ (- n) g) (/ (- d) g)))
	  ((< d 0) (cons (- (/ n g)) (/ (- d) g)))
	  (else (cons (/ n g) (/ d g))))))



(define (numer x)
  (car x))

(define (denom x)
  (cdr x))

;; The following functions are defined in terms of the constructor and selectors
;; defined above. This strategy of programming allows us to separate definition 
;; from use. The below code doesn't care about how the functions: 
;; make-rat, numer, and denom are defined

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
	       (* (numer y) (denom x)))
	    (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (+ (* (numer x) (denom y))
	       (* (numer y) (denom x)))
	    (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
	    (* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
	    (* (denom x) (numer y))))

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

(define (print-rat x)
  (newline) 
  (display (numer x))
  (display "/") 
  (display (denom x)))


