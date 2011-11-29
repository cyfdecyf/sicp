;; Ex 1.3

;; The first lisp program I written.
;; Er... not so familiar with it and feels strange.
;; But it's INTRESTRING!
(define (fun a b c)
  (+ (if (< a b)
	 (* b b)
	 (* a a))
     (if (< a b)
         (if (< a c)
             (* c c)
             (* a a))
         (if (< b c)
             (* c c)
             (* b b)))))

;; This is what I did the 2nd time I read the book.
(define (sum-larger-2 a b c)
  (cond ((and (< a b) (< a c))
	 (+ (* a a) (* b b)))
	((and (< b a) (< b c))
	 (+ (* a a) (* c c)))
	(else
	 (+ (* b b) (* c c)))))

;; Ex 1.5

;; For applicative-order evaluation, (test 0 (p)) will first evaluate
;; (p) and goes into infinite loop. For normal-order evaluation, it
;; will return 0.

;; Ex 1.6

;; Similar with Ex 1.5, it will lead to infinite loop because
;; sqrt-iter will always call sqrt-iter when evaluating the arguement
;; in new if.

;; Ex 1.11

(define (f n)
  (cond ((< n 3) n)
        (else (+ (f (- n 1))
                 (+ (* 2 (f (- n 2)))
                    (* 3 (f (- n 3))))))))

;; using so many parentheses is really not happy
;; without using kind of for or while loop statement,
;; writing loop structure at first is not so easy
;; since I don't know how to use local variable now.
;; However, using formal parameter solved the problem
(define (f-iter n)
  (define (iter count fn-3 fn-2 fn-1)
    (if (= count (+ n 1))
        fn-1
        (iter (+ count 1) fn-2 fn-1 
              (+ (* 3 fn-3) (+ (* 2 fn-2) fn-1)))))
  (cond ((< n 3) n)
        (else (iter 3 0 1 2))))


;; Ex 1.16

(define (expt-own base expon)
  (define (iter i a)
    (cond ((= i expon) a)
          ((= (- expon i) 1) (iter (+ i 1) (* a base)))
          (else (iter (+ i 2) (* a (* base base))))))
  (iter 0 1))


;; Ex 1.17

(define (mult a b)
  (cond ((= b 0) 0)
        ((even? b) (mult (double a) (halve b)))
        (else (+ a (mult a (- b 1))))))

(define (russian-peasant a b)
  (define (iter i result)
    (cond ((= i b) result)
          ((= i 0) (iter 1 a))
          ((> (* 2 i) b) (iter (+ i 1) (+ result a)))
          (else (iter (double i) (double result)))))
  (iter 0 0))

(define (double n)
  (* 2 n))

(define (halve b)
  (/ b 2))


;; Ex 1.19

(define (fib n)
  (fib-iter 1 0 n))

(define (fib-iter a b count)
  (cond ((= count 0) b)
	((even? count)
	 (fib-iter (+ a a b) (+ a b) (/ count 2)))
	(else (fib-iter (+ a b) a (- count 1)))))

(fib 6)


;; Ex 1.22

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (timed-prime? n)
  (newline)
  (display n)
  (start-prime-test n (current-milliseconds)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (current-milliseconds) start-time))))

(define (report-prime elapsed-time)
    (display " *** ")
    (display elapsed-time))

(define (square n)
  (* n n))

;; use this procedure to fine some big prime number
;; I've used this one 1000000000063
(define (search-prime from num)
  (cond ((> num 0)
       (cond ((prime? from) ((newline) (display from) (search-prime (+ 1 from) (- num 1))))
             (else (search-prime (+ 1 from) num))))
        (else ((newline) (display "Search finished")))))

;; this can display the test time of each prime number from low to high
(define (search-time low high)
  (timed-prime? low)
  (cond ((< low high)
         (search-time (+ 1 low) high))))


;; Ex 1.27

(define (carmichael n)
  (define (iter n i)
  (cond ((not (femert-test n i)) #f) ;; If can't pass femert test, return false
        ((< i (- n 1)) (iter n (+ 1 i)))
        (else #t)))
  (iter n 2))

(define (femert-test n a)
  (= (expmod a n n) a))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

(define (square n)
  (* n n))


;; Ex 1.28

(define (prime-mr? n times)
  (cond ((= times 0) #t)
        ((mr-test n) ((prime-mr? n (- times 1))))
        (else #f)))

(define (mr-test n)
  (define (try a)
    (= (expmod-mr a (- n 1) n) 1))
  (try (+ 1 (random (- n 1)))))

(define (trival-sqrt? a n)
  (= (remainder (square (remainder a n)) n) 1))

(define (square n)
  (* n n))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))


;; Ex 1.30

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (sum-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (+ a 1) (+ result (term a)))))
  (iter a 0))

(define (inc n)
  (+ n 1))

(define (add-from-to a b)
  (sum (lambda (x) x) a inc b))

(define (add-from-to-iter a b)
  (sum-iter (lambda (x) x) a inc b))


;; cuberoot.scm

(define (good-enough? guess x)
  (< (abs (- guess (improve guess x))) 0.0001))

(define (improve guess x)
  (/ (+ (/ x (* guess guess)) (* 2 guess)) 3))

(define (cube-root guess x)
  (if (good-enough? guess x)
       guess
       (cube-root (improve guess x) x)))


;; sqrt-iter.scm

(define (good-enough? guess x)
  (< (abs (- (* guess guess) x)) 0.0001))

(define (good-enough-better? guess x)
  (< (abs (- guess (improve guess x))) 0.0001))

(define (average x y)
  (/ (+ x y) 2))

(define (improve guess x)
  (average guess (/ x guess)))

;; An attempt to replace if with a procedure.
;; This will cause endless recursion in some casees.
(define (new-if predicate cont then)
  (cond (predicate cont)
        (else then)))

(define (sqrt-iter-better guess x)
  ;; If we use new-if here, we will enter endless recursion
  (if (good-enough-better? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))

(define (sqrt-iter guess x)
  ;; If we use new-if here, we will enter endless recursion
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))

