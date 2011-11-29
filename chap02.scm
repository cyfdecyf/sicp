;; Ex 2.18

;; The recursive one may suffer from efficiency
(define (reverse-rec l)
  (if (null? l)
      l
      (append (reverse-rec (cdr l)) (list (car l)))))

;; The iterative one
(define (reverse l)
  (define (iter result rest)
    (if (null? rest)
	result
	(iter (cons (car rest) result)
	      (cdr rest))))
  (iter '() l))

(reverse (list 1 2 3 4))


;; Ex 2.20

(define (same-parity p . q)
  ;; use this function to process the arguments other than the first
  ;; one
  (define (process l result filter)
    (if (null? l)
	result
	(if (filter (car l))
	    (process (cdr l) (append result (list (car l))) filter)
	    (process (cdr l) result filter))))
  (if (odd? p)
      (process q (list p) odd?)
      (process q (list p) even?))))

;; another solution from a Japanese site
(define (same-parity-2 x . y)
  (let ((parity (remainder x 2)))
    (define (process result old)
      (cond ((null? old) result)
	    ((= (remainder (car old) 2) parity)
	     (process (append result (list (car old))) (cdr old)))
	    (else (process result (cdr old)))))
    (process (list x) y)))


;; Ex 2.22

;; this process produces list that is in the reverse order
(define (square-list-1 items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things) 
              (cons (square (car things))
                    answer))))
  (iter items '()))

;; this process didn't produces right list
;; note in the first iterator, answer is nil
(define (square-list-2 items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons answer
                    (square (car things))))))
  (iter items '()))

;; Change to this will work
(define (square-list-correct items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (append answer
                    (list (square (car things)))))))
  (iter items '()))


;; Ex 2.23

;; for-each
(define (for-each proc items)
  (if (null? items)
      items
      (let ((tmp (car items)))
	(proc tmp)
	(for-each proc (cdr items)))))

;; another solution from a Japanese site
;; don't understand it very much
(define (for-each proc items)
  (if (null? items)
      items
      ((lambda (proc items)
	 (proc (car items))
	 (for-each proc (cdr items))) proc items)))

;; take a look at map
(define (my-map proc items)
  (if (null? items)
      items
      (cons (proc (car items))
            (my-map proc (cdr items)))))


;; Ex 2.27

;; The iterative one
(define (deep-reverse items)
  (define (iter lt result)
    (cond ((null? lt) result)
	  ((pair? (car lt))
	   (iter (cdr lt) (cons (reverse (car lt)) result)))
	  (else (iter (cdr lt) (cons (car lt) result)))))
  (iter items '()))


;; Ex 2.28

(define (fringe tree)
  (cond ((null? tree) tree)
	((pair? tree)
	 (append (fringe (car tree))
	       (fringe (cdr tree))))
	(else (list tree))))


;; Ex 2.30

;; define the procedure directly
(define (square-tree tree)
  (cond ((null? tree) tree)
	((not (pair? tree))
	 (square tree))
	(else (cons (square-tree (car tree))
		    (square-tree (cdr tree))))))

;; define the procedure using map
(define (square-tree tree)
  (map (lambda (subtree)
	 (if (pair? subtree)
	     (square-tree subtree)
	     (square subtree)))
       tree))


;; Ex 2.31

(define (tree-map proc tree)
  (map (lambda (subtree)
	 (if (pair? subtree)
	     (tree-map proc subtree)
	     (proc subtree)))
       tree))

(define (square-tree tree)
  (tree-map square tree))

;; Ex 2.32

(define (subset s)
  (if (null? s)
      (list s)
      (let ((rest (subset (cdr s))))
	(append rest
		(map (lambda (x)
		       (cons (car s) x))
		     rest)))))

(subset (list 1 2 3))


;; Ex 2.33

;; convert some of the previous defined procedure to the accumulate
;; form
(define (my-map p sequence)
  (accumulate
   (lambda (x y) (cons (p x) y))
   '()
   sequence)))

(define (my-append seq1 seq2)
  (accumulate cons seq2 seq1))

(define (length sequence)
  (accumulate
   (lambda (x y) (+ (cond ((null? x) 0)
			  (not (pair? x) 1)
			  (else (length x)))
		    (length y)))
   0
   sequence))

;; Ex 2.34

(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms)
		(+ this-coeff (* x higher-terms)))
              0
              coefficient-sequence))

;; it's 1 + 3 * x + 5 * x ^ 3 + 1 * x ^ 5
;; the value should be 79
(horner-eval 2 (list 1 3 0 5 0 1))

;; Ex 2.35

(define (count-leaves t)
  ;; Add the length together
  (accumulate (lambda (x y)
		(+ x y))
	      0
	      ;; The map procedure returns a list contains all the
	      ;; length of each subtree
	      (map (lambda (subtree)
		     (cond ((null? subtree) 0)
			   ((not (pair? subtree)) 1)
			   (else (count-leaves subtree))))
		   t)))

;; A simple test. Should return 4
(count-leaves (list 1 (list 2 (list 3 4))))


;; Ex 2.36

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate
	     op
	     init
	     (map (lambda (item)
		    (car item)) seqs))
            (accumulate-n
	     op
	     init
	     (map (lambda (item)
		    (cdr item)) seqs)))))

(define s (list (list 1 2 3)
		(list 4 5 6)
		(list 7 8 9)
		(list 10 11 12)))

;; test, should return (22 26 33)
(accumulate-n + 0 s)


;; Ex 2.37

;; The dot-product defined in the book didn't consider whether the
;; vector is a row vector or a column vector.
;; While doing this exercise, I simplified the problem as explained in
;; each procedure's comment

;; define some matrix and vector for use
(define vector-a (list 1 2 3))

(define vector-b (list 4 5 6))

(define vector-c (list 2 3))

(define mat-3x3-a (list (list 1 2 3)
			(list 4 5 6)
			(list 7 8 9)))

(define mat-3x3-b (list (list 1 0 0)
			(list 0 1 0)
			(list 0 0 1)))

(define mat-3x2 (list (list 1 1)
		      (list 1 1)
		      (list 1 1)))

(define mat-2x3 (list (list 1 2 3)
		      (list 4 5 6)))

;; accumulate is defined in ex 2.36
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate
	     op
	     init
	     (map (lambda (item)
		    (car item)) seqs))
            (accumulate-n
	     op
	     init
	     (map (lambda (item)
		    (cdr item)) seqs)))))

;; defined in the text book
(define (dot-product v w)
  (accumulate + 0 (map * v w)))

;; test dot-product, should return 32
(dot-product vector-a vector-b)

;; Here v is a column vector, the returned list should be a column
;; vector too. The length of the vector should be the same with the
;; column number of the matrix.
(define (matrix-*-vector m v)
  (map (lambda (s) (dot-product s v)) m))

;; test matrix-*-vector
;; (14 32 50) 
(matrix-*-vector mat-3x3-a vector-a)
;; (5 5 5)
(matrix-*-vector mat-3x2 vector-c)

;; the matrix should be N x N
(define (transpose mat)
  (accumulate-n (lambda (x y)
		  (cons x y))
		'()
		mat))

;; test transpose
(transpose mat-3x3-a)

;; If m is PxQ, then n should QxR
(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (row) (matrix-*-vector cols row)) m)))

;; test matrix-*-matrix
(matrix-*-matrix mat-3x3-a mat-3x3-b)
(matrix-*-matrix mat-3x2 mat-2x3)
(matrix-*-matrix mat-2x3 mat-3x2)


;; Ex 2.38

(define (fold-right op initial sequence)
  (define (iter result rest)
    (if (null? rest)
	result
	(iter (op (car rest) result)
	      (cdr rest))))
  (iter initial sequence))

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))


(define (fold-left-rec op initial sequence)
  (if (null? sequence)
      initial
      (op (fold-left-rec op initial (cdr sequence))
	  (car sequence))))

;; the value is 3/2
(fold-right / 1 (list 1 2 3))

;; the value is 1/6
(fold-left / 1 (list 1 2 3))

;; the value is (3 (2 (1 ())))
(fold-right list nil (list 1 2 3))

;; the value is (((() 1) 2) 3)
(fold-left list nil (list 1 2 3))

;; Ex 2.39

(define (reverse sequence)
  (fold-right (lambda (x y)
		(cons x y))
	      '()
	      sequence))

(define (reverse sequence)
  (fold-left (lambda (x y)
	       (cons y x))
	     '()
	     sequence))

(reverse (list 1 2 3 4))


;; Ex 2.40

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))

;;: (enumerate-interval 1 5)

(define (unique-pair n)
  (flatmap (lambda (i)
	     (map (lambda (j) (list i j))
		  (enumerate-interval 1 (- i 1))))
	   (enumerate-interval 1 n)))

;;: (unique-pair 2)

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
	       (unique-pair n))))

(prime-sum-pairs 6)


;; Ex 2.41

(define (unique-triple n)
  (accumulate
   append
   nil
   (flatmap (lambda (i)
	      (map (lambda (j)
		     (map (lambda (k)
			    (list k j i))
			  (enumerate-interval 1 (- j 1))))
		   (enumerate-interval 1 (- i 1))))
	    (enumerate-interval 1 n))))

;;: (unique-triple 5)

(define (triple-sum triple)
  (+ (car triple) (car (cdr triple)) (car (cdr (cdr triple)))))

;;: (triple-sum (list 1 2 3))

(define (triple-target-sum n sum)
  (filter (lambda (triple) (= sum (triple-sum triple)))
	  (unique-triple n)))

(triple-target-sum 10 9)


;; Ex 2.42

;; eight-queens puzzle

(define empty-board '())

;; The sequence of the append is important, this makes the first
;; element in the generated positions list always the k'th element
(define (adjoin-position new-row k rest-of-queens)
  (append (list (list new-row k)) rest-of-queens))

;; test adjoin-position
;;: (adjoin-position 1 3 (list (list 2 2) (list 4 1)))

;; Whether the new queen check in other queen in the same row.
;; To make the test easy, put this mechod outside the procedure safe?
(define (check-in-row? queen rest-queen)
  (if (null? rest-queen)
      #f
      (or (= (car queen) (car (car rest-queen)))
	  (check-in-row? queen (cdr rest-queen)))))

;; test check-in-row?
;;: (check-in-row? '(1 4) '((3 4) (8 7) (1 2)))

;; Don't need to check whether the new queen check in other queen in
;; the same column since there is exactly only one queen in each
;; column

;; whether the new queen check in other queen in the diagonal
(define (check-in-diagonal? queen rest-queen)
  (define (check-in? new-queen oth-queen)
    (let ((quotient (/ (- (car new-queen) (car oth-queen))
		       (- (cadr new-queen) (cadr oth-queen)))))
      ; If the quotient is 1 or -1, then the two queen check in
      ; This is like using the slope
      (if (= 1 (abs quotient))
	  #t
	  #f)))
  (if (null? rest-queen)
      #f
      (or (check-in? queen (car rest-queen))
	  (check-in-diagonal? queen (cdr rest-queen)))))

;; test check-in-diagonal?
;;: (check-in-diagonal? '(4 3) '((1 2) (2 1)))
;;: (check-in-diagonal? '(3 3) '((1 2) (2 1)))

;; Whether the positions is safe.  Only need to check the queen in the
;; k'th column. 
;;
;; The argument position passed by the procedure queen always has only
;; k columns and the proceure adjoin-position makes sure that the
;; first queen in the positions is the k'th column queen, so here k is
;; not needed.
;; But if we are developing a more general procedure then k may be
;; used.
(define (safe? k positions)
  (let ((new-queen (car positions))
	(rest-queen (cdr positions)))
    (not (or (check-in-row? new-queen rest-queen)
	     (check-in-diagonal? new-queen rest-queen)))))

;; test safe?
;;: (safe? 1 '((4 3) (1 2) (2 1)))
;;: (safe? 2 '((3 3) (1 2) (2 1)))

;; make-board. This is used to test whether the board generation is
;; correct
(define (make-board board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
	(flatmap
	 (lambda (rest-of-queens)
	   (map (lambda (new-row)
		  (adjoin-position new-row k rest-of-queens))
		(enumerate-interval 1 board-size)))
	 (queen-cols ( - k 1)))))
  (queen-cols board-size))

;; test make-board
;;: (make-board 2)
;;: (make-board 3)

;; The procedure to solve the problem
(define (queens board-size)
  (define (queen-cols k)  
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

;; test queens, (queens 8) should have 92 solutions counting the
;; rotational solution
;;(length (queens 8))

;(queens 4)


;; Ex 2.43

;; See how many times the procedure lambda z is called
;; Try to modify the length of the list passed to flatmap and see the
;; the result
(flatmap
 (lambda (x)
   (map (lambda (y)
	  nil)
	((lambda z (write-string "called\n") '(1)))))
 '(1 2 3 4))

