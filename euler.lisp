;;;;--------------------------------------------------------------------------
;;;;
;;;; euler.lisp - Common functions used in solutions to various
;;;;              problems in Project Euler.
;;;;
;;;; The functions here are concerned with mundane tasks that are repeatedly
;;;; required when solving problems in Project Euler. These include getting
;;;; the individual digits of an integer, testing for primality, generating
;;;; prime numbers, etc.
;;;;
;;;; Sudhir Shenoy, April 2010.
;;;;
;;;;--------------------------------------------------------------------------

(defpackage #:euler
  (:use :common-lisp)
  (:export #:integer-digits
	   #:digits-integer
	   #:prime-p
	   #:nth-prime
	   #:generate-primes
	   #:factorize
	   #:binomial
	   #:combinations
	   #:permutations
	   #:permutation-p
	   #:palindrome-p
	   #:pandigital-p
	   #:factorial))
(in-package :euler)

(defmacro while (test &body body)
  `(do ()
       ((not ,test))
     ,@body))

(defun integer-digits (n &optional (base 10))
  "Return list of digits in integer in order"
  (do ((digits '()))
      ((zerop n) digits)
    (multiple-value-bind (quotient remainder) (truncate n base)
      (push remainder digits)
      (setf n quotient))))

(defun digits-integer (list &optional (base 10))
  "Convert list of digits to corresponding integer"
  (do ((number 0 (+ (* number base) (pop list))))
      ((null list) number)))

(defun prime-p (n)
  "Check if given number is prime"
  (cond ((or (= n 2) (= n 3)) t)
	((or (zerop (rem n 2)) (< n 2)) nil)
	((< n 9) t)
	((zerop (rem n 3)) nil)
	(t (let ((limit (truncate (sqrt n)))
		 (divisor 5))
	     (while (<= divisor limit)
	       (when (or (zerop (rem n divisor))
			 (zerop (rem n (+ 2 divisor))))
		 (return-from prime-p nil))
	       (incf divisor 6))
	     t))))

(defun nth-prime (n)
  "Return the nth prime number (1st prime is '2')"
  (cond ((= n 1) 2)
	(t (let ((k 1))
	     (while (> n 1)
	       (setf k (+ k 2))
	       (when (prime-p k)
		 (decf n)))
	     k))))

(defun generate-primes (limit)
  "Generate list of all prime numbers below limit"
  (if (< limit 2)
      nil
      (let ((nums (make-array (1+ limit))))
	(loop for i from 3 to limit do
	      (loop for j from (+ i i) to limit by i do
		    (setf (aref nums j) 1)))
	(let ((primes (loop for i from 3 to limit by 2
			    when (= 0 (aref nums i))
			    collect i)))
	  (push 2 primes)
	  primes))))

(defun factorize (num &optional (only-primes nil))
  "Return the prime factors of an integer and (optionally) the power to which it should be raised"
  (if (or (zerop num) (= 1 num))
      nil
      (let ((factors nil))
	(flet ((trial-division (n)
		 (cond ((= n 1) 1)
		       ((= 0 (rem n 2)) 2)
		       ((= 0 (rem n 3)) 3)
		       ((= 0 (rem n 5)) 5)
		       (t (let ((m 7)
				(i 1)
				(sqrtn (isqrt n))
				(diff #(6 4 2 4 2 4 6 2)))
			    (or (while (<= m sqrtn)
				  (when (zerop (rem n m))
				    (return m))
				  (incf m (aref diff (mod i 8)))
				  (incf i))
				n))))))
	  (while (/= 1 num)
	    (let* ((prime (trial-division num))
		   (ans (do ((e 1 (1+ e))
			     (n (/ num prime) (/ n prime)))
			    ((/= 0 (rem n prime)) (cons e n)))))
	      (push (if only-primes prime (cons prime (car ans)))
		    factors)
	      (setf num (cdr ans))))
	  factors))))

(defun binomial (n k)
  "Return N choose K"
  (let ((nck 1))
    (dotimes (i (min k (- n k)))
      (setf nck (* nck (/ (- n i) (1+ i)))))
    nck))

(defun combinations (list num)
  "Return all unique combinations of num elements from given list.
Length of the returned list will be (binomial (length list) num)"
  (cond ((< (length list) num) nil)
	((= num 1) (mapcar #'list list))
	(t (nconc (mapcar (lambda (x) (cons (car list) x))  (combinations (cdr list) (1- num)))
		  (combinations (cdr list) num)))))

(defun permutations (list)
  "Return a list of all permutations of the given list"
  (if (null list)
      (list nil)
      (mapcan #'(lambda (first)
		  (mapcar #'(lambda (rest)
			      (cons first rest))
			  (permutations (remove first list :count 1 :test #'eq))))
	      list)))

(defun permutation-p (n1 n2)
  "Return true if digits of two numbers are permutations of each other"
  (let ((d1 (if (integerp n1) (integer-digits n1) (copy-list n1)))
	(d2 (if (integerp n2) (integer-digits n2) (copy-list n2))))
    (equal (sort d1 #'<) (sort d2 #'<))))

(defun palindrome-p (n)
  "Return true if digits of number can be reversed to get same number"
  (if (integerp n)
      (let ((digits (integer-digits n)))
	(equal digits (reverse digits)))
      (equal n (reverse n))))

(defun pandigital-p (n &optional (num-digits 9))
  "Return true if number has all digits from 1 to num-digits exactly once"
  (let ((digits (integer-digits n)))
    (and (= num-digits (length digits))
	 (every #'identity (loop for i from 1 to num-digits collect (find i digits))))))

(defun factorial (n)
  "Return factorial of given number"
  (let ((f 1))
    (loop for i from 2 to n do (setf f (* f i)))
    f))
