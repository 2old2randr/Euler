;;;; Counting problems (Project Euler 76 and 77)
;;;; - Sudhir Shenoy, April 2010.
;;;;
;;;; This class of problems involves finding combinations of integers
;;;; from a given set that add up to a given sum (repeated integers
;;;; are ok).
;;;;

(use-package :euler)			; in euler.lisp

(defun count-ways (target numbers)
  "Count ways to combine numbers to sum up to target. Each number can be used multiple times"
  (let ((counts (make-array (1+ target) :initial-element 0)))
    (setf (aref counts 0) 1)
    (dolist (i numbers)
      do (loop for j from i to target
	       do (incf (aref counts j) (aref counts (- j i)))))
    (aref counts target)))

;; Euler-76
;; How many different ways can one hundred be written as a sum of at least two positive integers?
(defun euler-76 (&optional (num 100))
  (count-ways num (loop for i from 1 below num collect i)))

;; Euler-77
;; It is possible to write ten as the sum of primes in exactly five different ways:
;;
;; 7 + 3
;; 5 + 5
;; 5 + 3 + 2
;; 3 + 3 + 2 + 2
;; 2 + 2 + 2 + 2 + 2
;;
;; What is the first value which can be written as the sum of primes in over five thousand different ways?
(defun euler-77 (&optional (target 5000))
  (let ((primes (generate-primes 100))	; use a list of the first 100 primes
	(num 5))
    (loop				; iteratively find the first number that works
     (let ((count (count-ways num primes)))
       (when (> count target)
	 (return (values num count)))
       (incf num)
       (when (> num (car (last primes))) ; for larger values of target
	 (error "Prime list needs expansion?"))))))
