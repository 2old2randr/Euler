;;;; Project Euler problem 90 - Sudhir Shenoy, April 2010
;;;;
;;;; The brute force solution (see below) solves this in a few milliseconds.
;;;;
;;;; We generate all combinations of 6 choices from the set of digits. The
;;;; digits 6 and 9 are replaced by a "don't-care" symbol 6-or-9 so that either
;;;; works.
;;;; We then exhaustively check every pair in these combinations to see if they
;;;; can display all the square numbers below 100 taking care to replace the
;;;; digits 6 and 9 by the don't-care symbol 6-or-9 and return the count of
;;;; those that do.
;;;;
;;;; Needs the function 'combinations' from euler.lisp

(defun euler-90 ()
  (let ((squares '((0 1) (0 4) (0 6-or-9) (1 6-or-9) (2 5)
		   (3 6-or-9) (4 6-or-9) (6-or-9 4) (8 1)))
	;; all combinations of 6 sided dice
	(face-combos (euler:combinations '(1 2 3 4 5 6-or-9 7 8 6-or-9 0) 6)))
    ;; count all combinations of two dice that can form all squares
    (loop for (die-1 die-2) in (euler:combinations face-combos 2)
	  count (every (lambda (x)
			 (let ((digit-1 (first x))
			       (digit-2 (second x)))
			   (or (and (member digit-1 die-1) (member digit-2 die-2))
			       (and (member digit-2 die-1) (member digit-1 die-2)))))
		       squares))))
