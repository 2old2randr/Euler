;;;; monopoly.lisp - Calculate the probability of landing on each square in
;;;;                 the Monopoly board game using a stochastic matrix
;;;;                 of transition probabilities.
;;;; - Sudhir Shenoy, April 2010
;;;;
;;;; In the game Monopoly, the standard board is set up in the following way:
;;;;              GO A1 CC1 A2 T1 R1 B1 CH1 B2 B3 JAIL
;;;;              H2                              C1
;;;;              T2                              U1
;;;;              H1                              C2
;;;;              CH3                             C3
;;;;              R4                              R2
;;;;              G3                              D1
;;;;              CC3                             CC2
;;;;              G2                              D2
;;;;              G1                              D3
;;;;              G2J F3 U2 F2 F1 R3 E3 E2 CH2 E1 FP
;;;;
;;;; When a player lands on the Community Chest (CC1-CC3) or Chance (CH1-CH3)
;;;; squares, one of 16 cards is drawn some of which cause the token to be moved
;;;; (others leave the token on the same square).
;;;; 	Community Chest (2/16 cards):
;;;; 		1. Advance to GO
;;;; 		2. Go to JAIL
;;;; 	Chance (10/16 cards):
;;;; 		Advance to GO
;;;; 		Go to JAIL
;;;; 		Go to C1 (St. Charles Place)
;;;; 		Go to E3 (Illinois Avenue)
;;;; 		Go to H2 (Boardwalk)
;;;; 		Go to R1 (Reading Railroad)
;;;; 		Go to next R (railroad R1-R3)
;;;; 		Go to next R (railroad R1-R3)
;;;; 		Go to next U (Electric Company or Water Works)
;;;; 		Go back 3 squares (Income Tax, New York Avenue, Community Chest).
;;;; Note that the 'Go Back 3 squares' card, if drawn while on CH3 will cause
;;;; a card to be drawn on CC3 which may, in turn, move the token to GO or JAIL.
;;;;
;;;; This program calculates the probability of occupying each square (so the probability
;;;; of occupying G2J is 0 because the token ends in jail). The possibility of staying in
;;;; jail for three turns (or until a double is thrown) is ignored and it is assumed that
;;;; the player always emerges from jail on his next turn, i.e., there is no distinction
;;;; between the 'Just Visiting' and 'Jail' states.
;;;;
;;;; Although, normally, two six-sided dice are used to play the game, this program also
;;;; calculates the probabilities using other dice (to solve problem 84 in Project Euler
;;;; which specifies 4-sided dice).
;;;;
;;;;----------------------------------------------------------------------------------------

(defpackage :monopoly
  (:use :cl)
  (:export :print-monopoly-probabilities))
(in-package :monopoly)

;; English names for all the squares
(defparameter +square-names+
  #("GO. Go" "A1. Meditteranean Ave" "CC1. Community Chest" "A2. Baltic Ave" "T1. Income Tax"
    "R1. Reading RR" "B1. Oriental Ave" "CH1. Chance" "B2. Vermont Ave" "B3. Connecticut Ave"
    "Jail / Just Visiting" "C1. St. Charles Place" "U1. Electric Co" "C2. States Ave"
    "C3. Virginia Ave" "R2. Pennsylvania RR" "D1. St. James Place" "CC2. Community Chest"
    "D2. Tennnessee Ave" "D3. New York Ave" "FP. Free Parking" "E1. Kentucky Ave"
    "CH2. Chance" "E2. Indiana Ave" "E3. Illinois Ave" "R3. B&O RR" "F1. Atlantic Ave"
    "F2. Ventnor Ave" "U2. Water Works" "F3. Marvin Gardens" "G2J. Go to Jail" "G1. Pacific Ave"
    "G2. North Carolina Ave" "CC3. Community Chest" "G3. Pennsylvania Ave" "R4. Short Line"
    "CH3. Chance" "H1. Park Place" "T2. Luxury Tax" "H2. Boardwalk"))

;; Indexes to special squares
(defparameter +go+ 0)
(defparameter +jail+ 10)
(defparameter +go-to-jail+ 30)
;; Go to next Railroad (2 chance cards)
(defparameter +reading-rr+ 5)
(defparameter +penn-rr+ 15)
(defparameter +bo-rr+ 25)
;; Got to next Utility (1 chance card)
(defparameter +electric+ 12)
(defparameter +water+ 27)
;; Chance squares
(defparameter +chance-1+ 7)	; Chance (1)
(defparameter +chance-2+ 22)    ; Chance (2)
(defparameter +chance-3+ 36)	; Chance (3)
;; Destination squares
(defparameter +cc-1+ 2)
(defparameter +cc-2+ 17)
(defparameter +cc-3+ 33)	; Go back 3 spaces from Chance (3)
(defparameter +income-tax+ 4)	; Go back 3 spaces from Chance (1)
(defparameter +st-charles+ 11)  ; St. Charles Place
(defparameter +new-york+ 19)    ; Go back 3 spaces from Chance (2)
(defparameter +illinois+ 24)    ; Advance to Illinois Ave
(defparameter +boardwalk+ 39)	; Advance to Boardwalk

;; Number of squares on the board
(defparameter +num-squares+ 40)

;; -----------------------------------------------------------------------------------------

;; Calculate and print a table with the long-term probabilities of ending up on each square
;; on a Monopoly board
(defun print-monopoly-probabilities (&key (num-dice-sides 6) (min-accuracy 1.0d-7)
				     (combine-chance-cc nil))
  (multiple-value-bind (m p pd) (init-probabilities num-dice-sides)
    (let ((probs (solve-matrix (compute-transition-probabilities num-dice-sides m p pd)
			       min-accuracy)))
      (assert (= 1 (reduce #'+ probs)))	; sanity check
      (let ((list (loop for i from 0 below 40
			when (or (not combine-chance-cc)
				 (and combine-chance-cc
				      (not (chance-square-p i))
				      (not (community-chest-p i))))
			collect (list (aref +square-names+ i)
				      (* 100.0d0 (aref probs i))))))
	(when combine-chance-cc
	  (setf list (append list (list (list "Chance (all)" (* 100.0d0 (+ (aref probs +chance-1+)
									   (aref probs +chance-2+)
									   (aref probs +chance-3+))))
					(list "Comm. Chest (all)" (* 100.0d0 (+ (aref probs +cc-1+)
										(aref probs +cc-2+)
										(aref probs +cc-3+))))))))
	(format t "~:{~&~22A ~8,6F~}~&----------~%"
		(cons (list "Square Name" "% Probability")
		      (cons (list "----------" "-------------")
			    (sort list #'> :key #'cadr))))))))

;; -----------------------------------------------------------------------------------------

(declaim (inline chance-square-p community-chest-p destination
		 init-square update-transitions))

(defun chance-square-p (idx)
  (or (= idx +chance-1+) (= idx +chance-2+) (= idx +chance-3+)))

(defun community-chest-p (idx)
  (or (= idx +cc-1+) (= idx +cc-2+) (= idx +cc-3+)))

;; compute new square from roll (wraparound at 40)
(defun destination (curr roll-value)
  (mod (+ curr roll-value) +num-squares+))

;; Initialize a square in the transition matrix. Since each square has
;; three rows & columns corresponding to it, 9 matrix cells are actually set
(defun init-square (matrix row-idx col-idx &optional (value 0))
  (let ((row (* 3 row-idx))
	(col (* 3 col-idx)))
    (loop for i from 0 to 2
	  do (loop for j from 0 to 2
		   do (setf (aref matrix (+ i row) (+ i col)) value)))))

;; Initialize the transition matrix and roll probabilities given
;; the number of sides on the dice
(defun init-probabilities (num-sides)
  (let ((matrix (make-array (list (* 3 +num-squares+) (* 3 +num-squares+)))) ;; matrix to hold transition probabilities
	(p-roll-value (make-array (1+ (* 2 num-sides)))) ; probability of each roll (excluding doubles)
	(p-is-double (make-array (1+ (* 2 num-sides))))) ; probability that given number is a double
    ;; initialize transition probabilities matrix
    ;; Each square has three entries corresponding to the state of doubles rolled
    ;; prior to this roll. Thus the transition from square i to square j will have 9
    ;; matrix elements affected (see update-transitions)
    (loop for i from 0 below +num-squares+
	  do (loop for j from 0 below +num-squares+
		   do (init-square matrix i j 0)))
    ;; initialize roll probabilities
    (let ((base-prob (/ 1 (* num-sides num-sides))))
      (loop for i from 1 below num-sides
	    do (let ((p (* i base-prob))
		     (max-roll (1+ (* 2 num-sides))))
		 (setf (aref p-roll-value (1+ i)) p
		       (aref p-roll-value (- max-roll i)) p)))
      ;; probability of rolling a 7 (or 5)
      (setf (aref p-roll-value (1+ num-sides)) (* num-sides base-prob))
      ;; probability that a roll is a double for each value
      (loop for i from 2 to (* 2 num-sides) by 2
	    do (progn (setf (aref p-is-double i) base-prob)
		      (decf (aref p-roll-value i) base-prob))))
    (values matrix p-roll-value p-is-double)))

;; Compute transition probabilities
(defun compute-transition-probabilities (num-sides matrix p-roll-value p-is-double)
  (loop for old-square from 0 below +num-squares+ ; for each square
	do (loop for roll from 2 to (* 2 num-sides) ; calculate destination square for each roll
		 do (let ((new-square (destination old-square roll))
			  (p (aref p-roll-value roll))
			  (pd (aref p-is-double roll)))
		      (when (= new-square +go-to-jail+)
			(setf new-square +jail+))
		      (cond ((community-chest-p new-square) (handle-community-chest matrix old-square new-square p pd))
			    ((chance-square-p new-square) (handle-chance-square matrix old-square new-square p pd))
			    (t (update-transitions matrix old-square new-square p pd))))))
  matrix)

(defun handle-community-chest (matrix from to p pd)
  ;; 2 cards out of 16 move the token
  (let ((p-move (* 1/16 p))
	(pd-move (* 1/16 pd)))
    (update-transitions matrix from to (* 7/8 p) (* 7/8 pd)) ; stay on CC square
    (update-transitions matrix from +jail+ p-move pd-move) ; go to jail
    (update-transitions matrix from +go+ p-move pd-move))) ; advance to go

(defun handle-chance-square (matrix from to p pd)
  ;; 10 cards out of 16 move the token
  (let ((p-move (* 1/16 p))
	(pd-move (* 1/16 pd)))
    (update-transitions matrix from to (* 3/8 p) (* 3/8 pd)) ; stay on chance square
    ;; Unconditional moves (6 cards)
    (update-transitions matrix from +go+ p-move pd-move) ; advance to go
    (update-transitions matrix from +jail+ p-move pd-move) ; go to jail
    (update-transitions matrix from +st-charles+ p-move pd-move) ; go to St. Charles Place
    (update-transitions matrix from +illinois+ p-move pd-move) ; go to Illinois Ave.
    (update-transitions matrix from +boardwalk+ p-move pd-move) ; take a walk on the Boardwalk
    (update-transitions matrix from +reading-rr+ p-move pd-move) ; advance to Reading Railroad
    ;; Advance to nearest RR (2 cards)
    (let ((nearest-rr (cond ((= to +chance-1+) +penn-rr+)
			    ((= to +chance-2+) +bo-rr+)
			    ((= to +chance-3+) +reading-rr+)
			    (t (error "Invalid chance idx ~A" to)))))
      (update-transitions matrix from nearest-rr (* 2 p-move) (* 2 pd-move)))
    ;; Advance to nearest Utility (1 card)
    (let ((nearest-util (cond ((= to +chance-1+) +electric+)
			      ((= to +chance-2+) +water+)
			      ((= to +chance-3+) +electric+))))
      (update-transitions matrix from nearest-util p-move pd-move))
    ;; Go back 3 spaces (1 card)
    (cond ((= to +chance-1+) (update-transitions matrix from +income-tax+ p-move pd-move))
	  ((= to +chance-2+) (update-transitions matrix from +new-york+ p-move pd-move))
	  ((= to +chance-3+) (handle-community-chest matrix from +cc-3+ p-move pd-move)))))

;; Update transition matrix for given from-square/to-square transition
(defun update-transitions (matrix from to p pd)
  (let ((row (* 3 from))
	(col (* 3 to)))
    ;; Zero prior doubles
    (incf (aref matrix row col) p)
    (incf (aref matrix row (+ 1 col)) pd) ; transition to 1 prior double state
    ;; One prior double
    (incf (aref matrix (+ 1 row) col) p) ; transition to 0 doubles state
    (incf (aref matrix (+ 1 row) (+ 2 col)) pd)	; transition to 2 doubles state
    ;; Two prior doubles
    (incf (aref matrix (+ 2 row) col) p) ; transition to 0 doubles state
    (incf (aref matrix (+ 2 row) +jail+) pd))) ; unconditionally go to jail

;; Solve for long-term transition probabilities
;; Exact answers can be obtained by computing the dominant eigenvector of the
;; transition matrix, but here we just iteratively compute the transitions until
;; the change is sufficiently small. This is fast enough and takes about 17 seconds
;; on a slowish PPC G5 Mac using Clozure CL.
(defun solve-matrix (matrix &optional (max-error 1.0d-7))
  (let* ((matrix-size (array-dimension matrix 0))
	 (curr (make-array matrix-size))
	 (next (make-array matrix-size)))
    (setf (aref curr 0) 1)		; starting position
    (loop
     (loop for i from 0 below matrix-size do
	   (setf (aref next i) 0))
     ;; Multiply current vector by transition matrix to give probabilities
     ;; of next state
     (loop for i from 0 below matrix-size do
	   (loop for j from 0 below matrix-size do
		 (incf (aref next j) (* (aref curr i) (aref matrix i j)))))
     ;; Return if the new state probabilities are close enough to the previous one
     (let ((max-diff (loop for i from 0 below matrix-size
			   maximizing (abs (- (aref curr i) (aref next i))))))
       (when (< max-diff max-error)
	 (let ((probs (make-array (/ matrix-size 3))))
	   (loop for i from 0 below (/ matrix-size 3)
		 do (setf (aref probs i) (+ (aref next (* i 3))
					    (aref next (+ 1 (* i 3)))
					    (aref next (+ 2 (* i 3))))))
	   (return-from solve-matrix probs))))
     (loop for i from 0 below matrix-size do
	   (setf (aref curr i) (aref next i))))))

;; -----------------------------------------------------------------------------------------
