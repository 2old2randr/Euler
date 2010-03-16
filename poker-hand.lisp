;;;; poker-hand.lisp - Evaluate who wins given two hands in 5-card poker.
;;;;
;;;; In the card game poker, a hand consists of five cards and are ranked,
;;;; from lowest to highest, in the following way:
;;;;
;;;; * High Card: Highest value card.
;;;; * One Pair: Two cards of the same value.
;;;; * Two Pairs: Two different pairs.
;;;; * Three of a Kind: Three cards of the same value.
;;;; * Straight: All cards are consecutive values.
;;;; * Flush: All cards of the same suit.
;;;; * Full House: Three of a kind and a pair.
;;;; * Four of a Kind: Four cards of the same value.
;;;; * Straight Flush: All cards are consecutive values of same suit.
;;;; * Royal Flush: Ten, Jack, Queen, King, Ace, in same suit.
;;;;
;;;; The cards are valued in the order:
;;;; 2, 3, 4, 5, 6, 7, 8, 9, 10, Jack, Queen, King, Ace.
;;;;
;;;; If two players have the same ranked hands then the rank made up of the
;;;; highest value wins; for example, a pair of eights beats a pair of fives.
;;;; But if two ranks tie, for example, both players have a pair of queens,
;;;; then highest cards in each hand are compared; if the highest cards tie
;;;; then the next highest cards are compared, and so on.

(defun euler-54 (&optional (file-name "poker.txt"))
  (with-open-file (stream file-name :direction :input)
      (let ((first-hand-wins 0))
	(loop for line = (read-line stream nil :eof)
	      until (eq line :eof)
	      do (multiple-value-bind (hand-1 hand-2) (hands-from-string line)
		   (when (= 1 (compare-hands hand-1 hand-2))
		     (incf first-hand-wins))))
	first-hand-wins)))

(defun hands-from-string (string)
  "Return two lists of five conses each. Each cons represents a card in the
form (pip . suit) where the pips are card face values (ace = 14, king = 13 etc)"
  (let ((hand1 nil) (hand2 nil)
	(pips '(#\2 2 #\3 3 #\4 4 #\5 5 #\6 6 #\7 7 #\8 8 #\9 9
		#\T 10 #\J 11 #\Q 12 #\K 13 #\A 14
		#\t 10 #\j 11 #\q 12 #\k 13 #\a 14))
	(suits '(#\S :spades #\H :hearts #\D :diamonds #\C :clubs
		 #\s :spades #\h :hearts #\d :diamonds #\c :clubs)))
    (flet ((string->card (str)
	     (cons (getf pips (char str 0))
		   (getf suits (char str 1)))))
      (with-input-from-string (stream string)
	  (dotimes (i 5)
	    (push (string->card (symbol-name (read stream))) hand1))
	  (dotimes (i 5)
	    (push (string->card (symbol-name (read stream))) hand2))))
      (values hand1 hand2)))

(defun compare-hands (hand1 hand2)
  "Return 1 if hand1 is better, 0 if there is a tie and 2 if hand2 is better"
  (let ((eval1 (evaluate-hand hand1))
	(eval2 (evaluate-hand hand2)))
    (loop for val1 in eval1
	  for val2 in eval2
	  when (/= val1 val2)
	  do (return-from compare-hands
	       (if (> val1 val2) 1 2)))
    0))

;; ranking constants (in ascending order starting from 15 so as to
;; outrank an ace-high hand, i.e., hand that contains 14)
(defparameter +pair+ 15)
(defparameter +two-pairs+ 16)
(defparameter +three-kind+ 17)
(defparameter +straight+ 18)
(defparameter +flush+ 19)
(defparameter +full-house+ 20)
(defparameter +four-kind+ 21)
(defparameter +straight-flush+ 22)	; also includes royal flush

(defun evaluate-hand (hand)
  "Return a list containing the class of hand followed by tie-breaking pip cards.
Suit information is discarded since suits are not ranked in poker. The returned list
will have different lengths as a result, e.g. (:straight 8), (:full-house 14 5), etc."
  (let* ((flush (flushp hand))
	 (hand (sort (mapcar #'car hand) #'>)) ; suit info can be discarded after checking for flush
	 (straight (straightp hand)))
    (cond ((and flush straight) (list +straight-flush+ (cadr straight)))
	  (flush flush)
	  (straight straight)
	  (t (pairs/high-card hand)))))

(defun flushp (cards)
  "Returns (:flush high-card) if all cards are of the same suit"
  (let ((suit (cdr (first cards))))
    (if (every (lambda(c) (eq (cdr c) suit)) cards)
	(list +flush+ (loop for card in cards maximizing (car card)))
	nil)))

(defun straightp (cards)
  "Returns (:straight high-card) if cards are in a sequence, including A-2-3-4-5.
Cards are assumed to be in descending sorted order"
  (cond ((equal cards '(14 5 4 3 2)) (list +straight+ 5)) ; special case
	((and (apply #'> cards)	 ; check no duplicates
	      (= 4 (- (car cards) (elt cards 4))))
	 (list +straight+ (car cards)))
	(t nil)))

(defun pairs/high-card (cards)
  "Returns one of the following depending on the hand (cards are assumed to be
  in sorted descending order) : (+four-kind+ card) /  (+full-house+ triple-card pair-card)
  (+three-kind+ card) / (+two-pairs+ high-pair-card low-pair-card spare-card)
  (+pair+ three spare cards) / (cards)"
  (cond
    ;; two possibilities for four of a kind
    ((= (car cards) (elt cards 3)) (list +four-kind+ (car cards)))
    ((= (cadr cards) (elt cards 4)) (list +four-kind+ (cadr cards)))
    ;; likewise, two possibilities for a full house
    ((and (= (car cards) (elt cards 2)) (= (elt cards 3) (elt cards 4)))
     (list +full-house+ (car cards) (elt cards 3)))
    ((and (= (car cards) (elt cards 1)) (= (elt cards 2) (elt cards 4)))
     (list +full-house+ (elt cards 2) (car cards)))
    ;; leaves us pairs, three of a kind or simply high-card
    (t (pairs-etc cards))))

(defun pairs-etc (cards)
  "Checks for three of a kind, two pairs and one pair and returns them if found"
  (let ((groups nil))
    (dolist (card cards)
      (if (assoc card groups)
	  (incf (cdr (assoc card groups)))
	  (setf groups (acons card 1 groups))))
    (setf groups (sort groups (lambda (x y) (and (>= (cdr x) (cdr y))
						 (> (car x) (car y))))))
    (cond ((= 3 (cdr (first groups)))
	   (list +three-kind+ (car (first groups))
		 (car (second groups)) (car (third groups))))
	  ((= 2 (cdr (first groups)) (cdr (second groups)))
	   (list +two-pairs+ (car (first groups))
		 (car (second groups)) (car (third groups))))
	  ((= 2 (cdr (first groups)))
	   (list +pair+ (car (first groups)) (car (second groups))
		 (car (third groups)) (car (fourth groups))))
	  (t cards))))			; high-card only

