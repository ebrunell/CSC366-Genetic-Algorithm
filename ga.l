;;; Genetic Algorithm to model short beak / long beak in finches in the Galapagos
;;; CSC 366 Final Project
;;; Authors: Emma Brunell, Tyler Dominick, Sean McGrath, Samson Manoah

;************************** Initial Statements *********************************************:

; number of finches 
(setf *finches* 20)

; randomly generate a length of finch beak
(defun finches ()
  (nth (random 2) '(long short) )
)

; generate a list of finches 
(defun finch-list (i)
  (cons (finches) (finch-list (- i 1) ) )
)

; string representation of number of random finches
(defun finch-string ()
  (finch-list *finches*)
)

;***************************************************************:

; mutate the string
(defmethod mutation ( (finch-string list) &aux p q)
  (setf p (random (length finch-string) ) )
  (setf q (others '(long short) (nth p finch-string) ) )
  (change finch-string (pick q) p )
)

(defmethod others ((lst list) x)
  (remove x lst)
)

(defmethod change ((lst list) x (i integer))
  (setf (nth i lst) x)
  lst
)

;****************************************************************************:

; crossover the string
(defmethod crossover ((mother list) (father list) )
  (setf position (+ 1 (random (length mother) ) ) )
  (append (firstn mother position) (restn father position) )
)

(defmethod firstn ((m list) (p integer))
  (subseq m 0 p)
)

(defmethod restn ((f list) (p integer))
  (subseq f p)
)

;****************************************************************************:

; fitness metric definitions
(defmethod fitness-long ((lst list))
  (count 'long lst)
)

(defmethod fitness-short ((lst list))
  (count 'short lst)
)
