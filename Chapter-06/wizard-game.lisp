; This ia definition of an assoication list which contains the names of the
; possible rooms as well as descriptions of the rooms. What's interesting is
; that the descriptions are not strings, rather they are symbols/data. 
(defparameter *nodes* '((living-room (you are in the living room. 
                                          a wizard is snoring loudly on the couch.))
                        (garden (you are in a beautiful garden. there is a well in front
                                     of you.))
                        (attic (you are in the attic. there is a giant welding torch in
                                    the corner.))))

; Describes the location passed in. The node list needs to be given which
; conatins the information for the location. 
(defun describe-location (location nodes)
  (cadr (assoc location nodes)))

; Defines an edges assoication where our key is the room and the value is a
; list containing where they can move to, in what direction, and in what
; manner they can move there.
(defparameter *edges* '((living-room (garden west door)
                                     (attic upstairs ladder))
                        (garden (living-room east door))
                        (attic (living-room downstairs ladder))))

; Describes an edge that is given to it.
(defun describe-path (edge)
  `(there is a ,(caddr edge) going ,(cadr edge) from here.))

; Gets all the possible paths from a given point
(defun describe-paths (location edges)
  (apply #'append (mapcar #'describe-path (cdr (assoc location edges)))))

; List of all the objects
(defparameter *objects* '(whiskey bucket frog chain))

; Alist for objects and their locations
(defparameter *object-locations* '((whiskey living-room)
                                   (bucket living-room)
                                   (chain garden)
                                   (frog garden)))

; Displays all the objects that are in the given room 
(defun objects-at (loc objs obj-locs)
  (labels ((at-loc-p (obj)
                     (eq (cadr (assoc obj obj-locs)) loc)))
    (remove-if-not #'at-loc-p objs)))

; Describes visible objects
(defun describe-objects (loc objs obj-loc)
  (labels ((describe-obj (obj)
                     `(you see a ,obj on the floor.)))
    (apply #'append (mapcar #'describe-obj (objects-at loc objs obj-loc)))))

; Current location
(defparameter *location* 'living-room)

; Describes my current location
(defun look ()
  (append (describe-location *location* *nodes*)
          (describe-paths *location* *edges*)
          (describe-objects *location* *objects* *object-locations*)))

; Walks in the given direction
(defun walk (direction)
  (let ((next (find direction
                    (cdr (assoc *location* *edges*))
                    :key #'cadr)))
    (if next 
      (progn (setf *location* (car next))
             (look))
      '(you cannot go that way.))))

; Picks up the object at the current location
(defun pickup (object)
  (cond ((member object 
                 (objects-at *location* *objects* *object-locations*))
         (push (list object 'body) *object-locations*)
         `(you are now carrying the ,object))
        (t '(you cannot get that.))))

; Displays the inventory
(defun inventory ()
  (cons 'items- (objects-at 'body *objects* *object-locations*)))

; My very own game REPL! 
(defun game-repl ()
  (let ((cmd (game-read)))
    (unless (eq (car cmd) 'quit)
      (game-print (game-eval cmd))
      (game-repl))))

;  My own read which attaches ()'s to everything and quotes as needed
(defun game-read ()
  (let ((cmd (read-from-string 
               (concatenate 'string "(" (read-line) ")"))))
    (flet ((quote-it (x)
                     (list 'quote x)))
      (cons (car cmd) (mapcar #'quote-it (cdr cmd))))))

; The allowed commands for the user
(defparameter *allowed-commands* '(look walk pickup inventory))

; Evals the good commands
(defun game-eval (sexp)
  (if (member (car sexp) *allowed-commands*)
    (eval sexp)
    '(i do not know that command.)))

; Does some crazy shit
(defun tweak-text (lst caps lit)
  (when lst
    (let ((item (car lst))
          (rest (cdr lst)))
      (cond ((eq item #\space) (cons item (tweak-text rest caps lit)))
            ((member item '(#\! #\? #\.)) (cons item (tweak-text rest t lit)))
            ((eq item #\") (tweak-text rest caps (not lit)))
            (lit (cons item (tweak-text rest nil lit)))
            ((or caps lit) (cons (char-upcase item) (tweak-text rest nil lit)))
            (t (cons (char-downcase item) (tweak-text rest nil nil)))))))

; Also crazy shit
(defun game-print (lst)
  (princ (coerce (tweak-text (coerce (string-trim "() "
                                                  (prin1-to-string lst))
                                     'list)
                             t
                             nil)
                 'string))
  (fresh-line))

