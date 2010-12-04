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

(defparameter *location* 'living-room)

(defun look ()
  (append (describe-location *location* *nodes*)
          (describe-paths *location* *edges*)
          (describe-objects *location* *objects* *object-locations*)))

(defun walk (direction)
  (let ((next (find direction
                    (cdr (assoc *location* *edges*))
                    :key #'cadr)))
    (if next 
      (progn (setf *location* (car next))
             (look))
      '(you cannot go that way.))))

(defun pickup (object)
  (cond ((member object 
                 (objects-at *location* *objects* *object-locations*))
         (push (list object 'body) *object-locations*)
         `(you are now carrying the ,object))
        (t '(you cannot get that.))))

(defun inventory ()
  (cons 'items- (objects-at 'body *objects* *object-locations*)))
