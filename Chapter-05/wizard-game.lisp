; This ia definition of an assoication list which contains the names of the
; possible rooms as well as descriptions of the rooms. What's interesting is
; that the descriptions are not strings, rather they are symbols/data. 
(defparameter *nodes* '((living-room (you are in the living room. 
										  a wizard is snoring loudly on the couch))
						(garden (you are in a beautiful garden. there is a well in front
									 of you.))
						(attic (you are in the attic. there is a giant welding torch in
									the corner.))))

; Describes the location passed in. The node list needs to be given which
; conatins the information for the location. 
(defun describe-location (location nodes)
  (cadr (assoc location nodes)))

(describe-location 'garden *nodes*)
