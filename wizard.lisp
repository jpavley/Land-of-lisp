;; The Wizard's Adventure Game
;; Land of LISP by Conrad Barski, M.D,
;; John Pavley
;; Creative Commons Attribution 4.0 International License

;; data

(defparameter *nodes* '((living-room (You are in the living-room.
                                      A wizard is snorning loudly on the couch.))
                        (garden      (You are in a beautiful garden.
                                      There is a well in front of you.))
                        (attic       (You are in the attic.
                                      There is a giant welding a torch in the corner.))))

(defparameter *edges* '((living-room (garden west door)
                                     (attic upstairs ladder))
                        (garden      (living-room east door))
                        (attic       (living-room downstairs ladder))))

(defparameter *objects* '(wiskey bucket frog chain))

(defparameter *object-locations* '((wiskey living-room)
                                  (bucket living-room)
                                  (frog garden)
                                  (chain garden)))

(defparameter *location* 'living-room)

(defparameter *allowed-commands* '(look walk pickup inventory))

; functions

(defun describe-location (location nodes)
  ;; cadr is equivalent to (car (cdr x)) and returns the 2nd element of a list
  (cadr (assoc location nodes)))

(defun describe-path (edge)
  ;; caddr is equivalent to (car (cdr (cdr x))) and returns the 3rd element of a list
  `(there is a ,(caddr edge) going ,(cadr edge) from here.))

(defun describe-paths (location edges)
  (apply #'append (mapcar #'describe-path (cdr (assoc location edges)))))

(defun objects-at (loc objs obj-locs)
  (labels ((at-loc-p (obj) (eq (cadr (assoc obj obj-locs)) loc)))
     (remove-if-not #'at-loc-p objs)))

(defun describe-objects (loc objs obj-loc)
  (labels ((describe-obj (obj) `(you see a ,obj on the floor.)))
    (apply #'append (mapcar #'describe-obj (objects-at loc objs obj-loc)))))

(defun look ()
  (append (describe-location *location* *nodes*)
          (describe-paths *location* *edges*)
          (describe-objects *location* *objects* *object-locations*)))

(defun walk (direction)
  (let ((next (find direction (cdr (assoc *location* *edges*)) :key #'cadr)))
    (if next
      (progn (setf *location* (car next)) (look))
      `(you cannot go that way.))))

(defun pickup (object)
  (cond ((member object (objects-at *location* *objects* *object-locations*))
    (push (list object 'body) *object-locations*) `(you are now carrying the ,object))
    (t '(you cannot get that.))))

(defun inventory ()
  (cons 'items- (objects-at 'body *objects* *object-locations*)))

(defun game-repl ()
  (let ((cmd (game-read)))
    (unless (eq (car cmd) 'quit)
      (game-print (game-eval cmd))
      (game-repl))))

(defun game-read ()
  (let ((cmd (read-from-string (concatenate 'string "(" (read-line) ")"))))
    (flet ((quote-it (x) (list 'quote x)))
      (cons (car cmd) (mapcar #'quote-it (cdr cmd))))))
