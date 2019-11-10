;;;-*- mode: Emacs-lisp; lexical-binding: t ; -*-

(setq lexical-binding t)


;;; These can't be used in a constructor function because then
;;; count-list and stack-list will be limited to the scope of the
;;; constructor


(let ((count-list nil))
  "This let construction makes a closure used for counting things.
   the function counts things using an association list.
   You can think of thie as an array indexed by the th8ngs 
   being couinted.  The first element of each of the association
   cells is one of the things to be counted (analogous to an 
   array index).  The second is a list containing a count of 
   those things.  This function always returns the current list
   of counts.  Calling this function with a non-nil value of thing
   either increments the count, or, if thing hasn't been seen 
   before, it creates a count for thing initializing it to 1.
   Calling it with a nil argument returns the
   count-list without changing any of the counts."
  (setq counter-closure
	(lambda (&optional thing) ;; thing is what is to be counted
	  ;; (mmp (list "Initial argument: thing: " thing))
	  (cond
	   ((and thing count-list) ;; Here we have a thing to be
	    ;; counted and a list of counts of things.  We're going
	    ;; to update the count of thing,  We'll then return
	    ;; count-list.
	    (let ((count-pair (assoc thing count-list)))
	      (if count-pair
		  (let ((current-count (car (cdr count-pair))))
		    ;; (mmp count-pair)
		    ;; (mmp current-count)
		    (setcdr count-pair (list (+ 1 current-count))))
		(setq count-list (cons (list thing 1) count-list)))
	      count-list))  ;; We'll return the count list
	   (thing
	    ;; This will be the first thing to be counted, so
	    ;; we have to create the count-list from scrath,
	    ;; then return it.
	    ;; (mmp (list "Entering initial count: " thing))
	    (setq count-list (list (list thing 1)))) 
	   (t
	    ;; We've been called with a nil argument, so we'll
	    ;; just return the count-list.
	    count-list)))))

(provide 'count-closure)
;;; Definition of count-closure ends here

(defun count-thing (thing)
  (funcall counter-closure thing))

(provide 'count-thing)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro make-stack (stack-name)
  "This macro returns a closure that can be used as a stack"
  (list 'let (list (list 'stack-list nil))
	(list  'setq stack-name
	       (list 'lambda (list '&optional 'thing) ;; push thing onto stack
		     (list 'if 'thing
			   (list 'push 'thing 'stack-list)
			   (list 'pop 'stack-list))))))




;; (let ((stack-list nil))
;;   "This let construction is a stack using the list stack-list
;;    as the stack structure itself.  If it is called with a non-nil
;;    argument, that argument is pushed onto the stack and the
;;    stacklist is returned.  If the argument is nil, then the stack
;;    is popped and the popped element is returned."
;;   (setq stack-closure
;; 	(lambda (&optional thing) ;; thing is what is to be counted
;; 	  ;; (mmp (list "Initial argument: thing: " thing))
;; 	    (if thing
;; 		(push thing stack-list)
;; 	      (pop stack-list)))))

(provide 'make-stack)
;;; Definition of stack-closure ends here

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



