;;;-*- mode: Emacs-lisp; lexical-binding: t ; -*-

(setq lexical-binding t)

;;; Usage examples for make-counter
;; (make-counter named-counter) ;; creates a counter named "named-counter"
;; (funcall named-counter 1) ;; starts the count for 1 yielding "((1 1))
;; (funcall named-counter 1) ;; bumps the count for 1 yielding "((1 2))
;; (funcall named-counter 1) ;; bumps the count for 1 yielding "((1 3))
;; (funcall named-counter "a") ;; starts the count for "a" yielding "(((1 3)("a" 1))"
;; (funcall named-counter) ;; returns the count list unchanged yielding "(((1 3)("a" 1))"


(defmacro make-counter (counter-closure)
  "This macro creates a closure used for counting things.
The produced function counts things using an association list.
You can think of thie as an array indexed by the things
being couinted.  The first element of each of the association
cells is one of the things to be counted (analogous to an
array index).  The second is a list containing a count of
those things.  This function always returns the current list
of counts.  Calling this function with a non-nil value of thing
either increments the count, or, if thing hasn't been seen
before, it creates a count for thing initializing it to 1.
Calling it with a nil argument returns the
count-list without changing any of the counts.
;;; Usage examples for make-counter
;; (make-counter named-counter) ;; creates a counter named \"named-counter\"
;; (funcall named-counter 1) ;; starts the count for 1 yielding ((1 1))
;; (funcall named-counter 1) ;; bumps the count for 1 yielding ((1 2))
;; (funcall named-counter 1) ;; bumps the count for 1 yielding ((1 3))
;; (funcall named-counter \"a\") ;; starts the count for \"a\" yielding (((1 3)(\"a\" 1))
;; (funcall named-counter) ;; returns the count list unchanged yielding (((1 3)(\"a\" 1))"
  (list 'let (list (list 'count-list nil))
        (list 'setq counter-closure
              (list 'lambda (list '&optional 'thing)
                    (list 'cond
                          (list (list 'and 'thing 'count-list)
                                (list 'let (list (list 'count-pair (list 'assoc 'thing 'count-list)))
                                      (list 'if 'count-pair
                                            (list 'let (list (list 'current-count (list 'car (list 'cdr 'count-pair))))
                                                  (list 'setcdr 'count-pair (list 'list (list  '1+ 'current-count))))
                                            (list 'setq 'count-list (list 'cons (list 'list 'thing 1) 'count-list)))
                                      'count-list))
                          (list 'thing
                                (list 'setq 'count-list (list 'list (list 'list 'thing 1)))) 
                          (list t
                                'count-list))))))
(provide 'count-closure)
;;; Definition of count-closure ends here

(defun count-thing (thing)
  (funcall counter-closure thing))

(provide 'count-thing)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro make-stack (stack-name)
  "This macro returns a closure that can be used as a stack
;;; Examples of use
;; (make-stack named-stack) ;; creates a stack named \"named-stack\"
;; (funcall named-stack 1) ;; yields (1)
;; (funcall named-stack 2) ;; yields (2 1)
;; (funcall named-stack 3) ;; yields (3 2 1)
;; (funcall named-stack nil) ;; yields 3 leaving (2 1)"

  (list 'let (list (list 'stack-list nil))
	(list  'setq stack-name
	       (list 'lambda (list '&optional 'thing) ;; push thing onto stack
		     (list 'if 'thing
			   (list 'push 'thing 'stack-list)
			   (list 'pop 'stack-list))))))


(provide 'make-stack)
;;; Definition of stack-closure ends here

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


