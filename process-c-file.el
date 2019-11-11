;;;-*- mode: Emacs-lisp; lexical-binding: t ; -*-

(load-file "loop-utilities.el")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This section defines the regular expression for finding
;;; the start of "for", "while", and "do-while" loops
;;;	
;;; This regexp is pattern for the parts of "for" and "while" loops,
;;; for example the statement  "for(1=0; i<100; i++){" is modeled as
;;; "for(<clause>;<clause>;<clause>){" and a while statement is modeled
;;; as "while(<clause>){".
(defconst clause (rx
		  (minimal-match
		   (zero-or-more (any "+" "-" "*" "/" "=" "." "<" ">" alnum)))))
;;; This builds the "(<clause>;<clause>;<clause>)" string for a "for" loop
(defconst for-args (rx
		    (minimal-match
		     (sequence "(" (eval clause) ";" (eval clause) ";"
			       (eval clause) ")" ))))
;;; Assembles the entire "for" statement including optional blanks
(defconst for-expr (rx (minimal-match
			(sequence "for" (zero-or-more  blank)
				 (eval for-args) (zero-or-more blank) "{"))))
;;; The "while" statement.  Note that by having only blanks between the
;;; the end of the "while" arguments and the "{" eliminates the "while"
;;; phrase terminating the "do-while" construct
(defconst while-expr (rx
		      (minimal-match
		       (sequence "while"
				 (zero-or-more blank) "(" (eval clause) ")"
				 (zero-or-more blank) "{"))))
;;; The simplest of all possible loops :-)
(defconst do-expr (rx (minimal-match (sequence "do" (zero-or-more blank) "{"))))

;;; This is the final regexp that should find all loops
;;(defconst loop-regexp (rx (minimal-match
;;			   (or (eval for-expr) (eval while-expr) (eval do-expr)))))

;;; End of loop-finding regexps
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq loop-regexp "for")
				 

;;; This function count the loop lengths one <filename>.c file
;;; the .c extension can be anything using basic C syntax

(defun process-c-buffer (in-buffer)
  "This function counts the loops of different lengths in a 
   buffer containing a .C-type file and returns an assocition
   list containg the counts of each buffer length. The
   buffer is assumed to be live and not checks are done."
  (message "entering process-c-buffer")
  (with-current-buffer in-buffer
    (goto-char (point-min))
    (while t ;(< (point) (point-max))
      (progn
	(re-search-forward loop-regexp)
	(let ((start-point (point)))
	  (message (concat "just before save-excursion in process-c-buffer at point: "
			   (format "%s" start-point)))
	  (save-excursion
	    (message "inside save excursion")
	    (forward-sexp)
	    (let ((length (- (point) start-point)))
	      (count-thing length))))))
      (count-thing)))

(defun process-c-file (in-file-name)
  "The function opens a file, then passes the resulting buffer
   into process-c-buffer to do the actual counting.  If the file
   can be opened, the function returns the a-list of lengths and
   counts.  If the file can't be opened, then the function returns
   an error string.  The calling function can determine whether
   or not this function exited with an error by checking the type
   of object returned: string or list"
  (message "Engering process-c-file")
  (let ((file-buffer (find-file-read-only in-file-name)))
    (if file-buffer
	(process-c-buffer file-buffer)
      (concat "process-c-file: could not open file: " in-file-name))))

(defun test-file-name (filename)
  (concat (file-truename "/Users/pcolsen/Dropbox/D.Projects/Loop Counter/") filename))
  
	 
			 
  
		  
	       
  
	
	  
    
    
    
 
	
