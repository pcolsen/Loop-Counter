;;;-*- mode: Emacs-lisp; lexical-binding: t ; -*-

(defvar input-buffer nil)
(defvar output-buffer nil)

(defun is-c-file-p (file-name)
  (let ((extension (file-name-extention file-name))
	(c-file-extensions (list "c" "cc" "c++" "cpp" "C" "CC" "CPP" "C++")))
    (if (member extension c-file-extensions)
	t
      '())))

(defun say-thing (thing)
  "This function takes a 'thing', formats it into 
   a string, then both writes it as a message and
   inserts it into the output buffer followed by '\n'."
  (let ((string (format "%s" thing))) 
    (message string)
    (with-current-buffer output-buffer
      (insert (concat ";;;" string "\n")))))
		 
;;; I HAVE TO FINISH REFACTORING THIS
(defun count-loops-directory (&optional target-directory-name
					target-buffer)

  (let ((target-directory-true-name (truename target-directory-name)))
    ;; Is the directory accessible and writable. Bail if not
    (if (file-accessible-directory-p target-directory-true-name)
	;; Directory is accessible and writable, so we process it.
	(let ((output-directory-name (file-name-as-directory target-directory-name))
	    ;; True name to label output file
	    (directory-true-name (truename target-directory-name)))
	(let (;; All output will be written to a buffer for one file.
	      (out-file-name  (concat output-directory-name
				      target-directory-name "-loop-counts.txt"))
	      ;; Header for output file to identify directory
	      (header-string
	       (concat ";;; loop counts for directory: " directory-true-name)))
	  ;; -----------------------------------------------
	  ;; "output-buffer" is global
	  (setq output-buffer (get-buffer-create out-file-name))
	  (if output-buffer ;; No point if we can't create the buffer
	      ;; Buffer was created
	      (with-current-buffer
		  (say-thing header-string)
		(seve-buffer output-buffer) ; Save it now if it crashes later.
		;; --------------------------------------
		;; Here is where all the work will be done.
		(process-directory target-directory-name)
		;; The rest is just testing conditions and comments
		;; --------------------------------------
		(say-thing (concat ";;; Processing: "
				   directory-true-name "finished"))
		(save-buffer output-buffer)) ; Make sure buffer is saved.
	    ;; If get here the Buffer wasn't created
	    ;; so we can't leave a message in it.
	    (message (concat "Error: Could not create output buffer for: "
			     outfile-name)))))
	;; We come here if the directory wasn't writable
	(progn
	  (message (concat "Directory '" (truename target-directory-name)
			   "' could not be "
		   "opened or is not writable")))))
  nil)

(defun test-setup ()
  (defvar output-buffer '())
  (defvar test-dir-name "~/temp/loop-counter-test-dir")
  (if (not ((file-accessible-directory-p test-dir-name)))
      (make-directory test-dir-name))
      
	
  
