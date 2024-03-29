(defconstant TAPE_LEN 30000)

; return hash tables to find matching braces
(defun build_jumps (program)
  ; first element of stack is top of stack
  (let ((stack nil)
	(forward (make-hash-table))
	(backward (make-hash-table)))
    (loop for i below (length program) 
	  for c = (char program i)
	  do (cond 
	       ((eq c #\[) (setf stack (cons i stack)))
	       ((eq c #\])
	         (let ((open (car stack))
	  	       (close i))
	  	 (progn
	  	   (setf stack (cdr stack))
	  	   (setf (gethash open forward) close)
	  	   (setf (gethash close backward) open)))))
	  finally 
	  	(return (if 
		  (eq nil stack)
		  (cons forward backward)
		  (error "jump stack not empty"))))))
(defun jump (table i)
  (if (gethash i table)
    (gethash i table)
    (error "missing jump.")))

(defun tape-inc (tape i)
  (if (< i 0)
    (error "negative index: " i)
    (setf (aref tape i) (rem (+ (aref tape i) 1) 256))))
(defun tape-dec (tape i)
  (if (< i 0)
    (error "negative index: " i)
    (setf (aref tape i) (rem (- (aref tape i) 1) 256))))

(defun bf (program)
  (let* ((jumps (build_jumps program))
	 (tape (make-array (list TAPE_LEN) :initial-element 0))
	 (pc 0)
	 (ptr 0)
	 (forward (car jumps))
	 (backward (cdr jumps)))
    (loop while (< pc (length program))
       do 
        (let ((c (char program pc)))
       	  (cond 
	       ((eq c #\+) (tape-inc tape ptr))
     	       ((eq c #\-) (tape-dec tape ptr))
     	       ((eq c #\>) (setf ptr (+ ptr 1) ))
     	       ((eq c #\<) (setf ptr (- ptr 1) ))
     	       ((eq c #\.) (princ (code-char (aref tape ptr))))
     	       ((eq c #\[) (if (= 0 (aref tape ptr))
     	  		     (setf pc (jump forward pc))))
     	       ((eq c #\]) (if (/= 0 (aref tape ptr))
     	  		     (setf pc (jump backward pc))))
     	       (t (error "unknown instruction" c))))
          (setf pc (+ 1 pc))
     	  finally (return "done"))))

(defun is-comment? (line)
  (and (> (length line) 0)
       (eq #\; (char line 0))))

; Read in a program, dropping lines that start with a ;
(defun read-program (input acc)
  (let ((line (read-line input nil)))
    (if (eq nil line)
      acc
      (read-program input 
		    (concatenate 'string acc
				 (if (is-comment? line)
				   ""
				   line))))))

(defun run_program (filename)
  (let* (
	 (file (open filename))
	 (program (read-program file (make-string 0))))
    (bf program)))
(defun main ()
	(run_program (nth 1 sb-ext:*posix-argv*)))
