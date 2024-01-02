(defconstant TAPE_LEN 30000)

; return hash tables to find matching braces
(defun build_jumps (program)
  ; first element of stack is top of stack
  (defun build_jumps_inner (i stack forward backward)
    (if (>= i (length program))
      (if (eq '() stack)
	(cons forward backward)
	(error "jump stack not empty"))
      (let ((c (char program i)))
	(cond ((eq c #\[)
	       (build_jumps_inner (+ 1 i) (cons i stack) forward backward))
	      ((eq c #\])
	       (let ((open (car stack))
		     (close i)
		     (stack (cdr stack)))
		 (progn
		   (setf (gethash open forward) close)
		   (setf (gethash close backward) open)
		   (build_jumps_inner (+ 1 i) stack forward backward))))
	      (t (build_jumps_inner (+ 1 i) stack forward backward))))))
  (build_jumps_inner 0 '() (make-hash-table) (make-hash-table)))
(defun jump (table i)
  (if (gethash i table)
    (+ 1 (gethash i table))
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
  (defun bf_state (pc ptr tape forward backward)
    (if (>= pc (length program))
      "done"
      (let ((c (char program pc))
	    (next (+ 1 pc)))
	(cond ((eq c #\+) (progn
			     (tape-inc tape ptr)
			     (bf_state next ptr tape forward backward)))
	      ((eq c #\-) (progn
			     (tape-dec tape ptr)
			     (bf_state next ptr tape forward backward)))
	      ((eq c #\>) (bf_state next (+ ptr 1) tape forward backward))
	      ((eq c #\<) (bf_state next (- ptr 1) tape forward backward))
	      ((eq c #\.) (progn
			     (princ (code-char (aref tape ptr)))
			     (bf_state next ptr tape forward backward)))
	      ((eq c #\[) (bf_state
			     (if (= 0 (aref tape ptr))
			       (jump forward pc)
			       next)
			     ptr tape forward backward))
	      ((eq c #\]) (bf_state
			     (if (= 0 (aref tape ptr))
			       next
			       (jump backward pc))
			     ptr tape forward backward))
	      (t (error "unknown instruction" c))))))
  (let ((jumps (build_jumps program)))
    (bf_state 0 0 (make-array (list TAPE_LEN) :initial-element 0)
	      (car jumps)
	      (cdr jumps))))

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

