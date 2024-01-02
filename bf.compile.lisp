#!/usr/local/bin/sbcl --script
(load "bf.lisp")

; Compile to an executable.
(defun save ()
    (sb-ext:save-lisp-and-die 
        "bf.sbcl.o"
        :toplevel #'main
        :executable t))
(save)


