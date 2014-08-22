#!/usr/bin/env sbcl --script
;; helpers
(defun split-by-char (string char)
  (loop for i = 0 then (1+ j)
     as j = (position char string :start i)
     collect (subseq string i j)
     while j))

;; program
(defvar *args* (cdr *posix-argv*)) ; YMMV. need to cdr to remove `sbcl`
(defvar *vals* '())
(defvar *file* "./list.txt")

;; read our list
(with-open-file (stream *file*)
      (do ((line (read-line stream nil)
		 (read-line stream nil)))
	        ((null line))
	      (setq *vals* (cons (split-by-char line #\=) *vals*))))

(defmacro cli (&rest forms)
  `(progn ,@(loop :for (cli-args action) :in forms
		 :collect `(format t "~a~%" ,(car cli-args))))
  )

(cli
 (("list") (format t "YO"))
 (("incr" name) (format t "~a" name))
 )

;; terrible handling of argv :(
;;(let ((type (car *args*)))
;;  (cond
;;    ((equal "list" type)
;;     (loop for (key val) in *vals* do
;;	  (format t "Name: ~a. Currently at: ~a~%" key val)))
;;    ((equal "incr" type)
;;     )
;;  ))
