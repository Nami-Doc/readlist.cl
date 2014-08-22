#!/usr/bin/env sbcl --script
;; helpers
(defun split-by-char (string char)
  (loop for i = 0 then (1+ j)
     as j = (position char string :start i)
     collect (subseq string i j)
     while j))

(defun range (max &key (min 0) (step 1))
  (loop for i from min below max by step collect i))

(defun mapcar-with-index (fn lst)
  (mapcar fn (range (length lst)) lst))

(defun filter (fn lst)
  (remove nil (mapcar fn lst)))

(defun partition (fn lst)
  (labels ((rec (lst truthies falsies)
	     (if (null lst)
		 (list truthies falsies)
		 (if (funcall fn (car lst))
		     (rec (cdr lst) (cons (car lst) truthies) falsies)
		     (rec (cdr lst) truthies (cons (car lst) falsies))))))
    (rec lst '() '())))

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
  `(cond ,@(loop for (cli-args action)
	      in forms
	      ;;for (lits idents) = (partition #'stringp cli-args)
	      ;;for idxlits = (remove-if-not (lambda (val) (stringp (car val)))
	      ;;		       (mapcar-with-index #'cons cli-args))
	      collect `(list 1 ,action)))
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
