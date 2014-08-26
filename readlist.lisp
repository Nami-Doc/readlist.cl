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

(defun save-list ()
   (with-open-file (str *file*
			:direction :output
			:if-exists :supersede
			:if-does-not-exist :create)
     (format str "~{~A~^~%~}"
	     (mapcar (lambda (cons) (format nil "~a=~a" (car cons) (cadr cons))) *vals*))))

;; read our list
(with-open-file (stream *file*)
      (do ((line (read-line stream nil)
		 (read-line stream nil)))
	        ((null line))
	      (setq *vals* (cons (split-by-char line #\=) *vals*))))

(defmacro match (args &rest forms)
  (let ((args-name (gensym "match-args")) (args-len-name (gensym "match-args-len")))
    `(let* ((,args-name ,args) (,args-len-name (length ,args-name)))
       (cond ,@(loop for (match . actions)
		  in forms
		    
		  for (idxlits idxidents) = (partition (lambda (val) (stringp (cadr val)))
						       (mapcar-with-index #'list match))
		  ;; generate (and ...) for the literals:
		  for lit-conds = (mapcar (lambda (idx-lit)
					    (destructuring-bind (idx lit) idx-lit 
					      `(equal (nth ,idx ,args-name) ,lit)))
					  idxlits)
		  ;; add length checking and join the lit-conds by "AND"
		  for conds = `(and (= ,args-len-name ,(length match))
				    ,@lit-conds)
		  ;; then generate the let to bind identifiers
		  for body = `(let ,(mapcar (lambda (idx-ident)
					      (destructuring-bind (idx ident) idx-ident
						`(,ident (nth ,idx ,args-name))))
					    idxidents)
				,@actions)
		  collect `(,conds ,body))
	     (t (error "Unable to deal with it."))))))

(defun const (val)
  (lambda (_) val))

(defun update-if (lst cond morph)
  (mapcar (lambda (cons) (if (funcall cond cons)
			     (funcall morph cons)
			     cons)) lst))

(defun find-val (lst val &key (compare #'equal) (morph #'identity))
  (find-if (lambda (cons) (funcall compare val (funcall morph cons))) lst)) 

(defun add-to-list (name num)
  (if (find-val *vals* name :morph #'car)
      (format t "~a is already there~%" name)
      (progn
	(format t "Adding ~a~%" name)
	(setq *vals* (cons (list name num) *vals*))
	(save-list))))

(defun change-list-val (name num)
  (if (not (find-val *vals* name :morph #'car))
    (format t "~a is not currently tracked~%" name) ;; TODO usage help?
    (progn
      (format t "Incrementing ~a by ~a~%" name num)
      (setq *vals* (update-if *vals*
			      (lambda (cons) (equal (car cons) name))
			      (lambda (cons) (list (car cons) (+ num (cadr cons))))))
      (save-list))))

(match *args*
 (("list")
  (format t "Current List:~%")
  (loop for (name num) in *vals*
       do (format t "  ~a: ~a~%" name num)))
 (("add" name) (add-to-list name 1))
 (("add" name num) (add-to-list name (the integer num)))
 (("inc" name) (change-list-val name 1))
 (("add" name num) (change-list-val name num))
 ;; TODO (() (usage-help))
 )
