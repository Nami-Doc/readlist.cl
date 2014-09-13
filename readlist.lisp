#!/usr/bin/env sbcl --script
;; helpers
(defun split-by-char (string char)
  (loop for i = 0 then (1+ j)
     as j = (position char string :start i)
     collect (subseq string i j)
     while j))

(defun string->integer (str)
  (declare (type string str))
  (nth-value 0 (parse-integer str)))

(defun string->uint (str)
  (let ((num (string->integer str)))
    (if (>= num 0)
	num
	(error "That... Doesn't really look positive."))))
    
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

(defun const (val)
  (lambda (_) val))

(defun update-if (lst cond morph)
  (mapcar (lambda (cons) (if (funcall cond cons)
			     (funcall morph cons)
			     cons)) lst))

(defun λ-reader (stream char)
    (declare (ignore char stream))
      'LAMBDA)

(set-macro-character #\λ #'λ-reader)

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
	     (mapcar (λ (book)
			(format nil "~a=~a"
				(book-name book) (book-num book)))
		     ;; remove "empty" books
		     (remove-if (lambda (book) (< (book-num book) 0)) *vals*)))))

(defstruct book name num)

;; read our list
(with-open-file (stream *file*)
      (do ((line (read-line stream nil)
		 (read-line stream nil)))
	        ((null line))
	(destructuring-bind (name num) (split-by-char line #\=)
	  (setq *vals* (cons (make-book :name name :num (string->uint num)) *vals*)))))

(defmacro match (args &rest forms)
  (let ((args-name (gensym)) (args-len-name (gensym)))
    `(let* ((,args-name ,args) (,args-len-name (length ,args-name)))
       (cond ,@(loop for (match . actions)
		  in forms
		    
		  for (idxlits idxidents) = (if (eq match 't)
						(list nil nil) ; no binding no literal ...
						(partition (lambda (val) (stringp (cadr val)))
						       (mapcar-with-index #'list match)))
		    
		  ;; generate (and ...) for the literals:
		  for lit-conds = (mapcar (lambda (idx-lit)
					    (destructuring-bind (idx lit) idx-lit 
					      `(equal (nth ,idx ,args-name) ,lit)))
					  idxlits)
		  ;; add length checking and join the lit-conds by "AND"
		  for conds = (if (eq match 't)
				  't
				  `(and (= ,args-len-name ,(length match))
					,@lit-conds))
		  ;; then generate the let to bind identifiers
		  for body = `(let ,(mapcar (lambda (idx-ident)
					      (destructuring-bind (idx ident) idx-ident
						`(,ident (nth ,idx ,args-name))))
					    idxidents)
				,@actions)
		  collect `(,conds ,body))
	     (t (error "Unable to deal with it."))))))

(defun curry (fn &rest initial-args)
  (lambda (&rest args)
    (apply fn (append initial-args args))))

(defun compose (&rest fns)
  (destructuring-bind (fn1 . rest) (reverse fns)
    #'(lambda (&rest args)
	(reduce #'(lambda (v f) (funcall f v))
		rest
		:initial-value (apply fn1 args)))))

(defun add-to-list (name num)
  (if (find name *vals* :test #'equal :key #'book-name)
      (format t "~a is already there~%" name)
      (progn
	(format t "Adding ~a~%" name)
	(setq *vals* (cons (make-book :name name :num num) *vals*))
	(save-list))))

(defun set-list-val (name num)
  (format t "Updating ~a to ~a~%" name num)
  (setq *vals* (update-if *vals*
			  (lambda (book) (equal (book-name book) name))
			  (lambda (book) ;; TODO: update-book book :num X?
			    (make-book :name (book-name book)
				       :num  num))))
  (save-list))

(defun change-list-val (name num)
  (if (not (find name *vals* :test #'equal :key #'book-name))
    (format t "~a is not currently tracked~%" name) ;; TODO usage help?
    (progn
      (format t "Incrementing ~a by ~a~%" name num)
      (setq *vals* (update-if *vals*
			      (lambda (book) (and
					      (equal (book-name book) name)
					      (>= (book-num book) 0)))
			      (lambda (book)
				(make-book :name (book-name book)
					   :num  (+ num (book-num book))))))
      (save-list))))

(match *args*
 (("list")
  (format t "Current List:~%")
  (loop for book in *vals*
       do (format t "  ~a: ~a~%" (book-name book) (book-num book))))
 (("add" name) (add-to-list name 1))
 (("add" name num) (add-to-list name (string->uint num)))
 (("inc" name) (change-list-val name 1))
 (("dec" name) (change-list-val name -1))
 (("set" name num) (set-list-val name (string->uint num)))
 (t (format t "  Usage:~%list~%add <name>~%add <name> <num>~%inc <name>~%"))
 )
