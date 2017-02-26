#!/usr/bin/env sbcl --script
(require "UTILS" "utils")

;; program
(defconstant +file+ "./list.txt")
(defvar *args* (cdr *posix-argv*)) ; YMMV. need to cdr to remove `sbcl`
(defvar *vals* '())

(defun save-list ()
  (with-open-file (str +file+
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
(with-open-file (stream +file+)
  (do ((line (read-line stream nil)
             (read-line stream nil)))
      ((null line))
    (destructuring-bind (name num) (split-by-char line #\=)
      (setq *vals* (cons (make-book :name name :num (string->uint num)) *vals*)))))

(defun add-to-list (name num)
  (if (find name *vals* :test #'equal :key #'book-name)
      (format t "~a is already there~%" name)
      (progn
        (format t "Adding ~a~%" name)
        (setq *vals* (cons (make-book :name name :num num) *vals*))
        (save-list))))

(defun book-num-mutator (mutator)
  (lambda (book)
    (make-book :name (book-name book)
               :num (funcall mutator (book-num book)))))

(defun set-list-val (name num)
  (format t "Updating ~a to ~a~%" name num)
  (setq *vals* (update-if *vals*
                          (lambda% (equal (book-name %) name))
                          (book-num-mutator (const num))))
  (save-list))

(defun change-list-val (name num)
  (if (not (find name *vals* :test #'equal :key #'book-name))
      (format t "~a is not currently tracked~%" name)
      (progn
        (format t "Incrementing ~a by ~a~%" name num)
        (setq *vals*
              (update-if
               *vals*
               (lambda% (and
                         (equal (book-name %) name)
                         (>= (book-num %) 0)))
               (book-num-mutator (lambda% (+ num %)))))
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
       (t (format t "Usage:~%- list~%- add <name>~%- add <name> <num>~%- inc <name>~%- set <name> <num>")))
