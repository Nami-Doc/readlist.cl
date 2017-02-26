(defun split-by-char (string char)
  (loop :for i = 0 :then (1+ j)
     :as j = (position char string :start i)
     :collect (subseq string i j)
     :while j))

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
  (lambda (&rest _)
    (declare (ignore _))
    val))

(defun update-if (lst cond morph)
  (mapcar (lambda (cons) (if (funcall cond cons)
														 (funcall morph cons)
                             cons)) lst))

(defun λ-reader (stream char)
	(declare (ignore char stream))
	'LAMBDA)

(set-macro-character #\λ #'λ-reader)

(defmacro match (args &rest forms)
  (let ((args-name (gensym)) (args-len-name (gensym)))
    `(let* ((,args-name ,args) (,args-len-name (length ,args-name)))
       (cond
         ,@(loop
              :for (match . actions)
              :in forms

              :for (idxlits idxidents) =
                (if (eq match 't)
                    (list nil nil) ; no bindings no literals ...
                    (partition (lambda (val)
                                (not (symbolp (cadr val))))
                                (mapcar-with-index #'list match)))

              ;; generate (and ...) for the literals:
              :for lit-conds = (mapcar (lambda (idx-lit)
                                         (destructuring-bind (idx lit) idx-lit
                                           `(equal (nth ,idx ,args-name) ,lit)))
                                       idxlits)
              ;; add length checking and join the lit-conds with "AND"
              :for conds = (if (eq match 't)
                               't
                               `(and (= ,args-len-name ,(length match))
                                     ,@lit-conds))
              ;; only value-matching idents
              :for matching-idxidents = (remove 'nil idxidents :key #'second)
              ;; then generate the let to bind identifiers
              :for body =
                `(let ,(mapcar (lambda (idx-ident)
                                (destructuring-bind (idx ident) idx-ident
                                    `(,ident (nth ,idx ,args-name))))
                                matching-idxidents)
                    ,@actions)
              :collect `(,conds ,body))
         (t (error "Match failed"))))))

(defmacro defun-match (name &rest conds)
  `(defun ,name (&rest args)
     (match args ,@conds)))

(defmacro lambda-match (&rest conds)
  `(lambda (&rest args)
     (match args ,@conds)))

(defun curry (fn &rest initial-args)
  (lambda (&rest args)
    (apply fn (append initial-args args))))

(defun compose (&rest fns)
  (destructuring-bind (fn1 . rest) (reverse fns)
    #'(lambda (&rest args)
				(reduce #'(lambda (v f) (funcall f v))
								rest
								:initial-value (apply fn1 args)))))
