(defclass binary-tree ()
  ((tree :accessor container
	 :initarg :tree
	 :initform nil)))

(defclass associative-list ()
  ((lst :accessor container
	:initarg :lst
	:initform nil)))

(defgeneric dict-get (dict key))
(defgeneric dict-add (dict key value))
(defgeneric dict-delete (dict key))

(defmethod dict-get ((dict binary-tree) key)
  (labels
      ((%binary-tree-get (tree)
	 (let
	     ((root (car tree))
	      (right-branch (caddr tree))
	      (left-branch (cadr tree)))
	   (cond
	     ((null tree) (values nil nil))
	     ((string= key (car root)) (values (cdr root) t))
	     ((string< key (car root)) (%binary-tree-get left-branch))
	     ((string> key (car root)) (%binary-tree-get right-branch))))))
    (%binary-tree-get (container dict))))

(defmethod dict-add ((dict binary-tree) key value)
  (labels
      ((%make-node (root left right)
	 (list root left right))
       (%binary-tree-add (tree)
	 (let
	     ((root (car tree))
	      (right-branch (caddr tree))
	      (left-branch (cadr tree)))
	   (cond
	     ((null tree) (%make-node (cons key value) nil nil))
	     ((string= key (car root))
	      (let ((old-value (cdr root)))
		(setf (cdr root) value)
		(return-from dict-add (values old-value t))))
	     ((string< key (car root))
	      (%make-node
	       root
	       (%binary-tree-add left-branch)
	       right-branch))
	     ((string> key (car root))
	      (%make-node
	       root
	       left-branch
	       (%binary-tree-add right-branch)))))))
    (setf (container dict) (%binary-tree-add (container dict)))
    (values nil nil)))
	       
(defmethod dict-delete ((dict binary-tree) key)
  (labels ((%tree-to-list (tree)
	     (let
		 ((root (car tree))
		  (right-branch (caddr tree))
		  (left-branch (cadr tree)))
	       (cond
		 ((null root) nil)
		 (t (append (list root) (tree-to-list left-branch) (tree-to-list right-branch)))))))
    (let* ((buff (remove-if (lambda (x)
			      (eq (car x) key)) (%tree-to-list (container dict))))
	   (elem-not-exist (equal buff (%tree-to-list (container dict)))))
      (if elem-not-exist
	  (return-from dict-delete (values nil nil))
	  (progn
	    (setf (container dict) '())
	    (dolist (elem buff)
	      (dict-add dict (car elem) (cdr elem)))
	    (return-from dict-delete (values 1 t))))))) ;; TODO values when elem is deleted
  

(defmethod dict-get ((dict associative-list) key)
  (dolist (x (container dict))
    (when (eq (car x) key)
      (return-from dict-get (values (cdr x) t))))
  (values nil nil))

(defmethod dict-add ((dict associative-list) key value)
  (mapcar (lambda (x)
	    (when (eq (car x) key)
	      (let ((old-value (cdr x)))
		(setf (cdr x) value)
		(return-from dict-add (values old-value t))))) (container dict))
  (setf (container dict) (append (container dict) (list (cons key value))))
  (values nil nil))

(defmethod dict-delete ((dict associative-list) key)
  (setf (container dict) (remove-if (lambda (x)
				      (eq (car x) key)) (container dict)))) ;; TODO values for both var
