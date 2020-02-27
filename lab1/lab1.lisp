(defun associative-list-add (lst key value)
  (append lst (list (cons key value))))
  
(defun associative-list-get (lst key)
  (cond
    ((null lst) (values nil nil))
    ((eq (caar lst) key) (values (cdar lst) t))
    (t (associative-list-get (cdr lst) key))))

(defun associative-list-get-ver-2 (lst key)
  (mapcar (lambda (x) (when (eq (car x) key)
			(return-from associative-list-get-ver-2 (values (cdr x) t)))) lst)
  (values nil nil))

(defun associative-list-get-ver-3 (lst key)
  (dolist (x lst)
    (when (eq (car x) key)
      (return-from associative-list-get-ver-3 (values (cdr x) t))))
  (values nil nil))

(defun property-list-add (lst key value)
  (append lst (list key value)))

(defun property-list-get (lst key)
  (cond
    ((null lst) (values nil nil))
    ((eq (car lst) key) (values (cadr lst) t))
    (t (property-list-get (cddr lst) key))))

(defun root (tree)
  (car tree))

(defun right-branch (tree)
  (caddr tree))

(defun left-branch (tree)
  (cadr tree))

(defun make-node (root left right)
  (list root left right))

(defun binary-tree-add (tree key value)
  (cond
    ((null tree) (make-node (cons key value) nil nil))
    ((string= key (car (root tree))) tree)
    ((string< key (car (root tree)))
     (make-node (root tree) (binary-tree-add (left-branch tree) key value) (right-branch tree)))
    ((string> key (car (root tree)))
     (make-node (root tree) (left-branch tree) (binary-tree-add (right-branch tree) key value)))))

(defun binary-tree-get (tree key)
  (cond
    ((null tree) (values nil nil))
    ((string= key (car (root tree))) (values (cdr (root tree)) t))
    ((string< key (car (root tree))) (binary-tree-get (left-branch tree) key))
    ((string> key (car (root tree))) (binary-tree-get (right-branch tree) key))))

(defun preorder-print (tree)
  (let ((acc-str (make-string 2 :initial-element #\-)))
    (labels ((%preorder (tree acc)
	     (cond
	       ((null tree))
	       (t (print (concatenate 'string acc (write-to-string (root tree))))
		  (%preorder (left-branch tree) (concatenate 'string acc "---"))
		  (%preorder (right-branch tree) (concatenate 'string acc "---"))))))
      (%preorder tree acc-str))))
