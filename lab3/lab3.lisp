(defun my-mapcar (func lst)
  (if (null lst)
      nil
      (cons (funcall func (car lst)) (my-mapcar func (cdr lst)))))


(defun my-reduce (func lst)
  (labels ((%helper (acc lst)
	     (if (null lst) acc
		 (%helper (funcall func acc (car lst)) (cdr lst)))))
    (%helper (car lst) (cdr lst))))

(defun my-remove-if (funcp lst)
  (cond ((null lst) nil)
	((not (funcall funcp (car lst))) (append (list (car lst)) (my-remove-if funcp (cdr lst))))
	(t (my-remove-if funcp (cdr lst)))))

(defun funcp (number)
  (if (evenp (* number number)) t nil))
