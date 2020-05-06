(defun make-sum-or-sub (sign expr1 expr2)
  (cond
    ((eq expr1 0) expr2)
    ((eq expr2 0) expr1)
    (t (list sign expr1 expr2))))

(defun make-prod (expr1 expr2)
  (cond
    ((eq expr1 1) expr2)
    ((eq expr2 1) expr1)
    ((or (eq expr1 0) (eq expr2 0)) 0)
    (t (list '* expr1 expr2))))

(defun make-div (expr1 expr2)
  (list '/ expr1 expr2))

(defun make-expt (base power)
  (cond
    ((eq power 1) base)
    ((eq power 0) 1)
    (t (list 'expt base power))))

(defun make-sin (expr)
  (list 'cos expr))

(defun make-cos (expr)
  (list '- (list 'sin expr)))

(defun differentiate (var expr)
  (cond
    ((numberp expr) 0)
    ((symbolp expr) (if (eq expr var) 1 0))
    (t (let
	   ((operator (first expr))
	    (arg1 (second expr))
	    (arg2 (third expr)))
	 (cond
	   ((eq '+ operator) (make-sum-or-sub '+
					     (differentiate var arg1)
					     (differentiate var arg2)))
	   ((eq '- operator) (make-sum-or-sub '-
						   (differentiate var arg1)
						   (differentiate var arg2)))
	   ((eq '* operator) (make-sum-or-sub '+
					      (make-prod (differentiate var arg1) arg2)
					      (make-prod arg1 (differentiate var arg2))))
	   ((eq '/ operator) (make-div
			     (make-sum-or-sub '-
					      (make-prod (differentiate var arg1) arg2)
					      (make-prod arg1 (differentiate var arg2)))
			     (make-expt arg2 2)))
	   ((eq 'expt operator) (make-prod arg2 (make-expt arg1 (1- arg2))))
	   ((eq 'sin operator) (make-sin arg1))
	   ((eq 'cos operator) (make-cos arg1)))))))
		   
		    
    
