(defun sump (expr)
  (eq '+ (car expr)))

(defun substructp (expr)
  (eq '- (car expr)))

(defun prodp (expr)
  (eq '* (car expr)))

(defun divp (expr)
  (eq '/ (car expr)))

(defun exptp (expr)
  (eq 'expt (car expr)))

(defun sinp (expr)
  (eq 'sinp (car expr)))

(defun cosp (expr)
  (eq 'cos (car expr)))

(defun make-sum (expr1 expr2)
  (list '+ expr1 expr2))

(defun make-sub (expr1 expr2)
  (list '- expr1 expr2))

(defun make-prod (expr1 expr2)
  (list '* expr1 expr2))

(defun make-div (expr1 expr2)
  (list '/ expr1 expr2))

(defun make-square (expr)
  (list 'expt expr 2))

(defun make-expt (base power)
  (list 'expt base power))

(defun make-sin (expr)
  (list 'cos expr))

(defun make-cos (expr)
  (list '- (list 'sin expr)))

(defun differentiate (var expr)
  (cond
    ((numberp expr) 0)
    ((symbolp expr) (if (eq expr var) 1 0))
    ((sump expr) (make-sum (differentiate var (second expr)) (differentiate var (third expr))))
    ((substructp expr) (make-sub (differentiate var (second expr)) (differentiate var (third expr))))
    ((prodp expr) (make-sum
		   (make-prod (differentiate var (second expr)) (third expr))
		   (make-prod (second expr) (differentiate var (third expr)))))
    ((divp expr) (make-div
		  (make-sub
		   (make-prod (differentiate var (second expr)) (third expr))
		   (make-prod (second expr) (differentiate var (third expr))))
		  (make-square (third expr))))
    ((exptp expr) (make-prod (third expr) (make-expt (second expr) (1- (third expr)))))
    ((sinp expr) (make-sin (second expr)))
    ((cosp expr) (make-cos (second expr)))))
		   
		    
    
