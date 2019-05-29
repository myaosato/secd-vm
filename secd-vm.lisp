(uiop/package:define-package :secd-vm/secd-vm (:nicknames) (:use :cl) (:shadow)
                             (:export :vm) (:intern))
(in-package :secd-vm/secd-vm)
;;don't edit above

(defun vm (s e c d)
  (cond
    ;; instructions
    ((and (null c) (null d))
     (car s))
    ((null c)
     (destructuring-bind (s2 e2 c2 &rest d2) d
       (vm (cons (car s) s2) e2 c2 d2)))
    ((eq (car c) nil)
     (vm (cons nil s) e (cdr c) d))
    ((eq (car c) :ld)
     (destructuring-bind (i &rest cr) (cdr c)
       (vm (cons (nth i e) s) e cr d)))
    ((eq (car c) :ldc)
     (destructuring-bind (x &rest cr) (cdr c)
       (vm (cons x s) e cr d)))
    ((eq (car c) :ldf)
     (destructuring-bind (cf &rest cr) (cdr c)
       (vm (cons (cons cf e) s) e cr d)))
    ((eq (car c) :ap)
     (let ((cr (cdr c))
           (cfe2 (car s)))
       (destructuring-bind (cf &rest e2) cfe2
         (destructuring-bind (v &rest s2) (cdr s)
           (vm nil (cons v e2) cf (append (list s2 e cr) d))))))
    ((eq (car c) :rtn)
     (destructuring-bind (s2 e2 c2 &rest d2) d
       (vm (cons (car s) s2) e2 c2 d2)))
    ((eq (car c) :dum)
     (list s (cons nil e) (cdr c) d))
    ((eq (car c) :rap)
     (let ((cr (cdr c))
           (cfe2 (car s)))
       (destructuring-bind (cf &rest e2) cfe2
         (destructuring-bind (v &rest s2) (cdr s)
           (setf (car e2) v)
           (list nil e2 cf (append (list s2 e cr) d))))))
    ((eq (car c) :sel)
     (destructuring-bind (ct cf &rest cr) (cdr c)
       (vm (cdr s) e (if (car s) ct cf) (cons cr d))))
    ((eq (car c) :join)
     (vm s e (car d) (cdr d)))
    ;; basic function
    ((eq (car c) :car)
     (vm (cons (car (car s)) (cdr s)) e (cdr c) d))
    ((eq (car c) :cdr)
     (vm (cons (cdr (car s)) (cdr s)) e (cdr c) d))
    ((eq (car c) :cons)
     (vm (cons (cons (cadr s) (car s)) (cddr s)) e (cdr c) d))
    ((eq (car c) :eq)
     (vm (cons (eq (cadr s) (car s)) (cddr s)) e (cdr c) d))
    ((eq (car c) :less)
     (vm (cons (< (cadr s) (car s)) (cddr s)) e (cdr c) d))
    ((eq (car c) :add)
     (vm (cons (+ (cadr s) (car s)) (cddr s)) e (cdr c) d))
    ((eq (car c) :sub)
     (vm (cons (- (cadr s) (car s)) (cddr s)) e (cdr c) d))
    ((eq (car c) :atom)
     (vm (cons (atom (car s)) (cdr s)) e (cdr c) d))
    ((eq (car c) :symbolp)
     (vm (cons (symbolp (car s)) (cdr s)) e (cdr c) d))
    ((eq (car c) :integerp)
     (vm (cons (integerp (car s)) (cdr s)) e (cdr c) d))
    (t (values :err (list s e c d)))))