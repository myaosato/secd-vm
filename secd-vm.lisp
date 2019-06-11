(uiop/package:define-package :secd-vm/secd-vm (:nicknames) (:use :cl) (:shadow)
                             (:export :make-vm :vm-run :vm-step :vm) (:intern))
(in-package :secd-vm/secd-vm)
;;don't edit above

(defun ld (s e c d)
  (destructuring-bind (i &rest cr) (cdr c)
    (list (cons (nth (cdr i) (nth (car i) e)) s) e cr d)))

(defun ldc (s e c d)
  (destructuring-bind (x &rest cr) (cdr c)
    (list (cons x s) e cr d)))

(defun ldf (s e c d)
  (destructuring-bind (cf &rest cr) (cdr c)
    (list (cons (cons cf e) s) e cr d)))

(defun ap (s e c d)
  (let ((cr (cdr c))
        (cfe2 (car s)))
    (destructuring-bind (cf &rest e2) cfe2
      (destructuring-bind (v &rest s2) (cdr s)
        (list nil (cons v e2) cf (append (list s2 e cr) d))))))

(defun rtn (s e c d)
  (destructuring-bind (s2 e2 c2 &rest d2) d
    (list (cons (car s) s2) e2 c2 d2)))

(defun dum (s e c d)
  (list s (cons nil e) (cdr c) d))

(defun rap (s e c d)
  (let ((cr (cdr c))
        (cfe2 (car s)))
    (destructuring-bind (cf &rest e2) cfe2
      (destructuring-bind (v &rest s2) (cdr s)
        (setf (car e2) v)
        (list nil e2 cf (append (list s2 (cdr e) cr) d))))))

(defun sel (s e c d)
  (destructuring-bind (ct cf &rest cr) (cdr c)
    (list (cdr s) e (if (car s) ct cf) (cons cr d))))

(defun join (s e c d)
  (list s e (car d) (cdr d)))

(defun vm-nil (s e c d)
  (list (cons nil s) e (cdr c) d))

(defun vm-car (s e c d)
  (list (cons (car (car s)) (cdr s)) e (cdr c) d))

(defun vm-cdr (s e c d)
  (list (cons (cdr (car s)) (cdr s)) e (cdr c) d))

(defun vm-cons (s e c d)
  (list (cons (cons (car s) (cadr s)) (cddr s)) e (cdr c) d))

(defun vm-eq (s e c d)
  (list (cons (eq (cadr s) (car s)) (cddr s)) e (cdr c) d))

(defun vm-less (s e c d)
  (list (cons (< (cadr s) (car s)) (cddr s)) e (cdr c) d))

(defun vm-add (s e c d)
  (list (cons (+ (cadr s) (car s)) (cddr s)) e (cdr c) d))

(defun vm-sub (s e c d)
  (list (cons (- (cadr s) (car s)) (cddr s)) e (cdr c) d))

(defun vm-mul (s e c d)
  (list (cons (* (cadr s) (car s)) (cddr s)) e (cdr c) d))

(defun vm-div (s e c d)
  (list (cons (floor (cadr s) (car s)) (cddr s)) e (cdr c) d))

(defun vm-rem (s e c d)
  (list (cons (cadr (multiple-value-list (floor (cadr s) (car s))))
              (cddr s))
        e (cdr c) d))

(defun vm-atom (s e c d)
  (list (cons (atom (car s)) (cdr s)) e (cdr c) d))

(defun vm-symbolp (s e c d)
  (list (cons (symbolp (car s)) (cdr s)) e (cdr c) d))

(defun vm-integerp (s e c d)
  (list (cons (integerp (car s)) (cdr s)) e (cdr c) d))

(defun vm (s e c d)
  (cond
    ;; instructions
    ((and (null c) (null d))
     (values (car s) t))
    ((null c) (rtn s e c d))
    ((eq (car c) nil) (vm-nil s e c d))
    ((eq (car c) :ld) (ld s e c d))
    ((eq (car c) :ldc) (ldc s e c d))
    ((eq (car c) :ldf) (ldf s e c d))
    ((eq (car c) :ap) (ap s e c d))
    ((eq (car c) :rtn) (rtn s e c d))
    ((eq (car c) :dum) (dum s e c d))
    ((eq (car c) :rap) (rap s e c d))
    ((eq (car c) :sel) (sel s e c d))     
    ((eq (car c) :join) (join s e c d))
    ;; basic function
    ((eq (car c) :car) (vm-car s e c d))
    ((eq (car c) :cdr) (vm-cdr s e c d))
    ((eq (car c) :cons) (vm-cons s e c d))
    ((eq (car c) :eq) (vm-eq s e c d))
    ((eq (car c) :less) (vm-less s e c d))
    ((eq (car c) :add) (vm-add s e c d))
    ((eq (car c) :sub) (vm-sub s e c d))
    ((eq (car c) :mul) (vm-mul s e c d))
    ((eq (car c) :div) (vm-div s e c d))
    ((eq (car c) :rem) (vm-rem s e c d))
    ((eq (car c) :atom) (vm-atom s e c d))
    ((eq (car c) :symbolp) (vm-symbolp s e c d))
    ((eq (car c) :integerp) (vm-integerp s e c d))
    (t (error "~{~A ~}" (list s e c d)))))

(defun vm-step (secd)
  (destructuring-bind (s e c d) secd
    (vm s e c d)))

(defun vm-run (s e c d)
  (do ((state (list s e c d))
       (stop nil))
      (stop state) 
    (multiple-value-bind (next-state stop?) (vm-step state)
      (setf state next-state)
      (setf stop stop?))))

(defun make-vm (c)
  (let ((state (list nil nil c nil))
        (stop nil))
    (lambda ()
      (if stop
          state
          (multiple-value-bind (next-state stop?) (vm-step state)
            (setf state next-state)
            (setf stop stop?)
            state)))))
  
