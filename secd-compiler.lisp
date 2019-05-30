(uiop/package:define-package :secd-vm/secd-compiler (:nicknames) (:use :cl)
                             (:shadow) (:export :compile-to-secd) (:intern))
(in-package :secd-vm/secd-compiler)
;;don't edit above

(defun compile-args (args names num)
  (if (/= (length args) num)
      (error "~A arguments expexted but given ~A" num (length args))
      (loop for ind from 0 to (1- num)
            append (compile-to-secd (nth ind args) names nil))))

(defun compile-list (exps names code)
  (if (eq exps nil)
      (cons :ldc (cons nil code))
      (compile-list (cdr exps)
                    names 
                    (compile-to-secd (car exps) names (cons :cons code)))))

(defun compile-embedded-op (op args num names code)
  (append (compile-args args names num) (cons op code)))

(defun compile-let-vars (list)
  (if list
      (cons (caar list) (compile-let-vars (cdr list)))
      nil))

(defun compile-let-exps (list)
  (if list
      (cons (cadar list) (compile-let-exps (cdr list)))
      nil))

(defun location (sym names)
  (if (member sym (car names))
      (cons 0 (position sym (car names)))
      (let ((loc (location sym (cdr names))))
        (cons (+ 1 (car loc)) (cdr loc)))))

(defun compile-to-secd (exp names code)
  (if (atom exp)
      (cond ((integerp exp)
             (cons :ldc (cons exp code)))
            ((symbolp exp)
             (cons :ld (cons (location exp names) code)))
            (t (error "~A is a not support type value" exp))) ;; TODO
      (let ((op (car exp))
            (args (cdr exp)))
        (cond ((eq op 'cons) (compile-embedded-op :cons (reverse args) 2 names code))
              ((eq op 'car) (compile-embedded-op :car args 1 names code))
              ((eq op 'cdr) (compile-embedded-op :cdr args 1 names code))
              ((eq op 'eq) (compile-embedded-op :eq args 2 names code))
              ((eq op '+) (compile-embedded-op :add args 2 names code))
              ((eq op '-) (compile-embedded-op :sub args 2 names code))
              ((eq op '<) (compile-embedded-op :less args 2 names code))
              ((eq op '>) (compile-embedded-op :less (reverse args) 2 names code))
              ((eq op 'atom) (compile-embedded-op :atom args 1 names code))
              ((eq op 'integerp) (compile-embedded-op :integerp args 1 names code))
              ((eq op 'symbolp) (compile-embedded-op :symbolp args 1 names code))
              ((eq op 'lambda)
               (let ((cf (compile-to-secd (caddr exp) 
                                          (cons (cadr exp) names) 
                                          (cons :rtn nil))))
                 (cons :ldf (cons cf code))))
              ((eq (car exp) 'let) ;; letは新しい束縛の環境でbodyを即評価するってことかな
               (let ((new-names (cons (compile-let-vars (cadr exp)) names))
                     (args (compile-let-exps (cadr exp))))
                 (let ((body (compile-to-secd (caddr exp)
                                              new-names
                                              (cons :rtn '()))))
                   (compile-list args names (cons :ldf (cons body (cons :ap code)))))))
              (t (compile-list (cdr exp)
                               names
                               (compile-to-secd (car exp) names (cons :ap code))))))))
