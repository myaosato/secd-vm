(uiop/package:define-package :secd-vm/mini-lisp-compiler (:nicknames) (:use :cl)
                             (:shadow :compile) (:export :compile) (:intern))
(in-package :secd-vm/mini-lisp-compiler)
;;don't edit above

(defun compile-args (args names num)
  (if (/= (length args) num)
      (error "~A arguments expexted but given ~A" num (length args))
      (loop for ind from 0 to (1- num)
            append (compile (nth ind args) names nil))))

(defun compile-list (exps names code)
  (if (null exps)
      (cons :ldc (cons nil code))
      (compile-list (cdr exps)
                    names 
                    (compile (car exps) names (cons :cons code)))))

(defun compile-embedded-op (op args num names code)
  (append (compile-args args names num) (cons op code)))

(defun get-let-vars (list)
  (if list
      (cons (caar list) (get-let-vars (cdr list)))
      nil))

(defun get-let-exps (list)
  (if list
      (cons (cadar list) (get-let-exps (cdr list)))
      nil))

(defun location (sym names)
  (if (null names) (error "unbound variable ~A" sym))
  (if (member sym (car names))
      (cons 0 (position sym (car names)))
      (let ((loc (location sym (cdr names))))
        (cons (+ 1 (car loc)) (cdr loc)))))

(defun compile-let (exp names code)
  ;; letは新しい束縛の環境でbodyを即評価するってことかな
  (let ((new-names (cons (get-let-vars (cadr exp)) names))
        (args (get-let-exps (cadr exp))))
    (let ((body (compile (caddr exp)
                                 new-names
                                 (cons :rtn nil))))
      (compile-list args names (cons :ldf (cons body (cons :ap code)))))))

(defun compile-let* (exp names code)
  (let ((new-names (cons (get-let-vars (cadr exp)) names))
        (args (get-let-exps (cadr exp))))
    (let ((body (compile (caddr exp)
                                 new-names
                                 (cons :rtn nil))))
      (cons :dum (compile-list args new-names (cons :ldf (cons body (cons :rap code))))))))

(defun compile-if (exp names code)
  (let ((code-then (compile (caddr exp) names (cons :join nil)))
        (code-else (compile (cadddr exp) names (cons :join nil))))
    (compile (cadr exp) 
             names
             (cons :sel (cons code-then (cons code-else code))))))

(defun compile-lambda (exp names code)
  (let ((cf (compile (caddr exp) 
                     (cons (cadr exp) names) 
                     (cons :rtn nil))))
    (cons :ldf (cons cf code))))

(defun compile-call (exp names code)
  (compile-list (cdr exp)
                names
                (compile (car exp) names (cons :ap code))))

(defun compile (exp names code)
  (if (atom exp)
      (cond ((integerp exp) (cons :ldc (cons exp code)))
            ((null exp) (cons :ldc (cons nil code)))
            ((symbolp exp) (cons :ld (cons (location exp names) code)))
            (t (error "~A is a not support type value" exp)))
      (let ((op (car exp))
            (args (cdr exp)))
        (cond ((eq op 'quote) (cons :ldc (cons (cadr exp) code)))
              ((eq op 'cons) (compile-embedded-op :cons (reverse args) 2 names code))
              ((eq op 'car) (compile-embedded-op :car args 1 names code))
              ((eq op 'cdr) (compile-embedded-op :cdr args 1 names code))
              ((eq op 'eq) (compile-embedded-op :eq args 2 names code))
              ((eq op '+) (compile-embedded-op :add args 2 names code))
              ((eq op '-) (compile-embedded-op :sub args 2 names code))
              ((eq op '*) (compile-embedded-op :mul args 2 names code))
              ((eq op '/) (compile-embedded-op :div args 2 names code))
              ((eq op 'rem) (compile-embedded-op :rem args 2 names code))
              ((eq op '<) (compile-embedded-op :less args 2 names code))
              ((eq op '>) (compile-embedded-op :less (reverse args) 2 names code))
              ((eq op 'atom) (compile-embedded-op :atom args 1 names code))
              ((eq op 'integerp) (compile-embedded-op :integerp args 1 names code))
              ((eq op 'symbolp) (compile-embedded-op :symbolp args 1 names code))
              ((eq op 'lambda) (compile-lambda exp names code))
              ((eq op 'let) (compile-let exp names code))
              ((eq op 'let*) (compile-let* exp names code))
              ((eq op 'if) (compile-if exp names code))
              (t (compile-call exp names code))))))
