(uiop/package:define-package :secd-vm/secd-compiler (:use :cl))
(in-package :secd-vm/secd-compiler)
;;;don't edit above

(defun compile-to-secd (in code)
  (if (atom in)
      (cond ((integerp in)
             (cons :ldc (cons in code)))
            ((symbolp in)
             (cons :ldc (cons in code)))
            (t (error "~A is a not support type value" in))) ;; TODO
      (let ((op (car in))
            (args (cdr in)))
        (cond ((eq op 'cons)
               (append (loop for arg in args
                           append (compile-to-secd arg nil))
                       (cons :cons code)))
              ((eq op 'car)
               (append (loop for arg in args
                           append (compile-to-secd arg nil))
                       (cons :car code)))
              ((eq op 'cdr)
               (append (loop for arg in args
                           append (compile-to-secd arg nil))
                       (cons :cdr code)))
              (t (error "~A is a not support type operator" op))
              ))))