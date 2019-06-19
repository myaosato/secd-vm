(uiop/package:define-package :secd-vm/mini-lisp 
  (:nicknames) 
  (:use :cl)
  (:shadowing-import-from :secd-vm/mini-lisp-compiler #:compile)
  (:import-from :secd-vm/secd-vm #:vm-run)
  (:export :run :as-mini-lisp)
  (:intern))
(in-package :secd-vm/mini-lisp)
;;don't edit above

(defun run (code)
  (vm-run nil nil (compile code nil nil) nil))

(defmacro as-mini-lisp (body)
  `(run ',body))