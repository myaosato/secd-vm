;;don't edit
(defsystem "secd-vm"
  :class :package-inferred-system
  :components ((:file "secd-vm")
               (:file "mini-lisp-compiler")
               (:file "mini-lisp" :depends-on ("secd-vm" "mini-lisp-compiler")))
  :author "myaosato"
  :mailto "tetu60u@yahoo.co.jp")
