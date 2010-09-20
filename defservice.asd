(defsystem :defservice
  :components ((:file "trivial-utf-8")
               (:file "defservice" :depends-on ("trivial-utf-8"))))
