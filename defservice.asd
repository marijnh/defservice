(defsystem :defservice
  :components ((:file "url-encode")
               (:file "defservice" :depends-on ("url-encode"))))
