(defsystem eth-cl
  :depends-on (ironclad)
  :components ((:file "package")
               (:file "ethereum")))

