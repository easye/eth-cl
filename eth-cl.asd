(defsystem eth-cl
  :depends-on (ironclad
               alexandria)
  :components ((:file "package")
               (:file "ethereum")
               (:file "codex")))


