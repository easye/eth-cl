;;; -*- Mode: LISP; Syntax: COMMON-LISP -*-

(defsystem eth-cl
  :version "0.2.0"
  :long-description "Potentially useful code for generating Ethereum artifacts ~
Cribbed from <https://www.reddit.com/r/ethereum/comments/169nztd/an_ethereum_library_in_common_lisp/>"
  :author (#p"https://www.reddit.com/user/tinkagames_g" #p"urn:easye.not.org")
  :depends-on (ironclad
               alexandria)
  :components ((:file "package")
               (:file "ethereum")
               (:file "codex")
               #+nil ;; needs debugging, hooking to JSON serialization
               (:file "archive")))

(defsystem eth-cl/t
  :defsystem-depends-on (prove-asdf)  
  :depends-on (prove
               eth-cl)
  :perform (test-op (o c)
              (uiop:symbol-call :prove-asdf 'run-test-system c))
  :components ((:module test
                :pathname "./t/"
                :components ((:test-file "codex")))))



