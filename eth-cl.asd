;;; -*- Mode: LISP; Syntax: COMMON-LISP -*-

(defsystem eth-cl/package
  :components ((:file "package")))

(defsystem eth-cl
  :version "0.3.0"
  :long-description "Potentially useful code for generating Ethereum artifacts ~
Cribbed from <https://www.reddit.com/r/ethereum/comments/169nztd/an_ethereum_library_in_common_lisp/>"
  :author (#p"https://www.reddit.com/user/tinkagames_g" #p"urn:easye.not.org")
  :depends-on (eth-cl/package
               ironclad
               alexandria)
  :in-order-to ((test-op (test-op eth-cl/t)))
  :components ((:module main :pathname "./"
                :components ((:file "rlp")
                             (:file "account")
                             (:file "transaction")
                             #+nil ;; needs debugging, hooking to JSON serialization
                             (:file "archive")))
               (:module codex :pathname "./"
                        :depends-on (main)
                :components ((:file "codex")))))

(defsystem eth-cl/t
  :defsystem-depends-on (prove-asdf)  
  :depends-on (prove
               eth-cl)
  :perform (test-op (o c)
              (uiop:symbol-call :prove-asdf 'run-test-system c))
  :components ((:module test
                :pathname "./t/"
                :components ((:test-file "rlp")
                             (:test-file "codex")))))

(defsystem eth-cl/rpc
  :depends-on (eth-cl/package
               jsown)
  :components ((:file "rpc")))
