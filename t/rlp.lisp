(in-package :cl-user)

(prove:plan 1)
(prove:ok
  (equalp
   (eth::rlp-encode "foo")
   '(131 #\f #\o #\o))
  "Able to RLP encode a simple string")

#|  FIXME
(prove:plan 1)
(let ((string "a simple string"))
  (prove:ok
   (equalp
    string
    (eth::rlp-decode (eth::rlp-encode string)))
"Able to roundtrip RLP encoding on a short string"))

|#

(prove:finalize)

  
            
