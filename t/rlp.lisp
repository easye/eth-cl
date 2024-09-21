(in-package :cl-user)

(prove:plan 1)
(prove:ok
  (equalp
   (eth::rlp-encode "foo")
   '(131 #\f #\o #\o))
  "Able to RLP encode a simple string")

(prove:plan 2)
(let* ((string
         "a simple string")
       (encoded
         (eth::rlp-encode string)))
  (prove:is
   (length string) (1- (length encoded))
   "RLP encoding length")
  (let* ((encoded-bytes
           (eth::as-bytes encoded))
         (decoded
           (eth::rlp-decode encoded-bytes)))
    (prove:is
     decoded string
     "Able to roundtrip RLP encoding on a short string")))

(prove:finalize)

  
            
