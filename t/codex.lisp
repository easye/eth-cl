(in-package :cl-user)

(prove:plan 1)
(prove:ok
 (eth::compute-address
  (alexandria:assoc-value (eth:make-account) :private-key))
 "Able to make an Ethereum account, and serialize the address")

(prove:finalize)


