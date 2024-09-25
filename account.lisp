(in-package :eth)

(defun compute-address (private-key)
  "Returns the Ethereum public address of a private key

<https://github.com/ethereumbook/ethereumbook/blob/develop/04keys-addresses.asciidoc>.

The capitalization of the public address is implemented using the
method described in <https://github.com/Ethereum/EIPs/blob/master/EIPS/eip-55.md>"
  (let* ((hex
           (ironclad:byte-array-to-hex-string
                    (ironclad:digest-sequence :keccak/256
                                              (subseq (getf (ironclad:destructure-private-key private-key) :y) 1))))
         (address
           (subseq hex 24)))
    (loop :for i :from 0 :to (1- (length address))
          :with hash
            = (ironclad:byte-array-to-hex-string
               (ironclad:digest-sequence :keccak/256 (ironclad:ascii-string-to-byte-array address)))
          :when (char>= (elt hash i) #\8)
            :do (setf (elt address i)
                      (char-upcase (elt address i))))
    address))


(defun make-account (&optional secret)
  "Make an Ethereum account chosen randomly

Optionally SECRET a hex string encoding of the private key."
  (let* ((bytes
           (if secret
               (ironclad:hex-string-to-byte-array secret)
               (ironclad:random-data 32)))
         (private-key
           (ironclad:make-private-key :secp256k1
                                      :x bytes))
         (public-key
           (ironclad:make-public-key :secp256k1
                                     :y (getf (ironclad:destructure-private-key private-key) :y)))
         (address
           (compute-address private-key)))
    `((:private-key . ,private-key)
      (:public-key . ,public-key)
      (:address . ,address))))

(defun address (account)
  "Returns the address of that account as a hexidecimal string prefixed with 0x"
  (concatenate 'string "0x"
               (cdr (assoc :address account))))

(defun private-key (account)
  "Returns the private key of the account as a hexidecimal string prefixed with 0x"
  (concatenate 'string "0x"
               (ironclad:byte-array-to-hex-string
                (getf (ironclad:destructure-private-key
                       (cdr (assoc :private-key account))) :x))))
  
(defun public-key (account)
  "Returns the public key of the account."
  (cdr (assoc :public-key account)))

