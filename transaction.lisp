(in-package eth)

(defclass transaction ()
  ((type 
    :initarg :type
    :initform 2)
   (chain-id
    :initform (eth/chain-id)
    :reader transaction-chain-id)
   (nonce
    :initarg :nonce)
   (max-priority-fee-per-gas
    :initarg :max-priority-fee-per-gas
    :initform (round (* 2.5 (expt 10 9))))
   (max-fee-per-gas
    :initarg :max-fee-per-gas)
   (gas
    :initarg :gas)
   (to
    :initarg :to)
   (from
    :initarg :from
    :initform (error "Must provide an account to send from.")
    :reader transaction-from)
   (value
    :initarg :value
    :initform 0)
   (data
    :initarg :data
    :reader transaction-data)
   (hash
    :reader transaction-hash)
   (receipt
    :reader transaction-receipt
    :initform nil)))

(defmethod initialize-instance :after ((tx transaction) &key)
  ;; Initialise max-fee-per-gas
  (unless (slot-boundp tx 'max-fee-per-gas)
    (with-slots (max-fee-per-gas max-priority-fee-per-gas) tx
      (setf max-fee-per-gas (+ max-priority-fee-per-gas (* 2 (eth/gas-price)))))))

(defun transaction-payload (tx)
  "Return the payload for tx."
  (with-slots (type chain-id nonce max-priority-fee-per-gas max-fee-per-gas gas to from value data) tx
    (let* ((result `(("type" . ,(format nil "0x~x" type))
                     ("chainId" . ,(format nil "0x~x" chain-id))
                     ("maxPriorityFeePerGas" . ,(format nil "0x~x" max-priority-fee-per-gas))
                     ("maxFeePerGas" . ,(format nil "0x~x" max-fee-per-gas))
                     ("from" . ,(address from))
                     ("value" . ,(format nil "0x~x" value)))))
                                        ;("nonce" . ,(format nil "0x~x" nonce))
      (when (slot-boundp tx 'nonce) (setf result (append result (list (cons "nonce" (format nil "0x~x" nonce))))))
      (when (slot-boundp tx 'data) (setf result (append result (list (cons "input" data)))))
      (when (slot-boundp tx 'to) (setf result (append result (list (cons "to" to)))))
      (when (slot-boundp tx 'gas) (setf result (append result (list (cons "gas" (format nil "0x~x" gas))))))
      result)))

(defun transaction->flat (tx)
  "Flattens a transaction in the format described here: https://eips.ethereum.org/EIPS/eip-1559."
  (labels ((field (x)
             (cdr (assoc x tx :test #'equal)))
           (from-integer (x)
             (ironclad:integer-to-octets (parse-integer (field x) :radix 16)))
           (from-string (x)
             (let* ((s (or (field x) ""))
                    (s (if (zerop (length s)) s (subseq s 2))))
               (ironclad:hex-string-to-byte-array s))))
    (list (from-integer "chainId")
          (from-integer "nonce")
          (from-integer "maxPriorityFeePerGas")
          (from-integer "maxFeePerGas")
          (from-integer "gas")
          (from-string "to")
          (from-integer "value")
          (from-string "input")
          nil)))

(defun transaction->hash (tx)
  "Computes the hash of a transaction."
  (ironclad:byte-array-to-hex-string
   (ironclad:digest-sequence
    :keccak/256 (coerce
                 (cons 2
                       (rlp-encode (transaction->flat tx))) '(simple-array (unsigned-byte 8) (*))))))

(defun encode-transaction (tx pk)
  "Produces a raw transaction to be sent to the blockchain

The transaction is encoded in the format describe in <https://eips.ethereum.org/EIPS/eip-1559>

Signing is done through a Python library as ironclad doesn't return
`v`. We initially signed through geth, but signing for a different
chain id seems unsupported. Sending the whole transaction structure to
web3.py is not satisfactory either as the library tampers with the
contents (for instance, adds a gas field to a EIP-1559 transaction.)"
  (let* ((flat (transaction->flat tx))
         (hash (transaction->hash tx))
         (jsown (jsown:parse (ecdsa-raw-sign hash pk)))
         (signature (jsown-as-hashtable jsown))
         (v (gethash "v" signature))
         (r (gethash "r" signature))
         (s (gethash "s" signature))
         (concatenated
           (append flat (list (ironclad:integer-to-octets v)
                              (ironclad:integer-to-octets r)
                              (ironclad:integer-to-octets s)))))
    (concatenate 'string "0x02"
                 (ironclad:byte-array-to-hex-string
                  (coerce (rlp-encode concatenated) '(simple-array (unsigned-byte 8) (*)))))))

(defun ecdsa-raw-sign (hash pk)
  (declare (ignore hash pk))
  (error "Unimplemented ECDSA signature"))


(defun decode-transaction (raw)
  "Decodes a raw (hex string) transaction."
  (let ((x (rlp-decode (ironclad:hex-string-to-byte-array (subseq raw 4)))))
    (values
     (make-instance 'transaction
                    :type (hex-string-to-integer
                           (subseq raw 0 4))
                    :chain-id (ironclad:octets-to-integer
                               (ironclad:hex-string-to-byte-array (nth 0 x)))
                    :nonce (ironclad:octets-to-integer
                            (ironclad:hex-string-to-byte-array (nth 1 x)))
                    :max-priority-fee-per-gas (ironclad:octets-to-integer
                                               (ironclad:hex-string-to-byte-array (nth 2 x)))
                    :max-fee-per-gas (ironclad:octets-to-integer
                                      (ironclad:hex-string-to-byte-array (nth 3 x)))
                    :gas (ironclad:octets-to-integer
                          (ironclad:hex-string-to-byte-array (nth 4 x)))
                    :to (if (zerop (length (nth 5 x)))
                            ""
                            (format nil "0x~a" (nth 5 x)))
                    :value (ironclad:octets-to-integer
                            (ironclad:hex-string-to-byte-array (nth 6 x)))
                    :data (if (zerop (length (nth 7 x)))
                              ""
                              (format nil "0x~a" (nth 7 x)))
                    :from (gethash "result"
                                   (jsown-as-hashtable (jsown:parse (recover-address raw)))))
     (ironclad:octets-to-integer (ironclad:hex-string-to-byte-array (nth 9 x)))
     (format nil "0x~a" (nth 10 x))
     (format nil "0x~a" (nth 11 x)))))
