(in-package :eth)

(defgeneric send (transaction)
  (:documentation "Send the transaction to the blockchain."))

(defmethod send :before ((tx transaction))
  (unless (slot-boundp tx 'nonce)
    (with-slots (from nonce) tx
      (setf nonce (eth/get-transaction-count (address from) "pending"))))
  (unless (slot-boundp tx 'gas)
    (with-slots (gas) tx
      (setf gas (eth/estimate-gas (transaction-payload tx))))))

(defmethod send ((tx transaction))
  "Send the transaction to the blockchain."
  (let* ((payload (transaction-payload tx))
         (encoded (encode-transaction payload (private-key (transaction-from tx))))
         (hash (eth/send-raw-transaction encoded)))
    (setf (slot-value tx 'hash) hash))
  tx)

(defclass concurrent-transaction () ())

(defmethod send ((tx concurrent-transaction))
  "Lock the nonce."
  #+nil
  (log:info "Locking!")
  (with-redis-lock
      (with-slots (from nonce) tx
        (setf nonce (eth/get-transaction-count (address from) "pending")))
    (call-next-method)))

(defgeneric wait (transaction)
  (:documentation "Wait for the transaction to be minted."))

              ;;; Signalled when waiting on an unsent transaction.
(define-condition unsent (error)
  ((transaction :initarg :transaction)))

(defmethod wait ((tx transaction))
  "Wait for the transaction to be minted."
  (unless (slot-boundp tx 'hash)
    (error 'unsent :transaction tx))
  (setf (slot-value tx 'receipt)
        (do ((receipt nil (eth/get-transaction-receipt (transaction-hash tx))))
            ((and receipt (not (equal 'null receipt))) receipt)
          (sleep 0.1)))
  tx)


