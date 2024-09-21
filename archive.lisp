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
  (labels ((field (x) (cdr (assoc x tx :test #'equal)))
           (from-integer (x) (ironclad:integer-to-octets (hex-string-to-integer (field x))))
           (from-string (x) (let* ((s (or (field x) ""))
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

(defun rlp-encode (x)
  "RLP encodes the list passed following the format described here: https://ethereum.org/en/developers/docs/data-structures-and-encoding/rlp/.
                    x is a list of byte arrays."
  (cond ((null x) (list #xc0))
        ((listp x) (let* ((encoded-x (mapcar #'rlp-encode x))
                          (lengths (mapcar #'length encoded-x))
                          (total-length (reduce #'+ lengths))
                          (total-length-bytes (map 'list #'identity (ironclad:integer-to-octets total-length)))
                          (bytes (reduce #'append encoded-x)))
                     (if (<= 0 total-length 55)
                         (cons (+ #xc0 total-length) bytes)
                         (cons (+ #xf7 (length total-length-bytes)) (append total-length-bytes bytes)))))
        (t (let*  ((bytes (map 'list #'identity x))
                   (bytes-length (length x))
                   (bytes-length-bytes (map 'list #'identity (ironclad:integer-to-octets bytes-length))))
             (cond ((and (= bytes-length 1) (<= (nth 0 bytes) #x7f)) bytes)
                   ((<= 0 bytes-length 55) (cons (+ #x80 bytes-length) bytes))
                   (t (cons (+ #xb7 (length bytes-length-bytes)) (append bytes-length-bytes bytes))))))))

(defun transaction->hash (tx)
  "Computes the hash of a transaction."
  (ironclad:byte-array-to-hex-string
   (ironclad:digest-sequence
    :keccak/256
    (coerce (cons 2 (rlp-encode (transaction->flat tx))) '(simple-array (unsigned-byte 8) (*))))))

#+nil 
(defun encode-transaction (tx pk)
  "Produces a raw transaction to be sent to the blockchain. It follows the format described here: https://eips.ethereum.org/EIPS/eip-1559.
                  Signing is done through a Python library as ironclad doesn't return `v`. We initially signed through geth, but signing for a different chain id seems unsupported. Sending the whole transaction structure to web3.py is not satisfactory either as the library tampers with the contents (for instance, adds a gas field to a EIP-1559 transaction.)"
  (let* ((flat (transaction->flat tx))
         (hash (transaction->hash tx))
         (signature (jzon:parse (ecdsa-raw-sign hash pk)))
         (v (gethash "v" signature))
         (r (gethash "r" signature))
         (s (gethash "s" signature))
         (concatenated (append flat (list (ironclad:integer-to-octets v) (ironclad:integer-to-octets r) (ironclad:integer-to-octets s)))))
    (concatenate 'string "0x02"
                 (ironclad:byte-array-to-hex-string
                  (coerce (rlp-encode concatenated) '(simple-array (unsigned-byte 8) (*)))))))

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

(defun rlp-decode (x)
  "RLP decodes the list passed following the format described here: https://eth.wiki/en/fundamentals/rlp.
            x is a byte array."
  (cond ((zerop (length x)) nil)
        ((<= 0 (elt x 0) #x7f) (cons (ironclad:byte-array-to-hex-string (subseq x 0 1)) (rlp-decode (subseq x 1))))
        ((<= #x80 (elt x 0) #xb7)
         (let ((length (- (elt x 0) #x80)))
           (cons (ironclad:byte-array-to-hex-string (subseq x 1 (+ 1 length))) (rlp-decode (subseq x (+ 1 length))))))
        ((<= #xb8 (elt x 0) #xbf)
         (let* ((length-length (- (elt x 0) #xb7))
                (length (ironclad:octets-to-integer (subseq x 1 (+ 1 length-length)))))
           (cons (ironclad:byte-array-to-hex-string (subseq x (+ 1 length-length) (+ 1 length-length length))) (rlp-decode (subseq x (+ 1 length-length length))))))
        ((= #xc0 (elt x 0)) (append (list nil) (rlp-decode (subseq x 1))))
        ((< #xc0 (elt x 0) #xf7)
         (let ((length (- (elt x 0) #xc0)))
           (append (rlp-decode (subseq x 1 (+ 1 length))) (rlp-decode (subseq x (+ 1 length))))))
        ((<= #xf8 (elt x 0) #xff)
         (let* ((length-length (- (elt x 0) #xf7))
                (length (ironclad:octets-to-integer (subseq x 1 (+ 1 length-length)))))
           (append (rlp-decode (subseq x (+ 1 length-length) (+ 1 length-length length))) (rlp-decode (subseq x (+ 1 length-length length))))))))

#+nil
(defun decode-transaction (raw)
  "Decodes a raw (hex string) transaction."
  (let ((x (rlp-decode (ironclad:hex-string-to-byte-array (subseq raw 4)))))
    (values (make-instance 'transaction :type (hex-string-to-integer (subseq raw 0 4))
                                        :chain-id (ironclad:octets-to-integer (ironclad:hex-string-to-byte-array (nth 0 x)))
                                        :nonce (ironclad:octets-to-integer (ironclad:hex-string-to-byte-array (nth 1 x)))
                                        :max-priority-fee-per-gas (ironclad:octets-to-integer (ironclad:hex-string-to-byte-array (nth 2 x)))
                                        :max-fee-per-gas (ironclad:octets-to-integer (ironclad:hex-string-to-byte-array (nth 3 x)))
                                        :gas (ironclad:octets-to-integer (ironclad:hex-string-to-byte-array (nth 4 x)))
                                        :to (if (zerop (length (nth 5 x))) "" (format nil "0x~a" (nth 5 x)))
                                        :value (ironclad:octets-to-integer (ironclad:hex-string-to-byte-array (nth 6 x)))
                                        :data (if (zerop (length (nth 7 x))) "" (format nil "0x~a" (nth 7 x)))
                                        :from (gethash "result" (jzon:parse (recover-address raw))))
            (ironclad:octets-to-integer (ironclad:hex-string-to-byte-array (nth 9 x)))
            (format nil "0x~a" (nth 10 x))
            (format nil "0x~a" (nth 11 x)))))

(defclass contract ()
  ((name 
    :reader contract-name)
   (data
    :reader contract-data)
   (address
    :reader contract-address)
   (call-transaction-type 
    :initform 'call-transaction
    :reader call-transaction-type)
   (deploy-transaction-type
    :initform 'deploy-transaction
    :reader deploy-transaction-type)))

(defmethod initialize-instance :after ((contract contract) &key path)
  "Build a contract object from a file."
  #+nil
  (log:info path)
  (when path
    (let* ((abbreviated-file (format nil "~a.~a" (pathname-name path) (pathname-type path)))
           (class-name (pathname-name path))
           (data (make-contract
                  (format nil "~a:~a" abbreviated-file class-name)
                  (format nil "~a~a.json" (make-pathname :directory (pathname-directory path)) (pathname-name path)))))
      (setf (slot-value contract 'name) class-name)
      (setf (slot-value contract 'data) data))))

#+nil
(defun make-contract (name file)
  "Makes a contract object from a solc compiled json file.
          solc --combined-json bin,bin-runtime,abi,storage-layout,hashes filename.sol -o . --overwrite"
  (gethash name (gethash "contracts" (with-open-file (in file) (jzon:parse in)))))

(defclass deploy-transaction (transaction)
  ((contract
    :initform (error "Must provide contract.")
    :initarg :contract
    :reader transaction-contract)
   logs))

(define-condition wrong-number-of-parameters (error)
  ((contract :initarg :contract)
   (required :initarg :required)
   (provided :initarg :provided)))

#+nil
(defmethod initialize-instance :after ((tx deploy-transaction) &key parameters)
  "Build the contract deploy transaction."
  (let* ((data (contract-data (transaction-contract tx)))
         (bytecode (bytecode data))
         (required (parameters data "constructor"))
         (count (parameters-count required))
         (encoded (if (constructor data)
                      (if (= count (length parameters))
                          (gethash "result" (jzon:parse (apply #'encode-parameters (parameters data "constructor") parameters)))
                          (error 'wrong-number-of-parameters :contract (transaction-contract tx) :required required :provided parameters))
                      "")))
    (setf (slot-value tx 'data) (format nil "0x~a~a" bytecode encoded))))

(defmethod wait :after ((tx deploy-transaction))
  "Set the contract address."
  (setf (slot-value (transaction-contract tx) 'address) (gethash "contractAddress" (transaction-receipt tx))))    

(defun bytecode (contract)
  "Returns the contract's bytecode."
  (gethash "bin" contract))

(defun parameters (contract signature)
  "Returns an entry's parameters, i.e. a signature without the name."
  (if (equal signature "constructor")
      (if (constructor contract)
          (arguments (gethash "inputs" (constructor contract)))
          "()")
      (subseq signature (position #\( signature))))

(defun parameters-count (parameters)
  "Returns the amount of parameters."
  (if (string= "()" parameters)
      0
      (1+ (count #\, parameters))))

(defun constructor (contract)
  "Returns the contract's constructor."
  (find-if #'(lambda (x) (equal (gethash "type" x) "constructor")) (abi contract)))

(defun arguments (v)
  "Returns the arguments list from an input or output list. Flattens structs."
  (labels ((rec (l)
             (cond ((null l) "")
                   ((or (equal "tuple" (gethash "type" (car l))) (equal "tuple[]" (gethash "type" (car l))))
                    (format nil
                            "(~a)~a~a~a"
                            (rec (map 'list #'identity (gethash "components" (car l))))
                            (if (equal "tuple[]" (gethash "type" (car l))) "[]" "")
                            (if (null (cdr l)) "" ",")
                            (rec (cdr l))))
                   (t (format nil "~a~a~a" (gethash "type" (car l)) (if (null (cdr l)) "" ",") (rec (cdr l)))))))
    (format nil "(~a)" (rec (map 'list #'identity v)))))

(defclass call-transaction (transaction)
  ((contract :initform (error "Must provide contract.")
             :initarg :contract
             :reader transaction-contract)
   (signature :initform (error "Must provide signature.")
              :initarg :signature)
   (result :reader transaction-result)
   logs))

(define-condition no-code (error)
  ((address :initarg :address)))

#+nil
(defmethod initialize-instance :after ((tx call-transaction) &key parameters)
  "Build the contract call transaction."
  (with-slots (signature to) tx
    (let ((address (if (slot-boundp tx 'to) to (contract-address (transaction-contract tx))))
          (data (contract-data (transaction-contract tx))))
      (abi-entry data signature)   ; check that the signature is valid
      (unless address (error "null address"))
      (let ((code (eth/get-code address "latest")))
        (when (string= code "0x")
          (error 'no-code :address address)))
      (if (= (parameters-count (parameters data signature)) (length parameters))
          (let ((encoded (gethash "result" (jzon:parse (apply #'encode-parameters (parameters data signature) parameters)))))
            (setf (slot-value tx 'to) address)
            (setf (slot-value tx 'data) (format nil "0x~a~a" (hash data signature) encoded)))
          (error 'wrong-number-of-parameters :contract (transaction-contract tx) :required (parameters data signature) :provided parameters)))))

#+nil ;; should be DEFMETHOD?
(defun call ((contract contract) &key address account signature (value 0) parameters)
  "Build a transaction, send it to eth/call and return it."
  (handler-case
      (let* ((tx (make-instance (call-transaction-type contract)
                                :contract contract
                                :signature signature
                                :value value
                                :to (if address address (contract-address contract))
                                :from account
                                :parameters parameters))
             (result (eth/call (transaction-payload tx) "latest")))
        (setf (slot-value tx 'result) (decode-result (contract-data contract) signature result))
        tx)
    (rpc-error (e)
      (when (data e) (setf (decoded-data e) (decode-error (contract-data contract) (data e))))
      (error e))))

#+nil
(defun decode-result (contract signature data)
  "Decode a function return value."
  (gethash "result" (jzon:parse (decode-parameters (arguments (gethash "outputs" (abi-entry contract signature))) data))))

#+nil
(defun decode-error (contract data)
  "Decode a contract error."
  (let* ((hash (subseq data 2 10))
         (signature (or (hash->signature contract hash) "Error(string)"))
         (parameters (parameters contract signature))
         (data (subseq data 10)))
    (list signature (gethash "result" (jzon:parse (decode-parameters parameters data))))))

(defun hash->signature (contract hash)
  "Gets an entry's signature from a hash."
  (let ((hm (gethash "hashes" contract)))
    (loop for k being the hash-keys in hm using (hash-value v)
          do (when (string= v hash) (return-from hash->signature k))))
  ;;this is an error or an event - those aren't output in the abi's hashes section.
  (let ((signatures (signatures contract)))
    (find-if #'(lambda(x) (equal hash (subseq (ironclad:byte-array-to-hex-string (ironclad:digest-sequence :keccak/256 (ironclad:ascii-string-to-byte-array x))) 0 8))) signatures)))

(defmethod send :before ((tx call-transaction))
  "Check that the contract call is not read-only."
  (when (read-only (contract-data (transaction-contract tx)) (slot-value tx 'signature))
    (error 'read-only :transaction tx)))

(defmethod wait :after ((tx call-transaction))
  "Decode the logs from the transaction receipt."
  (let ((data (contract-data (transaction-contract tx)))
        (address (contract-address (transaction-contract tx))))
    (setf (slot-value tx 'logs) (decode-logs data address (coerce (gethash "logs" (transaction-receipt tx)) 'list)))))

(defun decode-logs (contract address logs)
  "Decode logs from a transaction receipt."
  (mapcar #'(lambda (x) (decode-log contract address x)) logs))

#+nil
(defun decode-log (contract address log)
  "Decode a log entry. This handles indexed reference types too."
  (when (string= address (gethash "address" log))
    (let* ((topic (elt (gethash "topics" log) 0))
           (indexed (rest (coerce (gethash "topics" log) 'list)))
           (signature (hash->signature contract (subseq topic 2 10)))
           (inputs (coerce (gethash "inputs" (abi-entry contract signature)) 'list))
           (decoded-data (coerce (gethash "result" (jzon:parse (decode-parameters (format nil "(~{~a~^,~})" (map 'list #'(lambda (x) (gethash "type" x)) (remove-if-not #'(lambda (x) (not (gethash "indexed" x))) inputs))) (gethash "data" log)))) 'list)))
      (do ((inputs inputs (rest inputs))
           (current (car inputs) (cadr inputs))
           (indexed-index 0)
           (non-indexed-index 0)
           (result nil))
          ((null inputs) (cons signature (nreverse result)))
        (if (gethash "indexed" current)
            (progn
              (if (reference-p (gethash "type" current))
                  (push (nth indexed-index indexed) result) ; we cannot decode indexed reference types as they are hashes
                  (push (gethash "result" (jzon:parse (decode-parameters (gethash "type" current) (nth indexed-index indexed)))) result))
              (incf indexed-index))
            (progn
              (push (nth non-indexed-index decoded-data) result)
              (incf non-indexed-index)))))))
