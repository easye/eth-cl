
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
