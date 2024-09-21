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

