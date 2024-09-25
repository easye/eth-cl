(in-package :eth)

(defun generate-ethereum-keypair/codex (&key (directory #p"~/etc/"))
  (let* ((account
           (make-account))
         (address
           (address account))
         (address-pathname
           (merge-pathnames "eth.address" directory))
         (private-key
           (private-key account))
         (private-key-pathname
           (merge-pathnames "eth.key" directory)))
    (format *standard-output* "Writing Ethereum keypair to ~a" directory)
    (ensure-directories-exist directory)
    (alexandria:write-string-into-file
     address address-pathname :if-exists :supersede)
    (alexandria:write-string-into-file
     private-key private-key-pathname :if-exists :supersede)
    (values account
            address-pathname
            private-key-pathname)))

          
