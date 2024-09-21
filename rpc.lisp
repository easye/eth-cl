(in-package :eth/rpc)

(defmacro defrpc (method &optional (fn '#'identity))
  "Defines a function for a particular RPC method.

The name is converted to Common-Lisp style,
i.e. eth_getBalance -> eth/get-balance.

Returns the value of the key 'result' from the rpc call.

An optional function transforms the output, i.e.

  (eth/get-balance \"0xfde5e13d440a0c73b231766c76ac3268b52db346\" \"latest\")

returns 10 instead of \"0xa\"."
  (labels ((camel-case-to-kebab-case (s)
             (labels ((rec (l)
                        (cond ((null l) nil)
                              ((equal (car l) #\_) (cons #\/ (rec (rest l))))
                              ((upper-case-p (car l)) (append (list #\- (char-downcase (car l))) (rec (rest l))))
                              (t (cons (car l) (rec (rest l)))))))
               (map 'string #'identity (rec (map 'list #'identity s))))))
    (let ((name
            (camel-case-to-kebab-case method))
          (export-function-p ;; not working
            nil))
      `(values
        (defun ,(read-from-string name) (&rest params)
         (let ((response (jsown:parse (apply #'rpc (append (list *host* ,method) params)))))
           (multiple-value-bind (v p) (gethash "error" response)
             (if p
                 (error 'rpc-error :method ,name :code (gethash "code" v) :text (gethash "message" v) :data (gethash "data" v))
                 (funcall ,fn (gethash "result" response))))))
        ,(when export-function-p
           `(export (intern ,(string-upcase name) :eth/rpc)))))))

(defrpc "eth_getBalance" #'hex-string-to-integer)
(defrpc "eth_getTransactionCount" #'hex-string-to-integer)
(defrpc "eth_sendRawTransaction")
(defrpc "eth_getTransactionReceipt")
(defrpc "eth_call")

