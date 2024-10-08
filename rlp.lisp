(in-package :eth)

;;;; "Recursive Length Prefix encoding"
;;;;   :see <https://ethereum.org/en/developers/docs/data-structures-and-encoding/rlp/>

(defun rlp-encode (x)
  "Encodes the contents of X under Recursive Length Prefix

X is a list of byte arrays.

See <https://ethereum.org/en/developers/docs/data-structures-and-encoding/rlp/>."
  (cond ((null x)
         (list #xc0))
        ((listp x)
         (let* ((encoded-x (mapcar #'rlp-encode x))
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

(defun rlp-decode (x)
  "Decodes the byte array X as Recursive Length Prefix

See <https://ethereum.org/en/developers/docs/data-structures-and-encoding/rlp/>."
  (cond ((zerop (length x))
         nil)
        ((<= 0 (elt x 0) #x7f)
         (cons (ironclad:byte-array-to-hex-string (subseq x 0 1))
               (rlp-decode (subseq x 1))))
        ((<= #x80 (elt x 0) #xb7)
         (let ((length (- (elt x 0) #x80)))
           (cons (ironclad:byte-array-to-hex-string (subseq x 1 (+ 1 length)))
                 (rlp-decode (subseq x (+ 1 length))))))
        ((<= #xb8 (elt x 0) #xbf)
         (let* ((length-length
                  (- (elt x 0) #xb7))
                (length
                  (ironclad:octets-to-integer (subseq x 1 (+ 1 length-length)))))
           (cons (ironclad:byte-array-to-hex-string
                  (subseq x (+ 1 length-length) (+ 1 length-length length)))
                 (rlp-decode (subseq x (+ 1 length-length length))))))
        ((= #xc0 (elt x 0))
         (append (list nil)
                 (rlp-decode (subseq x 1))))
        ((< #xc0 (elt x 0) #xf7)
         (let ((length (- (elt x 0) #xc0)))
           (append (rlp-decode (subseq x 1 (+ 1 length)))
                   (rlp-decode (subseq x (+ 1 length))))))
        ((<= #xf8 (elt x 0) #xff)
         (let* ((length-length
                  (- (elt x 0) #xf7))
                (length
                  (ironclad:octets-to-integer (subseq x 1 (+ 1 length-length)))))
           (append (rlp-decode (subseq x (+ 1 length-length) (+ 1 length-length length)))
                   (rlp-decode (subseq x (+ 1 length-length length))))))))

(defun as-bytes (x)
  (flet ((as-integers (l)
           (mapcar (lambda (a)
                     (if (subtypep (type-of a) 'standard-char) ;;sbcl
                         (char-code a)
                         a))
                   l)))
    (make-array (length x)
                :element-type '(unsigned-byte 8)
                :initial-contents (as-integers x))))
