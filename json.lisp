(in-package :eth)

(defun jsown-as-hashtable (jsown)
  (alexandria:alist-hash-table (rest jsown)))

            
