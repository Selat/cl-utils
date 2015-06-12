(set-macro-character #\} (get-macro-character #\)))

;; Read #{a b} as list of all integers from a to b: (a (1+ a) ... (1- b) b)
(set-dispatch-macro-character
 #\# #\{
 (lambda (stream char1 char2)
   (let ((bounds (read-delimited-list #\} stream t)))
     (list 'quote (loop for i from (first bounds)
                          to (second bounds) collect i)))))

;; Convinient macro that enables representing hash tables in a set-like way:
;; #{1 3} - hash table that contains integers 1 and 3
(set-dispatch-macro-character
 #\# #\{
 (lambda (stream char1 char2)
   (let ((elements (read-delimited-list #\} stream t)))
     `(let ((ht (make-hash-table)))
        ,@(loop for x in elements collect
                `(setf (gethash ,x ht) t))
        ht))))

;; Double-macro: read Dx as (* 2 x)
(set-macro-character #\D (lambda (stream char)
                           `(* 2 ,(read stream t nil t))))

(defun prime-p (n)
  (loop for i from 2
        when (> (* i i) n) return t
          when (zerop (mod n i)) return nil))

(set-dispatch-macro-character #\# #\{ (lambda (stream char1 char2)
                                        (let ((bounds (read-delimited-list #\} stream t)))
                                          (list 'quote (loop for i from (first bounds) to (second bounds)
                                                             when (prime-p i) collect i)))))
