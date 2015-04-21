(defmacro printf (s)
  "Convinient way to print contence of variables
  The syntax is the following: (printf \"text: {var-name}\")"
  (let ((format-s (make-string 0))
        (var-list nil))
    (loop for i = (position #\{ s) until (not i)
          for j = (position #\} s) when (or (not j) (< j i)) return (error "Mismatched { and }") do
            (setf format-s (concatenate 'string format-s (subseq s 0 i) "~A"))
            (setf var-list (cons (find-symbol (string-upcase (subseq s (1+ i) j))) var-list))
            (setf s (subseq s (1+ j) (length s))))
    (setf format-s (concatenate 'string format-s s))
    `(format t ,format-s ,@var-list)))
