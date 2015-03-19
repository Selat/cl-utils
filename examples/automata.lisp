;; Macros automation generates set of function for processing finite automata
;; Example of use:
;; (setf tmp (automaton init machine))
;; (funcall tmp '(c a a d r))

(defvar machine
  '((init (c . more))
    (more (a . more)
     (d . more)
     (r . end))
    (end accept)))

(defun run-automaton (machine init-state stream)
  (labels ((walker (state stream)
             (if (null stream) t
                 (let* ((l (rest (assoc state machine)))
                        (next-state (assoc (first stream) l)))
                   (if (null next-state) nil
                       (walker (rest next-state) (rest stream)))))))
    (walker init-state stream)))

(defmacro process-state (state-list)
  (if (atom (first state-list)) (if (eq (first state-list) 'accept) '(null stream) nil)
      (append '(case) '((first stream))
              (loop for trans in state-list collect
                    `(,(first trans) (,(rest trans) (rest stream))))
              '((otherwise nil)))))

(defmacro automaton (init-state m)
  `(labels ,(loop for state in (symbol-value m)
                  for x = (rest state)
                  collect `(,(first state) (stream) ,(macroexpand `(process-state ,x))))
     (function ,init-state)))
