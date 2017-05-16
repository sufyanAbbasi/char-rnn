;;===============================
;; Psuedo make file
;;==============================

(defparameter *list-o-files*
    (list "rnn" "input" "run-training"))

(defun make
  ()
  (dolist (file *list-o-files*)
    (compile-file file)
    (load file))
  )
