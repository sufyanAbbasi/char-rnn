;;===============================
;; Psuedo make file
;;==============================

(defparameter *list-o-files*
    (list "input" "rnn" "run-training"))

(defun make
  ()
  (dolist (file *list-o-files*)
    (compile-file file)
    (load file))
  )
