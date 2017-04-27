;;===============================
;; Psuedo make file
;;==============================

(defparameter *list-o-files*
    (list "rnn-init"))

(defun make
    ()
  (dolist (file *list-o-files*)
    (compile-file file)
    (load file)))
