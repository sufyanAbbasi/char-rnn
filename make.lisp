;;===============================
;; Psuedo make file
;;==============================

(defparameter *list-o-files*
    (list "rnn-init" "input"))

(defun make
    ()
  (dolist (file *list-o-files*)
    (compile-file file)
    (load file))
  (setf bob (init-rnn 2 2))
  (setf ar (make-array 2))
  (setf ar0 (make-array 2))
  (setf ar1 (make-array 2))
  (setf (aref ar0 0) .1)
  (setf (aref ar0 1) .1)
  (setf (aref ar1 0) .1)
  (setf (aref ar1 1) .1)
  (setf (aref ar 0) ar0)
  (setf (aref ar 1) ar1)
  )
