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
  (setf bob (init-rnn 5 50))
  (setf iar1 (make-array 3 :initial-contents '(.5 .5 .5)))
  (setf iar2 (make-array 3 :initial-contents '(.5 .5 .5)))
  (setf iar3 (make-array 3 :initial-contents '(.5 .5 .5)))
  (setf iar4 (make-array 3 :initial-contents '(.5 .5 .5)))
  (setf iar5 (make-array 3 :initial-contents '(.5 .5 .5)))
  (setf iar (make-array 5 :initial-contents (list iar1 iar2 iar3 iar4 iar5)))

  (setf ar1 (make-array 3 :initial-contents '(1 0 0)))
  (setf ar2 (make-array 3 :initial-contents '(1 0 0)))
  (setf ar3 (make-array 3 :initial-contents '(0 1 0)))
  (setf ar4 (make-array 3 :initial-contents '(0 0 1)))
  (setf ar5 (make-array 3 :initial-contents '(0 0 1)))
  (setf ar (make-array 5 :initial-contents (list ar1 ar2 ar3 ar4 ar5)))

  )
