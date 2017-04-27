;;=====================================
;; Implementing an character level RNN
;; Authors: Thomas Lum
;;          Sufyan Abbasi
;;=====================================

(defun process-chars (path-to-file process-func &rest func-params)
  (let ((in (open path-to-file :if-does-not-exist nil)))
    (when in
      (loop for char = (read-char in nil)
	  while char do (apply process-func (append func-params (list char))))
      (close in))))

