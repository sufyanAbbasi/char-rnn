;;=====================================
;; Processes inputs into the RNN
;; Authors: Thomas Lum
;;          Sufyan Abbasi
;;=====================================

;; The following expressions ensure that tail-recursive function calls 
;; are handled appropriately/efficiently by the compiler.  

(setq compiler:tail-call-self-merge-switch t)
(setq compiler:tail-call-non-self-merge-switch t) 

(defparameter *ASCII_LENGTH* 128)
(defparameter *ASCII_OFFSET* 0)

;; CHAR-ONE-HOT-VEC
;;--------------------------------------
;; INPUT: char, an ascii character
;; OUTPUT: a one-hot vector representation of the character

(defun char-one-hot-vec (char)
  (let 
      ((vecty (make-array (- *ASCII_LENGTH* *ASCII_OFFSET*)
			  :initial-element 0
			  :adjustable NIL
			  :element-type 'integer)))
    (setf (svref vecty (- (char-code char) *ASCII_OFFSET*)) 1)
    vecty))

;; ONE-HOT-VEC-CHAR
;;--------------------------------------
;; INPUT: ohv, a one-hot-vector  
;; OUTPUT: the ascii character represented by the one-hot-vector

(defmacro one-hot-vec-char (ohv)
  `(code-char (+ (position 1 ,ohv) *ASCII_OFFSET*)))


;; INDEX-TO-CHAR
;;--------------------------------------
;; INPUT: index, an index into a one-hot vec
;; OUTPUT: the associated character 

(defmacro index-to-char (index)
    `(code-char (+ ,index *ASCII_OFFSET*)))

;; MAKE-RNN-INPUT
;;--------------------------------------
;; INPUT: sequence-length, the input length into the RNN
;;                         (aka the length of char-vec)
;;        char-list, a list of chars
;; OUTPUT: a vector of one-hot-vectors

(defun make-rnn-input (sequence-length char-list)
  (let
      ((char-vec (make-array sequence-length 
			     :initial-contents char-list)))
    (dotimes (i sequence-length char-vec)
      (setf (svref char-vec i) (char-one-hot-vec (svref char-vec i))))))


;; PROCESS-CHARS
;;-------------------------------------
;; INPUT: path-to-file, a file path string
;;        sequence-length, the length of the list of characters to build
;;        process-func, a function to call on each character list,
;;                     where each character is the last argument
;;       &rest func-params, the rest of the arguments in process-func
;; OUTPUT: NIL
;; SIDE-EFFECT: Calls process-func on each character in the text file

(defun process-chars (path-to-file sequence-length process-func &rest func-params)
  (let ((in (open path-to-file :if-does-not-exist nil)))
    (when in
      (labels
	  ((build-list (curr-char length acc)
	     (cond
	      ;;Base-Case: next-char is nil, at end of file,
	      ;;             so close the file
	      ((not curr-char) (close in))

	      ;;Recursive-Case 1: acc is not big enough,
	      ;;                  append the curr-char on to the list
	      ((< length sequence-length) (build-list (read-char in nil) 
						      (incf length) (append acc (list curr-char))))
	      ;;Recursive-Case 2: acc length is equal to sequence length,
	      ;;                  run function on acc and append the next char on
	      ((= sequence-length length) (apply process-func (append func-params (list acc)))
					  (build-list (read-char in nil) (incf length) (append acc (list curr-char))))
	      ;;Recursive-Case 3: acc length is greater than sequence length,
	      ;;                  run function on rest of acc and append the next char on the rest of acc
	      (t (apply process-func (append func-params (list (rest acc))))
		 (build-list (read-char in nil) length (append (rest acc) (list curr-char)))))))
	
	(build-list (read-char in nil) 0 nil)))))


(defmacro generate-inputs (url seq-len)
  `(process-chars ,url ,seq-len #'make-rnn-input ,seq-len))
