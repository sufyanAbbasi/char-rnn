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

;; TRAIN-RNN-INPUT
;;--------------------------------------
;; INPUT: sequence-length, the input length into the RNN
;;                         (aka the length of char-vec)
;;        train-rnn-func, the function that trains an RNN
;;        rnn, the rnn to train
;;        alpha, the alpha value for the rnn
;;        char-list-input, a list of chars as input
;;        char-list-output, a list of chars as output
;; OUTPUT: NIL
;; SIDE-EFFECT: calls the train-rnn function with the given input output one-hot-vector pairs

(defun train-rnn-input (sequence-length train-rnn-func rnn alpha char-list-input char-list-output)
  (let
      ((char-vec-input (make-array sequence-length 
				   :initial-contents char-list-input))
					;:element-type 'character)))
       (char-vec-output (make-array sequence-length
				    :initial-contents char-list-output)))
    
    (funcall train-rnn-func 
	     rnn 
	     alpha 
	     (dotimes (i sequence-length char-vec-input)
	       (setf (svref char-vec-input i) (char-one-hot-vec (svref char-vec-input i))))
	     (dotimes (i sequence-length char-vec-output)
	       (setf (svref char-vec-output i) (char-one-hot-vec (svref char-vec-output i)))))))
  
  
;; PROCESS-CHARS
;;-------------------------------------
;; INPUT: path-to-file, a file path string
;;        sequence-length, the length of the list of characters to build
;;        process-func, a function to call on each character list,
;;                     where each character is the last argument
;;       &rest func-params, the rest of the arguments in process-func
;; OUTPUT: NIL
;; SIDE-EFFECT: Calls process-func with char-list-input and char-list-output as last two parameters

(defun process-chars (path-to-file sequence-length process-func &rest func-params)
  (let ((in (open path-to-file :if-does-not-exist nil)))
    (when in
      (labels
	  ((build-list (curr-char length acc)
	     (cond
	      ;;Basecase: next-char is nil, at end of file,
	      ;;          so close the file
	      ((not curr-char) (close in))
	      
	      ;;Recursive Case 1: acc is not big enough,
	      ;;                  append the curr-char on to the list
	      ((< length (1+ sequence-length)) (build-list (read-char in nil) 
							   (incf length) (append acc (list curr-char))))
	      ;;Recursive Case 2: acc length is equal to sequence length + 1,
	      ;;                  run function on acc and append the next char on
	       ((= (1+ sequence-length) length) (apply process-func (append func-params (list (butlast acc) (rest acc))))
						(build-list (read-char in nil) (incf length) (append acc (list curr-char))))
	       ;;Recursive Case 3: acc length is greater than sequence length,
	       ;;                  run function on rest of acc and append the next char on the rest of acc
	       (t (apply process-func (append func-params (list (butlast (rest acc)) (rest (rest acc)))))
		  (build-list (read-char in nil) length (append (rest acc) (list curr-char)))))))
	   
	(build-list (read-char in nil) 0 nil)))))

(defmacro print-lists (url seq-len)
  `(process-chars ,url ,seq-len #'format t "~A, ~A~%"))

(defmacro train-rnn-text (url seq-len train-func rnn alpha)
  `(process-chars ,url ,seq-len #'train-rnn-input ,seq-len ,train-func ,rnn ,alpha))

