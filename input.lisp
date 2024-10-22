;;=====================================
;; Processes inputs into the RNN
;; Authors: Thomas Lum
;;          Sufyan Abbasi
;;=====================================

;; The following expressions ensure that tail-recursive function calls 
;; are handled appropriately/efficiently by the compiler. 

(defconstant *ASCII-LENGTH* 128)
(defconstant *ASCII-OFFSET* 32)
(defconstant *ONE-HOT-LENGTH* 96)
(defconstant *profile-num* 100)
(defconstant *default-char* #\_)

;;one hot vector will start at space character (32) and span to the end of ASCII table, 
;;then the last index will be line carriage (13)

;;hash table that maps a character to a one-hot-vector
(defparameter *one-hot-hash* (make-hash-table))

;;hash table that maps an index of one-hot-vector to a character
(defparameter *one-hot-index* (make-hash-table))

;; INDEX-ONE-HOT-VEC
;;--------------------------------------
;; INPUT: index, an index into a one-hot-vec
;; OUTPUT: a one-hot vector with index set to 1

(defmacro index-one-hot-vec (index)
  `(let
       ((vecty (make-array *ONE-HOT-LENGTH*
         :initial-element 0
         :adjustable NIL
         :element-type 'integer)))
     (setf (svref vecty ,index) 1)
     vecty))

;;FILL THE HASH TABLES

(dotimes (i *ASCII-LENGTH*)
  (cond
   ;;ASCII 13: map line carriage to the last index
   ((= i (char-code #\Return))
    (setf (gethash (code-char i) *one-hot-hash*) (index-one-hot-vec (1- *ONE-HOT-LENGTH*)))
    (setf (gethash (1- *ONE-HOT-LENGTH*) *one-hot-index*) #\Return))
   ;;ASCII 0 - 31: map control characters and DEL to a #
   ((or (< i *ASCII-OFFSET*) (= i (1- *ASCII-LENGTH*)))
    (setf (gethash (code-char i) *one-hot-hash*) (index-one-hot-vec (- (char-code *default-char*) *ASCII-OFFSET*)))
    (setf (gethash (- (char-code *default-char*) *ASCII-OFFSET*) *one-hot-index*) *default-char*))
   ;;ASCII 32 - 127: map character to its offsetted index
   (t 
    (setf (gethash (code-char i) *one-hot-hash*) (index-one-hot-vec (- i *ASCII-OFFSET*)))
    (setf (gethash (- i *ASCII-OFFSET*) *one-hot-index*) (code-char i))))
  (format t "~A : ~A~%" (code-char i) (one-hot-vec-char (char-one-hot-vec (code-char i))))
  )


;;ASCII characters 32 (space) -> 90 (Z) + line_carriage (13)
      
      
;; CHAR-ONE-HOT-VEC
;;--------------------------------------
;; INPUT: char, an ascii character
;; OUTPUT: a one-hot vector representation of the character

(defmacro char-one-hot-vec (char)
  `(gethash ,char *one-hot-hash*))

;; ONE-HOT-VEC-CHAR
;;--------------------------------------
;; INPUT: ohv, a one-hot-vector  
;; OUTPUT: the ascii character represented by the one-hot-vector

(defun one-hot-vec-char (ohv)
  (gethash (position 1 ohv) *one-hot-index*))


;; PROCESS-CHARS
;;-------------------------------------
;; INPUT: path-to-file, a file path string
;;        sequence-length, the length of the list of characters to build
;;        process-func, a function to call on each character list,
;;                     where each character is the last argument
;;       &key print-func, a function that prints the current state of the RNN, uses func-params
;;       &rest func-params, the rest of the arguments in process-func
;; OUTPUT: NIL
;; SIDE-EFFECT: Calls process-func with char-list-input and char-list-output as last two parameters

(defun process-chars (path-to-file sequence-length process-func print-func &rest func-params)
  (format t "Training")
  (let ((in (open path-to-file :if-does-not-exist nil))
        (num-processed 0)
        (starting-time (get-universal-time))
        (end-time 0)
        (num-dots 0))
    (when in
      (let ((length 0)
      (acc nil))
  (loop for curr-char = (read-char in nil)
      while curr-char do 
      (when (= (mod (incf num-processed) *profile-num*) 0)
        (format t ".")
        (incf num-dots)
        (when (and (= (mod num-dots 10) 0) (> num-dots 0) print-func)
          (format t "~%Output: ")
          (apply print-func func-params)
          (format t "~%")
          (setf end-time (get-universal-time))
          (format t "Time-Elapsed: ~$ minutes~%Processed: ~A~%Time/Processed: ~$ms~%~%" 
          (/ (- end-time starting-time) 60) 
          num-processed 
          (* (/ (- end-time starting-time) num-processed) 1000))
          (format t "Training")
        )
      )
      
        (cond
         ;;Case 1: acc is not big enough,
         ;;        append the curr-char on to the list
         ((< length (1+ sequence-length)) (incf length)
            (setf acc (append acc (list curr-char))))
         ;;Case 2: acc length is equal to sequence length + 1,
         ;;        run function on acc and append the next char on
         ((= (1+ sequence-length) length) (apply process-func (append func-params (list (butlast acc) (rest acc))))
            (incf length) 
            (setf acc (append acc (list curr-char))))
         ;;Case 3: acc length is greater than sequence length,
;        ;;        run function on rest of acc and append the next char on the rest of acc
         (t (apply process-func (append func-params (list (butlast (rest acc)) (rest (rest acc)))))
      (setf acc (append (rest acc) (list curr-char))))))

  ;;Grabs the last input output pair (it misses the last one otherwise)
  (apply process-func (append func-params (list (butlast (rest acc)) (rest (rest acc)))))
  (close in)
  (setf end-time (get-universal-time))
  (format t "Time-Elapsed: ~$ minutes~%Processed: ~A~%Time/Processed: ~$ms~%~%" 
    (/ (- end-time starting-time) 60) 
    num-processed 
    (* (/ (- end-time starting-time) num-processed) 1000))
  
  ;;runs the print function at the very end 
  (when print-func
    (format t "~%Final Output:")
    (apply print-func func-params))
  
  ))))


;; TRAIN-RNN-INPUT
;;--------------------------------------
;; INPUT: train-rnn-func, the function that trains an RNN
;;        rnn, the rnn to train
;;        alpha, the alpha value for the rnn
;;        char-list-input, a list of chars as input
;;        char-list-output, a list of chars as output
;; OUTPUT: NIL
;; SIDE-EFFECT: calls the train-rnn function with the given input output one-hot-vector pairs

(defun train-rnn-input (train-rnn-func rnn alpha verbose char-list-input char-list-output)
  (let
      ((char-vec-input (make-array (rnn-seq-len rnn) 
           :initial-contents char-list-input))
          
       (char-vec-output (make-array (rnn-seq-len rnn)
            :initial-contents char-list-output)))
    (funcall train-rnn-func 
       rnn 
       alpha 
       verbose
       (dotimes (i (rnn-seq-len rnn) char-vec-input)
         (setf (svref char-vec-input i) (char-one-hot-vec (svref char-vec-input i))))
       (dotimes (i (rnn-seq-len rnn) char-vec-output)
         (setf (svref char-vec-output i) (char-one-hot-vec (svref char-vec-output i)))))))


;; PRINT-CURR-RNN
;;--------------------------------------
;; INPUT: SAME AS TRAIN-RNN-INPUT
;; OUTPUT: NIL
;; SIDE EFFECT: runs the given function every 10 dots

(defun print-curr-rnn (train-rnn-func rnn alpha verbose)
  (babble rnn 25))

;; TRAIN-RNN-TEXT
;;---------------------
;; INPUTS: URL, a path to a text file
;;         TRAIN-FUNC, the training function
;;         RNN, an rnn struct
;;         alpha, the learning rate (float)
;;         verbose, boolean flag to print outputs at each step
(defmacro train-rnn-text (url train-func rnn alpha verbose)
  `(process-chars ,url (rnn-seq-len ,rnn) #'train-rnn-input #'print-curr-rnn ,train-func ,rnn ,alpha ,verbose))

;; TRAIN-TO-SPEAK
;;---------------------
;; INPUTS: URL, a path to a text file
;;         SEQ-LEN, the number of context characters
;;         NUM-HIDDEN, the number of hidden layers
;;         ALPHA, the learning rate (float)
;; OUTPUT: NIL
;; SIDEFFECT: Trains the RNN and prints out learned characters
(defun train-to-speak (url seq-len num-hidden alpha)
  (train-rnn-text url #'train-rnn-one (init-rnn seq-len num-hidden) alpha nil))

;; watch-training
;;---------------------
;; INPUTS: URL, a path to a text file
;;         SEQ-LEN, the number of context characters
;;         NUM-HIDDEN, the number of hidden layers
;;         ALPHA, the learning rate (float)
;; OUTPUT: NIL
;; SIDEFFECT: Trains the RNN and prints out learned characters at each step
(defun watch-training (url seq-len num-hidden alpha)
  (train-rnn-text url #'train-rnn-one (init-rnn seq-len num-hidden) alpha t))