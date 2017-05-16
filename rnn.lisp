;;=====================================
;; Implementing an character level RNN
;; Authors: Thomas Lum
;;          Sufyan Abbasi
;;=====================================

;;; ===============================
;;;  CMPU-365, Fall 2010
;;;  rnn.lisp
;;; ===============================
;;;  Implementation of recurrent neural networks

;; Make CC constant across


(defconstant *CC* *ONE-HOT-LENGTH*)


;;;  RNN struct
;;; ------------------------------------------------------
;;;  Generates an empty recurrent neural network
(defstruct rnn
    ;; SEQ-LEN:  the length of the sequence the RNN reads
    seq-len    
    ;; N-H:  the number of hidden nodes in the hidden layer
    n-h   

    ;; I-VECKs:  A vector of seq-len vectors of *CC* input nodes
    ;;           (svref output-veck i) -- is the ith input in the input sequence
    i-vecks
    ;; H-VECKs:  A vector of seq-len vectors of n-h hidden nodes
    ;;           (svref output-veck i) -- is the hidden layer values of 
    ;;             the ith network in the sequence
    h-vecks
    ;; O-VECKs:  A vector of seq-len vectors of *CC* output nodes
    ;;           (svref output-veck i) -- is the ith output in the output sequence
    o-vecks
    ;; I-H-WEIGHTS:  An MxN array holding the weights for all edges between an 
    ;;                 input layer and a hidden layer.
    ;;                 M = *CC*; N = n-h

    i-h-weights
    ;; H-H-WEIGHTS:  An MxM array holding the weights for all edges between a 
    ;;                 hidden layer and the next hidden layer in the sequence.
    ;;                 M = n-h
    h-h-weights
    ;; H-O-WEIGHTS:  An MxN array holding the weights for all edges between a 
    ;;                 hidden layer and an output layer.
    ;;                 M = n-h; N = *CC*
    h-o-weights
    i-h-gradi
    h-h-gradi
    h-o-gradi
    )


;;;  INIT-RNN
;;; -----------------------------------------
;;;  INPUT:  SEQ-LEN, the length of sequences 
;;;             N-H, the number of hidden nodes in the each 
;;;                   hidden layer of the RNN
;;;  OUTPUT:  A recurrent neural network (RNN struct) of those dimensions,
;;;           with weights randomly selected.

(defun init-rnn (seq-len n-h)
    (let* ((i-vecks (make-array seq-len))
        (h-vecks (make-array seq-len))
        (o-vecks (make-array seq-len))
        (i-h-weights (make-array (list *CC* n-h)))
        (h-h-weights (make-array (list n-h n-h)))
        (h-o-weights (make-array (list n-h *CC*)))
        (i-h-gradi (make-array n-h :initial-element 0.0 :element-type 'float))
        (h-h-gradi (make-array n-h :initial-element 0.0 :element-type 'float))
        (h-o-gradi (make-array *CC* :initial-element 0.0 :element-type 'float))
        (rnn (make-rnn 
            :seq-len seq-len
            :n-h n-h
            :i-vecks i-vecks
            :o-vecks o-vecks
            :h-vecks h-vecks
            :i-h-weights i-h-weights
            :h-h-weights h-h-weights
            :h-o-weights h-o-weights
            :i-h-gradi i-h-gradi
            :h-h-gradi h-h-gradi
            :h-o-gradi h-o-gradi)))

    ;; Initialize the input, hidden, and output nodes to 0
    (dotimes (i seq-len)
        (setf (svref i-vecks i) (make-array *CC* :initial-element 0 ))
        (setf (svref h-vecks i) (make-array n-h :initial-element 0 ))
        (setf (svref o-vecks i) (make-array *CC* :initial-element 0 ))
        )

    ;; Randomly initialize the input to hidden weights relative to *CC*
    (dotimes (j *CC*)
        (dotimes (k n-h)
            (setf (aref i-h-weights j k) (+ (random (/ 2 (sqrt *CC*))) (/ -1 (sqrt *CC*))))
            )
        )
    ;; Randomly initialize the hidden to hidden weights relative to n-h
    (dotimes (j n-h)
        (dotimes (k n-h)
            (setf (aref h-h-weights j k) (+ (random (/ 2 (sqrt n-h))) (/ -1 (sqrt n-h))))
            )
        )
    ;; Randomly initialize the hidden to output weights relative to n-h
    (dotimes (j n-h)
        (dotimes (k *CC*)
            (setf (aref h-o-weights j k) (+ (random (/ 2 (sqrt n-h))) (/ -1 (sqrt n-h))))
            )
        )

    ;; Return the initialized RNN
    rnn
    )
    )

;;;  RNN-FF
;;; ----------------------------------------------------------
;;;  INPUTS:  RNN, a recurrent neural network
;;;           INPUT-VECKS, a vector of length seq-len of 
;;;                           vectors of input value vectors of size *CC*
;;;  OUTPUT:  Nil
;;;  SIDE-EFFECT:  Applies the given INPUT values to the input layer of RNN
;;;   and propagates them forward to generate output values for all neurons
;;;   in all networks in the sequence

(defun rnn-ff (rnn input-vecks)
    (declare (optimize speed))
    ;; Set the input vector
    (setf (rnn-i-vecks rnn) input-vecks)

    ;; Grab fields of rnn
    (let ((seq-len (rnn-seq-len rnn))
        (n-h (rnn-n-h rnn))
        (i-vecks (rnn-i-vecks rnn))
        (h-vecks (rnn-h-vecks rnn))
        (o-vecks (rnn-o-vecks rnn))
        (i-h-weights (rnn-i-h-weights rnn))
        (h-h-weights (rnn-h-h-weights rnn))
        (h-o-weights (rnn-h-o-weights rnn)))

    ;; For each input in the sequence
    (dotimes (i (rnn-seq-len rnn))
        (declare (optimize speed))
        ;; Get the input, hidden, and output vector of this network in the sequence
        (let ((i-veck (svref i-vecks i))
            (h-veck (svref h-vecks i))
            (o-veck (svref o-vecks i)))

        ;; For each neuron in the hidden layer
        (dotimes (neuron-num n-h)
            (declare (optimize speed))
            ;; Compute output value of that neuron 
            (setf (svref h-veck neuron-num)


                (if (= i 0)
                    ;; If this is the first NN of the sequence

                    ;; tanh of the dot-product of i-h-weights and input nodes
                    (tanh 
                        (let ((dot-prod 0))
                            (dotimes (j *CC*)
                                (declare (optimize speed))
                                (incf dot-prod
                                    (* (svref i-veck j)
                                        (aref i-h-weights j neuron-num))))
                            dot-prod
                            )
                        )

                    ;; Otherwise consider the previous hidden layer in the sequence

                    ;; tanh of the dot-product of i-h-weights and input nodes
                    ;; + the dot-product of h-h-weights and the previous hidden nodes
                    (tanh
                        (+ 
                            (let ((dot-prod 0))
                                (dotimes (j n-h)
                                    (declare (optimize speed))
                                    (incf dot-prod
                                        (* (svref (svref h-vecks (- i 1)) j)
                                            (aref h-h-weights j neuron-num))))
                                dot-prod)
                            (let ((dot-prod2 0))
                                (dotimes (j *CC*)
                                    (declare (optimize speed))
                                    (incf dot-prod2
                                        (* (svref i-veck j)
                                            (aref i-h-weights j neuron-num))))
                                dot-prod2)
                            )
                        )
                    )
                )
            )


        ;; For each neuron in the output layer
        (dotimes (neuron-num *CC*)
            ;; Compute the output value of that neuron 
            (setf (svref o-veck neuron-num)
                ;; tanh of the dot-product of the h-o-weights and hidden nodes

                (tanh (let ((dot-prod 0))
                    (dotimes (j n-h)
                        (declare (optimize speed))
                        (incf dot-prod
                            (* (svref h-veck j)
                                (aref h-o-weights j neuron-num))))
                    dot-prod)
                )
                )
            )
        )
        )
    )
    )

(defmacro reset-vector (vecty)
    `(loop for i from 0 to (1- (length ,vecty))
    do
        (setf (aref ,vecty i) 0.0)
    ))

;;;  RNN-FF
;;; ----------------------------------------------------------
;;;  INPUTS:  RNN, a recurrent neural network
;;;           ALPHA, a small positive number that specifies the sensitivity
;;;             of updates to the error
;;;           INPUTS, a vector of length seq-len of 
;;;                vectors of input value vectors of size *CC*
;;;           TARGET-OUTPUTS, a vector of length seq-len of 
;;;                vectors of expected output value vectors of size *CC*
;;;  OUTPUT:  Nil
;;;  SIDE-EFFECT:  Feeds Forward (rnn-ff) the inputs into the rnn, then
;;;                    adjusts weights of the rnn based on the 
;;;                    expected target-outputs and the delta rule
(defun train-rnn-one (rnn alpha inputs target-outputs)
    (declare (optimize speed))
    ; Print the input chars and expected output chars
    ; (when T
    ;     (format t "~%Input: ")
    ;     (dotimes (i (rnn-seq-len rnn))
    ;         (format t "~A" (one-hot-vec-char (svref inputs i)))
    ;         )

    ;     (format t "~%Goal: ")
    ;     (dotimes (i (rnn-seq-len rnn))
    ;         (format t "~A" (one-hot-vec-char (svref target-outputs i)))
    ;         )
    ;     )

    ;FEED FORWARD
    (rnn-ff rnn inputs)


    ; Print the network's guess
    ; (when T
    ;     (let ((output (rnn-o-vecks rnn)))
    ;         (format t "~%Ours: ")
    ;         (dotimes (i (rnn-seq-len rnn))
    ;             ;(format t "~A~%" (svref output i))
    ;             (format t "~A" (one-hot-vec-char (to-one-hot (svref output i))))
    ;             )
    ;         )
    ;     )

    ;BACK PROPOGATION
    (let* 
        ((n-h (rnn-n-h rnn)))

        ;; For each input in the sequence
        (dotimes (n (rnn-seq-len rnn))
            (declare (optimize speed))
            (let* (;; The delta gradients
                (i-h-gradi (rnn-i-h-gradi rnn))
                (h-h-gradi (rnn-h-h-gradi rnn))
                (h-o-gradi (rnn-h-o-gradi rnn))
                ;; The weights & values for this nn of the sequence
                (target-output-n (svref target-outputs n))
                (i-h-weights (rnn-i-h-weights rnn))
                (h-h-weights (rnn-h-h-weights rnn))
                (h-o-weights (rnn-h-o-weights rnn))
                (i-veck (svref (rnn-i-vecks rnn) n))
                (h-veck (svref (rnn-h-vecks rnn) n))
                (o-veck (svref (rnn-o-vecks rnn) n))
                )
            (reset-vector i-h-gradi)
            (reset-vector h-h-gradi)
            (reset-vector h-o-gradi)
            ;; Determine gradients from Output to Hidden
            (dotimes (neuron-num *CC*)
                (declare (optimize speed))
                (let* (
                    (target-output (svref target-output-n neuron-num))
                    (my-output (svref o-veck neuron-num))
                    (diffy (- target-output my-output)))

                (setf (svref h-o-gradi neuron-num)
                    (* (abs (* my-output (- 1 my-output))) diffy))))

            ;; Determine gradients from Hidden to Inputs
            (dotimes (neuron-num n-h)
                (let* (
                    (my-output (svref h-veck neuron-num))
                    (sum (let ((dotty 0))
                        (dotimes (j *CC*)
                            (declare (optimize speed))
                            (incf dotty (* (aref h-o-weights neuron-num j)
                                (svref h-o-gradi j))))
                        dotty)))

                (incf (svref i-h-gradi neuron-num)
                    (* (abs (* my-output (- 1 my-output))) sum))
                )
                )


            ;; Determine gradients between hidden layers
            (dotimes (hnn n)
                (let*
                    ((hn (- (- n hnn) 1))
                        (hn-veck (svref (rnn-h-vecks rnn) hn)))

                    (dotimes (neuron-num n-h)
                        (declare (optimize speed))
                        (let* (
                            (my-output (svref hn-veck neuron-num))
                            (sum (let ((dotty 0))
                                (dotimes (j *CC*)
                                    (declare (optimize speed))
                                    (incf dotty (* (aref h-o-weights neuron-num j)
                                        (svref h-o-gradi j))))
                                dotty)))
                        (incf (svref h-h-gradi neuron-num)
                            (* (/ hn n) (* (abs (* my-output (- 1 my-output))) sum)))))

                    )
                )

            ;; Now, update all of the weights in the network using the gradient values

            ;; Update input weights
            (dotimes (i *CC*)
                (dotimes (j n-h)
                    (declare (optimize speed))
                    (incf (aref i-h-weights i j)
                        (* alpha 
                            (svref i-veck i) 
                            (svref i-h-gradi j)))))

            ;; Update hidden weights
            (dotimes (i n-h)
                (dotimes (j n-h)
                    (declare (optimize speed))
                    (incf (aref h-h-weights i j)
                        (* alpha 
                            (svref h-veck i) 
                            (svref h-h-gradi j)))))

            ;; Update output weights
            (dotimes (i n-h)
                (dotimes (j *CC*)
                    (declare (optimize speed))
                    (incf (aref h-o-weights i j)
                        (* alpha 
                            (svref h-veck i) 
                            (svref h-o-gradi j)))))

            )
            )
        ; (when T (format T "~%----------------~%"))
        )
    )

;;;  TO-ONE-HOT
;;; ----------------------------------------------------------
;;;  INPUTS:  vec - a vector of size *CC*
;;;  OUTPUT:  a vector of size *CC* with the highest value set to
;;;              1 and the rest set to zero
(defun to-one-hot (vec)
    (let (;Max value found
        (max 0)
        ;Flag that the max was found
        (foundHot nil)
        ;New onehot vector
        (oneHot (make-array *CC* :initial-element 0)))

    ;Find the max
    (dotimes (i *CC*)
        (when (and (< i *CC*) (> (svref vec i) max)) (setf max (svref vec i)))
        )


    (dotimes (i *CC*)
        ;Set the index of the max to be 1
        (when (or foundHot (not (= (svref vec i) max)))
            (setf (svref oneHot i) 0)
            )
        ;Set anything else to zero
        (when (and (= (svref vec i) max) (not foundHot))
            (setf (svref oneHot i) 1)
            (setf foundHot T)
            )
        )

    ;Return the one hot vector
    oneHot
    )
    )

;;;  TO-RAND-ONE-HOT
;;; ----------------------------------------------------------
;;;  INPUTS:  VEC - a vector of size *CC*
;;;              RAND - the randomness factor.  
;;;                  RAND == 1 -> no randomndess
;;;                RAND > 1 -> increasing randomness
;;;  OUTPUT:  a vector of size *CC* with the highest value set to
;;;              1 and the rest set to zero, however the highest
;;;           value is partially randomly chosen
(defun to-rand-one-hot (outputVec rand)
    (let (;Max value found
        (max 0)
        ;Flag that the max was found
        (foundHot nil)
        ;New onehot vector
        (oneHot (make-array *CC* :initial-element 0)))

    ;Find the max
    (dotimes (i *CC*)
        ;;Randomly scan for the next value in the vector
        (setf i (+ i (random rand)))
        (when (and (< i *CC*) (> (svref outputVec i) max)) (setf max (svref outputVec i)))
        )


    (dotimes (i *CC*)
        ;Set the index of the max to be 1
        (when (or foundHot (not (= (svref vec i) max)))
            (setf (svref oneHot i) 0)
            )
        ;Set anything else to zero
        (when (and (= (svref vec i) max) (not foundHot))
            (setf (svref oneHot i) 1)
            (setf foundHot T)
            )
        )

    ;Return the one hot vector
    oneHot
    )
    )


;;;  BABBLE
;;; ----------------------------------------------------------
;;;  INPUTS:  RNN - a recurrent neural network
;;;              LENGTH - the number of characters to generate  
;;;  OUTPUT:  nil
;;;  SIDE-EFFECT:  Prints Seq-len + Length characters from the RNN
;;;                   Feeding into itself from a random seed input
(defun babble (rnn length)
    (let*
        ((sl (rnn-seq-len rnn))
            (inputs (make-array sl)))

        ;; Initialize the input
        (setf inputs (rnn-i-vecks rnn))

        (dotimes (i length)
            
            ;;Feed forward
            (rnn-ff rnn inputs)
            ;; Print the last output character
            (format T "~A" (to-one-hot (svref (rnn-o-vecks rnn) (- sl 1))))
            ; Set our output to be the new input
            (dotimes (i sl)
                (setf (svref inputs i) (to-one-hot (svref (rnn-o-vecks rnn) i)))
                ;(format T "~A" (one-hot-vec-char (svref inputs i)))
            )
        )
    )
)

;;;  BABBLE
;;; ----------------------------------------------------------
;;;  INPUTS:  RNN - a recurrent neural network
;;;              LENGTH - the number of characters to generate  
;;;  OUTPUT:  nil
;;;  SIDE-EFFECT:  Prints Seq-len + Length characters from the RNN
;;;                   Feeding into itself from a random seed input
(defun babble-input (seed rnn length)
    (let*
        ((sl (rnn-seq-len rnn))
            (inputs (make-array sl)))

        ;; Initialize the input
        (setf inputs (rnn-i-vecks rnn))

        (dotimes (i length)

            (dotimes (i sl)
                (format T "~A" (one-hot-vec-char (svref inputs i)))
            )
            (format T "~%")

            ;;Feed forward
            (rnn-ff rnn inputs)
            ;; Print the last output character
            ;(format T "~A" (one-hot-vec-char (to-one-hot (svref (rnn-o-vecks rnn) (- sl 1)))))
            ; Set our output to be the new input
            (setf inputs (rnn-o-vecks rnn))
        )
    )
)

;;;  RAND-BABBLE
;;; ----------------------------------------------------------
;;;  INPUTS:  RNN - a recurrent neural network
;;;              LENGTH - the number of characters to generate  
;;;              RAND - the randomness factor.  
;;;                  RAND == 1 -> no randomndess
;;;                RAND > 1 -> increasing randomness
;;;  OUTPUT:  nil
;;;  SIDE-EFFECT:  Prints Seq-len + Length characters from the RNN
;;;                   Feeding into itself from a random seed input
;;;                   AND selecting slightly random best solutions
;;;                   rather than the absolute best
(defun rand-babble (rnn length rand)
    (let*
        ((sl (rnn-seq-len rnn))
            (inputs (make-array sl)))

        ;; Initialize the random seed input
        (dotimes (sn sl)
            (setf (svref inputs sn) (make-array *CC* :initial-element 0))
            (let ((index (random *CC*)))
                (dotimes (i *CC*)
                    (when (= index i) (setf (svref (svref inputs sn) i) 1))
                    )
                )
            )

        (dotimes (i length)
            ;;Feed forward
            (rnn-ff rnn inputs)
            ;; Print the last output character
            (format T "~A" (to-rand-one-hot (svref (rnn-o-vecks rnn) (- sl 1)) rand))
            ; Set our output to be the new input
            (setf inputs (rnn-o-vecks rnn))
        )
    )
)

;; Softmax
(defun softMax! (vec len)
    (let ((total 0))
        (dotimes (neuron-num len)
            (setf total (+ total (svref vec neuron-num)))
            )

        (dotimes (neuron-num len)
            (setf (svref vec neuron-num) (/ (svref vec neuron-num) total))
            )
        )
    )