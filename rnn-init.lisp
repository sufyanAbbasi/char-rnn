;;=====================================
;; Implementing an character level RNN
;; Authors: Thomas Lum
;;          Sufyan Abbasi
;;=====================================

;;; ===============================
;;;  CMPU-365, Fall 2010
;;;  NEW-NN.LISP
;;; ===============================
;;;  Implementation of neural networks
(defconstant *CC* 96)


;;;  RNN struct
;;; ------------------------------------------------------
;;;  Generates an empty recurrent neural network
(defstruct rnn
	;; SEQ-LEN:  the length of the sequence the RNN reads
	seq-len    
	;; n-h:  the number of hidden nodes in the hidden layer
	n-h   
	;; INPUT-VECKs:  A vector of seq-len vectors of size *CC*
	;;  (svref output-veck i) -- is the ith input in the input sequence
	i-vecks
	;; OUTPUT-VECKs:  A vector of seq-len vectors of size *CC*
	;;  (svref output-veck i) -- is the ith output in the output sequence
	o-vecks
	;; H-VECKs:  A vector of seq-len vectors of size n-h
	;;  (svref output-veck i) -- is the ith output in the output sequence
	h-vecks
	;; I-H-WEIGHTS:  A vector of arrays.
	;;  (svref i-h-weights i) -- is an MxN array holding the weights
	;;        for all edges between the input layer and the hidden layer
	;;        M = *CC*; N = n-h
	i-h-weights
	;; H-H-WEIGHTS:  A vector of arrays.
	;;  (svref h-h-weights i) -- is an NxN array holding the weights
	;;        for all edges between the hidden layer and the next hidden
	;;		  layer
	;;        N = n-h
	h-h-weights
	;; H-O-WEIGHTS:  A vector of arrays.
	;;  (svref h-h-weights i) -- is an NxM array holding the weights
	;;        for all edges between the hidden layer and the next hidden
	;;		  layer
	;;        N = n-h; M = *CC*
	h-o-weights
	)

(defun convertToOneHot (outputVec)
	(let ((max 0)
		(foundHot nil)
		(oneHot (make-array *CC* :initial-element 0))
		)
	(dotimes (i *CC*)
		(when (> (svref outputVec i) max) (setf max (svref outputVec i)))

		(when (= (random 2) 0) (setf i (+ i 1)))
		)
	;(format T "~A" max)

	(dotimes (i *CC*)
		(when (or foundHot (not (= (svref outputVec i) max)))
			(setf (svref oneHot i) 0)
			)
		(when (and (= (svref outputVec i) max) (not foundHot))
			(setf (svref oneHot i) 1)
			(setf foundHot T)
			)
		)
	;(format T "~%~A~%" outputVec)

	oneHot
	)
	)

(defun babble (rnn length)
	(let*
		(
			(sl (rnn-seq-len rnn))
			(inputs (make-array sl))
			)

		(dotimes (sn sl)
			(setf (svref inputs sn) (make-array *CC* :initial-element 0))
			)

		(dotimes (i length)
			(rnn-ff rnn inputs)
			(dotimes (sn sl)
				(let (
					(ohv-out (convertToOneHot (svref (rnn-o-vecks rnn) sn)))
					)
				; Print it
				; (format T "~A" ohv-out)
				(format T "~A" (one-hot-vec-char ohv-out))
				; Set to input
				(setf (svref inputs sn) ohv-out)
				)
				)
			)
		)
	)

;;;  INIT-RNN
;;; -----------------------------------------
;;;  INPUT:  SIZES-OF-LAYERS, a list of numbers indicating how
;;;           many neurons are in each layer.  (Layer 0 corresponds
;;;           to the input layer).
;;;  OUTPUT:  A neural network (NN struct) of that size, initialized
;;;           with weights randomly selected between -0.5 and +0.5.

(defun init-rnn (seq-len n-h)
	(let* (
		(i-vecks (make-array seq-len))
		(h-vecks (make-array seq-len))
		(o-vecks (make-array seq-len))
		(i-h-weights (make-array (list *CC* n-h)))
		(h-h-weights (make-array (list n-h n-h)))
		(h-o-weights (make-array (list n-h *CC*)))
		(rnn (make-rnn :seq-len seq-len
			:n-h n-h
			:i-vecks i-vecks
			:o-vecks o-vecks
			:h-vecks h-vecks
			:i-h-weights i-h-weights
			:h-h-weights h-h-weights
			:h-o-weights h-o-weights
			)))

	(dotimes (i seq-len)
		(setf (svref i-vecks i) (make-array *CC* :initial-element 0))
		(setf (svref h-vecks i) (make-array n-h :initial-element 0))
		(setf (svref o-vecks i) (make-array *CC* :initial-element 0))
		)

	(dotimes (j *CC*)
		(dotimes (k n-h)
			(setf (aref i-h-weights j k) (+ (random (/ 2 (sqrt *CC*))) (/ -1 (sqrt *CC*))))
			)
		)
	(dotimes (j n-h)
		(dotimes (k n-h)
			(setf (aref h-h-weights j k) (+ (random (/ 2 (sqrt n-h))) (/ -1 (sqrt n-h))))
			)
		)
	(dotimes (j n-h)
		(dotimes (k *CC*)
			(setf (aref h-o-weights j k) (+ (random (/ 2 (sqrt n-h))) (/ -1 (sqrt n-h))))
			)
		)

	;; return the NN
	rnn
	)
	)

;;;  ERASE-RNN-OUTPUTs
;;; -----------------------------------------------------
;;;  INPUT:  RNN, a recurrent neural network
;;;  OUTPUT:  T
;;;  SIDE-EFFECT: Destructively modifies NN by setting all output
;;;   values to NIL (usually done before the FEED-FORWARD process).

(defun erase-rnn-nodes (rnn)
	(let (
		(seq-len (rnn-seq-len rnn))
		(n-h (rnn-n-h rnn))
		(i-vecks (rnn-i-vecks rnn))
		(h-vecks (rnn-h-vecks rnn))
		(o-vecks (rnn-o-vecks rnn)))
	;; For each sequence
	(dotimes (i seq-len)
		(let (
			(i-nodes (svref i-vecks i))
			(h-nodes (svref h-vecks i))
			(o-nodes (svref o-vecks i)))
		;; For each neuron in that layer...
		(dotimes (j *CC*)
			;; Set that neuron's output value to NIL
			(setf (svref i-nodes j) nil)
			(setf (svref o-nodes j) nil)
			)
		(dotimes (j n-h)
			;; Set that neuron's output value to NIL
			(setf (svref h-nodes j) nil)
			)
		)
		)
	t))

;;;  SET-INPUTS
;;; --------------------------------------------------
;;;  INPUT:  NN, a neural network
;;;          INPUTS, a list of input values for the input neurons of NN
;;;  OUTPUT: NN
;;;  SIDE EFFECT:  Sets the "output" value of each neuron in the
;;;    input layer to the corresponding value in INPUTS.

(defun set-inputs (nn inputs)
	(let* ((out-vecks (nn-output-vecks nn))
		;; OUT-VECK-ZERO:  the vector of "output" values for layer 0 
		(out-veck-zero (svref out-vecks 0))
		(num-inputs (svref (nn-layer-sizes nn) 0)))
	(cond
		;; CASE 1:  INPUTS has the right number of input values
		((= num-inputs (length inputs))
			;; For each input value...
			(dotimes (i num-inputs)
				;; Set the "output" value for the corresponding neuron in layer 0 
				(setf (svref out-veck-zero i) (nth i inputs)))
			;; return the NN
			nn)
		;; Case 2:  Error!
		(t
			(format t "Whoops!  Wrong number of input values for this NN!~%")))))

;;;  SIGMOID
;;; ------------------------------
;;;  SIGMOID(X) = 1/(1 + e^(-x)) -- the sigmoid (or logistic) function

(defun sigmoid (x)
	(/ 1.0 (+ 1 (exp (- x)))))

;;;  RNN-FF
;;; ----------------------------------------------------------
;;;  INPUTS:  RNN, a recurrent neural network
;;;           INPUTS, a list of length seq-len of vectors of input values
;;;  OUTPUT:  NN
;;;  SIDE-EFFECT:  Applies the given INPUT values to the input layer of NN
;;;   and propagates them forward to generate output values for all neurons
;;;   in the network.

(defun rnn-ff (rnn input-vecks)
	;; Set the input vector
	(setf (rnn-i-vecks rnn) input-vecks)

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
		(let* (
			(i-veck (svref i-vecks i))
			(h-veck (svref h-vecks i))
			(o-veck (svref o-vecks i)))
		
		;; For each neuron in the hidden layer
		(dotimes (neuron-num n-h)
			;; Compute output value of that neuron 

			(setf (svref h-veck neuron-num)
				;; sigmoid of the DOT-PRODUCT of WEIGHTS and INPUT VALUES
				;;  (INPUTS for this neuron are OUTPUTS from neurons 
				;;     in previous layer)

				(if (= i 0)
					;; If this is the first of the sequence, just pass normally
					(sigmoid (let ((dot-prod 0))
						(dotimes (j *CC*)
							(incf dot-prod
								(* (svref i-veck j)
									(aref i-h-weights j neuron-num))))
						dot-prod))
					;; Otherwise consider the previous hidden layer in the sequence
					(sigmoid (+ 
						(let ((dot-prod 0))
							(dotimes (j n-h)
								(incf dot-prod
									(* (svref (svref h-vecks (- i 1)) j)
										(aref h-h-weights j neuron-num))))
							dot-prod)
						(let ((dot-prod 0))
							(dotimes (j *CC*)
								(incf dot-prod
									(* (svref i-veck j)
										(aref i-h-weights j neuron-num))))
							dot-prod)
						)
					)
					)
				)
			)


		;; For each neuron in the output layer
		(dotimes (neuron-num *CC*)
			;; Compute output value of that neuron 
			(setf (svref o-veck neuron-num)
				;; sigmoid of the DOT-PRODUCT of WEIGHTS and INPUT VALUES
				;;  (INPUTS for this neuron are OUTPUTS from neurons 
				;;     in previous layer)
				(sigmoid (let ((dot-prod 0))
					(dotimes (j n-h)
						(incf dot-prod
							(* (svref h-veck j)
								(aref h-o-weights j neuron-num))))
					dot-prod)))
			)
		)
		)
	)
	)

(defun train-rnn-one (rnn alpha inputs target-outputs)
	; (format T "~A" rnn)

	(format t "~%Input: ")
	(dotimes (i (rnn-seq-len rnn))
		(format t "~A" (one-hot-vec-char (svref inputs i)))
		)

	(format t "~%Goal: ")
	(dotimes (i (rnn-seq-len rnn))
		(format t "~A" (one-hot-vec-char (svref target-outputs i)))
		)

	;FEED FORWARD
	(rnn-ff rnn inputs)

	(let ((output (rnn-o-vecks rnn)))
		(format t "~%Our Guess: ")
		(dotimes (i (rnn-seq-len rnn))
			;(format t "~A~%" (svref output i))
			(format t "~A" (one-hot-vec-char (convertToOneHot (svref output i))))
			)
		)
	; (format T "~%-------- After Forward Pass --------~%")
	; (format T "~A" rnn)

	(let* 

		((n-h (rnn-n-h rnn)))

		;; For each input in the sequence
		(dotimes (n (rnn-seq-len rnn))
			;; Back prop algorithm...
			(let* 
				(
					;; The delta gradients
					(i-h-gradi (make-array n-h))
					(h-h-gradi (make-array n-h))
					(h-o-gradi (make-array *CC*))

					(target-output-n (svref target-outputs n))

					;; The weights & values for this member of the sequence
					(i-h-weights (rnn-i-h-weights rnn))
					(h-h-weights (rnn-h-h-weights rnn))
					(h-o-weights (rnn-h-o-weights rnn))
					(i-veck (svref (rnn-i-vecks rnn) n))
					(h-veck (svref (rnn-h-vecks rnn) n))
					(o-veck (svref (rnn-o-vecks rnn) n))
					)

				;; for each neuron in the output layer:
				(dotimes (neuron-num *CC*)
					(let* (
						(target-output (svref target-output-n neuron-num))
						(my-output (svref o-veck neuron-num))
						(diffy (- target-output my-output)))
					; (format T "~A" my-output)
					; (format T "~%[[[[[[[[~A]]]]]]]]~%" diffy)
					(setf (svref h-o-gradi neuron-num)
						(* my-output (- 1 my-output) diffy))))

				;; i-h
				(dotimes (neuron-num n-h)
					(let* (
						(my-output (svref h-veck neuron-num))
						(sum (let ((dotty 0))
							(dotimes (j *CC*)
								(incf dotty (* (aref h-o-weights neuron-num j)
									(svref h-o-gradi j))))
							dotty)))
					(incf (svref i-h-gradi neuron-num)
						(* my-output (- 1 my-output) sum))
					)
					)


				;; h-h
				;; i-h
				(dotimes (hnn n)
					(let*
						(
							(hn (- (- n hnn) 1))
							(hn-veck (svref (rnn-h-vecks rnn) hn))
							)

						;; h-h
						(dotimes (neuron-num n-h)
							(let* (
								(my-output (svref hn-veck neuron-num))
								(sum (let ((dotty 0))
									(dotimes (j *CC*)
										(incf dotty (* (aref h-o-weights neuron-num j)
											(svref h-o-gradi j))))
									dotty)))
							(incf (svref h-h-gradi neuron-num)
								(* my-output (- 1 my-output) sum))))

						;; i-h
						(dotimes (neuron-num n-h)
							(let* (
								(my-output (svref hn-veck neuron-num))
								(sum2 (let ((dotty 0))
									(dotimes (j n-h)
										(incf dotty (* (aref h-h-weights neuron-num j)
											(svref h-h-gradi j))))
									dotty))
								)
							(incf (svref i-h-gradi neuron-num)
								(* my-output (- 1 my-output) sum2))
							)
							)
						)
					)

				;; Now, update all of the weights in the network using the DELTA values
				;; >>> i-h
				;; For each neuron N_i in that layer...
				(dotimes (i *CC*)
					;; For each neuron N_j in the following layer...
					(dotimes (j n-h)
						;; Update the weight on the edge from N_i to N_j
						;; W_I_J += ALPHA * A_I * DELTA_J
						(incf (aref i-h-weights i j)
							(* alpha 
								(svref i-veck i) 
								(svref i-h-gradi j)))))

				; ;; >>> h-h
				; ;; For each neuron N_i in that layer...
				(dotimes (i n-h)
					;; For each neuron N_j in the following layer...
					(dotimes (j n-h)
						;; Update the weight on the edge from N_i to N_j
						;; W_I_J += ALPHA * A_I * DELTA_J
						(incf (aref h-h-weights i j)
							(* alpha 
								(svref h-veck i) 
								(svref h-h-gradi j)))))

				;; >>> h-o
				;; For each neuron N_i in that layer...
				(dotimes (i n-h)
					;; For each neuron N_j in the following layer...
					(dotimes (j *CC*)

						;; Update the weight on the edge from N_i to N_j
						;; W_I_J += ALPHA * A_I * DELTA_J
						(incf (aref h-o-weights i j)
							(* alpha 
								(svref h-veck i) 
								(svref h-o-gradi j)))))

				; (format t "~A~%" h-o-gradi)
				)
			)
		;; return the RNN
		; (format T "~%-------- After backprop --------~%")
		; (format T "~A" rnn)

		;(format t "~A~%" (babble rnn 10))
		; rnn
		)
	)

