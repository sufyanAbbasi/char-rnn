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
(defconstant *CC* 2)


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
		(setf (svref i-vecks i) (make-array *CC*))
		(setf (svref h-vecks i) (make-array n-h))
		(setf (svref o-vecks i) (make-array *CC*))
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
				;; TANH of the DOT-PRODUCT of WEIGHTS and INPUT VALUES
				;;  (INPUTS for this neuron are OUTPUTS from neurons 
				;;     in previous layer)

				(if (= i 0)
					;; If this is the first of the sequence, just pass normally
					(tanh (let ((dot-prod 0))
						(dotimes (j *CC*)
							(incf dot-prod
								(* (svref i-veck j)
									(aref i-h-weights j neuron-num))))
						dot-prod))
					;; Otherwise consider the previous hidden layer in the sequence
					(tanh (+ 
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
				;; TANH of the DOT-PRODUCT of WEIGHTS and INPUT VALUES
				;;  (INPUTS for this neuron are OUTPUTS from neurons 
				;;     in previous layer)
				(tanh (let ((dot-prod 0))
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



;;;  TRAIN-ONE
;;; ----------------------------------------------------
;;;  INPUTS:  NN, a neural network
;;;           ALPHA, a small positive number that specifies the sensitivity
;;;             of updates to the error
;;;           INPUTS, a list of input values for the neurons in the input layer
;;;           TARGET-OUTPUTS, the desired outputs for neurons in the output
;;;             layer, given the specified INPUTS.
;;;  OUTPUT:  NN
;;;  SIDE EFFECT:  Uses FEED-FORWARD to generate output values for
;;;   the given inputs; then uses the BACK-PROPAGATION algorithm to
;;;   generate DELTA values for each neuron (starting from output layer
;;;   and working back to first hidden layer); then uses the DELTA values
;;;   to update each non-input neuron.

(defun train-one (nn alpha inputs target-outputs)
	(feed-forward nn inputs)

	;; Back prop algorithm...
	(let* ((num-layers (nn-num-layers nn))
		(layer-sizes (nn-layer-sizes nn))
		;; The index for the output layer
		(last-layer-index (1- num-layers))
		(num-output-neurons (svref layer-sizes last-layer-index))
		;; The index for the layer just before the output layer
		(penult-layer-index (1- last-layer-index))
		;;(num-penult-neurons (svref layer-sizes penult-layer-index))
		(output-vecks (nn-output-vecks nn))
		;;(penult-output-veck (svref output-vecks penult-layer-index))
		(last-output-veck (svref output-vecks last-layer-index))
		(delta-vecks (nn-delta-vecks nn))
		(last-delta-veck (svref delta-vecks last-layer-index))
		(weight-arrays (nn-weight-arrays nn))
		;;(last-weight-array (svref weight-arrays penult-layer-index))
		)

	;; for each neuron in the output layer:
	(dotimes (neuron-num num-output-neurons)
		(let* ((target-output (nth neuron-num target-outputs))
			(my-output  (svref last-output-veck neuron-num))
			(diffy (- target-output my-output)))
		;;   DELTA_J = G'(IN_J) * (Y_J - A_J)
		;;           = G(IN_J)*(1 - G(IN_J))*(Y_J - A_J)
		;;           = A_J * (1 - A_J) * (Y_J - A_J)
		(setf (svref last-delta-veck neuron-num)
			(* my-output (- 1 my-output) diffy))))

	;; for each hidden layer...
	(do ((lay-num penult-layer-index (1- lay-num)))
		;; exit
		((= lay-num 0))
		;; BODY of DO
		;; ---------------------------
		(let* ((num-neurons (svref layer-sizes lay-num))
			(curr-out-veck (svref output-vecks lay-num))
			(next-delta-veck (svref delta-vecks (1+ lay-num)))
			(my-delta-veck (svref delta-vecks lay-num))
			(num-neurons-next-layer (svref layer-sizes (1+ lay-num)))
			(curr-weight-array (svref weight-arrays lay-num))
			)
		;; for each neuron in that layer...
		(dotimes (i num-neurons)
			;; DELTA_I = G'(IN_I) SUM [W_I_J DELTA_J]
			;;         = G(IN_I) * (1 - G(IN_I)) * SUM [ W_I_J DELTA_J ]
			;;         = A_I * (1 - A_I) * SUM [ W_I_J DELTA_J ]
			(let* ((my-output (svref curr-out-veck i))
				(sum (let ((dotty 0))
					(dotimes (j num-neurons-next-layer)
						(incf dotty (* (aref curr-weight-array i j)
							(svref next-delta-veck j))))
					dotty)))
			(setf (svref my-delta-veck i)
				(* my-output (- 1 my-output) sum))))))

	;; Now, update all of the weights in the network using the DELTA values
	;;  For each layer...
	(dotimes (lay-num (1- num-layers))
		(let ((weight-array (svref weight-arrays lay-num))
			(delta-veck (svref delta-vecks (1+ lay-num)))
			(output-veck (svref output-vecks lay-num)))
		;; For each neuron N_i in that layer...
		(dotimes (i (svref layer-sizes lay-num))
			;; For each neuron N_j in the following layer...
			(dotimes (j (svref layer-sizes (1+ lay-num)))
				;; Update the weight on the edge from N_i to N_j
				;; W_I_J += ALPHA * A_I * DELTA_J
				(incf (aref weight-array i j)
					(* alpha 
						(svref output-veck i) 
						(svref delta-veck j)))))))

	;; return the NN
	nn))




;;;  TRAIN-ONE
;;; ----------------------------------------------------
;;;  INPUTS:  NN, a neural network
;;;           ALPHA, a small positive number that specifies the sensitivity
;;;             of updates to the error
;;;           INPUTS, a list of input values for the neurons in the input layer
;;;           TARGET-OUTPUTS, the desired outputs for neurons in the output
;;;             layer, given the specified INPUTS.
;;;  OUTPUT:  NN
;;;  SIDE EFFECT:  Uses FEED-FORWARD to generate output values for
;;;   the given inputs; then uses the BACK-PROPAGATION algorithm to
;;;   generate DELTA values for each neuron (starting from output layer
;;;   and working back to first hidden layer); then uses the DELTA values
;;;   to update each non-input neuron.

(defun train-rnn-one (rnn alpha inputs target-outputs)
	(format T "~A" rnn)
	(rnn-ff rnn inputs)
	(format T "~%-------- After Forward Pass --------~%")
	(format T "~A" rnn)

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
					(setf (svref h-o-gradi neuron-num)
						(* my-output (- 1 my-output) diffy))))

				;; for each neuron in the hidden layer...
				(dotimes (neuron-num n-h)
					(let* (
						(my-output (svref h-veck neuron-num))
						(sum (let ((dotty 0))
							(dotimes (j *CC*)
								(incf dotty (* (aref h-o-weights neuron-num j)
									(svref h-o-gradi j))))
							dotty)))
					(setf (svref i-h-gradi neuron-num)
						(* my-output (- 1 my-output) sum))))

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
								(svref o-veck i) 
								(svref i-h-gradi j)))))
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

				)
			)
		;; return the RNN
		(format T "~%-------- After backprop --------~%")
		(format T "~A" rnn)
		(format T "~%~%~%~A" (rnn-o-vecks rnn))
		; rnn
		)
	)

;;;  TRAIN-ALL
;;; ------------------------------------------
;;;  INPUTS:  NN, a neural network
;;;           ALPHA, a training sensitivity parameter
;;;           IN-OUT-PAIRS, a list of training data (input-output pairs)
;;;  OUTPUT: NN
;;;  SIDE EFFECT:  Performs feed-forward/back-propagation on each 
;;;                  input-output pair.

(defun train-all (nn alpha in-out-pairs)
	(dolist (pair in-out-pairs)
		(train-one nn alpha (first pair) (second pair)))
	nn)

;;;  TRAIN-FOR-SINE
;;; ---------------------------------------------------
;;;  INPUT:  ALPHA, training sensitivity parameter
;;;          LISTY, a list of row-lengths for the neural network
;;;          (e.g., '(1 4 3 1))
;;;          NUM-TRIALS, number of training examples to run
;;;  OUTPUT:  trained network
;;;  SIDE EFFECT:  creates a neural network and performs NUM-TRIALS
;;;                rounds of training so that the network can "learn"
;;;                how to simulate the sine function

(defun train-for-sine (alpha listy num-trials)
	(let ((nn (init-nn listy)))

		(dotimes (i num-trials)
			(let* ((x (/ (random 100) 16.0))
				(y (sin (/ x 2))))
			(train-one nn alpha (list x) (list y))))
		nn))



;;;  GET-OUTPUT-FOR
;;; --------------------------------------
;;;  INPUTS:  NN, a neural network
;;;           INPUTS, a list of input values for the neurons in the
;;;             input layer of NN
;;;  OUTPUT:  A vector of output values corresponding to those inputs
;;;            (resulting from doing FEED-FORWARD)

(defun get-output-for (nn inputs)
	(feed-forward nn inputs)
	(let* ((num-layers (nn-num-layers nn))
		(out-vecks (nn-output-vecks nn))
		)
	(svref out-vecks (1- num-layers))))

;;;  VECTOR->LIST
;;; --------------------------------------------
;;;  INPUT:  VECK, a vector
;;;  OUTPUT:  A list containing the same elements as VECK

(defun vector->list (veck)
	(let ((listy nil))
		(dotimes (i (length veck))
			(push (svref veck i) listy))
		(nreverse listy)))

;;;  COMPARE-VALUES
;;; ------------------------------------------------
;;;  INPUT: NN, a neural network trained to simulate the SINE function
;;;         NUM, number of data points to compare NN vs. actual SINE func.
;;;  OUTPUT: NIL
;;;  SIDE EFFECT:  Displays a comparison of the true SINE values
;;;    and those computed by the network for a variety of inputs.

(defun compare-values (nn num)
	(dotimes (i num)
		(let* ((x (/ i 16))
			(output (first (vector->list (get-output-for nn (list x)))))
			(realout (sin (/ x 2))))
		(format t "X: ~6,3F, NN: ~6,3F, REAL: ~6,3F, DIFF: ~6,3F~%"
			x output realout (- output realout)))))

;;(setf nn (train-for-sine .2 '(1 4 5 4 1) 100000))
;;(setf nn (train-for-sine .2 '(1 4 3 1) 100000))
;;(compare-values nn 50)


;;  Training neural network for XOR

(defun xor (x y)
	(if (= (+ x y) 1) 1 0))

(defun train-for-xor (alpha listy num-trials)
	(let ((nn (init-nn listy)))
		(dotimes (i num-trials)
			(let* ((x (random 2))
				(y (random 2)))
			(train-one nn alpha (list x y) (list (xor x y)))))
		nn))

(defun train-for-binary (alpha listy num-trials func)
	(let ((nn (init-nn listy)))
		(dotimes (i num-trials)
			(let* ((x (random 2))
				(y (random 2)))
			(train-one nn alpha (list x y) (list (funcall func x y)))))
		nn))

(defun show-binary-results (nn func)
	(dotimes (x 2)
		(dotimes (y 2)
			(format t "(funk ~A ~A) ==> ~A; NN got: ~A~%" x y
				(funcall func x y) (get-output-for nn (list x y))))))

(defun show-xor-results (nn)
	(dolist (listy '((0 0 0) (0 1 1) (1 0 1) (1 1 0)))
		(let ((x (first listy))
			(y (second listy)))
		(format t "(xor ~A ~A) ==> ~A; NN got: ~A~%" x y (xor x y)
			(get-output-for nn (list x y))))))


;; (setf nn (train-for-xor 1 '(2 4 1) 10000))
;; (show-xor-results nn)


