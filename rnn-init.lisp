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
	;;  (aref output-veck i) -- is the ith input in the input sequence
	i-vecks
	;; OUTPUT-VECKs:  A vector of seq-len vectors of size *CC*
	;;  (aref output-veck i) -- is the ith output in the output sequence
	o-vecks
	;; H-VECKs:  A vector of seq-len vectors of size n-h
	;;  (aref output-veck i) -- is the ith output in the output sequence
	h-vecks
	;; I-H-WEIGHTS:  A vector of arrays.
	;;  (aref i-h-weights i) -- is an MxN array holding the weights
	;;        for all edges between the input layer and the hidden layer
	;;        M = *CC*; N = n-h
	i-h-weights
	;; H-H-WEIGHTS:  A vector of arrays.
	;;  (aref h-h-weights i) -- is an NxN array holding the weights
	;;        for all edges between the hidden layer and the next hidden
	;;		  layer
	;;        N = n-h
	h-h-weights
	;; H-O-WEIGHTS:  A vector of arrays.
	;;  (aref h-h-weights i) -- is an NxM array holding the weights
	;;        for all edges between the hidden layer and the next hidden
	;;		  layer
	;;        N = n-h; M = *CC*
	h-o-weights
	)

(defun convertToOneHot (outputVec)
	(let ((max 0)
		(foundHot nil)
		(oneHot (make-array *CC* :initial-element 0.0 :element-type 'float))
		)
	(dotimes (i *CC*)
		(setf i (+ i (random 10)))
		(when (and (< i *CC*) (> (aref outputVec i) max)) (setf max (aref outputVec i)))
		)

	(dotimes (i *CC*)
		(when (or foundHot (not (= (aref outputVec i) max)))
			(setf (aref oneHot i) 0)
			)
		(when (and (= (aref outputVec i) max) (not foundHot))
			(setf (aref oneHot i) 1)
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
			(setf (aref inputs sn) (make-array *CC* :initial-element 0.0 :element-type 'float))
			)

		(dotimes (i length)
			(rnn-ff rnn inputs)
			(dotimes (sn sl)
				(let (
					(ohv-out (convertToOneHot (aref (rnn-o-vecks rnn) sn)))
					)
				; Print it
				; (format T "~A" ohv-out)
				(format T "~A" (one-hot-vec-char ohv-out))
				; Set to input
				(setf (aref inputs sn) ohv-out)
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
		(i-h-weights (make-array n-h)) ;(list n-h *CC*)))
		(h-h-weights (make-array n-h)) ;(list n-h n-h)))
		(h-o-weights (make-array *CC*));(list *CC* n-h)))
		(rnn (make-rnn :seq-len seq-len
			:n-h n-h
			:i-vecks i-vecks
			:o-vecks o-vecks
			:h-vecks h-vecks
			:i-h-weights i-h-weights
			:h-h-weights h-h-weights
			:h-o-weights h-o-weights
			)))

	;;initializations of the nodes
	(dotimes (i seq-len)
		(setf (aref i-vecks i) (make-array *CC* :initial-element 0.0 
												:element-type 'float))
		(setf (aref h-vecks i) (make-array n-h :initial-element 0.0 
												:element-type 'float))
		(setf (aref o-vecks i) (make-array *CC* :initial-element 0.0 
												:element-type 'float))
	)
	
	;;initializations of the weights
	(dotimes (j n-h)
		(setf (aref i-h-weights j) (make-array *CC* :initial-element 0.0 
												   :element-type 'float))
		(setf (aref h-h-weights j) (make-array n-h :initial-element 0.0 
												   :element-type 'float))
		
		(dotimes (k n-h)
			(setf (aref (aref h-h-weights j) k) (+ (random (/ 2 (sqrt n-h))) (/ -1 (sqrt n-h))))
		)
		(dotimes (k *CC*)
			(setf (aref (aref i-h-weights j) k) (+ (random (/ 2 (sqrt *CC*))) (/ -1 (sqrt *CC*))))
		)
	)

	(dotimes (j *CC*)
		(setf (aref h-o-weights j) (make-array n-h :initial-element 0.0 
												   :element-type 'float))
		(dotimes (k n-h)
			(setf (aref (aref h-o-weights j) k) (+ (random (/ 2 (sqrt n-h))) (/ -1 (sqrt n-h))))
			
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
			(i-nodes (aref i-vecks i))
			(h-nodes (aref h-vecks i))
			(o-nodes (aref o-vecks i)))
		;; For each neuron in that layer...
		(dotimes (j *CC*)
			;; Set that neuron's output value to NIL
			(setf (aref i-nodes j) nil)
			(setf (aref o-nodes j) nil)
			)
		(dotimes (j n-h)
			;; Set that neuron's output value to NIL
			(setf (aref h-nodes j) nil)
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
		(out-veck-zero (aref out-vecks 0))
		(num-inputs (aref (nn-layer-sizes nn) 0)))
	(cond
		;; CASE 1:  INPUTS has the right number of input values
		((= num-inputs (length inputs))
			;; For each input value...
			(dotimes (i num-inputs)
				;; Set the "output" value for the corresponding neuron in layer 0 
				(setf (aref out-veck-zero i) (nth i inputs)))
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

;; DOT-PRODUCT
;; -------------------------------
;; INPUTS: a, the input vector
;;         b, 
;; OUTPUTS: their dot product

(defmacro dot-product (a b)
	`(loop for x across ,a
		for y across ,b
			summing (* x y)))

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
	(loop for i from 0 to (1- (rnn-seq-len rnn))
		do
		(let* (
			(i-veck (svref i-vecks i))
			(h-veck (svref h-vecks i))
			(o-veck (svref o-vecks i)))
			
			;; For each neuron in the hidden layer
			(loop for neuron-num from 0 to (1- n-h)
				do
				;; Compute output value of that neuron 
				(setf (aref h-veck neuron-num)
					;; sigmoid of the DOT-PRODUCT of WEIGHTS and INPUT VALUES
					;;  (INPUTS for this neuron are OUTPUTS from neurons 
					;;     in previous layer)

					(if (= i 0)
						;; If this is the first of the sequence, just pass normally
						(sigmoid (dot-product i-veck (aref i-h-weights neuron-num)))
						;; Otherwise consider the previous hidden layer in the sequence
						(sigmoid 
							(+
								(dot-product (aref h-vecks (- i 1)) (aref h-h-weights neuron-num))
								(dot-product i-veck (aref i-h-weights neuron-num))
							)
						)
					)
				)
			)

			
			; ;; For each neuron in the output layer
			(loop for neuron-num from 0 to (1- *CC*)
				do
				;; Compute output value of that neuron 
				(setf (aref o-veck neuron-num)
					;; sigmoid of the DOT-PRODUCT of WEIGHTS and INPUT VALUES
					;;  (INPUTS for this neuron are OUTPUTS from neurons 
					;;     in previous layer)
					(dot-product h-veck (aref h-o-weights neuron-num))
				)
			)
		)
	))
)

(defmacro update-gradiant (output change const)
	`(* ,const (abs (* ,output (- 1 ,output))) ,change))

(defun train-rnn-one (rnn alpha inputs target-outputs)
	; (format T "~A" rnn)

	; (format t "~%Input: ")
	; (dotimes (i (rnn-seq-len rnn))
	; 	(format t "~A" (one-hot-vec-char (aref inputs i)))
	; 	)

	; (format t "~%Goal: ")
	; (dotimes (i (rnn-seq-len rnn))
	; 	(format t "~A" (one-hot-vec-char (aref target-outputs i)))
	; 	)

	;FEED FORWARD
	(rnn-ff rnn inputs)

	; (let ((output (rnn-o-vecks rnn)))
	; 	(format t "~%Our Guess: ")
	; 	(dotimes (i (rnn-seq-len rnn))
	; 		;(format t "~A~%" (aref output i))
	; 		(format t "~A" (one-hot-vec-char (convertToOneHot (aref output i))))
	; 		)
	; 	)
	; (format T "~%-------- After Forward Pass --------~%")
	; (format T "~A" rnn)

	(let* 

		((n-h (rnn-n-h rnn))
		 (seq-len (rnn-seq-len rnn))
		 (i-h-weights (rnn-i-h-weights rnn))
		 (h-h-weights (rnn-h-h-weights rnn))
		 (h-o-weights (rnn-h-o-weights rnn))
		 )

		(loop for n from 0 to (1- seq-len)
			do
			
			(let
				((target-output-n (aref target-outputs n))
				 (i-veck (aref (rnn-i-vecks rnn) n))
				 (h-veck (aref (rnn-h-vecks rnn) n))
				 (o-veck (aref (rnn-o-vecks rnn) n))
				 (i-h-gradi (make-array *CC* :initial-element 0.0 :element-type 'float))
				 (h-h-gradi (make-array n-h :initial-element 0.0 :element-type 'float))
		         (h-o-gradi (make-array *CC* :initial-element 0.0 :element-type 'float))
				)

				;h-o
				(loop for neuron-num from 0 to (1- *CC*)
					do
					(let*
						(
						 (target-output (aref target-output-n neuron-num))
						 (my-output (aref o-veck neuron-num))
						 (diffy (- target-output my-output))
						)
					  (incf (aref h-o-gradi neuron-num)
					  	(update-gradiant my-output diffy 1)
					  )
					)
				)

				; i-h
				(loop for neuron-num from 0 to (1- n-h)
					do
					(let
						((my-output (aref h-veck neuron-num))
						 (sum (dot-product (aref h-o-weights neuron-num) h-o-gradi))
						)

					  (incf (aref i-h-gradi neuron-num)
		 				(update-gradiant my-output sum 1)
		 			  )
					)
				)
				
				;h-h
				;i-h
				(loop for hn from (1- n) downto 0
					do
					(let* 
						((hn-veck (aref (rnn-h-vecks rnn) hn)))
						;h-h
						(loop for neuron-num from 0 to (1- n-h)
							do
							(let* 
								((my-output (aref hn-veck neuron-num))
								(sum (dot-product (aref h-o-weights neuron-num) h-o-gradi)))
								(incf (aref h-h-gradi neuron-num)
									(update-gradiant my-output sum (/ hn n)))
							)
						)
						;i-h
						(loop for neuron-num from 0 to (1- n-h)
							do
							(let* 
								((my-output (aref hn-veck neuron-num))
								(sum (dot-product (aref h-h-weights neuron-num) h-h-gradi)))
								(incf (aref i-h-gradi neuron-num)
									(update-gradiant my-output sum 1))
							)
						)
					)
				)

				;; Now, update all of the weights in the network using the DELTA values
		 		;; >>> i-h
		 		;; For each neuron N_i in that layer...
		 		(loop for i from 0 to (1- n-h)
		 			do
		 			(let (
		 				(i-h-weight (aref i-h-weights i))
		 				(h-h-weight (aref h-h-weights i))
		 				(i-veck-val (aref i-veck i))
		 				(h-veck-val (aref h-veck i)))

			 			(loop for j from 0 to (1- *CC*)
			 				do
			 				(incf (aref i-h-weight j) 
			 					(* alpha 
		 						i-veck-val
								(aref i-h-gradi i)))
			 			)

			 			(loop for j from 0 to (1- n-h)
			 				do
			 				(incf (aref h-h-weight j) 
			 					(* alpha 
		 						h-veck-val
								(aref h-h-gradi j)))
			 			)
			 			(loop for j from 0 to (1- *CC*)
			 				do
				 			(incf (aref (aref h-o-weights j) i) 
				 					(* alpha 
			 						h-veck-val
									(aref h-o-gradi j)))
				 		)
		 			)
		 		)

			)
		)
	)
	rnn
)


		; 		;; Now, update all of the weights in the network using the DELTA values
		; 		;; >>> i-h
		; 		;; For each neuron N_i in that layer...
		; 		(dotimes (i *CC*)
		; 			;; For each neuron N_j in the following layer...
		; 			(dotimes (j n-h)
		; 				;; Update the weight on the edge from N_i to N_j
		; 				;; W_I_J += ALPHA * A_I * DELTA_J
		; 				(incf (aref i-h-weights i j)
		; 					(* alpha 
		; 						(aref i-veck i) 
		; 						(aref i-h-gradi j)))))

		; 		; ;; >>> h-h
		; 		; ;; For each neuron N_i in that layer...
		; 		(dotimes (i n-h)
		; 			;; For each neuron N_j in the following layer...
		; 			(dotimes (j n-h)
		; 				;; Update the weight on the edge from N_i to N_j
		; 				;; W_I_J += ALPHA * A_I * DELTA_J
		; 				(incf (aref h-h-weights i j)
		; 					(* alpha 
		; 						(aref h-veck i) 
		; 						(aref h-h-gradi j)))))

		; 		;; >>> h-o
		; 		;; For each neuron N_i in that layer...
		; 		(dotimes (i n-h)
		; 			;; For each neuron N_j in the following layer...
		; 			(dotimes (j *CC*)

		; 				;; Update the weight on the edge from N_i to N_j
		; 				;; W_I_J += ALPHA * A_I * DELTA_J
		; 				(incf (aref h-o-weights i j)
		; 					(* alpha 
		; 						(aref h-veck i) 
		; 						(aref h-o-gradi j)))))

		; 		; (format t "~A~%" h-o-gradi)
		; 		)
		; 	)
		; ; return the RNN
		; (format T "~%-------- After backprop --------~%")
		; (format T "~A" rnn)

		; (format t "~A~%" (babble rnn 10))


