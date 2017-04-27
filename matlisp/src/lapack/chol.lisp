;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Package: :matlisp; Base: 10 -*-
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Copyright (c) 2000 The Regents of the University of California.
;;; All rights reserved.
;;;
;;; Permission is hereby granted, without written agreement and without
;;; license or royalty fees, to use, copy, modify, and distribute this
;;; software and its documentation for any purpose, provided that the
;;; above copyright notice and the following two paragraphs appear in all
;;; copies of this software.
;;;
;;; IN NO EVENT SHALL THE UNIVERSITY OF CALIFORNIA BE LIABLE TO ANY PARTY
;;; FOR DIRECT, INDIRECT, SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES
;;; ARISING OUT OF THE USE OF THIS SOFTWARE AND ITS DOCUMENTATION, EVEN IF
;;; THE UNIVERSITY OF CALIFORNIA HAS BEEN ADVISED OF THE POSSIBILITY OF
;;; SUCH DAMAGE.
;;;
;;; THE UNIVERSITY OF CALIFORNIA SPECIFICALLY DISCLAIMS ANY WARRANTIES,
;;; INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
;;; MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE. THE SOFTWARE
;;; PROVIDED HEREUNDER IS ON AN "AS IS" BASIS, AND THE UNIVERSITY OF
;;; CALIFORNIA HAS NO OBLIGATION TO PROVIDE MAINTENANCE, SUPPORT, UPDATES,
;;; ENHANCEMENTS, OR MODIFICATIONS.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package #:matlisp)

(deft/generic (t/lapack-potrf! #'subtypep) sym (A lda uplo))
(deft/method t/lapack-potrf! (sym blas-mixin) (A lda uplo)
  (let ((ftype (field-type sym)))
    (using-gensyms (decl (A lda uplo))
      `(let* (,@decl)
	 (declare (type ,sym ,A)
		  (type index-type ,lda)
		  (type character ,uplo))
	 (ffuncall ,(blas-func "potrf" ftype)
		   (:& :char) ,uplo
		   (:& :int) (dimensions ,A 0)
		   (:* ,(lisp->mffi ftype) :+ (head ,A)) (the ,(store-type sym) (store ,A)) (:& :int) ,lda
		   (:& :int :output) 0)))))

;;
(closer-mop:defgeneric potrf! (a &optional uplo)
  (:documentation "
  Syntax
  ======
  (POTRF! a)

  Purpose
  =======
  POTRF computes the Cholesky factorization of a real symmetric
  positive definite matrix A.

  This is the block version of the algorithm, calling Level 3 BLAS.

  Return Values
  =============
  [1] The factor U or L from the Cholesky
	  factorization A = U**T*U or A = L*L**T.
  [2] INFO = T: successful
	     i:  U(i,i) is exactly zero.
")
  (:method :before ((a tensor) &optional (uplo *default-uplo*))
     (assert (typep a 'tensor-square-matrix) nil 'tensor-dimension-mismatch :message "Expected square matrix.")
     (assert (member uplo '(:l :u)) nil 'invalid-arguments :given uplo :expected `(member uplo '(:l :u))))
  (:generic-function-class tensor-method-generator))

(define-tensor-method potrf! ((a blas-mixin :x) &optional (uplo *default-uplo*))
  `(with-columnification (() (A))
     (let ((info (t/lapack-potrf! ,(cl :x) A (or (blas-matrix-compatiblep A #\N) 0) (char-upcase (aref (symbol-name uplo) 0)))))
       (unless (= info 0)
	 (if (< info 0)
	     (error "POTRF: the ~a'th argument had an illegal value." (- info))
	     (error 'matrix-not-pd :message "POTRF: the leading minor of order ~a is not p.d; the factorization could not be completed." :position info)))))
  'A)

(definline chol! (a &optional (uplo *default-uplo*))
  (tricopy! 0 (potrf! a uplo) (ecase uplo (:l :uo) (:u :lo))))
;;
(deft/generic (t/lapack-potrs! #'subtypep) sym (A lda B ldb uplo))
(deft/method t/lapack-potrs! (sym blas-mixin) (A lda B ldb uplo)
  (let ((ftype (field-type sym)))
    (using-gensyms (decl (A lda B ldb uplo))
      `(let* (,@decl)
	 (declare (type ,sym ,A ,B)
		  (type index-type ,lda ,ldb)
		  (type character ,uplo))
	 (ffuncall ,(blas-func "potrs" ftype)
	   (:& :char) ,uplo
	   (:& :int) (dimensions ,A 0) (:& :int) (dimensions ,B 1)
	   (:* ,(lisp->mffi ftype) :+ (head ,A)) (the ,(store-type sym) (store ,A)) (:& :int) ,lda
	   (:* ,(lisp->mffi ftype) :+ (head ,B)) (the ,(store-type sym) (store ,B)) (:& :int) ,ldb
	   (:& :int :output) 0)))))

;;
(closer-mop:defgeneric potrs! (A B &optional uplo)
  (:documentation "
  Syntax
  ======
  (POTRS! a b [:U :L])

  Purpose
  =======
  Solves a system of linear equations
      A * X = B  or  A' * X = B
  with a general N-by-N matrix A using the Cholesky LU factorization computed
  by POTRF.  A and are the results from POTRF, UPLO specifies
  the form of the system of equations:
	   = 'U':   A = U**T*U
	   = 'L':   A = L*L**T

  Return Values
  =============
  [1] The NxM matrix X. (overwriting B)
  [4] INFO = T: successful
	     i:  U(i,i) is exactly zero.  The LU factorization
		 used in the computation has been completed,
		 but the factor U is exactly singular.
		 Solution could not be computed.
")
  (:method :before ((A tensor) (B tensor) &optional (uplo *default-uplo*))
     (assert (and (typep A 'tensor-square-matrix) (<= (order B) 2) (= (dimensions A 0) (dimensions B 0))) nil 'tensor-dimension-mismatch)
     (assert (member uplo '(:l :u)) nil 'invalid-value :given uplo :expected `(member uplo '(:u :l))))
  (:generic-function-class tensor-method-generator))

(define-tensor-method potrs! ((A blas-mixin :x) (B blas-mixin :x t) &optional (uplo *default-uplo*))
  `(if (tensor-vectorp B)
       (potrs! A (suptensor~ B 2) uplo)
       (with-columnification (((A #\C)) (B))
	 (let ((info (t/lapack-potrs! ,(cl :x)
				      A (or (blas-matrix-compatiblep A #\N) 0)
				      B (or (blas-matrix-compatiblep B #\N) 0)
				      (aref (symbol-name uplo) 0))))
	   (unless (= info 0) (error "POTRS returned ~a. the ~:*~a'th argument had an illegal value." (- info))))))
  'B)
;;
(deft/generic (t/lapack-potri! #'subtypep) sym (A lda uplo))
(deft/method t/lapack-potri! (sym blas-mixin) (A lda uplo)
  (let ((ftype (field-type sym)))
    (using-gensyms (decl (A lda uplo))
      `(let* (,@decl)
	 (declare (type ,sym ,A)
		  (type index-type ,lda)
		  (type character ,uplo))
	 (ffuncall ,(blas-func "potri" ftype)
	   (:& :char) ,uplo (:& :int) (dimensions ,A 0)
	   (:* ,(lisp->mffi ftype) :+ (head ,A)) (the ,(store-type sym) (store ,A)) (:& :int) ,lda
	   (:& :int :output) 0)))))

(closer-mop:defgeneric potri! (A &optional uplo)
  (:documentation "
  Syntax
  ======
  (POTRI! a [:U :L])

  Purpose
  =======
  Computes the inverse of using the pre-computed Cholesky at A.
")
  (:method :before ((A blas-mixin) &optional (uplo *default-uplo*))
     (assert (and (typep A 'tensor-square-matrix)) nil 'tensor-dimension-mismatch)
     (assert (member uplo '(:l :u)) nil 'invalid-value :given uplo :expected `(member uplo '(:u :l))))
  (:generic-function-class tensor-method-generator))

(define-tensor-method potri! ((A blas-mixin :x) &optional (uplo *default-uplo*))
  `(with-columnification (() (A))
     (let ((info (t/lapack-potri! ,(cl :x)
				  A (or (blas-matrix-compatiblep A #\N) 0)
				  (aref (symbol-name uplo) 0))))
       (unless (= info 0) (error "POTRI returned ~a. the ~:*~a'th argument had an illegal value." (- info)))))
  'A)
;;

(defun chol (a &optional (uplo *default-uplo*))
  (declare (type (and tensor-square-matrix blas-mixin) a))
  (let ((l (copy a)))
    (labels ((call () (restart-case (potrf! l uplo)
			(increment-diagonal-and-retry (value)
			  (copy! a l) (axpy! value nil (diagonal~ l))
			  (call)))))
      (call))
    (tricopy! 0d0 l (ecase uplo (:u :lo) (:l :uo)))))
;;

;;
(deft/generic (t/lapack-ldl! #'subtypep) sym (A lda uplo ipiv &optional het?))
(deft/method t/lapack-ldl! (sym blas-mixin) (A lda uplo ipiv &optional het?)
  (let* ((ftype (field-type sym)) (complex? (subtypep ftype 'cl:complex)))
    (using-gensyms (decl (A lda uplo ipiv) (xxx lwork))
      `(let* (,@decl)
	 (declare (type ,sym ,A) (type index-type ,lda) (type character ,uplo)
		  (type (simple-array ,(matlisp-ffi:mffi->lisp :int) (*)) ,ipiv))
	 (with-lapack-query ,sym (,xxx ,lwork)
	   (ffuncall ,(blas-func (if (or (not het?) (not complex?)) "sytrf" "hetrf") ftype)
	     (:& :char) ,uplo
	     (:& :int) (dimensions ,A 0) (:* ,(lisp->mffi ftype) :+ (head ,A)) (the ,(store-type sym) (store ,A)) (:& :int) ,lda
	     (:* :int) (the (simple-array ,(matlisp-ffi:mffi->lisp :int) (*)) ,ipiv)
	     (:* ,(lisp->mffi ftype)) (the ,(store-type sym) ,xxx) (:& :int) ,lwork
	     (:& :int :output) 0))))))

(closer-mop:defgeneric ldl! (a &optional hermitian? uplo)
  (:method :before ((a tensor) &optional hermitian? (uplo *default-uplo*))
     (declare (ignore hermitian?))
     (assert (typep a 'tensor-square-matrix) nil 'tensor-dimension-mismatch :message "Expected square matrix.")
     (assert (member uplo '(:l :u)) nil 'invalid-arguments :given uplo :expected `(member uplo '(:l :u))))
  (:generic-function-class tensor-method-generator))

(define-tensor-method ldl! ((a blas-mixin :x) &optional (hermitian? t) (uplo *default-uplo*))
  '(declare (ignorable hermitian?))
  (let ((complex? (subtypep (field-type (cl :x)) 'cl:complex)))
    `(let ((ipiv (make-array (lvec-min (the index-store-vector (dimensions A))) :element-type ',(matlisp-ffi:mffi->lisp :int))))
       (with-columnification (() (A))
	 ,(recursive-append
	   (if complex?
	       `(if hermitian?
		    (let ((info (t/lapack-ldl! ,(cl :x) A (or (blas-matrix-compatiblep A #\N) 0) (char-upcase (aref (symbol-name uplo) 0)) ipiv t)))
		      (unless (= info 0)
			(if (< info 0)
			    (error "HETRF: the ~a'th argument had an illegal value." (- info))
			    (warn 'matrix-not-pd :message "HETRF: D(~a, ~:*~a) is exactly zero. The factorization has been completed, but the block diagonal matrix D is exactly singular, and division-by-zero by zero will occur if it is used to solve a system of equations." :position info))))))
	   `(let ((info (t/lapack-ldl! ,(cl :x) A (or (blas-matrix-compatiblep A #\N) 0) (char-upcase (aref (symbol-name uplo) 0)) ipiv nil)))
	      (unless (= info 0)
		(if (< info 0)
		    (error "SYTRF: the ~a'th argument had an illegal value." (- info))
		    (error 'matrix-not-pd :message "SYTRF: D(~a, ~:*~a) is exactly zero. The factorization has been completed, but the block diagonal matrix D is exactly singular, and division by zero will occur if it is used to solve a system of equations." :position info))))))
       (setf (gethash '|(SY/HE)TRF| (memos A)) ipiv)
       (values a ipiv))))

(defun ldl (a &optional (hermitian? t) (uplo *default-uplo*))
  (declare (type (and tensor-square-matrix blas-mixin) a))
  (letv* ((ret ipiv (ldl! (copy a) hermitian? uplo))
	  (D (zeros (dimensions A) (class-of A))))
    (tricopy! (diagonal~ ret) D :d)
    (tricopy! 1 (tricopy! 0 ret (ecase uplo (:u :lo) (:l :uo))) :d)
    (iter (for σi in-vector ipiv with-index i)
	  (when (< σi 0)
	    (ecase uplo
	      (:u (rotatef (ref D i (1+ i)) (ref ret i (1+ i))))
	      (:l (rotatef (ref D i (1+ i)) (ref ret (1+ i) i))))
	    (setf (ref D (1+ i) i) (ref D i (1+ i)))
	    (when hermitian?
	      (ecase uplo
		(:u (setf (ref D (1+ i) i) (conjugate (ref D i (1+ i)))))
		(:l (setf (ref D i (1+ i)) (conjugate (ref D (1+ i) i))))))
	    (incf i)))
    (values (ldl-permute! ret ipiv uplo) D
	    (let ((p (with-no-init-checks (make-instance 'permutation-pivot-flip :size (length ipiv) :store (pflip.f->l ipiv uplo)))))
	      (ecase uplo (:u (permutation/ p)) (:l p))))))

(defun ldl-permute! (m ipiv &optional (uplo *default-uplo*))
  (declare (type (and tensor-square-matrix blas-mixin) m)
	   (type (simple-array (signed-byte 32)) ipiv))
  (let ((rv (slice~ m 0)) (rvσ (slice~ m 0))
	(n (length ipiv)) (hd (head m)) (sr (strides m 0)) (sc (strides m 1)))
    (declare (type index-type n sr sc hd))
    (ecase uplo
      (:u
       (macrolet ((update! (x r d)
		    `(setf (aref (dimensions ,x) 0) (the index-type (- n ,d 1))
			   (slot-value ,x 'head) (the index-type (+ hd (the index-type (* sc (1+ ,d))) (the index-type (* sr ,r)))))))
	 (iter (for σi in-vector ipiv with-index i downto 0)
	       (unless (= i (1- σi))
		 (if (< σi 0)
		     (progn (update! rv (1- i) i) (update! rvσ (1- (- σi)) i) (decf i))
		     (progn (update! rv i i) (update! rvσ (1- σi) i)))
		 (when (< 0 (dimensions rv 0))
		   (swap! rv rvσ))))))
      (:l
       (macrolet ((update! (x r d)
		    `(setf (aref (dimensions ,x) 0) (the index-type ,d)
			   (slot-value ,x 'head) (the index-type (+ hd (the index-type (* sr ,r)))))))
	 (iter (for σi in-vector ipiv with-index i)
	       (unless (= i (1- σi))
		 (if (< σi 0)
		     (progn (update! rv (1+ i) i) (update! rvσ (1- (- σi)) i) (incf i))
		     (progn (update! rv i i) (update! rvσ (1- σi) i)))
		 (when (< 0 (dimensions rv 0))
		   (swap! rv rvσ))))))))
  m)
