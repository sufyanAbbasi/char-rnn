(in-package #:matlisp)

;; (permute!  '(jobvs sort select n a lda sdim w rwork vs ldvs work lwork bwork info) )
;;
(deft/generic (t/lapack-gees! #'subtypep) sym (A lda vs ldvs wr wi))
(deft/method t/lapack-gees! (sym blas-mixin) (A lda vs ldvs wr wi)
  (let* ((ftype (field-type sym)))
    (using-gensyms (decl (A lda vs ldvs wr wi) (lwork xxx))
      `(let (,@decl)
	 (declare (type ,sym ,A)
		  (type index-type ,lda)
		  (type ,(store-type sym) ,wr ,wi))
	 (with-lapack-query ,sym (,xxx ,lwork)
	   (ffuncall ,(blas-func "gees" ftype)
	     ,@(let ((args `((:& :char) (if ,vs #\V #\N) (:& :char) #\N :* (cffi:null-pointer)
			     (:& :int) (dimensions ,A 0)
			     (:* ,(lisp->mffi ftype) :+ (head ,A)) (the ,(store-type sym) (store ,A)) (:& :int) ,lda
			     (:& :int) 0
			     (:* ,(lisp->mffi ftype)) (the ,(store-type sym) ,wr) (:* ,(lisp->mffi ftype)) (the ,(store-type sym) ,wi)
			     (:* ,(lisp->mffi ftype) :+ (if ,vs (head ,vs) 0)) (if ,vs (the ,(store-type sym) (store ,vs)) (cffi:null-pointer)) (:& :int) (if ,vs ,ldvs 1)
			     (:* ,(lisp->mffi ftype)) (the ,(store-type sym) ,xxx) (:& :int) ,lwork
			     :* (cffi:null-pointer) (:& :int :output) 0)))
		    (if (subtypep ftype 'cl:complex)
			(apply #'append (permute (pair args) (make-instance 'permutation-cycle :store (list (idxv 12 11 10 9 8)))))
			args))))))))
;;
(closer-mop:defgeneric schur (A &optional job)
  (:documentation "
    Syntax
    ------
    (schur A &optional JOB) => λs, T &optional S

    Computes the eigenvalues @arg{λs}, the upper (quasi-upper) triangular Schur form @arg{U}, and optionally an orthogonal (unitary) basis of Schur
    vectors @arg{S} for the real (complex) square matrix A.")
  (:method :before ((A tensor) &optional (job :v))
     (assert (and (typep A 'tensor-square-matrix) (member job '(:v :n))) nil 'invalid-arguments :message "argument is not a square matrix."))
  (:generic-function-class tensor-method-generator))

(define-tensor-method schur ((A blas-mixin :x) &optional (job :v))
  `(let-typed ((tret (with-colm (copy A)))
	       (vs (when (eq job :v) (with-colm (zeros (dimensions A) ',(cl :x)))))
	       (wr (t/store-allocator ,(cl :x) (dimensions a 0)) :type ,(store-type (cl :x)))
	       (wi (t/store-allocator ,(cl :x) (dimensions a 0)) :type ,(store-type (cl :x))))
      (let ((info (t/lapack-gees! ,(cl :x)
				  tret (or (blas-matrix-compatiblep tret #\N) 0)
				  vs (when vs (or (blas-matrix-compatiblep vs #\N) 0))
				  wr wi)))
	(unless (= info 0)
	  (if (< info 0)
	      (error "GEES: Illegal value in the ~:r argument." (- info))
	      (error "GEES: (~a) the QR algorithm failed to compute all the eigenvalues." info))))
      (values-n (if vs 3 2)
		(make-instance ',(complexified-tensor (cl :x))
			       :dimensions (coerce (list (dimensions A 0)) 'index-store-vector)
			       :strides (coerce (list 1) 'index-store-vector)
			       :head 0
			       :store (t/geev-output-fix ,(cl :x) wr wi))
		tret vs)))
