(in-package #:matlisp)

(deft/generic (t/zeros #'subtypep) sym (dims &optional initarg))

(deft/method t/zeros (class stride-accessor) (dims &optional initial-element)
  (with-gensyms (dimsv strdv tsize init ret)
    `(letv* ((,dimsv (coerce ,dims 'index-store-vector) :type index-store-vector)
	     (,strdv ,tsize (make-stride ,dimsv) :type index-store-vector index-type)
	     ,@(when initial-element `((,init ,initial-element)))
	     ;;(,ret (sb-pcl::allocate-standard-instance ,(sb-pcl::class-wrapper (sb-pcl::ensure-class-finalized (find-class class)))))
	     (,ret (allocate-instance ,(find-class class))))
       (setf (slot-value ,ret 'dimensions) ,dimsv
	     (slot-value ,ret 'strides) ,strdv
	     (slot-value ,ret 'head) 0
	     (slot-value ,ret 'store) ,(recursive-append
				       (when initial-element `(if ,init (t/store-allocator ,class ,tsize :initial-element (t/coerce ,(field-type class) ,init))))
				       `(t/store-allocator ,class ,tsize))
	     (slot-value ,ret 'parent) nil
	     (slot-value ,ret 'memos) nil)
       ,ret)))

(deft/method (t/zeros #'hash-table-storep) (class stride-accessor) (dims &optional size)
  (with-gensyms (dimsv strdv tsize)
    `(letv* ((,dimsv (coerce ,dims 'index-store-vector) :type index-store-vector)
	     (,strdv ,tsize (make-stride-cmj ,dimsv) :type index-store-vector index-type))
       (make-instance ',class :dimensions ,dimsv :head 0 :strides ,strdv :stride-pivot (stride-pivot ,strdv)
		      :store (t/store-allocator ,class ,tsize :size (cl:max (cl:ceiling (cl:* *default-sparsity* ,tsize)) (or ,size 0)))))))

(deft/method t/zeros (class graph-accessor) (dims &optional size)
  (with-gensyms (dimsv nnz)
    `(letv* ((,dimsv (coerce ,dims 'index-store-vector) :type (index-store-vector 2))
	     (,nnz (cl:max (cl:ceiling (* *default-sparsity* (lvec-foldr #'* ,dimsv))) (or ,size 0))))
       (make-instance ',class :dimensions ,dimsv
		      :fence (t/store-allocator index-store-vector (1+ (aref ,dimsv 1))) ;;Compressed Columns by default
		      :neighbors (t/store-allocator index-store-vector ,nnz)
		      ,@(when (subtypep class 'tensor) `(:store (t/store-allocator ,class ,nnz)))))))

(deft/method t/zeros (class coordinate-accessor) (dims &optional size)
  (with-gensyms (dimsv nnz)
    `(letv* ((,dimsv (coerce ,dims 'index-store-vector) :type index-store-vector)
	     (,nnz (max (ceiling (* *default-sparsity* (lvec-foldr #'* ,dimsv))) (or ,size 0))))
       (make-instance ',class :dimensions ,dimsv
		      :indices (t/store-allocator index-store-matrix (list ,nnz (length ,dimsv)))
		      :stride-hash (t/store-allocator index-store-vector ,nnz)
		      :strides (make-stride-cmj ,dimsv)
		      ,@(when (subtypep class 'base-tensor) `(:store (t/store-allocator ,class ,nnz)))))))

#+nil
(deft/method t/zeros (class compressed-sparse-matrix) (dims &optional nz)
  (with-gensyms (dsym)
    `(let ((,dsym ,dims))
       (destructuring-bind (vr vd) (t/store-allocator ,class ,dsym ,nz)
	 (make-instance ',class
			:dimensions (make-index-store ,dims)
			:neighbour-start (allocate-index-store (1+ (second ,dsym)))
			:neighbour-id vr
			:store vd)))))

;;
;; (deft/method t/zeros (class permutation-cycle) (dims &optional nz)
;;   (using-gensyms (decl (dims))
;;     `(let (,@decl)
;;        (declare (type index-type ,dims))
;;        (with-no-init-checks
;;	   (make-instance ',class
;;			  :store nil
;;			  :size 0)))))

;; (deft/method t/zeros (class permutation-action) (dims &optional nz)
;;   (using-gensyms (decl (dims))
;;     `(let (,@decl)
;;        (declare (type index-type ,dims))
;;        (with-no-init-checks
;;	   (make-instance ',class
;;			  :store (index-id ,dims)
;;			  :size ,dims)))))

;; (deft/method t/zeros (class permutation-pivot-flip) (dims &optional nz)
;;   (using-gensyms (decl (dims))
;;     `(let (,@decl)
;;        (declare (type index-type ,dims))
;;        (with-no-init-checks
;;	   (make-instance ',class
;;			  :store (index-id ,dims)
;;			  :size ,dims)))))

;;
(defgeneric zeros-generic (dims dtype &optional initarg)
  (:documentation "
    A generic version of @func{zeros}.
")
  (:method ((dims list) (dtype t) &optional initarg)
    ;;(assert (tensor-leafp dtype) nil 'tensor-abstract-class :tensor-class dtype)
    (compile-and-eval
     `(closer-mop:defmethod zeros-generic ((dims list) (dtype (eql ',dtype)) &optional initarg)
	(t/zeros ,dtype dims initarg)))
    (zeros-generic dims dtype initarg)))

(definline zeros (dims &optional type initarg)
"
    Create a tensor with dimensions @arg{dims} of class @arg{dtype}.
    The optional argument @arg{initarg} is used in two completely
    incompatible ways.

    If @arg{dtype} is a dense tensor, then @arg{initial-element}, is used to
    initialize all the elements. If @arg{dtype} is however, a sparse tensor,
    it is used for computing the number of nonzeros slots in the store.

    Example:
    M> (zeros 3)
    #<MATLISP::|<BLAS-MIXIN DENSE-TENSOR: DOUBLE-FLOAT>| #(3)
     0.000   0.000   0.000
    >

    M> (zeros 3 (tensor '(complex double-float) 'simple-dense-tensor) 2)
    #<MATLISP::|<BLAS-MIXIN DENSE-TENSOR: (COMPLEX DOUBLE-FLOAT)>| #(3)
     2.000   2.000   2.000
    >

    M> (zeros '(10000 10000) (tensor 'fixnum 'simple-graph-tensor) 10000)
    #<MATLISP::|<GRAPH-TENSOR: FIXNUM>| #(10000 10000), size: 0/100000>
"
  (let ((type (let ((type (or type *default-tensor-type*)))
		(etypecase type (standard-class (class-name type)) (symbol type) (list (apply #'tensor type))))))
    (etypecase dims
      (list (zeros-generic dims type initarg))
      (vector (zeros-generic (lvec->list dims) type initarg))
      (fixnum (zeros-generic (list dims) type initarg)))))

(declaim (ftype (function ((or list vector fixnum) &optional t t) t) zeros))
