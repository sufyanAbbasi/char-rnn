t;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Package: :matlisp; Base: 10 -*-
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package #:matlisp)

(deft/generic (t/scdi! #'subtypep) sym (x y &key scal? numx?))
(deft/method t/scdi! (sym dense-tensor) (x y &key (scal? t) (numx? nil))
  (using-gensyms (decl (x y) (ref-x ref-y))
    (with-gensyms (idx)
      `(let (,@decl)
	 (declare (type ,sym ,@(unless numx? `(,x)) ,y)
		  ,@(when numx? `((type ,(field-type sym) ,x))))
	 (very-quickly
	   (dorefs (,idx (dimensions ,y))
		   (,@(unless numx? `((,ref-x ,x :type ,sym)))
		      (,ref-y ,y :type ,sym))
	     (setf ,ref-y (,(if scal? 't/f* 't/f/) ,(field-type sym) ,ref-y  ,(if numx? x ref-x)))))
	 ,y))))
;;
(closer-mop:defgeneric scal! (alpha x)
  (:documentation
   "
  Syntax
  ======
  (SCAL! alpha x)

  Purpose
  =======
  X <- alpha .* X
")
  (:method :before ((x dense-tensor) (y dense-tensor))
	   (assert (very-quickly (lvec-eq (dimensions x) (dimensions y) #'=)) nil 'tensor-dimension-mismatch))
  (:generic-function-class tensor-method-generator))

(define-tensor-method scal! ((x dense-tensor :x) (y dense-tensor :x t))
  `(t/scdi! ,(cl :x) x y :scal? t :numx? nil)
  'y)

(define-tensor-method scal! ((x t) (y dense-tensor :x))
  `(let ((x (t/coerce ,(field-type (cl :x)) x)))
     (declare (type ,(field-type (cl :x)) x))
     (unless (t/f= ,(field-type (cl :x)) x (t/fid* ,(field-type (cl :x))))
       (t/scdi! ,(cl :x) x y :scal? t :numx? t))
     y))

(defgeneric scal (alpha x)
  (:documentation
   "
  Syntax
  ======
  (SCAL alpha x)

  Purpose
  =======
  Computes and returns a new tensor equal to

             alpha .* X

  where alpha is a scalar and X is a tensor.

")
  (:method ((alpha number) (x number))
    (* alpha x))
  (:method (alpha (x dense-tensor))
    (scal! alpha (copy x (when (complexp alpha) (complexified-tensor (class-name (class-of x)))))))
  ;;TODO: There is an issue here when x is not coerceable into the tensor class of alpha.
  (:method ((alpha dense-tensor) (x t))
    ;;We assume commutation of course.
    (scal! x (copy alpha (when (complexp x) (complexified-tensor alpha))))))

;;These should've been auto-generated.
(closer-mop:defgeneric div! (alpha x)
  (:documentation
   "
  Syntax
  ======
  (DIV! alpha x)

s  Purpose
  =======
  X <- X ./ alpha

  Yes the calling order is twisted.
")
  (:method :before ((x dense-tensor) (y dense-tensor))
	   (assert (very-quickly (lvec-eq (dimensions x) (dimensions y) #'=)) nil 'tensor-dimension-mismatch))
  (:generic-function-class tensor-method-generator))

(define-tensor-method div! ((x dense-tensor :x) (y dense-tensor :x t))
  `(t/scdi! ,(cl :x) x y :scal? nil :numx? nil)
  'y)

(define-tensor-method div! ((x t) (y dense-tensor :x))
  `(let ((x (t/coerce ,(field-type (cl :x)) x)))     
     (declare (type ,(field-type (cl :x)) x))
     (unless (t/f= ,(field-type (cl :x)) x (t/fid* ,(field-type (cl :x))))
       (t/scdi! ,(cl :x) x y :scal? nil :numx? t))
     y))

(defgeneric div (x y)
  (:documentation "
  Syntax
  ======
  (div! alpha x)

  Purpose
  =======
  X ./ alpha

  Yes the calling order is twisted.
")
  (:method ((alpha number) (x number))
    (/ x alpha))
  (:method (alpha (x dense-tensor))
    (div! alpha (copy x (when (complexp alpha) (complexified-tensor x)))))
  ;;TODO: There is an issue here when x is not coerceable into the tensor class of alpha
  (:method ((alpha dense-tensor) (x t))
    (div! alpha (copy! x (zeros (dimensions alpha)
				(if (or (complexp x) (subtypep (field-type (class-of alpha)) 'complex))
				    (complexified-tensor alpha)
				    (class-of alpha)))))))

;;Diagonal scaling.
(closer-mop:defgeneric scald! (x m &optional axis)
  (:method :before ((x dense-tensor) (m dense-tensor) &optional (axis 0))
     (declare (type index-type axis))
     (assert (and (tensor-vectorp x) (= (dimensions x 0) (dimensions m axis))) nil 'tensor-dimension-mismatch))
  (:generic-function-class tensor-method-generator))

(define-tensor-method scald! ((x dense-tensor :x) (m dense-tensor :m t) &optional (axis 0))
  `(let-typed ((sto-m (store m) :type ,(store-type (cl :m)))
	       (sto-x (store x) :type ,(store-type (cl :x)))
	       (axis (modproj axis (order m))))
     (very-quickly
       (iter (for-mod idx from 0 below (dimensions m) with-iterator ((:stride ((of-m (strides m) (head m))
									       (of-x (let ((tmp (t/store-allocator index-store-vector (order m))))
										       (setf (aref tmp axis) (strides x 0)) tmp)
										     (head x))))))
	     (setf (t/store-ref ,(cl :m) sto-m of-m)
		   (t/f* ,(field-type (cl :m))
			 (t/strict-coerce (,(field-type (cl :x)) ,(field-type (cl :m))) (t/store-ref ,(cl :x) sto-x of-x))
			 (t/store-ref ,(cl :m) sto-m of-m))))))
  'm)
