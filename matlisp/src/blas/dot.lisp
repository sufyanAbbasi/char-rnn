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

(deft/generic (t/dot #'subtypep) sym (x y &optional conjp num-y?))

(deft/method t/dot (sym dense-tensor) (x y &optional (conjp t) (num-y? nil))
  (using-gensyms (decl (x y) (sto-x sto-y of-x of-y stp-x stp-y dot))
    `(let (,@decl)
       (declare (type ,sym ,x ,@(unless num-y? `(,y)))
		,@(when num-y? `((type ,(field-type sym) ,y))))
       (let ((,sto-x (store ,x))
	     (,stp-x (aref (the index-store-vector (strides ,x)) 0))
	     (,of-x (head ,x))
	     ,@(unless num-y?
		       `((,sto-y (store ,y))
			 (,stp-y (aref (the index-store-vector (strides ,y)) 0))
			 (,of-y (head ,y))))
	     (,dot (t/fid+ ,(field-type sym))))
	 (declare (type ,(store-type sym) ,sto-x ,@(unless `(,sto-y)))
		  (type index-type ,stp-x ,of-x ,@(unless num-y? `(,stp-y ,of-y)))
		  (type ,(field-type sym)))
	 (very-quickly
	   (loop :repeat (aref (the index-store-vector (dimensions ,x)) 0)
	      :do (setf ,dot (t/f+ ,(field-type sym) ,dot
				   (t/f* ,(field-type sym)
					 ,(recursive-append (when conjp `(t/fc ,(field-type sym))) `(t/store-ref ,sym ,sto-x ,of-x))
					 ,(if num-y? y `(t/store-ref ,sym ,sto-y ,of-y))))
			,of-x (+ ,of-x ,stp-x)
			,@(unless num-y? `(,of-y (+ ,of-y ,stp-y))))))
	 ,dot))))

;;---------------------------------------------------------------;;
(closer-mop:defgeneric dot (x y &optional conjugate-p)
  (:documentation
   "
  Sytnax
  ======
  (DOT x y [conjugate-p])

  Purpose
  =======
  Computes the inner product of X,Y.

  CONJUGATE-P       Computed Result
  ---------------------------------
                         H
  T (default)           X * Y
                         T
  NIL                   X * Y


  If X is real then CONJUGATE-P has no 
  effect since for real vectors:

                H   T
               X = X

  If X and Y are both scalars then this is the same
  as (* (CONJUGATE X) Y) if CONJUAGTE-P and (* X Y)
  otherwise.
")
  (:method :before ((x tensor) (y tensor) &optional (conjugate-p t))
    (declare (ignore conjugate-p))
    (assert (and (tensor-vectorp x) (tensor-vectorp y) (= (dimensions x 0) (dimensions y 0))) nil 'tensor-dimension-mismatch))
  (:generic-function-class tensor-method-generator))

(closer-mop:defmethod dot ((x number) (y number) &optional (conjugate-p t))
  (if conjugate-p
      (* (cl:conjugate (the number x)) y)
      (* x y)))

(define-tensor-method dot ((x dense-tensor :x) (y dense-tensor :x) &optional (conjugate-p t))
  `(if conjugate-p
       (t/dot ,(cl :x) x y t)
       (t/dot ,(cl :x) x y nil)))

(define-tensor-method dot ((x dense-tensor :x) (y t) &optional (conjugate-p t))
  `(let ((y (t/coerce ,(field-type (cl :x)) y)))
     (declare (type ,(field-type (cl :x)) y))
     (if conjugate-p	 
	 (t/dot ,(cl :x) x y t t)
	 (t/dot ,(cl :x) x y nil t))))
