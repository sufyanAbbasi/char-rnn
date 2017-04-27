(in-package #:matlisp)

(defun loadtxt (fname &key (type *default-tensor-type*) (format :csv) (delimiters '(#\Space #\Tab #\,)) comments (newlines '(#\Newline #\;)) (skip-rows 0))
  (let* ((f-string (file->string fname))
	 (*read-default-float-format* 'double-float))
    (multiple-value-bind (lns nrows) (split-seq #'(lambda (x) (member x newlines)) f-string)
      (setf nrows (+ nrows 1 (- skip-rows))
	    lns (nthcdr skip-rows lns))
      (unless (null lns)
	(let* ((ncols (1+ (nth-value 1 (split-seq #'(lambda (x) (member x delimiters)) (car lns)))))
	       (ret (zeros (if (> ncols 1) (list nrows ncols) (list nrows)) type)))
	  (if (> ncols 1)
	      (loop :for line :in lns
		 :for i := 0 :then (1+ i)
		 :do (loop :for num :in (split-seq #'(lambda (x) (member x delimiters)) line)
			:for j := 0 :then (1+ j)
			:do (setf (ref ret i j) (read-from-string num))))
	      (loop :for line :in lns
		 :for i := 0 :then (1+ i)
		 :do (setf (ref ret i) (read-from-string (car (split-seq #'(lambda (x) (member x delimiters)) line))))))
	  ret)))))

(defun savetxt (fname mat &key (delimiter #\Tab) (newline #\Newline))
  (with-open-file (out fname :direction :output :if-exists :supersede :if-does-not-exist :create)
    (cond
      ((tensor-matrixp mat)
       (let ((ncols (ncols mat)))
	 (loop :for i :from 0 :below (nrows mat)
	    :do (loop :for j :from 0 :below ncols
		   :do (format out "~a~a" (ref mat i j) (if (= j (1- ncols)) newline delimiter))))))
      ((tensor-vectorp mat)
       (loop :for i :from 0 :below (aref (dimensions mat) 0)
	  :do (format out "~a~a" (ref mat i) newline)))
      (t
       (let ((dims (dimensions mat))
	     (strd (strides mat)))
	 (format out ":head~a~a~a" delimiter (head mat) newline)
	 (format out ":dimensions~a" delimiter)
	 (loop :for i :from 0 :below (length dims)
	    :do (format out "~a~a" (aref dims i) (if (= i (1- (length dims))) newline delimiter)))
	 (format out ":strides~a" delimiter)
	 (loop :for i :from 0 :below (length dims)
	    :do (format out "~a~a" (aref strd i) (if (= i (1- (length dims))) newline delimiter)))
	 (let ((sto (store mat)))
	   (loop :for i :from 0 :below (length sto)
	      :do (format out "~a~a" (aref sto i) (if (= i (1- (length sto))) newline delimiter)))))))
    nil))

;;
;; (definline parse-integer! (astring)
;;   (declare (type (vector base-char) astring))
;;   (very-quickly
;;     (iter (for i from 0 below (length astring)) (with ret = 0) (with pl = 1) (declare (type index-type ret pl i))
;; 	  (incf ret (the index-type (* pl (the index-type (- (char-code (vector-pop astring)) #.(char-code #\0)))))) (setf pl (the index-type (* 10 pl)))
;; 	  (finally (return ret)))))

;; (definline read-number (str stack)
;;   (setf (fill-pointer stack) 0)
;;   (iter (for cc next (read-char str)) (with decimal? = nil)
;; 	(when (char= cc #\.) (setf decimal? t))
;; 	(if (not (or (char= cc #\|) (char= cc #\,) (char= cc #\Newline)))
;; 	    (vector-push-extend cc stack)
;; 	    (return (values (if decimal? (read-from-string stack) (parse-integer! stack)) cc)))))

;; (defun warm-up (data)
;;   (with-open-file (out data)
;;     (let ((user-table (make-hash-table))
;; 	  (feat-table (make-hash-table)))
;;       (iter (for data next (handler-case (read-line out)
;; 			     (end-of-file () (terminate))))
;; 	    (let ((split (split-seq #'(λ (x) (char= x #\|)) data)))
;; 	      (setf (gethash (parse-integer (nth 3 split)) user-table) t)
;; 	      (mapcar #'(λ (x) (setf (gethash (parse-integer x) feat-table) t)) (split-seq #'(λ (x) (char= x #\,)) (ref split -1)))
;; 	      (summing (read-from-string (first split)) into sum)
;; 	      (counting t into n))
;; 	    (finally (return (values (list sum n) (hash-table-count feat-table) (hash-table-count user-table))))))))

;;     (let ((pre (letv* ((spt n (split-seq #'(lambda (x) (member x '(#\Space #\Return #\Tab))) (read-line fs))))
;; 		 (assert (and (= n 4) (string= (first spt) "%%MatrixMarket")) nil "invalid header")
;; 		 (mapcar #'read-from-string (cdr spt)))))
;;       pre
;;       )

(defun loadmtx (fname)
  (let ((delims '(#\Space #\Return #\Tab)))
    (with-open-file (fs fname)
      (read-line fs)
      (letv* ((dims (mapcar #'read-from-string (split-seq #'(lambda (x) (member x delims)) (read-line fs))))
	      (mtx (zeros (subseq dims 0 2) '(double-float stride-accessor hash-table) (third dims)))
	      (*read-default-float-format* 'double-float))
	(iter (for data next (handler-case (read-line fs) (end-of-file () (return mtx))))
	      (let ((line (mapcar #'read-from-string (split-seq #'(lambda (x) (member x delims)) data))))
		(setf (apply #'ref (list* mtx (mapcar #'1- (butlast line)))) (third line))))))))

  ;; (multiple-value-bind (lns nrows) (split-seq #'(lambda (x) (member x newlines)) f-string)
  ;;     (loop :for 
  ;;     (unless (null lns)
  ;; 	(let* ((ncols (second (multiple-value-list (split-seq #'(lambda (x) (member x delimiters)) (car lns)))))
  ;; 	       (ret (zeros (if (> ncols 1) (list nrows ncols) (list nrows)) 'real-tensor)))
  ;; 	  (if (> ncols 1)
  ;; 	      (loop :for line :in lns
  ;; 		 :for i := 0 :then (1+ i)
  ;; 		 :do (loop :for num :in (split-seq #'(lambda (x) (member x delimiters)) line)
  ;; 			:for j := 0 :then (1+ j)
  ;; 			:do (setf (ref ret i j) (t/coerce (t/field-type real-tensor) (read-from-string num)))))
  ;; 	      (loop :for line :in lns
  ;; 		 :for i := 0 :then (1+ i)
  ;; 		 :do (setf (ref ret i) (t/coerce (t/field-type real-tensor) (read-from-string (car (split-seq #'(lambda (x) (member x delimiters)) line)))))))
  ;; 	  ret)))))
