(defpackage "RT-POINT"
  (:use "COMMON-LISP")
  (:export "POINT"
	   "MAKE"
	   "NTH-COMPONENT"
	   "DIMENSIONS"
	   "WITH"
	   "*ORIGIN*"
	   "EQUIV"))
 
(in-package rt-point)

(defclass point ()
  ((components :type array :reader components :initarg :components)))

(defun make (points)
  (declare (list points))
  (make-instance 'point
		 :components (make-array (length points) :initial-contents points :element-type 'real)))
		    

(defun nth-component (p n)
  (declare (point p) (integer n))
  (if (and (< n (length (components p))) (>= n 0))
      (aref (components p) n)
      0.0d0))

(defmacro dimensions (p)
  `(length (components ,p)))

(defmacro with (p vars &body body)
  (declare (list vars body))
  (let* ((p-sym (gensym))
	 (n 0)
	 (var-spec (mapcar #'(lambda (var)
			       (incf n)
			       `(,var (nth-component ,p-sym ,(1- n))))
			   vars)))
    `(let* ((,p-sym ,p)
	    ,@var-spec)
       (declare (point p))
       ,@(mapcar #'(lambda (var) `(declare (real ,var))) vars)
       ,@body)))

(defun equiv (p1 p2)
  (let* ((pc1 (components p1))
	 (pc2 (components p2))
	 (len1 (length pc1))
	 (len2 (length pc2))
	 (len (min len1 len2))
	 (equal t))
    (do ((i 0 (1+ i)))
	((or (not equal) (>= i len)) equal)
      (setf equal (= (aref pc1 i) (aref pc2 i))))))

(defparameter *origin* (make '()))
