;(defpackage "RT-VEC"
;  (:use "COMMON-LISP")
;  (:require "RT-POINT")
;  (:export "VEC"
;	   "MAKE-VEC"
;	   "VEC-NTH-COMPONENT"
;	   "VEC-DIMENSIONS"
;	   "WITH-VEC"))

;(in-package rt-vec)

(defclass vec ()
  ((point-1 :type rt-point:point :reader point-1 :initarg :point-1)
   (point-2 :type rt-point:point :reader point-2 :initarg :point-2)))

(defun make (p1 p2)
  (declare (rt-point:point p1) (rt-point:point p2))
  (make-instance 'vec
		 :point-1 p1
		 :point-2 p2))

(defun nth-component (v n)
  (declare (vec v) (integer n))
  (- (rt-point:nth-component (point-2 v) n)
     (rt-point:nth-component (point-1 v) n)))

(defun dimensions (v)
  (max (rt-point:dimensions (point-1 v)) (rt-point:dimensions (point-2 v))))

(defun magnitude (v)
  (declare (vec v))
  (let ((d (dimensions v)))
    (flet ((squared-diff (i) (expt (- (rt-point:nth-component (point-1 v) i)
				      (rt-point:nth-component (point-2 v) i))
				   2)))
      (do* ((i 0 (1+ i))
	    (m (squared-diff i) (+ m (squared-diff i))))
	   ((>= i d) (sqrt m))))))

(defun add (v1 v2 &optional center)
  (declare (vec v1) (vec v2))
  


  


