(defpackage "RAY-TRACER"
  (:use "COMMON-LISP" "3D-IMAGES" "3D")
  (:export "TRACER"))

(in-package ray-tracer)

(defun tracer (world eye pathname &optional (res 1))
  (declare (list world) (3d-point eye) (integer res))
  (with-open-file (p pathname :direction :output :if-exists :supersede)
    (format p "P2 ~A ~A 255" (* res 100) (* res 100))
    (let ((inc (/ res)))
      (do ((y -50 (+ y inc)))
	  ((< (- 50 y) inc))
	(do ((x -50 (+ x inc)))
	    ((< (- 50 x) inc))
	  (print (color-at world eye x y) p))))))

(defun color-at (world eye x y)
  (declare (list world) (3d-point eye) (number x y))
  (with-3d-point (ex ey ez) eye
    (round (* (send-ray world eye (unit-vector (make-instance '3d-vector
							      :x (- x ex)
							      :y (- y ey)
							      :z (- ez))))
	      256))))

(defun send-ray (world pt v)
  (declare (list world) (3d-point pt) (3d-vector v))
  (multiple-value-bind (s hit) (first-hit world pt v)
    (if s
	(* (light-intensity s hit v) (surface-color s))
	0)))

(defun first-hit (world pt v)
  (declare (list world) (3d-point pt) (3d-vector v))
  (let (surface hit dist)
    (dolist (o world)
      (let ((h (intersect o pt v)))
	(when h
	  (let ((d (distance h pt)))
	    (when (or (null dist) (< d dist))
	      (setf surface o hit h dist d))))))
    (values surface hit)))


(defun light-intensity (o pt v)
  (declare (3d-point pt) (3d-vector v))
  (max 0 (dot-product (normal o pt) v)))
