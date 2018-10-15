(defpackage "3D-IMAGES"
  (:use "COMMON-LISP" "3D")
  (:export "SURFACE" "SURFACE-COLOR" "SPHERE-IMAGE" "PLANE-IMAGE" "DEFOBJECT"
	   "DEFSPHERE" "DEFPLANE"))
   
(in-package 3d-images)
(defclass surface ()
  ((color :type integer :reader surface-color :initarg :color)))

(defclass sphere-image (sphere surface)
  ())

(defclass plane-image (plane surface)
  ())

(defmacro defobject (obj world)
  `(push ,obj ,world))

(defmacro defsphere (x y z r c w)
  (let ((s-sym (gensym)))
    `(let ((,s-sym (make-instance 'sphere-image
				  :center (make-instance '3d-point
							 :x ,x
							 :y ,y
							 :z ,z)
				  :radius ,r
				  :color ,c)))
       (push ,s-sym ,w))))

(defmacro defplane (x1 y1 z1 x2 y2 z2 x3 y3 z3 c w)
  (let ((p-sym (gensym)))
    `(let ((,p-sym (make-instance 'plane-image
				  :p1 (make-instance '3d-point
						     :x ,x1
						     :y ,y1
						     :z ,z1)
				  :p2 (make-instance '3d-point
						     :x ,x2
						     :y ,y2
						     :z ,z2)
				  :p3 (make-instance '3d-point
						     :x ,x3
						     :y ,y3
						     :z ,z3)
				  :color ,c)))
       (push ,p-sym ,w))))
