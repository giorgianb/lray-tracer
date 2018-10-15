(defpackage "3D"
   (:use "COMMON-LISP")
   (:export "3D-VECTOR" "3D-VECTOR-X" "3D-VECTOR-Y" "3D-VECTOR-Z"
	    "WITH-3D-VECTOR" "MAGNITUDE" "UNIT-VECTOR"
	    "ADD" "SUBTRACT" "MULTIPLY" "DIVIDE"
	    "DOT-PRODUCT" "CROSS-PRODUCT" "ANGLE"
	    "3D-POINT" "3D-POINT-X" "3D-POINT-Y" "3D-POINT-Z"
	    "WITH-3D-POINT" "DISTANCE"
	    "PLANE" "PLANE-P1" "PLANE-P2" "PLANE-P3"
	    "WITH-PLANE" "INTERSECT" "NORMAL"
	    "SPHERE" "WITH-SPHERE" "SPHERE-CENTER"
	    "SPHERE-RADIUS"))


(in-package 3d)

(defclass 3d-vector ()
  ((x :type number :reader 3d-vector-x :initarg :x)
   (y :type number :reader 3d-vector-y :initarg :y)
   (z :type number :reader 3d-vector-z :initarg :z)))

(defmacro with-3d-vector (vars v &body body)
  (declare (list vars body))
  (if (/= (length vars) 3)
      (error "Variables specification must specify three variables."))
  (let ((v-sym (gensym)))
    `(let* ((,v-sym ,v)
	    (,(first vars) (3d-vector-x ,v-sym))
	    (,(second vars) (3d-vector-y ,v-sym))
	    (,(third vars) (3d-vector-z ,v-sym)))
       (declare (3d-vector ,v-sym))
       ,@body)))

(defmethod magnitude ((v 3d-vector))
  (sqrt (+ (expt (3d-vector-x v) 2)
	   (expt (3d-vector-y v) 2)
	   (expt (3d-vector-z v) 2))))

(defmethod unit-vector ((v 3d-vector))
  (let ((d (magnitude v)))
    (make-instance '3d-vector
		   :x (/ (3d-vector-x v) d)
		   :y (/ (3d-vector-y v) d)
		   :z (/ (3d-vector-z v) d))))

(defmethod add ((a 3d-vector) (b 3d-vector))
  (with-3d-vector (ax ay az) a
    (with-3d-vector (bx by bz) b
      (make-instance '3d-vector
		     :x (+ ax bx)
		     :y (+ ay by)
		     :z (+ az bz)))))

(defmethod subtract ((a 3d-vector) (b 3d-vector))
  (with-3d-vector (ax ay az) a
    (with-3d-vector (bx by bz) b
      (make-instance '3d-vector
		     :x (- ax bx)
		     :y (- ay by)
		     :z (- az bz)))))

(defmethod multiply ((v 3d-vector) (k number))
  (with-3d-vector (x y z) v
    (make-instance '3d-vector
		   :x (* x k)
		   :y (* y k)
		   :z (* z k))))

(defmethod divide ((v 3d-vector) (k number))
  (with-3d-vector (x y z) v
    (make-instance '3d-vector
		   :x (/ x k)
		   :y (/ y k)
		   :z (/ z k))))

(defmethod dot-product ((a 3d-vector) (b 3d-vector))
  (with-3d-vector (ax ay az) a
    (with-3d-vector (bx by bz) b
      (+ (* ax bx)
	 (* ay by)
	 (* az bz)))))

(defmethod cross-product ((a 3d-vector) (b 3d-vector))
  (with-3d-vector (ax ay az) a
    (with-3d-vector (bx by bz) b
      (make-instance '3d-vector
		     :x (- (* ay bz) (* az by))
		     :y (- (* az bx) (* ax bz))
		     :z (- (* ax by) (* ay bx))))))

(defmethod angle ((a 3d-vector) (b 3d-vector))
  (acos (/ (dot-product a b)
	   (* (magnitude a) (magnitude b)))))

(defclass 3d-point ()
  ((x :type number :reader 3d-point-x :initarg :x)
   (y :type number :reader 3d-point-y :initarg :y)
   (z :type number :reader 3d-point-z :initarg :z)))

(defmacro with-3d-point (vars p &body body)
  (declare (list vars body))
  (if (/= (length vars) 3)
      (error "Variable specification must specify 3 variables."))
  
  (let ((p-sym (gensym)))
    `(let* ((,p-sym ,p)
	    (,(first vars) (3d-point-x ,p-sym))
	    (,(second vars) (3d-point-y ,p-sym))
	    (,(third vars) (3d-point-z ,p-sym)))
       (declare (3d-point ,p-sym))
       ,@body)))

(defmethod distance ((a 3d-point) (b 3d-point))
  (with-3d-point (ax ay az) a
    (with-3d-point (bx by bz) b
      (sqrt (+ (expt (- ax bx) 2)
	       (expt (- ay by) 2)
	       (expt (- az bz) 2))))))

(defclass plane ()
  ((p1 :type 3d-point :reader plane-p1 :initarg :p1)
   (p2 :type 3d-point :reader plane-p2 :initarg :p2)
   (p3 :type 3d-point :reader plane-p3 :initarg :p3)))

(defmacro with-plane (vars p &body body)
  (declare (list vars body))
  (if (/= (length vars) 3)
      (error "Variable specification must specify 3 variables."))
  (let ((p-sym (gensym)))
    `(let* ((,p-sym ,p)
	    (,(first vars) (plane-p1 ,p-sym))
	    (,(second vars) (plane-p2 ,p-sym))
	    (,(third vars) (plane-p3 ,p-sym)))
       (declare (plane ,p-sym))
       ,@body)))

(defun 3d-point-to-3d-vector (p)
  (declare (3d-point p))
  (with-3d-point (x y z) p
    (make-instance '3d-vector :x x :y y :z z)))

(defmethod intersect ((p plane) (vp 3d-point) (v 3d-vector))
  (let ((n (normal p nil)))
    (unless (zerop (dot-product n v))
      (let ((i (solve-plane-intersect (plane-p1 p) n vp v)))
	(make-instance '3d-point
		       :x (+ (3d-point-x vp) (* (3d-vector-x v) i))
		       :y (+ (3d-point-y vp) (* (3d-vector-y v) i))
		       :z (+ (3d-point-z vp) (* (3d-vector-z v) i)))))))

(defmethod normal ((p plane) pt)
  (declare (ignore pt))
  (with-plane (p1 p2 p3) p
    (let ((a (3d-point-to-3d-vector p1))
	  (b (3d-point-to-3d-vector p2))
	  (c (3d-point-to-3d-vector p3)))
      (unit-vector (cross-product (subtract a b)
				  (subtract a c))))))


(defun solve-plane-intersect (a n o v)
  (declare (3d-point a o) (3d-vector n v))
  (let ((ax (3d-point-x a))
	(ay (3d-point-y a))
	(az (3d-point-z a))
	(nx (3d-vector-x n))
	(ny (3d-vector-y n))
	(nz (3d-vector-z n))
	(ox (3d-point-x o))
	(oy (3d-point-y o))
	(oz (3d-point-z o))
	(dx (3d-vector-x v))
	(dy (3d-vector-y v))
	(dz (3d-vector-z v)))
    (/ (- (+ (* nx ax) (* ny ay) (* nz az))
	  (+ (* nx ox) (* ny oy) (* nz oz)))
       (+ (* nx dx) (* ny dy) (* nz dz)))))

(defclass sphere ()
  ((center :type 3d-point :reader sphere-center :initarg :center)
   (radius :type number :reader sphere-radius :initarg :radius)))

(defmacro with-sphere (vars s &body body)
  (declare (list vars body))
  (if (/= (length vars) 2)
      (error "Variable specification must specify 2 variables!"))
  (let ((s-sym (gensym)))
    `(let* ((,s-sym ,s)
	    (,(first vars) (sphere-center ,s-sym))
	    (,(second vars) (sphere-radius ,s-sym)))
       (declare (sphere ,s-sym))
       ,@body)))

(defmethod intersect ((s sphere) (vp 3d-point) (v 3d-vector))
  (with-sphere (c r) s
    (with-3d-vector (x y z) v
      (let ((n (min-root (+ (expt x 2) (expt y 2) (expt z 2))
			 (* 2 (+ (* (- (3d-point-x vp) (3d-point-x c)) x)
				 (* (- (3d-point-y vp) (3d-point-y c)) y)
				 (* (- (3d-point-z vp) (3d-point-z c)) z)))
			 (+ (expt (- (3d-point-x vp) (3d-point-x c)) 2)
			    (expt (- (3d-point-y vp) (3d-point-y c)) 2)
			    (expt (- (3d-point-z vp) (3d-point-z c)) 2)
			    (- (expt r 2))))))
	(if n
	    (make-instance '3d-point
			   :x (+ (3d-point-x vp) (* n x))
			   :y (+ (3d-point-y vp) (* n y))
			   :z (+ (3d-point-z vp) (* n z))))))))

(defmethod normal ((s sphere) (pt 3d-point))
  (with-sphere (c r) s
    (declare (ignore r))
    (unit-vector (make-instance '3d-vector
				:x (- (3d-point-x c) (3d-point-x pt))
				:y (- (3d-point-y c) (3d-point-y pt))
				:z (- (3d-point-z c) (3d-point-z pt))))))

(defun min-root (a b c)
  (declare (number a b c))
  (if (zerop a)
      (/ (- c) b)
      (let ((disc (- (expt b 2) (* 4 a c))))
	(unless (minusp disc)
	  (let ((discrt (sqrt disc)))
	    (min (/ (+ (- b) discrt) (* 2 a))
		 (/ (- (- b) discrt) (* 2 a))))))))
