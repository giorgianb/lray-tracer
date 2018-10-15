(defun sq (x) (* x x))

(defun mag (x y z)
  (sqrt (+ (sq x) (sq y) (sq z))))

(defun unit-vector (x y z)
  (let ((d (mag x y z)))
    (values (/ x d) (/ y d) (/ z d))))

(defstruct (point (:conc-name nil))
  x y z)

(defun distance (p1 p2)
  (mag (- (x p1) (x p2))
       (- (y p1) (y p2))
       (- (z p1) (z p2))))

(defun minroot (a b c)
  (if (zerop a)
      (/ (- c) b)
    (let ((disc (- (sq b) (* 4 a c))))
      (unless (minusp disc)
	(let ((discrt (sqrt disc)))
	  (min (/ (+ (- b) discrt) (* 2 a))
	       (/ (- (- b) discrt) (* 2 a))))))))

(defstruct surface color)

(defparameter *world* nil)
(defconstant eye (make-point :x 0 :y 0 :z 200))

(defun tracer (pathname &optional (res 1))
  (with-open-file (p pathname :direction :output)
		  (format p "P2 ~A ~A 255" (* res 100) (* res 100))
		  (let ((inc (/ res)))
		    (do ((y -50 (+ y inc)))
			((< (- 50 y) inc))
			(do ((x -50 (+ x inc)))
			    ((< (- 50 x) inc))
			    (print (color-at x y) p))))))

(defun color-at (x y)
  (multiple-value-bind (xr yr zr)
		       (unit-vector (- x (x eye))
				    (- y (y eye))
				    (- 0 (z eye)))
		       (round (* (sendray eye xr yr zr) 256))))

(defun sendray (pt xr yr zr)
  (multiple-value-bind (s int) (first-hit pt xr yr zr)
		       (if s
			   (* (lambert s int xr yr zr) (surface-color s))
			 0)))

(defun first-hit (pt xr yr zr)
  (let (surface hit dist)
    (dolist (s *world*)
      (let ((h (intersect s pt xr yr zr)))
	(when h
	  (let ((d (distance h pt)))
	    (when (or (null dist) (< d dist))
	      (setf surface s hit h dist d))))))
    (values surface hit)))

(defun lambert (s int xr yr zr)
  (multiple-value-bind (xn yn zn) (normal s int)
		       (max 0 (+ (* xr xn) (* yr yn) (* zr zn)))))

(defstruct (sphere (:include surface))
  radius center)

(defun defsphere (x y z r c)
  (let ((s (make-sphere
	    :radius r
	    :center (make-point :x x :y y :z z)
	    :color c)))
    (push s *world*)
    s))

(defun intersect (s pt xr yr zr)
  (funcall (typecase s
		     (sphere #'sphere-intersect)
		     (cube #'cube-intersect))
	   s pt xr yr zr))

(defun sphere-intersect (s pt xr yr zr)
  (let* ((c (sphere-center s))
	 (n (minroot (+ (sq xr) (sq yr) (sq zr))
		     (* 2 (+ (* (- (x pt) (x c)) xr)
			     (* (- (y pt) (y c)) yr)
			     (* (- (z pt) (z c)) zr)))
		     (+ (sq (- (x pt) (x c)))
			(sq (- (y pt) (y c)))
			(sq (- (z pt) (z c)))
			(- (sq (sphere-radius s)))))))
    (if n
	(make-point :x (+ (x pt) (* n xr))
		    :y (+ (y pt) (* n yr))
		    :z (+ (z pt) (* n zr))))))

(defun normal (s pt)
  (funcall (typecase s
		     (sphere #'sphere-normal)
		     (cube #'cube-normal))
	   s pt))

(defun sphere-normal (s pt)
  (let ((c (sphere-center s)))
    (unit-vector (- (x c) (x pt))
		 (- (y c) (y pt))
		 (- (z c) (z pt)))))

(defun ray-test (&optional (res 1))
  (setf *world* nil)
;  (defsphere 0 -300 -1200 200 .8) 
;  (defsphere -80 -150 -1200 200 .7)
;  (defsphere 70 -100 -1200 200 .9)
  (defcube 70 -100 -1200 200 .9)
  (do ((x -2 (1+ x)))
      ((> x 2))
      (do ((z 2 (1+ z)))
	  ((> z 7))
;	  (defsphere (* x 200) 300 (* z -400) 40 .75)
	  (defcube (* x 200) 300 (* z -400) 60 .75)))
  (tracer (make-pathname :name "pic.pgm") res))

(defstruct (cube (:include surface))
  side-length center)

(defun defcube (x y z s c)
  (let ((cube (make-cube
	       :side-length s
	       :center (make-point :x x :y y :z z)
	       :color c)))
    (push cube *world*)))

(defun cube-intersect (c pt xr yr zr)
  (let (points)
    (dolist (face (list #'cube-bottom-intersect
			#'cube-top-intersect
			#'cube-left-intersect
			#'cube-right-intersect
			#'cube-front-intersect
			#'cube-back-intersect)
		  (closest-to pt points))
      (let ((p (funcall face c pt xr yr zr)))
	(if (and p (not (cube-edge-point-p c p pt)))
	    (push p points))))))


(defun cube-normal (cube pt)
  (multiple-value-bind (x-min x-max y-min y-max z-min z-max) (cube-bounds cube)
		       (cond ((= (x pt) x-min) (unit-vector 1 0 0))
			     ((= (x pt) x-max) (unit-vector -1 0 0))
			     ((= (y pt) y-min) (unit-vector 0 1 0))
			     ((= (y pt) y-max) (unit-vector 0 -1 0))
			     ((= (z pt) z-min) (unit-vector 0 0 1))
			     ((= (z pt) z-max) (unit-vector 0 0 -1)))))

				
(defun cube-bounds (cube &optional (origin (make-point :x 0 :y 0 :z 0)))
  (let ((off (/ (cube-side-length cube) 2))
	(c (cube-center cube)))
    (values (- (x c) off (x origin)) (+ (x c) (- off (x origin)))
	    (- (y c) off (y origin)) (+ (y c) (- off (y origin)))
	    (- (z c) off (z origin)) (+ (z c) (- off (z origin))))))


(defun cube-left-intersect (c pt xr yr zr)
  (unless (zerop xr)
    (multiple-value-bind (x-min x-max y-min y-max z-min z-max) (cube-bounds c pt)
			 (let ((scale (/ x-min xr)))
			   (if (and (<= y-min (* yr scale) y-max)
				    (<= z-min (* zr scale) z-max))
			       (make-point :x (+ x-min (x pt)) :y (+ (* yr scale) (y pt)) :z (+ (* zr scale) (z pt))))))))

(defun cube-right-intersect (c pt xr yr zr)
  (unless (zerop xr)
    (multiple-value-bind (x-min x-max y-min y-max z-min z-max) (cube-bounds c pt)
			 (let ((scale (/ x-max xr)))
			   (if (and (<= y-min (* yr scale) y-max)
				    (<= z-min (* zr scale) z-max))
			       (make-point :x (+ x-max (x pt)) :y (+ (* yr scale) (y pt)) :z (+ (* zr scale) (z pt))))))))

(defun cube-bottom-intersect (c pt xr yr zr)
  (unless (zerop yr)
    (multiple-value-bind (x-min x-max y-min y-max z-min z-max) (cube-bounds c pt)
			 (let ((scale (/ y-min yr)))
			   (if (and (<= x-min (* xr scale) x-max)
				    (<= z-min (* zr scale) z-max))
			       (make-point :x (+ (* xr scale) (x pt)) :y (+ y-min (y pt)) :z (+ (* zr scale) (z pt))))))))

(defun cube-top-intersect (c pt xr yr zr)
  (unless (zerop yr)
    (multiple-value-bind (x-min x-max y-min y-max z-min z-max) (cube-bounds c pt)
			 (let ((scale (/ y-max yr)))
			   (if (and (<= x-min (* xr scale) x-max)
				    (<= z-min (* zr scale) z-max))
			       (make-point :x (+ (* xr scale) (x pt)) :y (+ y-max (y pt)) :z (+ (* zr scale) (z pt))))))))

(defun cube-front-intersect (c pt xr yr zr)
  (unless (zerop zr)
    (multiple-value-bind (x-min x-max y-min y-max z-min z-max) (cube-bounds c pt)
			 (let ((scale (/ z-min zr)))
			   (if (and (<= x-min (* xr scale) x-max)
				    (<= y-min (* yr scale) y-max))
			       (make-point :x (+ (* xr scale) (x pt)) :y (+ (* yr scale) (y pt)) :z (+ z-min (z pt))))))))

(defun cube-back-intersect (c pt xr yr zr)
  (unless (zerop zr)
    (multiple-value-bind (x-min x-max y-min y-max z-min z-max) (cube-bounds c pt)
			 (let ((scale (/ z-max zr)))
			   (if (and (<= x-min (* xr scale) x-max)
				    (<= y-min (* yr scale) y-max))
			       (make-point :x (+ (* xr scale) (x pt)) :y (+ (* yr scale) (y pt)) :z (+ z-max (z pt))))))))

(defun closest-to (pt points)
  (let ((min (first points)))
    (dolist (point (rest points) min)
      (if (< (distance point pt) (distance min pt))
	  (setf min point)))))

(defun cube-edge-point-p (c pt origin)
  (multiple-value-bind (x-min x-max y-min y-max z-min z-max) (cube-bounds c origin)
		       (let ((x (x pt))
			     (y (y pt))
			     (z (z pt)))
			 (or (and (= x x-min) (= y y-min))
			     (and (= x x-min) (= y y-max))
			     (and (= x x-min) (= z z-min))
			     (and (= x x-min) (= z z-max))
			     (and (= x x-max) (= y y-min))
			     (and (= x x-max) (= y y-max))
			     (and (= x x-max) (= z z-min))
			     (and (= x x-max) (= z z-max))
			     (and (= y y-min) (= z z-min))
			     (and (= y y-min) (= z z-max))
			     (and (= y y-max) (= z z-min))
			     (and (= y y-max) (= z z-min))))))
