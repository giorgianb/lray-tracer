(defstruct planer-object
  planes)

(defun vector-cross-product (x1r y1r z1r x2r y2r z2r)
  (values (- (* y1r z2r) (* z1r y2r))
	  (- (* z1r x2r) (* x1r z2r))
	  (- (* x1r y2r) (* y1r x2r))))

(defun plane-normal-vector (p)
  (


(defun plane-intersect (p pt xr yr zr)
  (multiple-value-bind (xn yn zn) (vector-cross-product
  
