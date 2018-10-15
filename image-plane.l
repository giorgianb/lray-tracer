(defpackage "IMAGE-PLANE"
  (:use "COMMON-LISP" "PLANE" "SURFACE")
  (:export "IMAGE-PLANE" "WITH-IMAGE-PLANE"))

(defstruct (image-plane-base (:include plane)))
(defstruct (image-plane (:include surface)))

(defmacro with-image-plane (vars p &body body)
  (if (/= (length vars) 4)
      (error "Variable specification must specify 4 variables."))
  (let ((p-sym (gensym)))
    `(let* ((,p-sym ,p)
	    (,(first vars) (plane-p1 ,p-sym))
	    (,(second vars) (plane-p2 ,p-sym))
	    (,(third vars) (plane-p3 ,p-sym))
	    (,(fourth vars) (surface-color ,p-sym)))
       ,@body)))
