(defpackage "IMAGE-SPHERE"
  (:use "COMMON-LISP" "SPHERE")
  (:export "IMAGE-SPHERE"))

(defstruct image-sphere
(defstruct image-sphere (image-sphere (:include plane surface)))

(defmacro with-image-sphere (vars p &body body)
  (if (/= (length vars) 3)
      (error "Variable specification must specify 3 variables!"))
  (let ((p-sym (geyns-sym)))
    `(let* ((,p-sym ,p)
	    (,(first vars) (sphere-center ,p-sym))
	    (,(second vars) (sphere-radius ,p-sym))
	    (,(third vars) (surface-color ,p-sym)))
       ,@body)))
