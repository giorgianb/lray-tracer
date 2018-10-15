(defpackage "SURFACE"
  (:use "COMMON-LISP")
  (:export "SURFACE" "WITH-SURFACE"))

(defstruct surface
  color)

(defmacro with-surface (vars p &body body)
  (if (/= (length vars) 1)
      (error "Variable specification must specify 1 variables."))
  (let ((p-sym (gensym)))
    `(let* ((,p-sym ,p)
	    (,(first vars) (surface-color ,p-sym)))
       ,@body)))
