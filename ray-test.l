(defpackage "RAY-TEST"
  (:use "COMMON-LISP" "RAY-TRACER" "3D-IMAGES" "3D")
  (:export "RAY-TEST"))

(in-package ray-test)

(defun ray-test (&optional (res 1))
  (declare (integer res))
  (let (world
	(eye (make-instance '3d-point
			   :x 0
			   :y 0
			   :z 200)))
    (defsphere 0 -300 -1200 200 .8 world)
    (defsphere -80 -150 -1200 200 .7 world)
    (defsphere 70 -100 -1200 200 .9 world)
    (defplane -400 300 -600
	      -400 300 -1200
	      -200 300 -600
	      .9 world)
    (do ((x -2 (1+ x)))
	((> x 2))
      (do ((z 2 (1+ z)))
	  ((> z 7))
	(defsphere (* x 200) 300 (* z -300) 40 .75 world)))
    (tracer world eye (make-pathname :name "test.pgm") res)))

