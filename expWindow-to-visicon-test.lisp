(load "/home/ausmanpa/actr7.x/load-act-r.lisp")

(run-environment)

(define-model dummyModel (sgp :force-visual-commands t))

(defvar *window*)


(setf *window* (open-exp-window "test" :x 0 :y 0 :width 500 :height 500))

(install-device *window*)


(add-line-to-exp-window *window* (list 0 0) (list 500 500))
