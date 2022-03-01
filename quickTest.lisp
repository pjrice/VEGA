(setf *default-pathname-defaults* #P"/home/ausmanpa/")

(load (merge-pathnames "actr7.x/load-act-r.lisp"))

(load (merge-pathnames "gp/VEGA/combine.lisp"))

(run-environment)



(run-single '("No-Lines-Color-refactor" nil nil nil nil nil) nil nil t)

;(run-single '("No-Lines-Color-Box-refactor" nil nil nil nil nil) nil nil t)

; realtime use-model visible dolog
(vote nil nil t nil)


(run-single '("No-Lines-Color-Box-refactor" "all-perfect" "Top-To-Bottom-Left-To-Right-Box" "Relative-Positions-Color" "VG-Serial-Retrieve-Party" "click-closest-modified") t t t)
