(setf *default-pathname-defaults* #P"/home/ausmanpa/")

(load (merge-pathnames "actr7.x/load-act-r.lisp"))

(load (merge-pathnames "Desktop/A-Voting-Folder/combine.lisp"))

(run-environment)



(run-single '("No-Lines-Color" nil nil nil nil nil) nil nil t)

(run-single '("No-Lines-Color-Box" nil nil nil nil nil) nil nil t)



(run-single '("No-Lines-Color-Box" "all-perfect" "Top-To-Bottom-Left-To-Right-Box" "Relative-Positions-Color" "VG-Serial-Retrieve-Party" "click-closest-modified") t t t)
