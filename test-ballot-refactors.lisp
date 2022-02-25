(setf *default-pathname-defaults* #P"/home/ausmanpa/")

(load (merge-pathnames "actr7.x/load-act-r.lisp"))

(load (merge-pathnames "Desktop/A-Voting-Folder/combine.lisp"))

(run-environment)

(run-single '("No-Lines-Color-Box" nil nil nil nil nil) t nil t)

(vote nil nil t nil)
