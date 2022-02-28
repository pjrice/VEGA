(setf *default-pathname-defaults* #P"/Users/pjr5/")

(load (merge-pathnames "Desktop/actr7.x/load-act-r.lisp"))

(load (merge-pathnames "Desktop/A-Voting-Folder/combine.lisp"))

(run-environment)




(run-single '("No-Lines-Color" nil nil nil nil nil) t nil t)

(run-single '("No-Lines-Color-Box" nil nil nil nil nil) t nil t)




(vote nil nil t nil)