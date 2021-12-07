;;------------------------------------- set default pathname -------------------------------------
(setf *default-pathname-defaults* #P"/home/ausmanpa/")

;;------------------------------------- load in ACT-R -------------------------------------
(load (merge-pathnames "Desktop/ACTR_testing/actr7.x/load-act-r.lisp" *load-truename*))

;; ------------load JKL's visual grouping system (not needed this if the VG file is in the actr7/user-load)
;(load (merge-pathnames "actr7/user-loads/visual-grouping-modified.lisp" *load-truename*))

;;------------------------------------- load functions file -------------------------------------
(load (merge-pathnames "gp/VEGA/combine.lisp" *load-truename*))

;; ------------------------------------- load ACT-R environment -------------------------------------
(run-environment)

;;--------------------------------------- load ballot --------------------------------------------
;; vertically aligned ballots
(run-single '("No-Lines-Color" nil nil nil nil nil) t nil t)
(run-single '("No-Lines-Color-Box" "all-perfect" nil nil nil nil) nil nil t)

;; vertically misaligned ballots, random generated
(run-single '("NLC-Offset-Random" nil nil nil nil nil) nil nil t)
(run-single '("NLC-Offset-Random-Box" nil nil nil nil nil) nil nil t)

;; wisconsin ballot
(run-single '("WisconsinBallot-Box" nil nil nil nil nil) nil nil t)


;; ------------------------------------- test a single model -------------------------------------
;;top-to-bottom-left-to-right
(run-single '("No-Lines-Color-Box" "all-perfect" "Top-To-Bottom-Left-To-Right" "Relative-Positions-Color" "VG-Serial-Retrieve-Party" "click-closest-modified") nil t t)
(run-single '("WisconsinBallot-Box" "all-perfect" "Top-To-Bottom-Left-To-Right-Box" "Relative-Positions-Color" "VG-Serial-Recognize-Party" "click-closest-modified") nil t t)

;; left-to-right-top-to-bottom
(run-single '("No-Lines-Color-Box" "all-perfect" "Left-To-Right-Top-To-Bottom-Box" "Relative-Positions-Color" "VG-Serial-Retrieve-Party" "click-closest-modified") nil t t)


;;---------------------------------------- run simulation -------------------------------------
;; used to test random generated ballots; put only one file into each of the 1-6 folders
(test-ranges)

;; used to run every single possible combination of strategies and log them all into a single file
(run-lists 100 nil)
