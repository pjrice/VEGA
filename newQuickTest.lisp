(cond ((string= (software-type) "Darwin") 
        (progn
            (setf *default-pathname-defaults* #P"/Users/pjr5/")
            (load (merge-pathnames "Desktop/actr7.x/load-act-r.lisp"))
        )
      )
	  ((string= (software-type) "Linux") 
        (progn
            (setf *default-pathname-defaults* #P"/home/ausmanpa/")
            (load (merge-pathnames "actr7.x/load-act-r.lisp"))
        )
      )
)


(load (merge-pathnames "gp/VEGA/combine.lisp"))

(run-environment)



;(run-single '("No-Lines-Color-refactor" nil nil nil nil nil) nil nil t)
;(run-single '("No-Lines-Color-Box-refactor" nil nil nil nil nil) nil nil t)
;(run-single '("WisconsinBallot-Box-refactor" nil nil nil nil nil) nil nil t)
;(run-single '("BrowardBallot-refactor" nil nil nil nil nil) nil nil t)
;(run-single '("WisconsinBallot-Instruction-refactor" nil nil nil nil nil) nil nil t)
;(run-single '("WisconsinBallot-Instruction-Long-refactor" nil nil nil nil nil) nil nil t)
;(run-single '("ballot_instructions_1-refactor" nil nil nil nil nil) nil nil t)
;(run-single '("ballot_instructions_2-refactor" nil nil nil nil nil) nil nil t)
;(run-single '("ballot_instructions_2_nofooter-refactor" nil nil nil nil nil) nil nil t)
;(run-single '("ballot_instructions_3-refactor" nil nil nil nil nil) nil nil t)
;(run-single '("ballot_instructions_3_nofooter-refactor" nil nil nil nil nil) nil nil t)
(run-single '("ballot_instructions_4-refactor" nil nil nil nil nil) nil nil t)


; realtime use-model visible dolog
(vote nil nil t nil)


(run-single '("No-Lines-Color-Box-refactor" "all-perfect" "Top-To-Bottom-Left-To-Right-Box" "Relative-Positions-Color" "VG-Serial-Retrieve-Party" "click-closest-modified") t t t)
