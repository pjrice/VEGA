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
;(run-single '("BrowardBallot-refactor" nil nil nil nil nil) nil nil t) ; contains instructions
;(run-single '("WisconsinBallot-Instruction-refactor" nil nil nil nil nil) nil nil t)
;(run-single '("WisconsinBallot-Instruction-Long-refactor" nil nil nil nil nil) nil nil t)
;(run-single '("ballot_instructions_1-refactor" nil nil nil nil nil) nil nil t)
;(run-single '("ballot_instructions_2-refactor" nil nil nil nil nil) nil nil t)
;(run-single '("ballot_instructions_2_nofooter-refactor" nil nil nil nil nil) nil nil t)
;(run-single '("ballot_instructions_3-refactor" nil nil nil nil nil) nil nil t)
;(run-single '("ballot_instructions_3_nofooter-refactor" nil nil nil nil nil) nil nil t)
;(run-single '("ballot_instructions_4-refactor" nil nil nil nil nil) nil nil t)
;(run-single '("ballot_instructions_2_plus-refactor" nil nil nil nil nil) nil nil t)
;(run-single '("ballot_instructions_4_a1-refactor" nil nil nil nil nil) nil nil t)
;(run-single '("ballot_instructions_4_a2-refactor" nil nil nil nil nil) nil nil t)
;(run-single '("ballot_instructions_4_a4-refactor" nil nil nil nil nil) nil nil t)
;(run-single '("ballot_instructions_4_plus-refactor" nil nil nil nil nil) nil nil t)
;(run-single '("ballot_instructions_4_plus_nofooter-refactor" nil nil nil nil nil) nil nil t)

; realtime use-model visible dolog
;(vote nil nil t nil)

;;;;;;;
; Ballots without instructions
;;;;;;;

; No-Lines-Color-Box

; fails after casting first vote; move-mouse-to-bubble clears the visual buffer; click-bubble fires; find-race-title-box does not fire because visual location buffer is empty
; with fix to move-mouse-to-bubble, it finishes ballot
(run-single '("No-Lines-Color-Box-refactor" "all-perfect" "Top-To-Bottom-Left-To-Right-Box" "Relative-Positions-Color" "VG-Random-Retrieve-Party" "click-closest-modified") nil t t)

; works with move-mouse-to-bubble fix
(run-single '("No-Lines-Color-Box-refactor" "all-perfect" "Left-To-Right-Top-To-Bottom-Box" "Relative-Positions-Color" "VG-Random-Retrieve-Party" "click-closest-modified") nil t t)

; works with move-mouse-to-bubble fix
(run-single '("No-Lines-Color-Box-refactor" "all-perfect" "Snake-Box" "Relative-Positions-Color" "VG-Random-Retrieve-Party" "click-closest-modified") nil t t)


;WisconsinBallot-Box

; works with move-mouse-to-bubble fix
(run-single '("WisconsinBallot-Box-refactor" "all-perfect" "Top-To-Bottom-Left-To-Right-Box" "Relative-Positions-Color" "VG-Random-Retrieve-Party" "click-closest-modified") nil t t)

; generally works, but seems to miss Commissioner of Agriculture and Presiding Judge Texas contests?
(run-single '("WisconsinBallot-Box-refactor" "all-perfect" "Left-To-Right-Top-To-Bottom-Box" "Relative-Positions-Color" "VG-Random-Retrieve-Party" "click-closest-modified") nil t t)

; generally works, but seems to miss Commissioner of Agriculture and Presiding Judge Texas contests? (same behavior as Left-To-Right-Top-To-Bottom-Box)
(run-single '("WisconsinBallot-Box-refactor" "all-perfect" "Snake-Box" "Relative-Positions-Color" "VG-Random-Retrieve-Party" "click-closest-modified") nil t t)


;;;;;;;
; Ballots with instructions
;;;;;;;

(run-single '("BrowardBallot-refactor" "all-perfect" "DownEachColumn-Box-Instructions-1" "Relative-Positions-Color" "VG-Random-Retrieve-Party" "click-closest-modified") nil t t)


;;; fails without voting: productions fired: find-header; attend-header; find-header-first-line-1; attending-header-first-line. THEN, retrieving-header-first-line SHOULD fire, but it doesn't exist in the productions list - it is not defined when model is loaded!!!
(run-single '("WisconsinBallot-Instruction-Long-refactor" "all-perfect" "DownEachColumn-Box-Instructions-1" "Relative-Positions-Color" "VG-Serial-Retrieve-Party" "click-closest-modified") t t t)
