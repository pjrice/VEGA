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

(define-model dummy (sgp :force-visual-commands t))
(chunk-type (group (:include visual-object)) (group t))


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
; From Xianni's test2.lisp file
;;;;;;;

; fails; gets stuck trying to find the header
(run-single '("BrowardBallot-refactor" "all-perfect-broward" "DownEachColumn-Box-Instructions-2" "Relative-Positions-Color" "VG-Random-Recognize-Party" "click-closest-modified")nil t t)

; when trying to find the instructions, attends to first race instead and gets stuck
(run-single '("WisconsinBallot-Instruction-Long-refactor" "all-perfect-longwisconsin" "DownEachColumn-Box-Instructions-2" "Relative-Positions-Color" "VG-Serial-Recognize-Party" "click-closest-modified")t t t)



;;----------------------------------------  test non-race content ---------------------------------------

;down-each-column (version A: cursory strategies) (1:basic down-each-column  2:snake;   3:middle,right,left   4:middle,left,right)

;top-instruction ballot
; this works!
(run-single '("ballot_instructions_4_plus-refactor" "all-perfect-longwisconsin" "DownEachColumn-Box-Instructions-A1" "Relative-Positions-Color" "VG-Serial-Recognize-Party" "click-closest-modified")nil t t)

; fails; find-loc-failure in encode-race production
(run-single '("ballot_instructions_4_plus-refactor" "all-perfect-longwisconsin" "DownEachColumn-Box-Instructions-A2" "Relative-Positions-Color" "VG-Serial-Recognize-Party" "click-closest-modified")nil t t)

;left-instruction ballot
; this works!
(run-single '("ballot_instructions_2_plus-refactor" "all-perfect-longwisconsin" "DownEachColumn-Box-Instructions-A1" "Relative-Positions-Color" "VG-Serial-Recognize-Party" "click-closest-modified")nil t t)

; fails; find-loc-failure in encode-race production
(run-single '("ballot_instructions_2_plus-refactor" "all-perfect-longwisconsin" "DownEachColumn-Box-Instructions-A2" "Relative-Positions-Color" "VG-Serial-Recognize-Party" "click-closest-modified")nil t t)

; this works!
(run-single '("ballot_instructions_2_plus-refactor" "all-perfect-longwisconsin" "DownEachColumn-Box-Instructions-A3" "Relative-Positions-Color" "VG-Serial-Recognize-Party" "click-closest-modified")nil t t)

; this works!
(run-single '("ballot_instructions_2_plus-refactor" "all-perfect-longwisconsin" "DownEachColumn-Box-Instructions-A4" "Relative-Positions-Color" "VG-Serial-Recognize-Party" "click-closest-modified")nil t t)

; this works; did last column from bottom up?
(run-single '("ballot_instructions_1-refactor" "all-perfect-longwisconsin" "DownEachColumn-Box-Instructions-A2" "Relative-Positions-Color" "VG-Serial-Recognize-Party" "click-closest-modified")nil t t)


;ballot with no footer
; this works
(run-single '("ballot_instructions_4_plus_nofooter-refactor" "all-perfect-longwisconsin" "DownEachColumn-Box-Instructions-A1" "Relative-Positions-Color" "VG-Serial-Recognize-Party" "click-closest-modified")nil t t)

; this works
(run-single '("ballot_instructions_2_nofooter-refactor" "all-perfect-longwisconsin" "DownEachColumn-Box-Instructions-A3" "Relative-Positions-Color" "VG-Serial-Recognize-Party" "click-closest-modified")nil t t)

; ;------down-each-column (version B: thorough strategies)  (1:basic down-each-column  2:snake;   3:middle,right,left   4:middle,left,right) ------ ; ;

;top-instruction ballot
; this works
(run-single '("ballot_instructions_4_plus-refactor" "all-perfect-longwisconsin" "DownEachColumn-Box-Instructions-B1" "Relative-Positions-Color" "VG-Serial-Recognize-Party" "click-closest-modified")nil t t)

; fails; find-loc-failure in encode-race production
(run-single '("ballot_instructions_4_plus-refactor" "all-perfect-longwisconsin" "DownEachColumn-Box-Instructions-B2" "Relative-Positions-Color" "VG-Serial-Recognize-Party" "click-closest-modified")nil t t)

;left-instruction ballot
; this works
(run-single '("ballot_instructions_2_plus-refactor" "all-perfect-longwisconsin" "DownEachColumn-Box-Instructions-B1" "Relative-Positions-Color" "VG-Serial-Recognize-Party" "click-closest-modified")nil t t)

; fails; find-loc-failure in encode-race production
(run-single '("ballot_instructions_2_plus-refactor" "all-perfect-longwisconsin" "DownEachColumn-Box-Instructions-B2" "Relative-Positions-Color" "VG-Serial-Recognize-Party" "click-closest-modified")nil t t)

; this works; did last column from bottom up?
(run-single '("ballot_instructions_1-refactor" "all-perfect-longwisconsin" "DownEachColumn-Box-Instructions-B2" "Relative-Positions-Color" "VG-Serial-Recognize-Party" "click-closest-modified")nil t t)

; works; vote on first column last
(run-single '("ballot_instructions_2_plus-refactor" "all-perfect-longwisconsin" "DownEachColumn-Box-Instructions-B3" "Relative-Positions-Color" "VG-Serial-Recognize-Party" "click-closest-modified")nil t t)

;ballot with no footer
; this works
(run-single '("ballot_instructions_4_plus_nofooter-refactor" "all-perfect-longwisconsin" "DownEachColumn-Box-Instructions-B1" "Relative-Positions-Color" "VG-Serial-Recognize-Party" "click-closest-modified")nil t t)

; works; vote on first column last
(run-single '("ballot_instructions_2_nofooter-refactor" "all-perfect-longwisconsin" "DownEachColumn-Box-Instructions-B3" "Relative-Positions-Color" "VG-Serial-Recognize-Party" "click-closest-modified")nil t t)


; ; ------reading order ------ ; ;

; votes on all contests, but then gets stuck in a loop switching attention between the last two races of the first column until the time limit is reached
(run-single '("ballot_instructions_2_plus-refactor" "all-perfect-longwisconsin" "ReadingOrder-Box-Instructions" "Relative-Positions-Color" "VG-Serial-Recognize-Party" "click-closest-modified")nil t t)

; this works; skipped last race in second column
(run-single '("ballot_instructions_4_plus-refactor" "all-perfect-longwisconsin" "ReadingOrder-Box-Instructions" "Relative-Positions-Color" "VG-Serial-Recognize-Party" "click-closest-modified")nil t t)

;no footer
; this works
(run-single '("ballot_instructions_4_plus_nofooter-refactor" "all-perfect-longwisconsin" "ReadingOrder-Box-Instructions" "Relative-Positions-Color" "VG-Serial-Recognize-Party" "click-closest-modified")nil t t)

; ; ------snake order ------ ; ;
; this works
(run-single '("ballot_instructions_2_plus-refactor" "all-perfect-longwisconsin" "Snake-Box-Instructions" "Relative-Positions-Color" "VG-Serial-Recognize-Party" "click-closest-modified")nil t t)

; this works
(run-single '("ballot_instructions_4_plus-refactor" "all-perfect-longwisconsin" "Snake-Box-Instructions" "Relative-Positions-Color" "VG-Serial-Recognize-Party" "click-closest-modified")nil t t)

;no footer
; this works; skipped last race on both the second and third columns
(run-single '("ballot_instructions_4_plus_nofooter-refactor" "all-perfect-longwisconsin" "Snake-Box-Instructions" "Relative-Positions-Color" "VG-Serial-Recognize-Party" "click-closest-modified")nil t t)

; ------------------------------------------
;what if there is no footer on the ballot?

; these all fail because the macro strategies look for a footer, but each ballot specifically doesn't have one

; fails; switches attention between the last races of the three columns until time limit
(run-single '("ballot_instructions_3_nofooter-refactor" "all-perfect-longwisconsin" "DownEachColumn-Box-Instructions-1" "Relative-Positions-Color" "VG-Serial-Recognize-Party" "click-closest-modified")nil t t)

; fails; switches attention between the last races of the three columns until time limit
(run-single '("ballot_instructions_2_nofooter-refactor" "all-perfect-longwisconsin" "DownEachColumn-Box-Instructions-1" "Relative-Positions-Color" "VG-Serial-Recognize-Party" "click-closest-modified")nil t t)

; fails; switches attention between the last races of the three columns until time limit
(run-single '("ballot_instructions_3_nofooter-refactor" "all-perfect-longwisconsin" "DownEachColumn-Box-Instructions-3" "Relative-Positions-Color" "VG-Serial-Recognize-Party" "click-closest-modified")nil t t)

; fails; switches attention between the last races of the three columns until time limit
(run-single '("ballot_instructions_2_nofooter-refactor" "all-perfect-longwisconsin" "DownEachColumn-Box-Instructions-3" "Relative-Positions-Color" "VG-Serial-Recognize-Party" "click-closest-modified")nil t t)

; fails; switches attention between the last races of the three columns until time limit
(run-single '("ballot_instructions_2_nofooter-refactor" "all-perfect-longwisconsin" "DownEachColumn-Box-Instructions-2" "Relative-Positions-Color" "VG-Serial-Recognize-Party" "click-closest-modified")nil t t)

; fails; switches attention between the last races of the three columns until time limit
(run-single '("ballot_instructions_2_nofooter-refactor" "all-perfect-longwisconsin" "ReadingOrder-Box-Instructions" "Relative-Positions-Color" "VG-Serial-Recognize-Party" "click-closest-modified")nil t t)

; fails; switches attention between the last races of the three columns until time limit
(run-single '("ballot_instructions_3_nofooter-refactor" "all-perfect-longwisconsin" "ReadingOrder-Box-Instructions" "Relative-Positions-Color" "VG-Serial-Recognize-Party" "click-closest-modified")nil t t)

; fails; switches attention between the last races of the three columns until time limit
(run-single '("ballot_instructions_2_nofooter-refactor" "all-perfect-longwisconsin" "Snake-Box-Instructions" "Relative-Positions-Color" "VG-Serial-Recognize-Party" "click-closest-modified")nil t t)

; fails; switches attention between the last races of the three columns until time limit
(run-single '("ballot_instructions_3_nofooter-refactor" "all-perfect-longwisconsin" "Snake-Box-Instructions" "Relative-Positions-Color" "VG-Serial-Recognize-Party" "click-closest-modified")nil t t)


;;----------------------------------------  testing ballot with only races --------
;;top-to-bottom-left-to-right
; no Top-To-Bottom-Left-To-Right; replaced with Top-To-Bottom-Left-To-Right-Box, works
(run-single '("No-Lines-Color-Box-refactor" "all-perfect" "Top-To-Bottom-Left-To-Right-Box" "Relative-Positions-Color" "VG-Serial-Retrieve-Party" "click-closest-modified") nil t t)

; works
(run-single '("WisconsinBallot-Box-refactor" "all-perfect-wisconsin" "Top-To-Bottom-Left-To-Right-Box" "Relative-Positions-Color" "VG-Serial-Retrieve-Party" "click-closest-modified") nil t t)

;; left-to-right-top-to-bottom
; works
(run-single '("WisconsinBallot-Box-refactor" "all-perfect-wisconsin" "Left-To-Right-Top-To-Bottom-Box" "Relative-Positions-Color" "VG-Serial-Retrieve-Party" "click-closest-modified") nil t t)

; haven't refactored NLC-Offset-Random-Box
(run-single '("NLC-Offset-Random-Box" "all-perfect-broward" "Top-To-Bottom-Left-To-Right-Box" "Relative-Positions-Color" "VG-Random-Recognize-Party" "click-closest-modified") nil t t)

; works
(run-single '("WisconsinBallot-Box-refactor" "all-perfect-wisconsin" "Left-To-Right-Top-To-Bottom-Box" "Relative-Positions-Color" "VG-Random-Recognize-Party" "click-closest-modified") nil t t)

; works
(run-single '("WisconsinBallot-Box-refactor" "all-perfect-wisconsin" "Snake-Box" "Relative-Positions-Color" "VG-Random-Recognize-Party" "click-closest-modified") nil t t)

;;snake
; haven't refactored NLC-Offset-Random-Box-Long
(run-single '("NLC-Offset-Random-Box-Long" "all-perfect" "Snake-Box" "Relative-Positions-Color" "VG-Random-Recognize-Party" "click-closest-modified") nil t t)




























