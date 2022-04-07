;;------------------------------------- load in ACT-R -------------------------------------
(load (merge-pathnames "actr7/load-act-r.lisp" *load-truename*))

;; ------------load JKL's visual grouping system (not needed this if the VG file is in the actr7/user-load)
;(load (merge-pathnames "actr7/user-loads/visual-grouping-modified.lisp" *load-truename*))

;;------------------------------------- load functions file -------------------------------------
(load (merge-pathnames "combine.lisp" *load-truename*))

;; ------------------------------------- load ACT-R environment -------------------------------------
(run-environment)

;;--------------------------------------- load a ballot --------------------------------------------
;; vertically aligned ballots
(run-single '("No-Lines-Color" nil nil nil nil nil) t nil t)
(run-single '("No-Lines-Color-Box" "all-perfect" nil nil nil nil) nil nil t)

;; vertically misaligned ballots, random generated
(run-single '("NLC-Offset-Random-Box" nil nil nil nil nil) nil nil t)
(run-single '("NLC-Offset-Random-Box-Long" nil nil nil nil nil) nil nil t)

;; wisconsin ballot
(run-single '("WisconsinBallot-Box" nil nil nil nil nil) nil nil t)
(run-single '("WisconsinBallot-Instruction-Long" nil nil nil nil nil) nil nil t)

;; broward county ballot (with instructions)
(run-single '("BrowardBallot" nil nil nil nil nil) nil nil t)

;;ballot with instructions (1&2 left-instr ballots, 3&4 top-instr ballots)
(run-single '("ballot_instructions_1" nil nil nil nil nil) nil nil t)
(run-single '("ballot_instructions_2" nil nil nil nil nil) nil nil t)
(run-single '("ballot_instructions_3" nil nil nil nil nil) nil nil t)
(run-single '("ballot_instructions_4" nil nil nil nil nil) nil nil t)

;;ballot with more non-race content
(run-single '("ballot_instructions_4_a1" nil nil nil nil nil) nil nil t)
(run-single '("ballot_instructions_4_a2" nil nil nil nil nil) nil nil t)
(run-single '("ballot_instructions_4_a4" nil nil nil nil nil) nil nil t)

(run-single '("ballot_instructions_4_plus" nil nil nil nil nil) nil nil t)
(run-single '("ballot_instructions_2_plus" nil nil nil nil nil) nil nil t)

;;ballot without footer
(run-single '("ballot_instructions_3_nofooter" nil nil nil nil nil) nil nil t)
(run-single '("ballot_instructions_2_nofooter" nil nil nil nil nil) nil nil t)
(run-single '("ballot_instructions_4_plus_nofooter" nil nil nil nil nil) nil nil t)

;; ------------------------------------- test a single model -------------------------------------
;instructions
;(run-single '("WisconsinBallot-Instruction-Long" "all-perfect-wisconsin" "DownEachColumn-Box-Instructions" "Relative-Positions-Color" "VG-Random-Recognize-Party" "click-closest-modified") nil t t)
(run-single '("BrowardBallot" "all-perfect-broward" "DownEachColumn-Box-Instructions2" "Relative-Positions-Color" "VG-Random-Recognize-Party" "click-closest-modified")nil t t)
;serial, wisconsin
(run-single '("WisconsinBallot-Instruction-Long" "all-perfect-longwisconsin" "DownEachColumn-Box-Instructions2" "Relative-Positions-Color" "VG-Serial-Recognize-Party" "click-closest-modified")t t t)

; ----------------------------------------------------------
;reading order strategy (basic)
(run-single '("ballot_instructions_1" "all-perfect-longwisconsin" "ReadingOrder-Box-Instructions" "Relative-Positions-Color" "VG-Serial-Recognize-Party" "click-closest-modified")nil t t)
(run-single '("ballot_instructions_2" "all-perfect-longwisconsin" "ReadingOrder-Box-Instructions" "Relative-Positions-Color" "VG-Serial-Recognize-Party" "click-closest-modified")nil t t)
(run-single '("ballot_instructions_3" "all-perfect-longwisconsin" "ReadingOrder-Box-Instructions" "Relative-Positions-Color" "VG-Serial-Recognize-Party" "click-closest-modified")nil t t)
(run-single '("ballot_instructions_4" "all-perfect-longwisconsin" "ReadingOrder-Box-Instructions" "Relative-Positions-Color" "VG-Serial-Recognize-Party" "click-closest-modified")nil t t)

;snake order strategy (basic)
(run-single '("ballot_instructions_1" "all-perfect-longwisconsin" "Snake-Box-Instructions" "Relative-Positions-Color" "VG-Serial-Recognize-Party" "click-closest-modified")nil t t)
(run-single '("ballot_instructions_2" "all-perfect-longwisconsin" "Snake-Box-Instructions" "Relative-Positions-Color" "VG-Serial-Recognize-Party" "click-closest-modified")nil t t)
(run-single '("ballot_instructions_3" "all-perfect-longwisconsin" "Snake-Box-Instructions" "Relative-Positions-Color" "VG-Serial-Recognize-Party" "click-closest-modified")nil t t)
(run-single '("ballot_instructions_4" "all-perfect-longwisconsin" "Snake-Box-Instructions" "Relative-Positions-Color" "VG-Serial-Recognize-Party" "click-closest-modified")nil t t)

;test snake strategy with shorter ballot
(run-single '("ballot_instructions_33" nil nil nil nil nil) nil nil t)
(run-single '("ballot_instructions_33" "all-perfect-longwisconsin" "Snake-Box-Instructions" "Relative-Positions-Color" "VG-Serial-Recognize-Party" "click-closest-modified")nil t t)

(run-single '("NLC-Offset-Random-Box" nil nil nil nil nil) nil nil t)



;;----------------------------------------  test non-race content ---------------------------------------

;down-each-column (version A: cursory strategies) (1:basic down-each-column  2:snake;   3:middle,right,left   4:middle,left,right)
;top-instruction ballot
(run-single '("ballot_instructions_4_plus" "all-perfect-longwisconsin" "DownEachColumn-Box-Instructions-A1" "Relative-Positions-Color" "VG-Serial-Recognize-Party" "click-closest-modified")nil t t)
(run-single '("ballot_instructions_4_plus" "all-perfect-longwisconsin" "DownEachColumn-Box-Instructions-A2" "Relative-Positions-Color" "VG-Serial-Recognize-Party" "click-closest-modified")nil t t)
;left-instruction ballot
(run-single '("ballot_instructions_2_plus" "all-perfect-longwisconsin" "DownEachColumn-Box-Instructions-A1" "Relative-Positions-Color" "VG-Serial-Recognize-Party" "click-closest-modified")nil t t)
(run-single '("ballot_instructions_2_plus" "all-perfect-longwisconsin" "DownEachColumn-Box-Instructions-A2" "Relative-Positions-Color" "VG-Serial-Recognize-Party" "click-closest-modified")nil t t)
(run-single '("ballot_instructions_2_plus" "all-perfect-longwisconsin" "DownEachColumn-Box-Instructions-A3" "Relative-Positions-Color" "VG-Serial-Recognize-Party" "click-closest-modified")nil t t)
(run-single '("ballot_instructions_2_plus" "all-perfect-longwisconsin" "DownEachColumn-Box-Instructions-A4" "Relative-Positions-Color" "VG-Serial-Recognize-Party" "click-closest-modified")nil t t)

(run-single '("ballot_instructions_1" "all-perfect-longwisconsin" "DownEachColumn-Box-Instructions-A2" "Relative-Positions-Color" "VG-Serial-Recognize-Party" "click-closest-modified")nil t t)

;ballot with no footer
(run-single '("ballot_instructions_4_plus_nofooter" "all-perfect-longwisconsin" "DownEachColumn-Box-Instructions-A1" "Relative-Positions-Color" "VG-Serial-Recognize-Party" "click-closest-modified")nil t t)
(run-single '("ballot_instructions_2_nofooter" "all-perfect-longwisconsin" "DownEachColumn-Box-Instructions-A3" "Relative-Positions-Color" "VG-Serial-Recognize-Party" "click-closest-modified")nil t t)

; ;------down-each-column (version B: thorough strategies)  (1:basic down-each-column  2:snake;   3:middle,right,left   4:middle,left,right) ------ ; ;
;top-instruction ballot
(run-single '("ballot_instructions_4_plus" "all-perfect-longwisconsin" "DownEachColumn-Box-Instructions-B1" "Relative-Positions-Color" "VG-Serial-Recognize-Party" "click-closest-modified")nil t t)
(run-single '("ballot_instructions_4_plus" "all-perfect-longwisconsin" "DownEachColumn-Box-Instructions-B2" "Relative-Positions-Color" "VG-Serial-Recognize-Party" "click-closest-modified")nil t t)
;left-instruction ballot
(run-single '("ballot_instructions_2_plus" "all-perfect-longwisconsin" "DownEachColumn-Box-Instructions-B1" "Relative-Positions-Color" "VG-Serial-Recognize-Party" "click-closest-modified")nil t t)
(run-single '("ballot_instructions_2_plus" "all-perfect-longwisconsin" "DownEachColumn-Box-Instructions-B2" "Relative-Positions-Color" "VG-Serial-Recognize-Party" "click-closest-modified")nil t t)
(run-single '("ballot_instructions_1" "all-perfect-longwisconsin" "DownEachColumn-Box-Instructions-B2" "Relative-Positions-Color" "VG-Serial-Recognize-Party" "click-closest-modified")nil t t)

(run-single '("ballot_instructions_2_plus" "all-perfect-longwisconsin" "DownEachColumn-Box-Instructions-B3" "Relative-Positions-Color" "VG-Serial-Recognize-Party" "click-closest-modified")nil t t)

;ballot with no footer
(run-single '("ballot_instructions_4_plus_nofooter" "all-perfect-longwisconsin" "DownEachColumn-Box-Instructions-B1" "Relative-Positions-Color" "VG-Serial-Recognize-Party" "click-closest-modified")nil t t)
(run-single '("ballot_instructions_2_nofooter" "all-perfect-longwisconsin" "DownEachColumn-Box-Instructions-B3" "Relative-Positions-Color" "VG-Serial-Recognize-Party" "click-closest-modified")nil t t)


; ; ------reading order ------ ; ;
(run-single '("ballot_instructions_2_plus" "all-perfect-longwisconsin" "ReadingOrder-Box-Instructions" "Relative-Positions-Color" "VG-Serial-Recognize-Party" "click-closest-modified")nil t t)
(run-single '("ballot_instructions_4_plus" "all-perfect-longwisconsin" "ReadingOrder-Box-Instructions" "Relative-Positions-Color" "VG-Serial-Recognize-Party" "click-closest-modified")nil t t)
;no footer
(run-single '("ballot_instructions_4_plus_nofooter" "all-perfect-longwisconsin" "ReadingOrder-Box-Instructions" "Relative-Positions-Color" "VG-Serial-Recognize-Party" "click-closest-modified")nil t t)

; ; ------snake order ------ ; ;
(run-single '("ballot_instructions_2_plus" "all-perfect-longwisconsin" "Snake-Box-Instructions" "Relative-Positions-Color" "VG-Serial-Recognize-Party" "click-closest-modified")nil t t)
(run-single '("ballot_instructions_4_plus" "all-perfect-longwisconsin" "Snake-Box-Instructions" "Relative-Positions-Color" "VG-Serial-Recognize-Party" "click-closest-modified")nil t t)
;no footer
(run-single '("ballot_instructions_4_plus_nofooter" "all-perfect-longwisconsin" "Snake-Box-Instructions" "Relative-Positions-Color" "VG-Serial-Recognize-Party" "click-closest-modified")nil t t)


; ------------------------------------------
;what if there is no footer on the ballot?
(run-single '("ballot_instructions_3_nofooter" "all-perfect-longwisconsin" "DownEachColumn-Box-Instructions-1" "Relative-Positions-Color" "VG-Serial-Recognize-Party" "click-closest-modified")nil t t)
(run-single '("ballot_instructions_2_nofooter" "all-perfect-longwisconsin" "DownEachColumn-Box-Instructions-1" "Relative-Positions-Color" "VG-Serial-Recognize-Party" "click-closest-modified")nil t t)

(run-single '("ballot_instructions_3_nofooter" "all-perfect-longwisconsin" "DownEachColumn-Box-Instructions-3" "Relative-Positions-Color" "VG-Serial-Recognize-Party" "click-closest-modified")nil t t)
(run-single '("ballot_instructions_2_nofooter" "all-perfect-longwisconsin" "DownEachColumn-Box-Instructions-3" "Relative-Positions-Color" "VG-Serial-Recognize-Party" "click-closest-modified")nil t t)

(run-single '("ballot_instructions_2_nofooter" "all-perfect-longwisconsin" "DownEachColumn-Box-Instructions-2" "Relative-Positions-Color" "VG-Serial-Recognize-Party" "click-closest-modified")nil t t)

(run-single '("ballot_instructions_2_nofooter" "all-perfect-longwisconsin" "ReadingOrder-Box-Instructions" "Relative-Positions-Color" "VG-Serial-Recognize-Party" "click-closest-modified")nil t t)
(run-single '("ballot_instructions_3_nofooter" "all-perfect-longwisconsin" "ReadingOrder-Box-Instructions" "Relative-Positions-Color" "VG-Serial-Recognize-Party" "click-closest-modified")nil t t)

(run-single '("ballot_instructions_2_nofooter" "all-perfect-longwisconsin" "Snake-Box-Instructions" "Relative-Positions-Color" "VG-Serial-Recognize-Party" "click-closest-modified")nil t t)
(run-single '("ballot_instructions_3_nofooter" "all-perfect-longwisconsin" "Snake-Box-Instructions" "Relative-Positions-Color" "VG-Serial-Recognize-Party" "click-closest-modified")nil t t)


;;----------------------------------------  testing ballot with only races --------
;;top-to-bottom-left-to-right
(run-single '("No-Lines-Color-Box" "all-perfect" "Top-To-Bottom-Left-To-Right" "Relative-Positions-Color" "VG-Serial-Retrieve-Party" "click-closest-modified") nil t t)
(run-single '("WisconsinBallot-Box" "all-perfect-wisconsin" "Top-To-Bottom-Left-To-Right-Box" "Relative-Positions-Color" "VG-Serial-Retrieve-Party" "click-closest-modified") nil t t)

;; left-to-right-top-to-bottom
(run-single '("WisconsinBallot-Box" "all-perfect-wisconsin" "Left-To-Right-Top-To-Bottom-Box" "Relative-Positions-Color" "VG-Serial-Retrieve-Party" "click-closest-modified") nil t t)
(run-single '("NLC-Offset-Random-Box" "all-perfect-broward" "Top-To-Bottom-Left-To-Right-Box" "Relative-Positions-Color" "VG-Random-Recognize-Party" "click-closest-modified") nil t t)

(run-single '("WisconsinBallot-Box" "all-perfect-wisconsin" "Left-To-Right-Top-To-Bottom-Box" "Relative-Positions-Color" "VG-Random-Recognize-Party" "click-closest-modified") nil t t)
(run-single '("WisconsinBallot-Box" "all-perfect-wisconsin" "Snake-Box" "Relative-Positions-Color" "VG-Random-Recognize-Party" "click-closest-modified") nil t t)

;;snake
(run-single '("NLC-Offset-Random-Box-Long" "all-perfect" "Snake-Box" "Relative-Positions-Color" "VG-Random-Recognize-Party" "click-closest-modified") nil t t)


;;---------------------------------------- run simulation -------------------------------------
;; used to test random generated ballots; put only one file into each of the 1-6 folders
(test-ranges)

;; used to run every single possible combination of strategies and log them all into a single file
(run-lists 100 nil)