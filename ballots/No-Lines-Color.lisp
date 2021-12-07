;;;  -*- mode: LISP; Syntax: COMMON-LISP;  Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Author      : Joshua Engels
;;; Copyright   : (c) 2019 Joshua Engels
;;; Address     : Lovett College
;;;             : Rice University
;;;             : Houston, TX 77005
;;;             : jae4@rice.edu
;;; 
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Filename    : No-Lines-Color.lisp
;;; Version     : 1
;;; 
;;; Description : A ballot
;;;				: * This file establishes an act-r window that represents a complete ballot with no lines seperating the races.
;;;
;;; Bugs        : * None known
;;;
;;; To do       : * The logging does not work if a button is unclicked (because there is no such method yet in logging) 
;;;				: * See logging.lisp todo for more info
;;; 
;;; ----- History -----
;;; 2019.9.27   Joshua Engels
;;;				: * Created the file
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; General Docs:
;;;
;;; Each race defined in the cntst-lst construction is sequentially put on the ballot from top to bottom and left to right. 
;;; The races are offset from each other with basically arbitrary x and y values (just what made it work), so care should be taken
;;; when writing new ballot functions using this as a model or when changing the races (i.e. adding more candidates would screw things up)
;;; Pressing the buttons causes a log candidate event to occur with the given candidate and other neccesary information (that's what the maps
;;; are for). The ballot is regularly laid out (with no noise), with candidates and parties and buttons sharing the same y value.
;;; Plus colors!
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; This first section of code defines the list of races: their names, candidates, and parties, and are used in the second section to construction
;; the ballot
(defparameter cntst-lst nil)

; A single race
(defclass contest ()
  ((office-name :accessor office-name :initarg :office-name :initform nil)

   (cand-lst :accessor cand-lst :initarg :cand-lst :initform nil)
   (selection :accessor selection :initarg :selection :initform nil)
   (office-field :accessor office-field :initarg :office-field)
	)
  )
  

; A single candidate  
(defclass cand-choice ()
  ((cand-name :accessor cand-name :initarg :cand-name :initform nil)
   (party-name :accessor party-name :initarg :party-name :initform "")
   (selected-p :accessor selected-p :initarg :selected-p :initform nil)
   (parent :accessor parent :initarg :parent :initform nil)
   (my-idx :accessor my-idx :initarg :my-idx :initform nil)
   ))

; The contest list
(setf cntst-lst
	  (list
	   
	   (make-instance 'contest
		 :office-name "PresidentoftheUnitedStates"
		 :cand-lst
		 (list
		  (make-instance 'cand-choice
			:cand-name "GordonBearce" :party-name "REP")
		  (make-instance 'cand-choice
			:cand-name "VernonStanleyAlbury" :party-name "DEM")
		  (make-instance 'cand-choice
			:cand-name "JanetteFroman" :party-name "LIB")))
	   
	   (make-instance 'contest
		 :office-name "UnitedStatesSenator"
		 :cand-lst
		 (list
		  (make-instance 'cand-choice
			:cand-name "CecileCadieux" :party-name "REP")
		  (make-instance 'cand-choice
			:cand-name "FernBrzezinski" :party-name "DEM")
		  (make-instance 'cand-choice
			:cand-name "CoreyDery" :party-name "IND")))
	   
	   (make-instance 'contest
		 :office-name "UnitedStatesRepresentativeDistrict7"
		 :cand-lst
		 (list
		  (make-instance 'cand-choice
			:cand-name "PedroBrouse" :party-name "REP")
		  (make-instance 'cand-choice
			:cand-name "RobertMettler" :party-name "DEM")))

	   (make-instance 'contest
		 :office-name "Governor"
		 :cand-lst
		 (list
		  (make-instance 'cand-choice
			:cand-name "GlenTravisLozier" :party-name "REP")
		  (make-instance 'cand-choice
			:cand-name "RickStickles" :party-name "DEM")
		  (make-instance 'cand-choice
			:cand-name "MauriceHumble" :party-name "IND")))

	   (make-instance 'contest
		 :office-name "LieutenantGovernor"
		 :cand-lst
		 (list
		  (make-instance 'cand-choice
			:cand-name "ShaneTerrio" :party-name "REP")
		  (make-instance 'cand-choice
			:cand-name "CassiePrincipe" :party-name "DEM")))

	   (make-instance 'contest
		 :office-name "AttorneyGeneral"
		 :cand-lst
		 (list
		  (make-instance 'cand-choice
			:cand-name "TimSpeight" :party-name "REP")
		  (make-instance 'cand-choice
			:cand-name "RickOrgan" :party-name "DEM")))

	   
	   (make-instance 'contest
		 :office-name "ComptrollerofPublicAccounts"
		 :cand-lst
		 (list
		  (make-instance 'cand-choice
			:cand-name "ThereseGustin" :party-name "IND")
		  (make-instance 'cand-choice
			:cand-name "GregConverse" :party-name "DEM")))

	   
	   (make-instance 'contest
		 :office-name "CommissionerofGeneralLandOffice"
		 :cand-lst
		 (list
		  (make-instance 'cand-choice
			:cand-name "SamSaddler" :party-name "REP")
		  (make-instance 'cand-choice
			:cand-name "EliseEllzey" :party-name "DEM")))

	   
	   (make-instance 'contest
		 :office-name "CommissionerofAgriculture"
		 :cand-lst
		 (list
		  (make-instance 'cand-choice
			:cand-name "PollyRylander" :party-name "REP")
		  (make-instance 'cand-choice
			:cand-name "RobertoAron" :party-name "DEM")))

	   
	   (make-instance 'contest
		 :office-name "RailroadCommissioner"
		 :cand-lst
		 (list
		  (make-instance 'cand-choice
			:cand-name "JillianBalas" :party-name "REP")
		  (make-instance 'cand-choice
			:cand-name "ZacharyMinick" :party-name "DEM")))

	   
	   (make-instance 'contest
		 :office-name "StateSenator"
		 :cand-lst
		 (list
		  (make-instance 'cand-choice
			:cand-name "RicardoNigro" :party-name "REP")
		  (make-instance 'cand-choice
			:cand-name "WesleyStevenMillette" :party-name "DEM")))

	   
	   (make-instance 'contest
		 :office-name "StateRepresentativeDistrict134"
		 :cand-lst
		 (list
		  (make-instance 'cand-choice
			:cand-name "PetraBencomo" :party-name "REP")
		  (make-instance 'cand-choice
			:cand-name "SusanneRael" :party-name "DEM")))

	   
	   (make-instance 'contest
		 :office-name "MemberStateBoardofEducationDistrict2"
		 :cand-lst
		 (list
		  (make-instance 'cand-choice
			:cand-name "PeterVarga" :party-name "REP")
		  (make-instance 'cand-choice
			:cand-name "MarkBaber" :party-name "DEM")))

	   
	   (make-instance 'contest
		 :office-name "PresidingJudgeTexasSupremeCourtPlace2"
		 :cand-lst
		 (list
		  (make-instance 'cand-choice
			:cand-name "TimGrasty" :party-name "DEM")))

	   
	   (make-instance 'contest
		 :office-name "PresidingJudgeCourtofCriminalAppeals"
		 :cand-lst
		 (list
		  (make-instance 'cand-choice
			:cand-name "DanPlouffe" :party-name "REP")
		  (make-instance 'cand-choice
			:cand-name "DerrickMelgar" :party-name "DEM")))

	   
	   (make-instance 'contest
		 :office-name "DistrictAttorney"
		 :cand-lst
		 (list
		  (make-instance 'cand-choice
			:cand-name "CoreyBehnke" :party-name "REP")
		  (make-instance 'cand-choice
			:cand-name "JenniferALundeed" :party-name "DEM")))

	   
	   (make-instance 'contest
		 :office-name "CountyTreasurer"
		 :cand-lst
		 (list
		  (make-instance 'cand-choice
			:cand-name "DeanCaffee" :party-name "REP")
		  (make-instance 'cand-choice
			:cand-name "GordonKallas" :party-name "DEM")))

	   
	   (make-instance 'contest
		 :office-name "Sheriff"
		 :cand-lst
		 (list
		  (make-instance 'cand-choice
			:cand-name "StanleySaari" :party-name "REP")
		  (make-instance 'cand-choice
			:cand-name "JasonValle" :party-name "DEM")))

	   
	   (make-instance 'contest
		 :office-name "CountyTaxAssessor"
		 :cand-lst
		 (list
		  (make-instance 'cand-choice
			:cand-name "HowardGrady" :party-name "REP")
		  (make-instance 'cand-choice
			:cand-name "RandyHClemons" :party-name "DEM")))

	   
	   (make-instance 'contest
		 :office-name "JusticeofthePeace"
		 :cand-lst
		 (list
		  (make-instance 'cand-choice
			:cand-name "DeborahKamps" :party-name "REP")
		  (make-instance 'cand-choice
			:cand-name "ClydeGaytonJr" :party-name "DEM")))

	   
	   (make-instance 'contest
		 :office-name "CountyJudge"
		 :cand-lst
		 (list
		  (make-instance 'cand-choice
			:cand-name "DanAtchley" :party-name "REP")
		  (make-instance 'cand-choice
			:cand-name "LewisShine" :party-name "DEM")))
	))


;; Gets a random number between 1 and n
(defun rand (n)
	(- (random (1+ (* 2 n))) n))

;; A ballot function that gets called by combine.lisp and that runs the model (or a human) on the constructed ballot
;; realtime is a boolean that specifies whether to run thge model in real time or not (only pertanent if use-model is true)
;; use-model is a boolean that specifies whether to use the model or allow just human interaction with the ballot
;; visible is a boolean that specifies whether the ballot window should be made visible or not
(defun vote (realtime use-model visible dolog)

  
	; Resets the act r enviroment
	(reset)
  
  
	; Constructs the window and populates it with race-titles, candidates, parties, and buttons
	(let* (
		(window (open-exp-window "Ballot" :x 50 :y 50 :width 900 :height 700 :visible visible)) 
		(starting-x 10) 
		(i 0) ; column number
		(j 0) ; row number
		
		; Maps the buttons to various objects so that these values can be accessed in the click response method, where we only have accessed
		; to the object itself
		(button-map (make-hash-table)) ; Maps the buttons to the array of candidate and party objects on screen (to use to change their color to blue)
		(button-state (make-hash-table)) ; Maps the buttons to their state (to know whether to set to blue or black)
		(button-index (make-hash-table)) ; Maps the button to their race's index in cntst-lst 
		(button-candidate (make-hash-table)) ; Maps the button to their associated candidate object from cntst-lst
		
		 ; Holdovers from constructing a ballot with noise. Can make a new ballot with the values increased to get a ballot with noise
		(noise 0)
		(noise_macro 0))
		
	
		; Places all of the races on the screen
		(loop
	
			(setf j 0) ; row reset to 0
			(setf starting-x (+ (* i 300) 10)) ; starting-x incremented (because it is a new column)
		
			
			(loop 	
				
				
				; Constructs the ballot
				(let* (

					; starting-y incremented (because it is a new row within the column)
					(starting-y (+ (* j 85) 10))

					; Again, a holdover. Randomx = startingx here
					(randomx (+ starting-x (rand noise_macro)))
					(randomy (+ starting-y (rand noise_macro)))
				
				
					; initialization to build the race
					(candidate-party-object-array (make-array '(6)))
					(contest (pop cntst-lst))
					(candidates (cand-lst contest))
					(candidate (pop candidates))
					(y-offset 20)
					(index 0)
					(button_temp nil)
					)					

					
					; Adds the race name
					(add-text-to-exp-window window (office-name contest) :color 'red :x randomx :y randomy)

				
					; Constructs the rest of the ballot
					(loop while candidate 
			
					do 	(progn 
						
						; Displays and stores candidates
						(setf (aref candidate-party-object-array index) (add-text-to-exp-window window (cand-name candidate) :color 'purple :x (+ randomx 30 (rand noise)) :y (+ randomy y-offset (rand noise))))
						
						; Displays and stores parties
						(setf (aref candidate-party-object-array (+ index 3)) (add-text-to-exp-window window (party-name candidate) :color 'blue :x (+ randomx 200 (rand noise)) :y (+ randomy y-offset (rand noise))))

						; Displays and stores buttons
						(setf button_temp (add-button-to-exp-window window :color 'white :text "" :x randomx :y (+ randomy y-offset 2) :width 20 :height 10 :action 
						(lambda (button)
						(if (= (gethash button button-state) 0) 
							(progn
								(modify-button-for-exp-window button :color 'black)
								; (modify-text-for-exp-window (aref (gethash button button-map) (gethash button button-index)) :color 'purple)
								; (modify-text-for-exp-window (aref (gethash button button-map) (+ (gethash button button-index) 3)) :color 'purple)
								; (log-candidate (cand-name (gethash button button-candidate)) (gethash button button-index)) 
								(setf (gethash button button-state) 1))
							(progn
								(modify-button-for-exp-window button :color 'white)
								; (modify-text-for-exp-window (aref (gethash button button-map) (gethash button button-index)) :color 'black)
								; (modify-text-for-exp-window (aref (gethash button button-map) (+ (gethash button button-index) 3)) :color 'blue)
								;(unlog-candidate (gethash button button-candidate)) 
								(setf (gethash button button-state) 0))))))	

						; Button information, stored in hashmaps
						(setf (gethash button_temp button-map) candidate-party-object-array)
						(setf (gethash button_temp button-state) 0)
						(setf (gethash button_temp button-index) index)
						(setf (gethash button_temp button-candidate) candidate)

						
						; Loop increment operations
						(setf y-offset (+ y-offset 15))
						(setf candidate (pop candidates))
						(setf index (+ index 1)))))
			
				(setq j (+ j 1))
			
				(when (> j 7) (return j)) ; more than 7 rows, break the loop
				
				(when (not cntst-lst) (return j)) ;checks if we've run out of races
		
			)
		
		(setq i (+ i 1))
				
		(when (> i 2) (return i)) ; more than 3 column, break the loop
		
		(when (not cntst-lst) (return i)) ;checks if we've run out of races

				
		
	
	)
	
	; Runs the model for 200 seconds on the ballot if use-model is true
	(if use-model
		(progn
		(install-device window)
		(proc-display)
		(start-hand-at-mouse)
		(if realtime (run 200 t) (run 200))
		;(if dolog (log-ballot))
		))
))
