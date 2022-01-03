;;;  -*- mode: LISP; Syntax: COMMON-LISP;  Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Author      : Xianni Wang
;;; Copyright   : (c) 2020 Xianni Wang
;;; Address     : Rice University
;;;             : Houston, TX 77005
;;;             : xw48@rice.edu
;;; 
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  
;;; Filename    : WisconsinBallot-Box.lisp
;;; Version     : 1
;;; 
;;; Description : Wisconsin ballot
;;;				: * This file establishes an act-r window that represents a complete ballot with no lines seperating the races.
;;;
;;; Bugs        : * None known
;;;
;;; To do       : * The logging does not work if a button is unclicked (because there is no such method yet in logging) 
;;;				: * See logging.lisp todo for more info
;;; 
;;; ----- History -----
;;; 2020.05.17  Xianni Wang
;;;				: * Created the file
;;;				: * Modified from No-Lines-Color.lisp and NLC-Offset-Random-Box.lisp
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
  (
   (office-name :accessor office-name :initarg :office-name :initform nil)
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

; Wisconsin Error Expected Here

	   (make-instance 'contest
		 :office-name "GovernorLieutenantGovernor"
		 :cand-lst
		 (list
		  (make-instance 'cand-choice
			:cand-name "JimDoyle" :party-name "DEM")
		  (make-instance 'cand-choice
			:cand-name "ScottMcCallum" :party-name "REP")
                  (make-instance 'cand-choice
			:cand-name "JimYoung" :party-name "WIS")
                  (make-instance 'cand-choice
			:cand-name "EdThompson" :party-name "LIB")
                  (make-instance 'cand-choice
			:cand-name "AlanDEisenberg" :party-name "REF")))


(make-instance 'contest
		 :office-name "GovernorLieutenantGovernor"
		 :cand-lst
		 (list
		  (make-instance 'cand-choice
			:cand-name "TyABollerud" :party-name "IND")
		  (make-instance 'cand-choice
			:cand-name "MikeMangan" :party-name "GUE")
                  (make-instance 'cand-choice
			:cand-name "AnebJahRasta" :party-name "RAS")))

; Winsconsin Eror End


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

	   
	))


;; A ballot function that gets called by combine.lisp and that runs the model (or a human) on the constructed ballot
;; realtime is a boolean that specifies whether to run thge model in real time or not (only pertanent if use-model is true)
;; use-model is a boolean that specifies whether to use the model or allow just human interaction with the ballot
;; visible is a boolean that specifies whether the ballot window should be made visible or not
(defun vote (realtime use-model visible dolog &optional (contest-parameters '(35 35 18)))
;(25 35 20)
;(10 20 16)
	;; Logging
	(defparameter race-sizes '())
	(defparameter race-sizes-column '())
	(defparameter race-indexes-voted-on '())
	(defparameter race-parameters contest-parameters)
  
	; Resets the act r enviroment
	(reset)
  
  
	; Constructs the window and populates it with race-titles, candidates, parties, and buttons
	(let* (
		
		(y-spacing-between-races (nth 0 contest-parameters))
		(y-spacing-after-title (nth 1 contest-parameters))
		(y-spacing-between-candidates (nth 2 contest-parameters))

 		(wheight 600)
		(wwidth 750)
                (window (open-exp-window "WisconsinBallot" :x 50 :y 50 :width 830 :height 590 :visible visible)) 
                (starting-x 10) 		
                (starting-y 20)
		(current-x starting-x) 
		(current-y starting-y)
               	(x-offset-button 0)
		(x-offset-candidate 30)
		(x-offset-party 180)
                (column-width 275)


		; Maps the buttons to various objects so that these values can be accessed in the click response method, where we only have accessed
		; to the object itself
		(button-map (make-hash-table)) ; Maps the buttons to the array of candidate and party objects on screen (to use to change their color to blue)
		(button-state (make-hash-table)) ; Maps the buttons to their state (to know whether to set to blue or black)
		(button-index (make-hash-table)) ; Maps the button to their race's index in cntst-lst 
		(button-candidate (make-hash-table)) ; Maps the button to their associated candidate object from cntst-lst
	        (button-race (make-hash-table))) ; Maps the button to their race index		
	
	
		; Places all of the races on the screen
                ; Column loop
		(loop
                        (setf current-column-sizes '())
			
			; Item low
			(loop 	
				
				; Constructs the ballot
				(let* (
				
					; initialization to build the race
					(candidate-party-object-array (make-array '(10)))
					(contest (pop cntst-lst))
					(candidates (cand-lst contest))
					(candidate (pop candidates))
					(index 0)
					(button_temp nil))					

					; Log creation of race
					(setf race-sizes (append race-sizes (list (+ (list-length candidates) 1))))
					(setf current-column-sizes (append current-column-sizes (list (+ (list-length candidates) 1))))

                                       ;add background boxes
                                         ;; explicitly add instances of the new item to that window
                                         (add-image-to-exp-window window "box" "lb.gif" :x (- current-x 5) :y (- current-y 5) :width 250 :height (+ y-spacing-after-title (* (- (list-length candidates) 1) y-spacing-between-candidates) (* (list-length (list (+ (list-length candidates) 1))) 14) 35))
                                        ;(add-items-to-exp-window 
   
                                         ;; here we create our new items assuming that the corresponding files
                                         ;; are available if we intend to have them shown in a visible window.
                                         ;; the dialog-item-text is the value which the model will see when it
                                         ;; attends to the image.
                                         
                                         ;; The first one does not provide an action function and thus the default
                                         ;; action of printing the info when :vwt is true will be used.
  
                                        ;(make-instance 'image-vdi :file "lb.gif" :dialog-item-text "box" :x-pos (- current-x 5) :y-pos (- current-y 5) :width 250 :height (+ y-spacing-after-title (* (- (list-length candidates) 1) y-spacing-between-candidates) (* (list-length (list (+ (list-length candidates) 1))) 14) 35) ) )



					; Adds the race name
					(add-text-to-exp-window window (office-name contest) :color 'red :x current-x :y current-y)
                                        (setf current-y (+ current-y y-spacing-after-title))
				
					; Constructs the rest of the ballot
					(loop while candidate 
			
		
					  do 	(progn 
						
						; Displays and stores candidates
						(setf (aref candidate-party-object-array index) (add-text-to-exp-window window (cand-name candidate) :color 'purple :x (+ x-offset-candidate current-x) :y current-y))
						
						; Displays and stores parties
						(setf (aref candidate-party-object-array (+ index 3)) (add-text-to-exp-window window (party-name candidate) :color 'blue :x (+ x-offset-party current-x) :y current-y))

						; Displays and stores buttons
						(setf button_temp (add-button-to-exp-window window :color 'white :text "" :x current-x :y (+ current-y 1) :width 20 :height 10 :action 
						(lambda (button)
						(if (= (gethash button button-state) 0) 
							(progn
								(modify-button-for-exp-window button :color 'black)
								(proc-display)
                                                                ;(log-candidate (cand-name (gethash button button-candidate)) (gethash button button-index))
								(setf (gethash button button-state) 1)
								(setf race-indexes-voted-on (append race-indexes-voted-on (list (gethash button button-race))))
                                                        )
							(progn
								(modify-button-for-exp-window button :color 'white)
								(proc-display)
								(setf (gethash button button-state) 0)))))
                                                        )	

						; Button information, stored in hashmaps
						(setf (gethash button_temp button-map) candidate-party-object-array)
						(setf (gethash button_temp button-state) 0)
						(setf (gethash button_temp button-index) index)
						(setf (gethash button_temp button-candidate) candidate)
						(setf (gethash button_temp button-race) (- (length race-sizes) 1))

						
						; Loop increment operations
                                                (setf current-y (+ current-y y-spacing-between-candidates))
						(setf candidate (pop candidates))
						(setf index (+ index 1)))
                                          ))
			
				(when (not cntst-lst) (return)) ;checks if we've run out of races

				(setf current-y (+ current-y y-spacing-between-races))
				(setf next-cntst (car cntst-lst))
				(when (> 
					(+ current-y y-spacing-after-title (* (length (cand-lst next-cntst)) y-spacing-between-candidates)) 
					wheight)
					(return)) ; y too big
					
			)
		
		; More logging
                (setf race-sizes-column (append race-sizes-column (list current-column-sizes)))
		(setf current-x (+ current-x column-width))
		(setf current-y	starting-y)				
		(when (not cntst-lst) (return)) ;checks if we've run out of races
		(when (> current-x wwidth) (return)) ;checks if we've run out of left right room
		
	
	)
	
	;this should be removed in the future; for testing only (it is called if the if statement below)
	(install-device window)
	
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


