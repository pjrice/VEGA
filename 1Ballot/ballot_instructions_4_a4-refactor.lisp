;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Define global parameters and variables that will be used in the (vote)
;; function to construct the ballot

;; variable that the ACT-R experiment window will be associated with
(defvar *window* nil)

;; The following parameters map the buttons to various objects so that these 
;; values can be accessed in the click response function

;; Will be used to map the buttons to their state (to know whether to set to black or white)
(defparameter button-state (make-hash-table))
;; Will be used to map the buttons to the array of candidate and party text objects on 
;; the experiment window (to use to change their color)
(defparameter button-map (make-hash-table))
;; Will be used to map a given button to the associated candidate-party name's race index in cntst-lst
(defparameter button-index (make-hash-table))
;; Will be used to map a given button to the associated candidate's name
(defparameter button-candidate (make-hash-table))

; Holdovers from construcing the ballot with noise
; Values can be increased to add random element to positioning of features on experiment window
(defparameter *noise* 0)
(defparameter *noise-macro* 0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utility functions that will be used while constructing the ballot

;; Gets a random number between 1 and n
(defun rand (n)
    (- (random (1+ (* 2 n))) n))

;; Function to associated with an ACT-R button's :action parameter;
;; will be called when button is clicked
(defun on-button-press (button)
    (if (= (gethash button button-state) 0)
        (progn
            (modify-button-for-exp-window button :color 'black)
            (setf (gethash button button-state) 1))
        (progn
            (modify-button-for-exp-window button :color 'white)
            (setf (gethash button button-state) 0))
    )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Define the list of races: their names, candidates, and parties
;; Used in the below (vote) function to construct the ballot
(defparameter cntst-lst nil)
(defparameter instr nil)

; A single race
(defclass contest ()
  ((office-name :accessor office-name :initarg :office-name :initform nil)
   (cand-lst :accessor cand-lst :initarg :cand-lst :initform nil)
   (selection :accessor selection :initarg :selection :initform nil)
   (office-field :accessor office-field :initarg :office-field)
;instructions
   (ballot-title :accessor ballot-title :initarg :ballot-title :initform nil)
   (header-a :accessor header-a :initarg :header-a :initform nil)
   (header-b :accessor header-b :initarg :header-b :initform nil)
   (header-c :accessor header-c :initarg :header-c :initform nil)
   (footer-a :accessor footer-a :initarg :footer-a :initform nil)
   (footer-b :accessor footer-b :initarg :footer-b :initform nil)
   (instr-a :accessor instr-a :initarg :instr-a :initform nil)
   (instr-b :accessor instr-b :initarg :instr-b :initform nil)
;   (instr-c :accessor instr-c :initarg :instr-c :initform nil)
   (instr-d :accessor instr-d :initarg :instr-d :initform nil)
   (nonrace :accessor nonrace :initarg :nonrace :initform nil)
   ))

; A single candidate  
(defclass cand-choice ()
  ((cand-name :accessor cand-name :initarg :cand-name :initform nil)
   (party-name :accessor party-name :initarg :party-name :initform nil)
   (selected-p :accessor selected-p :initarg :selected-p :initform nil)
   (parent :accessor parent :initarg :parent :initform nil)
   (my-idx :accessor my-idx :initarg :my-idx :initform nil)
   ))


; instruction
(setf instr
   (make-instance 'contest
                :header-a "KEWAUNEE COUNTY"

                :header-b "STATE OF WISCONSIN"

                :header-c "NOVEMBER 5, 2002"
                
                :footer-a "CARLTON TOWN"

                :footer-b "TO CONTINUE VOTING,
PLEASE TURN BALLOT OVER."

                :ballot-title "OFFICIAL BALLOT"

                :instr-a "NOTICE TO ELECTORS: This ballot may be invalid unless initialed by 2 election inspectors. 
If cast as an absentee ballot, the ballot must bear the initials of the manucipal clerk 
or deputy clerk. To vote for the candidate of your choice, blacken the oval to the left
of the candidate's name. To vote for a person whose name does not appear on the ballot, 
write the person's name on the line provided and blaken the oval to the left of the line."

                :instr-d "IMPORTANT: Blacken the oval to the LEFT of the name of the candidate. When voting for 
governor and lieutenant governor, you may vote only for the candidate on ticket jointly."   

                :nonrace "By casting this ballot, I do
pledge myself to abide by the
results of this Primary 
Election."
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
			:cand-name "VernonAlbury" :party-name "DEM")
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
		 :office-name "UnitedStatesRepresentativeD7"
		 :cand-lst
		 (list
		  (make-instance 'cand-choice
			:cand-name "PedroBrouse" :party-name "REP")
		  (make-instance 'cand-choice
			:cand-name "RobertMettler" :party-name "DEM")
                  (make-instance 'cand-choice
			:cand-name "EdThompson" :party-name "LIB")))

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
			:cand-name "WesleyMillette" :party-name "DEM")))
	   
	   (make-instance 'contest
		 :office-name "StateRepresentativeD134"
		 :cand-lst
		 (list
		  (make-instance 'cand-choice
			:cand-name "PetraBencomo" :party-name "REP")
		  (make-instance 'cand-choice
			:cand-name "SusanneRael" :party-name "DEM")
                  (make-instance 'cand-choice
			:cand-name "AlanDEisenberg" :party-name "REF")))


	   (make-instance 'contest
		 :office-name "JusticeofthePeace"
		 :cand-lst
		 (list
		  (make-instance 'cand-choice
			:cand-name "PeterVarga" :party-name "REP")
		  (make-instance 'cand-choice
			:cand-name "MarkEBaber" :party-name "DEM")))

	   (make-instance 'contest
		 :office-name "MemberStateBoardofEducationD2"
		 :cand-lst
		 (list
		  (make-instance 'cand-choice
			:cand-name "PeterAVarga" :party-name "REP")
		  (make-instance 'cand-choice
			:cand-name "MarkBaber" :party-name "DEM")))	   

	   (make-instance 'contest
		 :office-name "CityController"
		 :cand-lst
		 (list
		  (make-instance 'cand-choice
			:cand-name "TimGrasty" :party-name "DEM")))

	   (make-instance 'contest
		 :office-name "CountyJudge"
		 :cand-lst
		 (list
		  (make-instance 'cand-choice
			:cand-name "ElaineTucker" :party-name "REP")
		  (make-instance 'cand-choice
			:cand-name "LuisKim" :party-name "DEM")))


	   (make-instance 'contest
		 :office-name "CommissionerofGeneralLandOffice"
		 :cand-lst
		 (list
		  (make-instance 'cand-choice
			:cand-name "TiffanyPerry" :party-name "REP")
		  (make-instance 'cand-choice
			:cand-name "OpalStone" :party-name "DEM")
                  (make-instance 'cand-choice
			:cand-name "MikeMangan" :party-name "GUE")))

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

	   
	))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; A ballot function that gets called within combine.lisp. Constructs the ballot on the ACT-R experiment window
;; so that a model or human can interact with the ballot.
;; realtime is a boolean that specifies whether to run the model in real time or not (only pertanent if use-model is true)
;; use-model is a boolean that specifies whether to use the model or allow just human interaction with the ballot
;; visible is a boolean that specifies whether the ballot window should be made visible or not
;; dolog is a boolean that specifies whether to log the results of voting in a logfile.
;; contest-parameters is a list that contains three integers. The first value modifies the y spacing between races; the second value modifies the y spacing after the race title; and the third value modifies the y spacing between candidates of a race.

(defun vote (realtime use-model visible dolog &optional (contest-parameters '(30 35 18)))

    ; Resets the act r enviroment
    ;(reset)
    
    ; Logging
	(defparameter race-sizes '())
	(defparameter race-sizes-column '())
	(defparameter race-indexes-voted-on '())
	(defparameter race-parameters contest-parameters)

    ; Initializes the experiment window
    (setf *window* (open-exp-window "Ballot" :visible t :x 0 :y 0 :width 710 :height 720))
    
    ; Initializes values that will be used to construct ballot, then loops over columns/rows to do so
    (let* (
        (y-spacing-between-races (nth 0 contest-parameters))
		(y-spacing-after-title (nth 1 contest-parameters))
		(y-spacing-between-candidates (nth 2 contest-parameters))
		(wheight 720)
		(wwidth 710)
		(instr-x 10)
		(instr-y 35)
        (instr-y-header 10)
        (instr-y-bottom 175)
        (starting-x 10) 
        (starting-y 250)
        (ending-y 690)
        (current-x starting-x)
		(current-y starting-y)
        (x-offset-button 0)
		(x-offset-candidate 30)
		(x-offset-party 168)
        (column-width 236)
        )
        
        ; Place header on the screen
        ; add image box for instructions
        (add-image-to-exp-window *window* "box" "lb.gif" :x (- instr-x 6) :y instr-y-header :width 710 :height 13 :clickable nil)
        
        ;add instruction texts    
        (add-text-to-exp-window *window* (header-a instr) :color 'black :x instr-x :y instr-y-header)
        (add-text-to-exp-window *window* (header-b instr) :color 'black :x (+ instr-x 240) :y instr-y-header)
        (add-text-to-exp-window *window* (header-c instr) :color 'black :x (+ instr-x 480) :y instr-y-header)
        
        ; Place instructions on the screen
        ; add image box for instructions
        (add-image-to-exp-window *window* "box" "lb.gif" :x (- instr-x 6) :y instr-y :width 720 :height 130 :clickable nil)
        
        ;add instruction texts    
        (add-text-to-exp-window *window* (ballot-title instr) :color 'black :x instr-x :y instr-y)
        (add-text-to-exp-window *window* (instr-a instr) :color 'black :x instr-x :y (+ instr-y 22))
        (add-text-to-exp-window *window* (instr-d instr) :color 'black :x instr-x :y (+ instr-y 90))
        
        ; Place footer on the screen
        ; add image box for footer
        (add-image-to-exp-window *window* "box" "lb.gif" :x (- instr-x 6) :y (- wheight 40) :width 720 :height 33 :clickable nil)
        
        ;add instruction texts    
        (add-text-to-exp-window *window* (footer-a instr) :color 'black :x instr-x :y (- wheight 38))
        (add-text-to-exp-window *window* (footer-b instr) :color 'black :x (+ instr-x 500) :y (- wheight 38))
        
        ; Place non-race in the left column
        ; add image box
        (add-image-to-exp-window *window* "box" "lb.gif" :x (- instr-x 4) :y 175 :width 225 :height 50 :clickable nil)
        
        ;add non-race texts    
        (add-text-to-exp-window *window* (nonrace instr) :color 'black :x (- instr-x 1) :y (- wheight 175))
                 
        
        ; Loop over columns on the ballot
		(loop
			(setf current-column-sizes '())
			
			; Loop over rows within a column
            (loop
                ; Calls exp-window functions to place ballot features on experiment window
                (let* (
					(candidate-party-object-array (make-array '(16)))
					(contest (pop cntst-lst))
					(candidates (cand-lst contest))
					(candidate (pop candidates))
					(index 0)
					(button_temp nil)
					)
					
					; Log creation of race
					(setf race-sizes (append race-sizes (list (+ (list-length candidates) 1))))
					(setf current-column-sizes (append current-column-sizes (list (+ (list-length candidates) 1))))
					
					; Add the background box for this race to the experiment window
                    (add-image-to-exp-window *window* "box" "lb.gif" :x (- current-x 4) :y current-y :width 225 :height (+ y-spacing-after-title (* (- (list-length candidates) 1) y-spacing-between-candidates) (* (list-length (list (+ (list-length candidates) 1))) 14) 13) :clickable nil)
                    
                    ; Add the name of the contest to the experiment window
                    (add-text-to-exp-window *window* (office-name contest) :color 'red :x current-x :y current-y)
                    
                    ; update the current-y position
                    (setf current-y (+ current-y y-spacing-after-title))
                    
                    ; Loop over the candidate names/parties/buttons to add them to the experiment window
                    (loop while candidate
                    do  (progn
                            
                            ; Displays the name of the candidate on the experiment window and stores this in candidate-party-object-array 
                            (setf (aref candidate-party-object-array index) (add-text-to-exp-window *window* (cand-name candidate) :color 'purple :x (+ x-offset-candidate current-x) :y current-y))
                            
                            ; Displays the party of the candidate on the experiment window and stores this in candidate-party-object-array
                            (setf (aref candidate-party-object-array (+ index 3)) (add-text-to-exp-window *window* (party-name candidate) :color 'blue :x (+ x-offset-party current-x) :y current-y))
                            
                            ; Displays the button associated with the candidate and subsequently stores the button's info in hash tables and modifies the button's action
                            (let* (
                                (button (add-button-to-exp-window *window* :color 'white :text "" :x current-x :y (+ current-y 1) :width 20 :height 10 :action nil))
                                )
                                ;
                                (setf (gethash button button-state) 0) ; sets the button's state to 0 (unselected, white)
                                (setf (gethash button button-map) candidate-party-object-array) ; stores the candidate name/party exp-window objects associated with the button
                                (setf (gethash button button-index) index) ; stores the index of the candidate/party's race in cntst-list
                                (setf (gethash button button-candidate) candidate) ; stores the candidate associated with the button
                                (modify-button-for-exp-window button :action (list 'on-button-press button)) ; modifies the button's action with the (on-button-press) function
                            )
                            
                            ; increment values needed to display/store info for the next candidate in the loop
                            (setf current-y (+ current-y y-spacing-between-candidates))
                            (setf candidate (pop candidates))
                            (setf index (+ index 1))
                        ) ; end of progn within candidate loop
                    ) ; end of loop over candidates within a race
                ) ; end of (let*) statement that exp-window functions are called within
                
                ; check for break conditions and update info for next contest
                (when (not cntst-lst) (return)) ;checks if we've run out of races

				(setf current-y (+ current-y y-spacing-between-races))
				(setf next-cntst (car cntst-lst))
				
				(when (> 
					(+ current-y y-spacing-after-title (* (length (cand-lst next-cntst)) y-spacing-between-candidates)) 
					ending-y)
					(return)) ; y too big
            ) ; end of row loop
            
            ; More logging
			(setf race-sizes-column (append race-sizes-column (list current-column-sizes)))
			(setf current-x (+ current-x column-width))
			(setf current-y	instr-y-bottom)				
			(when (not cntst-lst) (return)) ;checks if we've run out of races
			(when (> current-x wwidth) (return)) ;checks if we've run out of left right room
        ) ; end of column loop
        
        ; this should be removed in the future; for testing only (it is called in the if statement below)
        (install-device *window*)
        
        ; Runs the model for 200 seconds on the ballot if use-model is true
        (if use-model
            (progn
                (install-device *window*)
                (start-hand-at-mouse)
                (if realtime (run 200 t) (run 200))
                (if dolog (log-ballot))
            )
        )
    ) ; end of outer (let*) statement
) ; end of function definition
