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
;;; Description : A ballot (with color, and background)
;;;				: * This file establishes an act-r window that represents a complete ballot with no lines seperating the races.
;;;
;;; Bugs        : * None known
;;;
;;; To do       : * The logging does not work if a button is unclicked (because there is no such method yet in logging) 
;;;				: * See logging.lisp todo for more info
;;; 
;;; ----- History -----
;;; 2019.9.20   Xianni Wang
;;;				: * Created the file, inherited from Joshua Engels' "No-Line.lisp"
;;; 2019.9.20   Xianni Wang
;;;				: * added Dan's code (image-vdi) for adding image to background 
;;;				: * added add-items-to-exp-window to display boxes (box first, then texts)
;;;				: * added a paragraph in General Docs
;;; 2019.9.25   Xianni Wang
;;;				: * add tracing command (sgp :v t :vwt t :show-focus t)
;;; 2019.10.7   Xianni Wang
;;;				: * remove tracing command (sgp :v t :vwt t :show-focus t)
;;;				: * modified and updated from "No-Line-Colors.lisp"
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
;;;
;;; To make the background box work, put the image .gif file into actr7/Environment/GUI/images
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;; ------------------------------------------------------------------ ;;;;;
;;;;; Dan Bothell's code to handle images
;;;;; ------------------------------------------------------------------ ;;;;;

(defclass image-vdi (virtual-dialog-item)
  
  ;; add a new slot for this item which will be used to hold a string
  ;; specifying the name of a file to display if using the 
  ;; additional extension for the ACT-R Environment to extend
  ;; the visible-virtual-windows as well.
  
  ((file :accessor file :initform nil :initarg :file))
  
  (:default-initargs
    :id (new-name-fct "IMAGE-VDI") ;; make sure it gets a unique name
    :handles-click-p nil             ;; indicate that this item can be clicked on by a model
    :action 'default-image-click)) ;; specify a default function to call when it is clicked

;;; Here we define the default function to call when
;;; an image-vdi feature is clicked.  It uses the vw-output
;;; command to print information in the model trace when the
;;; :vwt parameter in the model is set to t, and that will show
;;; the window, file, and text information from the image-vdi 
;;; item along with the position of the click.

(defmethod default-image-click ((self image-vdi) position)
  (vw-output (view-container self)
             "image with file ~s and text ~s clicked at relative position ~d ~d" 
             (file self) (dialog-item-text self) (px position) (py position)))

;;; The vv-click-event-handler is the method which must be written
;;; to actually process the click which the model produces.  This is
;;; the internal virtual window method that gets called and we use
;;; that to call the action function provided when the creation of
;;; an image-vdi item.  It computes the relative position of the click
;;; within the image and passes that to the action function.

(defmethod vv-click-event-handler ((self image-vdi) position)
  (when (or (functionp (action-function self))
            (and (symbolp (action-function self)) (fboundp (action-function self))))
    (funcall (action-function self) self (vector (- (px position) (x-pos self)) (- (py position) (y-pos self))))))

;;; The build-vis-locs-for method is typically written for the
;;; whole device, but the virtual windows in ACT-R extend that so
;;; that the device actually calls that method for each of the
;;; items in the window to create the features.  It needs to 
;;; return the chunk with the visual-location information desired.
;;;
;;; In this example it takes the extra step of being efficient and
;;; only creating a chunk when needed (one for each model which may be
;;; using it as the device) and then modifying that chunk each time
;;; the display is processed to provide the current information.
;;;
;;; It also sets some special parameters on that chunk to allow the
;;; vision module to automatically hide some information in the
;;; visual location chunk and fill in the details for the visual
;;; object.

;(defmethod build-vis-locs-for ((self image-vdi) (vis-mod vision-module))
;  ;; The chunks for the item are stored in the hash-table found in the
;  ;; loc-chunks slot of the object which is created by default for all
;  ;; virtual-dialog-item instances.  The hash-table uses an 'equal
;  ;; test and the recommended key is a cons of the current meta-process
;  ;; and current model to make sure that each model which may be using
;  ;; this device gets its own chunk.  The values are lists of chunks,
;  ;; which in this case is just a single chunk per item.
;  (let* ((c (gethash (cons (current-mp) (current-model)) (loc-chunks self)))
;         
;         (f (cond ((and c (chunk-p-fct (car c)))
;                   ;; have a chunk already so just modify the 
;                   ;; standard visual-location slots with the current values from
;                   ;; the image-vdi object
;                   (mod-chunk-fct (car c) `(color ,(color self)
;                                            screen-x ,(+ (x-pos self) (round (width self) 2))
;                                            screen-y ,(+ (y-pos self) (round (height self) 2))
;                                            width ,(width self)
;                                            height ,(height self)))
;                   (car c))
;                  (t
;                   ;; Since we are setting the value and kind slots to the symbol
;                   ;; image we should make sure that is defined as a chunk otherwise
;                   ;; the model will create a warning indicating that it has to
;                   ;; do so by default.
;                   
;                   (unless (chunk-p image)
;                     (define-chunks (image)))
;                   
;                   ;; Create the new chunk with the current item's values and
;                   ;; store it in the table.  Here we set the value and kind
;                   ;; slots both to the chunk image, but below we indicate that
;                   ;; the 'real' value is actually the text of the item -- the
;                   ;; real value is the one that a model can use to find a location
;                   ;; but the resulting slot will always have the value image since
;                   ;; content isn't supposed to be available until the item is attended.
;                   
;                   (car (setf (gethash (cons (current-mp) (current-model)) (loc-chunks self)) 
;                          (define-chunks-fct `((isa visual-location
;                                                    color ,(color self)
;                                                    value image
;                                                    kind image
;                                                    screen-x ,(+ (x-pos self) (round (width self) 2))
;                                                    screen-y ,(+ (y-pos self) (round (height self) 2))
;                                                    width ,(width self)
;                                                    height ,(height self))))))))))
;    
;    ;; Setting the visual-object parameter in a chunk which is
;    ;; returned as a visual feature will cause the vision
;    ;; vision module to call the vis-loc-to-obj method on
;    ;; the value of that parameter instead of the device itself
;    ;; to create the visual-object for the item.  This is convenient
;    ;; when extending the system because it means that the parent
;    ;; device's method does not have to be modified or extended in
;    ;; some way.
;    (setf (chunk-visual-object f) self)
;    
;    ;; Setting the real-visual-value parameter of a chunk which is 
;    ;; returned as a visual feature lets the vision module know
;    ;; what to show for the value slot of the item with print-visicon
;    ;; if it should differ from the value which is in the value slot
;    ;; of the chunk that gets set in the visual-location buffer.
;    
;    (setf (chunk-real-visual-value f) (dialog-item-text self))
;    
;    ;; return the visual location chunk
;    f))
  

;;; Because we set the visual-object parameter of the visual-location
;;; chunk we can write a vis-loc-to-obj method for our image-vdi class
;;; to create the visual-object chunk when the item is attended.
;;; For our item we do not have any custom slots in the visual-object
;;; representation and thus we will use the fill-default-vis-obj-slots
;;; function to automatically set the color, height, and width slots
;;; based on the visual-location chunk along with the value slot
;;; based on the real-visual-value parameter if set, or the value
;;; slot of the visual-location chunk if it is not.

(defmethod vis-loc-to-obj ((self image-vdi) loc)
  
  ;; We are creating a sub-type of the visual-object chunk-type
  ;; which has a slot named image with a default value of t so that
  ;; one can use "isa image" in chunks and productions and have it
  ;; work basically like things used to (that's how the other default
  ;; features of text, buttons, and lines work).
  
  (unless (chunk-type-p image)
    (chunk-type (image (:include visual-object)) (image t)))
  
  ;; Create a new chunk and then just call fill-default-vis-obj-slots
  ;; to set the standard slots with the values from the visual-location
  ;; chunk.  If we wanted to include more information we could do so
  ;; before or after filling the default slots.
  
  (fill-default-vis-obj-slots (car (define-chunks (isa image))) loc))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; The code above is sufficient for creating a new virtual dialog item ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; The code below this point extends that virtual image dialog item to actually
;;; have it display a .gif file in a visible-virtual window in the ACT-R
;;; Environment in conjunction with the 999-creating-a-new-virtual-item.tcl
;;; file that extends the Environment interface.
;;;
;;; This code depends on internal mechanisms of the Environment connection
;;; which are not really documented or to be considered part of the actual API
;;; of the ACT-R software.  It is provided as a demonstration for those that
;;; are comfortable working with both Lisp and Tcl/Tk, and who are also 
;;; willing to extend the software in ways that may break under future 
;;; updates.


;;; The add-visual-items-to-rpm-window method on the visible-virtual-window class
;;; is responsible for sending the information to the Environment that it uses to
;;; display the subviews.  Because it wasn't originally built to be extended or 
;;; modified (you can't subclass the visible-virtual-window class since open-exp-window
;;; doesn't provide a way to do so and the original method assumes that all the 
;;; subviews are of known types) your best option is to just replace it with a
;;; version that contains the original items and any new ones you add.  At some
;;; point that may be refactored to provide an easy extension mechanism.

(defmethod add-visual-items-to-rpm-window ((win visible-virtual-window) &rest items)
  (dolist (item items)
    (add-subviews win item)
    (send-env-window-update 
     (case (type-of item)
       (env-button-vdi
        (list 'button (id win) (id item) (x-pos item) (y-pos item) 
          (width item) (height item) (dialog-item-text item) (color-symbol->env-color (color item))))
       (env-text-vdi
        (list 'text (id win) (id item) (x-pos item) (y-pos item) 
          (color-symbol->env-color (color item))  (dialog-item-text item) (round (text-height item) 10/12)))
       (env-line-vdi
        (list 'line (id win) (id item) (x-pos item) (y-pos item) 
              (color-symbol->env-color (color item)) (width item) (height item)))
       
       ;; add a case for the class of the new item and create the list of features
       ;; needed to display it in the Environment with the corresponding Tcl/Tk code
       ;; extension in 999-creating-a-new-virtual-item.tcl
       
       (image-vdi
        (list 'image (id win) (id item) (x-pos item) (y-pos item)
              (file item) (width item) (height item)))))))


;;;;; ------------------------------------------------------------------ ;;;;;
;;;;; end image handling code
;;;;; ------------------------------------------------------------------ ;;;;;

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
		(button-race (make-hash-table))  ; Maps the button to their race index

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
					(button_temp nil))					

					;add background boxes
					 ;; explicitly add instances of the new item to that window
                                        (add-items-to-exp-window window
   
                                         ;; here we create our new items assuming that the corresponding files
                                         ;; are available if we intend to have them shown in a visible window.
                                         ;; the dialog-item-text is the value which the model will see when it
                                         ;; attends to the image.
                                         
                                         ;; The first one does not provide an action function and thus the default
                                         ;; action of printing the info when :vwt is true will be used.
   
                                        (make-instance 'image-vdi :file "lb.gif" :dialog-item-text "box" :x-pos (- randomx 5) :y-pos (- randomy 5) :width 280 :height 75))

					; Adds the race name
					(add-text-to-exp-window window (office-name contest) :color 'red :x randomx :y randomy)

				
					; Constructs the rest of the ballot
					(loop while candidate 
			
					do 	(progn 
						
						; Displays and stores candidates
						(setf (aref candidate-party-object-array index) (add-text-to-exp-window window (cand-name candidate) :color 'purple :x (+ randomx 30 (rand noise)) :y (+ 1 randomy y-offset (rand noise))))
						
						; Displays and stores parties
						(setf (aref candidate-party-object-array (+ index 3)) (add-text-to-exp-window window (party-name candidate) :color 'blue :x (+ randomx 200 (rand noise)) :y (+ 1 randomy y-offset (rand noise))))

						; Displays and stores buttons
						(setf button_temp (add-button-to-exp-window window :color 'white :text "" :x randomx :y (+ randomy y-offset 2) :width 20 :height 10 :action 
						(lambda (button)
						(if (= (gethash button button-state) 0) 
							(progn
								(modify-button-for-exp-window button :color 'black)
                                                                (proc-display)
								; (modify-text-for-exp-window (aref (gethash button button-map) (gethash button button-index)) :color 'purple)
								; (modify-text-for-exp-window (aref (gethash button button-map) (+ (gethash button button-index) 3)) :color 'purple)
								; (log-candidate (cand-name (gethash button button-candidate)) (gethash button button-index)) 
								(setf (gethash button button-state) 1))
							(progn
								(modify-button-for-exp-window button :color 'white)
                                                                (proc-display)
								; (modify-text-for-exp-window (aref (gethash button button-map) (gethash button button-index)) :color 'black)
								; (modify-text-for-exp-window (aref (gethash button button-map) (+ (gethash button button-index) 3)) :color 'blue)
								;(unlog-candidate (gethash button button-candidate)) 
								(setf (gethash button button-state) 0))))))	

						; Button information, stored in hashmaps
						(setf (gethash button_temp button-map) candidate-party-object-array)
						(setf (gethash button_temp button-state) 0)
						(setf (gethash button_temp button-index) index)
						(setf (gethash button_temp button-candidate) candidate)
                                                (setf (gethash button_temp button-race) (- (length race-sizes) 1))

						
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
