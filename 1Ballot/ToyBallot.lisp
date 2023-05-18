;;;  -*- mode: LISP; Syntax: COMMON-LISP;  Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Author      : Mike Byrne [and others]
;;; Copyright   : (c) 2019 Mike Byrne
;;; Address     : Department of Psychology 
;;;             : Rice University
;;;             : Houston, TX 77005
;;;             : byrne@rice.edu
;;; 
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  
;;; Filename    : BrowardBallot.lisp
;;; Version     : 1
;;; 
;;; Description : The ballot from Broward County, Florida 2018.
;;;				: * This file establishes an act-r window that represents a complete ballot with no lines seperating the races.
;;;
;;; Bugs        : * None known
;;;
;;; To do       : * The logging does not work if a button is unclicked (because there is no such method yet in logging) 
;;;				: * See logging.lisp todo for more info
;;; 
;;; ----- History -----
;;; 2021.10.05  Xianni Wang
;;;				: * Created the file
;;;				: * Modified from WisconsinBallot-Box-Instruction-Long.lisp
;;;				: * Add header
;;;				: * Modified races
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

(defmethod build-vis-locs-for ((self image-vdi) (vis-mod vision-module))
  ;; The chunks for the item are stored in the hash-table found in the
  ;; loc-chunks slot of the object which is created by default for all
  ;; virtual-dialog-item instances.  The hash-table uses an 'equal
  ;; test and the recommended key is a cons of the current meta-process
  ;; and current model to make sure that each model which may be using
  ;; this device gets its own chunk.  The values are lists of chunks,
  ;; which in this case is just a single chunk per item.
  (let* ((c (gethash (cons (current-mp) (current-model)) (loc-chunks self)))
         
         (f (cond ((and c (chunk-p-fct (car c)))
                   ;; have a chunk already so just modify the 
                   ;; standard visual-location slots with the current values from
                   ;; the image-vdi object
                   (mod-chunk-fct (car c) `(color ,(color self)
                                            screen-x ,(+ (x-pos self) (round (width self) 2))
                                            screen-y ,(+ (y-pos self) (round (height self) 2))
                                            width ,(width self)
                                            height ,(height self)))
                   (car c))
                  (t
                   ;; Since we are setting the value and kind slots to the symbol
                   ;; image we should make sure that is defined as a chunk otherwise
                   ;; the model will create a warning indicating that it has to
                   ;; do so by default.
                   
                   (unless (chunk-p image)
                     (define-chunks (image)))
                   
                   ;; Create the new chunk with the current item's values and
                   ;; store it in the table.  Here we set the value and kind
                   ;; slots both to the chunk image, but below we indicate that
                   ;; the 'real' value is actually the text of the item -- the
                   ;; real value is the one that a model can use to find a location
                   ;; but the resulting slot will always have the value image since
                   ;; content isn't supposed to be available until the item is attended.
                   
                   (car (setf (gethash (cons (current-mp) (current-model)) (loc-chunks self)) 
                          (define-chunks-fct `((isa visual-location
                                                    color ,(color self)
                                                    value image
                                                    kind image
                                                    screen-x ,(+ (x-pos self) (round (width self) 2))
                                                    screen-y ,(+ (y-pos self) (round (height self) 2))
                                                    width ,(width self)
                                                    height ,(height self))))))))))
    
    ;; Setting the visual-object parameter in a chunk which is
    ;; returned as a visual feature will cause the vision
    ;; vision module to call the vis-loc-to-obj method on
    ;; the value of that parameter instead of the device itself
    ;; to create the visual-object for the item.  This is convenient
    ;; when extending the system because it means that the parent
    ;; device's method does not have to be modified or extended in
    ;; some way.
    (setf (chunk-visual-object f) self)
    
    ;; Setting the real-visual-value parameter of a chunk which is 
    ;; returned as a visual feature lets the vision module know
    ;; what to show for the value slot of the item with print-visicon
    ;; if it should differ from the value which is in the value slot
    ;; of the chunk that gets set in the visual-location buffer.
    
    (setf (chunk-real-visual-value f) (dialog-item-text self))
    
    ;; return the visual location chunk
    f))
  

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
(defparameter instr nil)

; A single race
(defclass contest ()
  ((office-name 
    :accessor office-name 
    :initarg 
    :office-name 
    :initform nil)
   (cand-lst 
    :accessor cand-lst 
    :initarg 
    :cand-lst 
    :initform nil)
   (selection 
    :accessor selection 
    :initarg 
    :selection 
    :initform nil)
   (office-field 
    :accessor office-field 
    :initarg 
    :office-field)
;instructions
   (ballot-title 
    :accessor ballot-title 
    :initarg :ballot-title :initform nil)
   (header-a 
    :accessor header-a 
    :initarg :header-a 
    :initform nil)
   (header-b 
    :accessor header-b 
    :initarg :header-b 
    :initform nil)
   (header-c 
    :accessor header-c 
    :initarg 
    :header-c 
    :initform nil)
   (instr-a 
    :accessor instr-a 
    :initarg :instr-a 
    :initform nil)
   (instr-b 
    :accessor instr-b 
    :initarg 
    :instr-b 
    :initform nil)
   (instr-c 
    :accessor instr-c 
    :initarg 
    :instr-c 
    :initform nil)
   (instr-d 
    :accessor instr-d 
    :initarg 
    :instr-d 
    :initform nil)
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
                :header-a "Ballot Style 58
Official Genearl Election Ballot
November 6, 2018"

                :header-b "Boleta Oficial De La Eleccion
General"
                :header-c "Offisyel Jeneral Eleksyon
Bilten"
                :ballot-title "Ballot Instructions"

                :instr-a "FOR PARTISAN OFFICE AND 
REFERENDUM
NOTICE TO ELECTORS:
This ballot may be invalid 
unless initialed by 2 election 
inspectors.In Cast as an 
absentee ballot, the ballot 
must bear the initials of 
the manucipal clerk or deputy
clerk."
                :instr-b "To vote for the candidate of 
your choice, blacken the oval
to the left of the candidate's
name. To vote for a person 
whose name does not appear on 
the ballot, write the person's 
name on the line provided and
blaken the oval to the left of 
the line."
                :instr-c "STRAIGHT PARTY
If you desire to vote for a 
straight party ticket for all
state, congressional, 
legistiative, and county off-
ices, blacken the oval to the 
LEFT of the party of your 
choice. A straight party vote
cannot be cast for independent
candidates of your choice."

                :instr-d "IMPORTANT:
Blacken the oval to the LEFT
of the name of the candidate.
When voting for governor and
lieutenant governor, you may
vote only for the candidate
on ticket jointly."          
    ))

; The contest list
(setf cntst-lst
	  (list


; Undervote Error Expected Here

	   (make-instance 'contest
		 :office-name "UnitedStatesSenator"
		 :cand-lst
		 (list
		  (make-instance 'cand-choice
			:cand-name "RickScott" :party-name "REP")
		  (make-instance 'cand-choice
			:cand-name "BillNelson" :party-name "DEM")))


(make-instance 'contest
		 :office-name "RepresentativeInCongress"
		 :cand-lst
		 (list
		  (make-instance 'cand-choice
			:cand-name "AlceeLHastings" :party-name "DEM")))

; Undervote Eror End


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
			:cand-name "TyABollerud" :party-name "IND")))

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
			:cand-name "MarkBaber" :party-name "DEM")))

	   (make-instance 'contest
		 :office-name "MemberStateBoardofEducationD2"
		 :cand-lst
		 (list
		  (make-instance 'cand-choice
			:cand-name "PeterVarga" :party-name "REP")
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

	   
	))


;; A ballot function that gets called by combine.lisp and that runs the model (or a human) on the constructed ballot
;; realtime is a boolean that specifies whether to run thge model in real time or not (only pertanent if use-model is true)
;; use-model is a boolean that specifies whether to use the model or allow just human interaction with the ballot
;; visible is a boolean that specifies whether the ballot window should be made visible or not
(defun vote (realtime use-model visible dolog &optional (contest-parameters '(30 35 18)))
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

 		;(wheight 700)
                (wheight 800)
		(wwidth 680)
                (window (open-exp-window "Broward Ballot" :x 50 :y 10 :width wwidth :height wheight :visible visible)) 
                (instr-x 10)
                (instr-y-header 10)
                (instr-y 70)
                (starting-x 10) 		
                (starting-y 600)
		(current-x starting-x)
		(current-y starting-y)
               	(x-offset-button 0)
		(x-offset-candidate 30)
		(x-offset-party 168)
                (column-width 233)

               
		; Maps the buttons to various objects so that these values can be accessed in the click response method, where we only have accessed
		; to the object itself
		(button-map (make-hash-table)) ; Maps the buttons to the array of candidate and party objects on screen (to use to change their color to blue)
		(button-state (make-hash-table)) ; Maps the buttons to their state (to know whether to set to blue or black)
		(button-index (make-hash-table)) ; Maps the button to their race's index in cntst-lst 
		(button-candidate (make-hash-table)) ; Maps the button to their associated candidate object from cntst-lst
	        (button-race (make-hash-table))) ; Maps the button to their race index		
       



                ; Place header on the screen
                ; add image box for instructions
                 ;;(add-items-to-exp-window 

                 ;;(make-instance 'image-vdi :file "lblue.gif" :dialog-item-text "box" :x-pos (- instr-x 6) :y-pos instr-y-header :width 700 :height 48))

                 ;add instruction texts    
                 (add-text-to-exp-window :text (header-a instr) :color 'black :x instr-x :y instr-y-header)
                 (add-text-to-exp-window :text (header-b instr) :color 'black :x (+ instr-x 250) :y (+ instr-y-header 10))
                 (add-text-to-exp-window :text (header-c instr) :color 'black :x (+ instr-x 480) :y (+ instr-y-header 10))    



                ; Place instructions on the screen
                ; add image box for instructions
                 ;;(add-items-to-exp-window 
   
                 ;; here we create our new items assuming that the corresponding files
                 ;; are available if we intend to have them shown in a visible window.
                 ;; the dialog-item-text is the value which the model will see when it
                 ;; attends to the image.
                                         
                 ;; The first one does not provide an action function and thus the default
                 ;; action of printing the info when :vwt is true will be used.

                 ;;(make-instance 'image-vdi :file "lblue.gif" :dialog-item-text "box" :x-pos (- instr-x 4) :y-pos (+ instr-y 2) :width 225 :height 525))

                 ;add instruction texts    
                 (add-text-to-exp-window :text (ballot-title instr) :color 'black :x instr-x :y instr-y)
                 (add-text-to-exp-window :text (instr-a instr) :color 'black :x instr-x :y (+ instr-y 22))
                 (add-text-to-exp-window :text (instr-b instr) :color 'black :x instr-x :y (+ instr-y 158))    
                 (add-text-to-exp-window :text (instr-c instr) :color 'black :x instr-x :y (+ instr-y 282))
                 (add-text-to-exp-window :text (instr-d instr) :color 'black :x instr-x :y (+ instr-y 420))


		; Places all of the races on the screen
                ; Column loop
		(loop
                        (setf current-column-sizes '())
			
			; Item low
			(loop 	
				
				; Constructs the ballot
				(let* (
				
					; initialization to build the race
					(candidate-party-object-array (make-array '(16)))
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
                                        (add-items-to-exp-window 
   
                                         ;; here we create our new items assuming that the corresponding files
                                         ;; are available if we intend to have them shown in a visible window.
                                         ;; the dialog-item-text is the value which the model will see when it
                                         ;; attends to the image.
                                         
                                         ;; The first one does not provide an action function and thus the default
                                         ;; action of printing the info when :vwt is true will be used.
  
                                        (make-instance 'image-vdi :file "lblue.gif" :dialog-item-text "box" :x-pos (- current-x 4) :y-pos current-y :width 225 :height (+ y-spacing-after-title (* (- (list-length candidates) 1) y-spacing-between-candidates) (* (list-length (list (+ (list-length candidates) 1))) 14) 13) ) )
                            		; Adds the race name
					(add-text-to-exp-window :text (office-name contest) :color 'red :x current-x :y current-y)
                                        (setf current-y (+ current-y y-spacing-after-title))
				
					; Constructs the rest of the ballot
					(loop while candidate 
		
					  do 	(progn
                                                  
						; Displays and stores candidates
						(setf (aref candidate-party-object-array index) (add-text-to-exp-window :text (cand-name candidate) :color 'purple :x (+ x-offset-candidate current-x) :y current-y))
						
						; Displays and stores parties
						(setf (aref candidate-party-object-array (+ index 3)) (add-text-to-exp-window :text (party-name candidate) :color 'blue :x (+ x-offset-party current-x) :y current-y))

						; Displays and stores buttons
						(setf button_temp (add-button-to-exp-window :color 'white :text "" :x current-x :y (+ current-y 1) :width 20 :height 10 :action 
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
		(setf current-y	instr-y)				
		(when (not cntst-lst) (return)) ;checks if we've run out of races
		(when (> current-x wwidth) (return)) ;checks if we've run out of left right room
		
	
	)
	
	; Runs the model for 200 seconds on the ballot if use-model is true
	(if use-model
		(progn
		(install-device window)
		(proc-display)
		(start-hand-at-mouse)
		(if realtime (run 200 :real-time t) (run 200))
		(if dolog (log-ballot))))
))


