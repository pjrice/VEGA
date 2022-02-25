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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; A ballot function that gets called within combine.lisp. Constructs the ballot on the ACT-R experiment window
;; so that a model or human can interact with the ballot.
;; realtime is a boolean that specifies whether to run the model in real time or not (only pertanent if use-model is true)
;; use-model is a boolean that specifies whether to use the model or allow just human interaction with the ballot
;; visible is a boolean that specifies whether the ballot window should be made visible or not
;; dolog is a boolean that specifies whether to log the results of voting in a logfile.

(defun vote (realtime use-model visible dolog)

    ; Resets the act r enviroment
    ;(reset)

    ; Initializes the experiment window
    (setf *window* (open-exp-window "test" :visible t :x 0 :y 0 :width 500 :height 500))
    
    ; Initializes values that will be used to construct ballot, then loops over columns/rows to do so
    (let* (
        (starting-x 10) ; initial x coordinate of ballot feature 
        (i 0) ; column index
        (j 0) ; row index
        )
        
        ; Loop over columns on the ballot
        (loop
            
            (setf j 0) ; reset the row index
            (setf starting-x (+ (* i 300) 10)) ; increment the starting-x value by 300*(column index)
            
            ; Loop over rows within a column
            (loop
            
                ; Calls exp-window functions to place ballot features on experiment window
                (let* (
                    (starting-y (+ (* j 85) 10)) ; increment starting-y value (new row within column)
                    ; Holdovers from constructing the ballot with noise
                    ; randomx/y are the values actually associated with the :x and :y parameters in the exp-window functions
                    ; when *noise-macro* is 0 (currently default), randomx/y will be equal to starting-x/y
                    (randomx (+ starting-x (rand *noise-macro*)))
                    (randomy (+ starting-y (rand *noise-macro*)))
                    ; 6d array to store references to the candidate/party text placed on the experiment window
                    ; this array is associated with the corresponding button's button-map hash table
                    (candidate-party-object-array (make-array '(6)))
                    (contest (pop cntst-lst)) ; get the current contest (race) by popping it out of the list of contests
                    (candidates (cand-lst contest)) ; get the list of candidates for this race of of the current contest
                    (candidate (pop candidates)) ; get the initial candidate out of the list of candidates
                    (y-offset 20) ; offset value for the y coordinates of candidate/party names and corresponding buttons
                    (index 0) ; indexes the candidate name/party in the candidate-party-object-array and maps the button to their race's index in cntst-lst
                    )
                    
                    ; Add the background box for this race to the experiment window
                    (add-image-to-exp-window *window* "box" "lb.gif" :x (- randomx 5) :y (- randomy 5) :width 280 :height 75)
                    
                    ; Add the name of the contest to the experiment window
                    (add-text-to-exp-window *window* (office-name contest) :color 'red :x randomx :y randomy)
                    
                    ; Loop over the candidate names/parties/buttons to add them to the experiment window
                    (loop while candidate
                    do  (progn
                            
                            ; Displays the name of the candidate on the experiment window and stores this in candidate-party-object-array 
                            (setf (aref candidate-party-object-array index) (add-text-to-exp-window *window* (cand-name candidate) :color 'purple :x (+ randomx 30 (rand noise)) :y (+ 1 randomy y-offset (rand noise))))
                            
                            ; Displays the party of the candidate on the experiment window and stores this in candidate-party-object-array
                            (setf (aref candidate-party-object-array (+ index 3)) (add-text-to-exp-window *window* (party-name candidate) :color 'blue :x (+ randomx 200 (rand noise)) :y (+ 1 randomy y-offset (rand noise))))
                            
                            ; Displays the button associated with the candidate and subsequently stores the button's info in hash tables and modifies the button's action
                            (let* (
                                (button (add-button-to-exp-window *window* :color 'white :text "" :x randomx :y (+ randomy y-offset 2) :width 20 :height 10 :action nil))
                                )
                                ;
                                (setf (gethash button button-state) 0) ; sets the button's state to 0 (unselected, white)
                                (setf (gethash button button-map) candidate-party-object-array) ; stores the candidate name/party exp-window objects associated with the button
                                (setf (gethash button button-index) index) ; stores the index of the candidate/party's race in cntst-list
                                (setf (gethash button button-candidate) candidate) ; stores the candidate associated with the button
                                (modify-button-for-exp-window button :action (list 'on-button-press button)) ; modifies the button's action with the (on-button-press) function
                            )
                            
                            ; increment values needed to display/store info for the next candidate in the loop
                            (setf y-offset (+ y-offset 15))
                            (setf candidate (pop candidates))
                            (setf index (+ index 1))
                            
                        ) ; end of progn within candidate loop
                    ) ; end of loop over candidates within a race
                ) ; end of (let*) statement that exp-window functions are called within
                
                ; increment row index and check for break conditions
                (setq j (+ j 1))
                (when (> j 7) (return j)) ; more than 7 rows, break the loop
                (when (not cntst-lst) (return j)) ;checks if we've run out of races
                
            ) ; end of row loop
            
            ; increment column index and check for break conditions
            (setq i (+ i 1))
            (when (> i 2) (return i)) ; more than 3 columns, break the loop
            (when (not cntst-lst) (return i)) ;checks if we've run out of races
        ) ; end of column loop
        
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





































