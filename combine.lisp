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
;;; Filename    : combine.lisp
;;; Version     : 1
;;; 
;;; Description : * Combines the different pieces of a model (rigourously described in the contract pdf) in two useful ways:
;;;             : * One function, run-single, runs a single model composed of specified or default model pieces, useful for testing
;;;             : * The other, run-lists, runs every possible combination of models a given nubmer of times for creating data sets
;;;
;;; Bugs        : * None known
;;;
;;; To do       : * 
;;; 
;;; ----- History -----
;;; 2019.4.24   Joshua Engels
;;;				: * Documented the file, cleaned it up overall
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; General Docs:
;;;
;;; For a general overview, what these functions do is read in the s expressions in order from each type of file: 
;;; ballot, memory, macronavigation, encoding, micronavigation, click as well as a few single files: logging.lisp, parameters.lisp.
;;; They then turn them into a full act r model (just a simple concatentaion and eval) and run the voting function from the loaded ballot
;;; file with specified parameters.
;;; See function descriptions, read me, and contract.pdf for more details
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Base folder name (need to set)
(defparameter base-file-name "/Users/pjr5/gp/VEGA/")
;(defparameter base-file-name "/home/ausmanpa/gp/VEGA/")
;(cond ((string= (software-type) "Darwin") (defparameter base-file-name "/Users/pjr5/gp/VEGA/")
;       (string= (software-type) "Linux") (defparameter base-file-name "/home/ausmanpa/gp/VEGA/")
;      )
;)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; new-name seems to be an act-r function; do we still want to use this?

;(defparameter vg-naming-type 'generic)

;(defun gen-n-syms (n)
;  (case vg-naming-type
;    (sequential
;      (loop for i from 1 to n
;        append (list (new-name "group")) into gps
;        finally (return gps)
;      )
;    )
;    (generic
;      (loop for i from 1 to n
;        append (list (gensym)) into gps
;        finally (return gps)
;      )
;    )
;  )
;)

(defun gen-n-syms (n)
    (loop for i from 1 to n
        append (list (gensym)) into gps
        finally (return gps)
    )
)

(add-act-r-command "gen-n-syms" 'gen-n-syms)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Utility function that should not be called externally
;; Creates a new model in the output file location with the given file names for each piece of the model and then loads this file name
;; into the current lisp environment
;; The nth element of final names should be the nth piece of the model or nil if a default choice is desired
(defun create-specific (file-names)
	
	(print file-names) ; prints the current model makeup, useful for when running many trials
	
	; Loads in the ballot (needs to be a different type than below because it is a straight load instead of an open
	(if (nth 0 file-names) (load (concatenate 'string base-file-name "1Ballot/" (nth 0 file-names) ".lisp"))
											(load (car (directory (concatenate 'string base-file-name "1Ballot/*.lisp")))))


	(let 
	(
		; Gets the name of each file to combine to create the model
		(parameters-file (open (concatenate 'string base-file-name "parameters.lisp")))
		(memory-file (if (nth 1 file-names) (open (concatenate 'string base-file-name "2Memory/" (nth 1 file-names) ".lisp"))
											(open (car (directory (concatenate 'string base-file-name "2Memory/*.lisp"))))))
		(macronavigation-file (if (nth 2 file-names) (open (concatenate 'string base-file-name "3Macronavigation/" (nth 2 file-names) ".lisp"))
											(open (car (directory (concatenate 'string base-file-name "3Macronavigation/*.lisp"))))))
		(encoding-file (if (nth 3 file-names) (open (concatenate 'string base-file-name "4Encoding/" (nth 3 file-names) ".lisp"))
											(open (car (directory (concatenate 'string base-file-name "4Encoding/*.lisp"))))))
		(micronavigation-file (if (nth 4 file-names) (open (concatenate 'string base-file-name "5Micronavigation/" (nth 4 file-names) ".lisp"))
											(open (car (directory (concatenate 'string base-file-name "5Micronavigation/*.lisp"))))))
		(click-file (if (nth 5 file-names) (open (concatenate 'string base-file-name "6Click/" (nth 5 file-names) ".lisp"))
											(open (car (directory (concatenate 'string base-file-name "6Click/*.lisp"))))))
		
		; Start of model, to add to
		(model-combination '(define-model combined))
	)
	

			
		; Function that takes in a file and returns a list of the s-expressions in it
		(defun get-to-add (file)
			(let ((line (read file nil)) (so-far '()))

			(loop while line 
		
				do 	(progn 
					(setf so-far (append so-far (list line)))
					(setf line (read file nil))
				)
			)
			
			(return-from get-to-add so-far)

		))
		
		
		;Creates and writes the model file
		(setf model-combination (append model-combination (get-to-add parameters-file))) ;Adds parameters to the model
		(setf model-combination (append model-combination (get-to-add memory-file))) ;Adds declarative memory to the model
		(setf model-combination (append model-combination (get-to-add macronavigation-file))) ;Adds macronavigation to the model
		(setf model-combination (append model-combination (get-to-add encoding-file))) ;Adds encoding to the model
		(setf model-combination (append model-combination (get-to-add micronavigation-file))) ;Adds micronavigation to the model
		(setf model-combination (append model-combination (get-to-add click-file))) ;Adds click to the model
	
		
		(setf model-combination (append model-combination (list '(goal-focus Vote)))) ;Adds the start goal focus
		
		; Loads model into this file
		(clear-all)
		(eval model-combination)
	)
	
	
	

)

;; Runs a single model on a single ballot
;; List of components that specify the model is the first parameter, in the same format as the create-specific function above
;; Whether to run the model in realtime or not is the second parameter (only relevant if use-model, the next parameter, is true)
;; Whether to use the model or not (just for a human) is the third parameter 
;; Whether to make the ballot visible or not is the fourth parameter
;; Example run if one wants to test how a ballot looks: "(run-single '(ballot-name nil nil nil nil nil) t nil t)" 
;; (in this case the realtime parameter does not matter)
(defun run-single (file-names realtime use-model visible)

	(create-specific file-names)
	;(vote realtime use-model visible t)
	
	;(run-program "python" '("/home/ausmanpa/gp/VEGA/visual_grouping.py") :wait nil :output nil)
	;(run-program "konsole" '("-e" "python -i /home/ausmanpa/gp/VEGA/visual_grouping.py") :wait nil) ;Better - does not require the python while loop+(clear-all)!
	;(vote realtime use-model visible t)
	;(clear-all) ; necessary to halt the python background process

)

;; A utility function that gets the date stamp in the form year-month-day_hour-minute-second
(defun datestamp ()
  (multiple-value-bind
    (second minute hour day month year day-of-week dst-p tz)
    (get-decoded-time)
    (declare (ignore day-of-week dst-p tz))
    (format nil "~4d-~2,'0d-~2,'0d_~2,'0d-~2,'0d-~2,'0d"
	      year month day hour minute second))
)


(defun test-ranges ()
	
	(setf logging-file-name (concatenate 'string base-file-name "data/" (datestamp) ".csv"))
	(setf *random-state* (make-random-state t)) ;; randomly initialized by some means

;(loop for i from 1 to 3 do
	(loop for race-spacing from 5 to 10 do
		(loop for after-title-spacing from 20 to 22 do
			(loop for between-candidates-spacing from 15 to 18 do (
				progn
				(create-specific '(nil nil nil nil nil nil))
				(vote nil t nil nil (list race-spacing after-title-spacing between-candidates-spacing))
				(setf data (append race-sizes-column (list race-sizes race-indexes-voted-on race-parameters)))
				(with-open-file (strm logging-file-name :direction :output :if-exists :append :if-does-not-exist :create)
					(format strm "~{~a~^, ~}~%"  (mapcar #'(lambda (x) (format nil "~{~a~^ ~}" x)) data)))))))
;)

)


(defun test-ranges2 ()

	
	(setf logging-file-name (concatenate 'string base-file-name "data/" (datestamp) ".csv" ))
	

				(loop for i from 0 to 5000 do (
					progn
					(create-specific '(nil nil nil nil nil nil))
					(vote nil t nil nil)
					(setf data (append race-sizes-column (list race-sizes race-indexes-voted-on race-parameters)))
					(with-open-file (strm logging-file-name :direction :output :if-exists :append :if-does-not-exist :create)
						(format strm "~{~a~^, ~}~%"  (mapcar #'(lambda (x) (format nil "~{~a~^ ~}" x)) data)))))

)


;; Used to run every single possible combination of strategies and log them all into a single file
;; Input is the number of trials to run for each combination of strategies and whether to make the models visible,
;; output is a file logged with the date and time 
;; Example: to create the 500 run file from earlier research, call (run-lists 500 nil)
(defun run-lists (num-trials visible)

	(load (concatenate 'string base-file-name "logging.lisp")) ;load in the logging functions
	(create-new-file) ;create a new logging file
	
	(let (
	
	
		(micros '(
				;"VG-Random-Retrieve-Recognize-Party"
				;"VG-Serial-Recognize-Party"
			        ;"VG-Serial-Retrieve-Party"
				;"VG-Serial-RetrieveParty-Party"
				;"VG-Serial-Retrieve-Recognize-Party"
				;"VG-Random-Recognize-Party"
				;"VG-Random-Retrieve-Party"
				;"VG-Random-RetrieveParty-Party"
				))
		
		(dms '(
			"all-perfect"
			;"all-rolloff"
			;"most-rolloff"
			;"most-perfect"
			;"full-dm"
			))
			
			
		)
		
		; Loops through every possible micro and dm and runs that model num-trial number of times and logs results
		(loop for micro in micros
		do (loop for dm in dms do (progn
			(setf current-dm dm)
			(setf current-micro micro)
			(reset-count)
			(dotimes (trial num-trials)
				(progn 
				(print trial)
				(create-specific (list nil dm nil nil micro nil))
				(vote nil t visible t))
				)
			)))
			
))

