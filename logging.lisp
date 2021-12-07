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
;;; Filename    : logging.lisp
;;; Version     : 1
;;; 
;;; Description : This file contains a number of logging functions that are read in by combine.lisp and called by the model to track voting.
;;;
;;; Bugs        : * None known
;;;
;;; To do       : * The logging process works for now but there are a number of ways it should be improved
;;;				: * The unlog should be implemented, so that a person could click and then unclick a candidate
;;;				: * The main problem is that choosing to abstain does not have a button trigger. Figuring out a better way to track the model's
;;;				: * decesions is important.
;;;
;;; ----- History -----
;;; 2019.4.28   Joshua Engels
;;;				: * Documented the file
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; General Docs:
;;; Logging works as follos:
;;; - When starting a new list of runs, create-new-file is called, which creates the new output file in data with the timestamp as its name
;;; - Everytime someone votes for a candidate, the button response method calls log-candidate which appends the pertanent infromation to the info lists
;;; - When state next-race is reached, log-finish is called, which checks if any candidate was voted on; if not, it appends an abstension to the lists
;;; - When a ballot is finished by a specific model, the model's results and description is output to the output file in data
;;; The files created are saved in the data file with a time stamp as its title. Runs that one wants to save and analyze can have these file names
;;; changed.
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; Logging parameters used by this file
(defparameter logging-file-name nil)
(defparameter run-number nil)
(defparameter final-strats '())
(defparameter final-candidates '())
(defparameter final-indices '())
(defparameter vote-for-this-race nil)

; Parameters set in micronavigation strategies
(defparameter current-strat nil)
(defparameter current-dm nil)
(defparameter current-micro nil)


;; Sets up the logging functions to write to a new logging file in data with the current time stamp
;; Resets all parameters, so one can call this function to start a new file in the middle of a program (although not recommended behavior)
(defun create-new-file ()

	(setf logging-file-name (concatenate 'string base-file-name "data/" (datestamp) ".tsv" ))
	(setf run-number 1)
	(setf final-strats '())
	(setf final-candidates '())
	(setf final-indices '())

	; logs the header
	(with-open-file (strm logging-file-name :direction :output :if-exists :append :if-does-not-exist :create)
		(format strm "ts	model	dm	run-num	race-num	race	evt-id	data~%")
	)
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


;; Logs the results of a a single ballot run and resets the global variables for the next ballot (to be called at end of ballot function)
(defun log-ballot ()

	(with-open-file (strm logging-file-name :direction :output :if-exists :append :if-does-not-exist :create)
    (format strm "~S	~S	~S	~S	~S	~S	~S	~S~%" (get-time) (read-from-string current-micro) (read-from-string current-dm) run-number '22 'Nameofrace 'VOTE-SUMMARY (list (append final-candidates '(nil nil)) (append final-indices '(nil nil)) final-strats))
	)

	(setf run-number (+ run-number 1))
	(setf final-strats '())
	(setf final-candidates '())
	(setf final-indices '())
	
)


;; Logs a single candidate (to be called within ballot function when button is pressed)
(defun log-candidate (candidate index)

	(setf final-strats (append final-strats (list current-strat)))
	(setf final-candidates (append final-candidates (list candidate)))
	(setf final-indices (append final-indices (list index)))
	(setf vote-for-this-race t)

)


;; Called whenever find-next-race is called in order to track abstentions and resets the boolean
(defun log-finish ()

	(if (not vote-for-this-race) 
		(log-candidate nil nil))
	(setf vote-for-this-race nil)

)


;; TODO, but don't need for now
(defun unlog-candidate ()

)

; Resets the count. To be called whenever something changes on the model level (e.g. a micronavigation strategy change)
(defun reset-count ()
	(setf run-number 1)
)