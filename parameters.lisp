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
;;; Filename    : parameters.lisp
;;; Version     : 1
;;; 
;;; Description : Contains the parameters for the constructed model
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; General Docs: Read in by combined.lisp. Change parameters to change combined model parameters.
;;;
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Parameters
(sgp :v t :esc t :show-focus t :force-visual-commands t)
; (sgp :v nil :esc t :show-focus t :process-cursor nil)

	
	
(sgp :visual-finst-span 10) ;neccesary to avoid forgetting where we looked for the recognition strategies

(sgp :visual-num-finsts 10)

; (sgp :cursor-noise t) ; Turn this on and off to turn cursor noise on and off

; Grouping Parameters
(setf vg-glomming-radius 8)
(setf vg-collision-type 'box) ;'point is faster, but less plausible (also needs a larger radius to work similarly)
(setf vg-naming-type 'sequential) 
		
;********************************
; Enable forgetting / Activation 
(sgp :ans 0.3)
(sgp :rt 0)
;********************************

; (sgp :act t)

(setf *actr-enabled-p* t)

; Declarative Memory Chunk Types
(chunk-type MakeVote instruction keyword race candidate party button position screen state handpos to-do found default endState left right top bottom)
(chunk-type Candidate name party race)
(chunk-type VoteParty default)
(chunk-type Abstain contest)
(chunk-type Instruction keyword area)
(chunk-type VisualGroup race-group candidate-group party-group nextpage-group nextpage-text party-text candidate-text race-text button-group)
		
; First Goal and other chunks
;(add-dm (Vote ISA FindInstr state find-instr))
(add-dm (Vote ISA MakeVote state start-voting default "DEM" endState "nameofrace" direction rightwards anchor nil) (anchor) (direction) (leftwards) (rightwards))