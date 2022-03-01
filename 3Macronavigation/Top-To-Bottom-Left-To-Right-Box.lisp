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
;;; Filename    : Top-To-Bottom-Left-To-Right.lisp
;;; Version     : 1
;;; 
;;; Description : Macronavigation strategy
;;;				: * If starting, finds the race in the top left corner. Otherwise tries to find the next race in the column, or if there is 
;;;				: * no such race the top race in the next column, or if there is no such next column ends the model run.
;;;
;;; Bugs        : * None known
;;;
;;; To do       : * There are a few places where I have had to cheat to make the model work (because we do not have access to relative group 
;;;				: * positios or super and sub groups). These places are documented, but eventually it would be better if they were removed. The
;;;				: * reason these cheats are neccesary is mostly because of navigating a ballot with noise. If one is not using a ballot with noise,
;;;				: * and these cheats are causing problems, they can be removed.
;;;
;;; ----- History -----
;;; 2020.02.10   Xianni Wang
;;;				: * Created the file; modified from "top-to-bottom-left-to-right.lisp"
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; General Docs:
;;;
;;; This model uses the primary macronavigation strategy observed in subjects (at least when they are given an in order ballot): top to bottom and 
;;; then left to right. It only finds the piece of text in the next expected spot; it does not check if that piece of text is a race title.
;;;   
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(P Find-First-Box

=goal>
	state		start-voting

==>

+visual-location>
	ISA	        visual-location
	kind		image
	screen-left     lowest
	screen-y	lowest
	
=goal>
	state		attending-first-box
)

;attend box in the left-top corner
(P Attend-First-Box

=goal>
	state		attending-first-box
	
?visual>
	state		free

=visual-location>
	ISA		visual-location	
	kind		image
		      
==>

+visual>
	ISA			move-attention
	screen-pos	        =visual-location
	
=visual-location>
	ISA		visual-location	
	kind		image
	
=goal>
	state		encoding-first-box	
	
)

(P Encode-First-Box

=goal>
	state   	encoding-first-box

=visual>
	
=visual-location>
	ISA		visual-location	
	kind		image

==>

+visual>
	ISA		move-attention
	screen-pos	=visual-location

=visual-location>

=goal>
	state		finding-race

)

; Find race title
(P Find-First-Race-Title

=goal>
	state   	finding-race

=visual>
	
=visual-location>
	ISA		visual-location	
	kind		image
	screen-top	=current-top
        screen-bottom   =current-bottom

==>

+visual-location>
	ISA		visual-location	
	kind		text
        >= screen-top	 =current-top
        <= screen-bottom =current-bottom
        :nearest         current
        color            red

=goal>
	state		attending-race

)


; We have found the race and so we pass control to encoding process (call relative positions file)
; ; attending race. works either same row or next row
(P Attend-First-Race-Title

=goal>
	state		attending-race

?visual>
	state		free

=visual-location>
	ISA		visual-location	
	kind		text

?imaginal>
        state           free

==>

+imaginal>
	race-group		none
	candidate-group  	none
	party-group		none

+visual>
	ISA			move-attention
	screen-pos	        =visual-location
	
=visual-location>
	ISA		visual-location	
	kind		text

=goal>
	state		storing-race-group

)

; ;------------------------------------------------------------------------------------------------ 
; ;------------------------------------------------------------------------------------------------ 
; ------------------- These next productions find the race box and request a race box in the same column
; ;------------------------------------------------------------------------------------------------ 
; ;------------------------------------------------------------------------------------------------ 

; Makes a visual location request for the current race title (Box)

(P Find-Race-Title-Box

=goal>
	state		find-next-race

=visual-location>
	screen-top	=current-top
	
==>

+visual-location>
	ISA		visual-location
        kind            image
      <= screen-top     =current-top
        :nearest        current

	
=goal>
	state		attending-race-title

)

; Attend the race title box
(P Attend-Race-Title-Box

=goal>
	state		attending-race-title
	
=visual-location>
	ISA	        visual-location	
	kind            image
        
?visual>
	state		free

==>

+visual>
	ISA		move-attention
	screen-pos	=visual-location
	
=visual-location>
	
=goal>
	state		finding-racebox-same-column
	
)

; 
(P Find-RaceBox-Same-Column

=goal>
	state		finding-racebox-same-column

=visual>

=visual-location>
        kind            image
	screen-right	=current-right
	screen-bottom	=current-bottom

==>

+visual-location>
	ISA		        visual-location
        kind                    image
        > screen-top	       =current-bottom
        < screen-left          =current-right
  ;      screen-x                highest
  ;      screen-y                lowest
        :nearest                current

=goal>
	state		attending-racebox-same-column

; !output! ("Last race outline top ~s bottom ~s left ~s right~s" =top-bound =bottom-bound =left =right)

)

; ;
(P Attend-RaceBox-Same-Column

=goal>
	state		attending-racebox-same-column
	
?visual>
	state		free

=visual-location>
	ISA		visual-location	
	kind		image

==>

+visual>
	ISA			move-attention
	screen-pos	        =visual-location
	
=visual-location>
	ISA		visual-location	
	kind		image
	
=goal>
	state		finding-race-2	
	
)

; Find race title
; ; works for races other than the first race
(P Find-Race-Title

=goal>
	state   	finding-race-2

=visual>
	
=visual-location>
	ISA		visual-location	
	kind		image
	screen-top	=current-top
        screen-bottom   =current-bottom

==>

+visual-location>
	ISA		visual-location	
	kind		text
        >= screen-top	 =current-top
        <= screen-bottom =current-bottom
        :nearest         current
        color            red

=goal>
	state		attending-race-2

)

(P Attend-Race-Title

=goal>
	state		attending-race-2

?visual>
	state		free

=visual-location>
	ISA		visual-location	
	kind		text

=imaginal>

==>

=imaginal>
	race-group		none
	candidate-group  	none
	party-group		none

+visual>
	ISA			move-attention
	screen-pos	        =visual-location
	
=visual-location>
	ISA		visual-location	
	kind		text

=goal>
	state		storing-race-group

)


; ;------------------------------------------------------------------------------------------------ 
; ;------------------------------------------------------------------------------------------------ 
;-------------------------------- These next productions either pass control to encoding if a race is found or move to the next column
; ;------------------------------------------------------------------------------------------------ 
; ;------------------------------------------------------------------------------------------------ 

; ; This production being called means that we have reached the end of a row, so we begin the process of finding the top race in the next row
(P Find-RaceBox-Same-Column-No-Match

=goal>
	state		attending-racebox-same-column	

?visual-location>
	buffer		failure
	
==>

=goal>
	state		find-top-race

)

;****************************************
; This is the next of the productions that find the next race if it is in a different column (after find-race-same-column-no-match)
; It finds the top race in this column to prepare for the switch to the next column
(P Find-Top-Race

=goal>
	state		find-top-race
	
==>

+visual-location>
	ISA		visual-location
        kind            image
	screen-top	lowest
	:nearest	current-x

=goal>
	state		attending-top-race

)


;****************************************
; This production attends the top race after we have found its location in preperation for the move to the next column.
(P Attend-Top-Race

=goal>
	state		attending-top-race
	
=visual-location>
	ISA		visual-location	
	kind		image
	
?visual>
	state		free

==>

+visual>
	ISA		move-attention
	screen-pos	=visual-location

=visual-location>
	
=goal>
	state		find-racebox-next-column
	
)


;****************************************
; This production makes the visual location request for the top race in the next column over
; Another big cheat is here: we make a guess (called right-guess) for an x location that is to the right of any text in this column
; and hopefully to the left of any text in the next column
(P Find-RaceBox-Next-Column

=goal>
	state 		find-racebox-next-column
;	right		=right-bound
;	left		=left-bound	
=visual>

=visual-location>
	screen-right	=current-right
==>

+visual-location>
	ISA			visual-location
	kind			image
	> screen-left	        =current-right
	screen-left		lowest
	:nearest		current-y


=goal>
	state		attending-racebox-next-column

)


(P Attend-RaceBox-Next-Column

=goal>
	state		attending-racebox-next-column
		
?visual>
	state		free

=visual-location>
	ISA		visual-location	
	kind		image

==>

+visual>
	ISA			move-attention
	screen-pos	        =visual-location
	
=visual-location>
	ISA		visual-location	
	kind		image
	
=goal>
	state		finding-race-2
	
)


; ;------------------------------------------------------------------------------------------------ 
; ;------------------------------------------------------------------------------------------------ 
; ; If there is nothing found when looking for a new row, we are at the bottom right corner of the ballet and there are no more race boxes, 
; ; so we can end the model
; ;------------------------------------------------------------------------------------------------ 
; ;------------------------------------------------------------------------------------------------ 

(P Find-RaceBox-Next-Row-No-Match

=goal>
	state		attending-racebox-next-column

?visual-location>
	buffer		failure
	
==>

=goal>
	state		end

)

