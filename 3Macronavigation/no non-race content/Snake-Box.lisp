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
;;; Filename    : Snake-Box.lisp
;;; Version     : 1
;;; 
;;; Description : Macronavigation strategy
;;;				: * If starting, finds the race in the top left corner. 
;;;
;;; Bugs        : * None known
;;;
;;; To do       :
;;;
;;; ----- History -----
;;; 2020.09.12   Xianni Wang
;;;				: * Created the file; modified from "Snake2.lisp"
;;; 2020.10.08   Xianni Wang
;;;				: * Add productions Find-RaceBox-Same-Row-Right-2 and Find-RaceBox-Same-Row-Right-3
;;;				: * updated productions that deal with "Already-voted" situation
;;; 2020.10.14   Xianni Wang
;;;				: * Add productions Find-RaceBox-Same-Row-Left-2 and Find-RaceBox-Same-Row-Left-3
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; General Docs:
;;;
;;; This model uses the macronavigation strategy: left to right then top to bottom.
;;;   
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Assumes verticles are exactly lines up

(P Find-First-Box

=goal>
	state		start-voting

?imaginal>
	state		free

==>

+visual-location>
	ISA	        visual-location
	kind		image
	screen-left     lowest
	screen-y	lowest
	
=goal>
	state		attending-first-box
        direction	rightwards
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
	state		finding-race-title1	
	
)


; Find race title & attend race title 
;----------------- reuse after encoding race boxes

(P Find-First-Race-Title

=goal>
	state   	finding-race-title1

=visual>
	
=visual-location>
	ISA		visual-location	
	kind		image
	screen-top	=current-top
        screen-bottom   =current-bottom

==>

+visual-location>
	ISA		 visual-location	
	kind		 text
        >= screen-top	 =current-top
        <= screen-bottom =current-bottom
        :nearest         current
        color            red
;        < screen-y      current
;        :nearest	 current

=goal>
	state		attending-race-title1

)

(P Attend-First-Race-Title

=goal>
	state		attending-race-title1
	
?visual>
	state		free

=visual-location>
	ISA		visual-location	
	kind		text

?imaginal>
	state		free

==>

+imaginal>
	race-group		none
	candidate-group  	none
	party-group		none
        button-group            none
 	first-race-col	        nil

+visual>
	ISA		move-attention
	screen-pos	=visual-location
	
=visual-location>

=goal>
	state			storing-race-group
	anchor			=visual-location
		
)


; -----------------------reuse these two productions for the rest of the races -----------


(P Find-Race-Title

=goal>
	state   	finding-race-title

=visual>
	
=visual-location>
	ISA		visual-location	
	kind		image
	screen-top	=current-top
        screen-bottom   =current-bottom

==>

+visual-location>
	ISA		 visual-location	
	kind		 text
        >= screen-top	 =current-top
        <= screen-bottom =current-bottom
        :nearest         current
        color            red
;        < screen-y      current
;        :nearest	 current

=goal>
	state		attending-race-title

)

(P Attend-Race-Title

=goal>
	state		attending-race-title
	
?visual>
	state		free

=visual-location>
	ISA		visual-location	
	kind		text

==>


+visual>
	ISA		move-attention
	screen-pos	=visual-location
	
=visual-location>

=goal>
	state			storing-race-group
;	first-race-col	        true
	anchor			=visual-location
		
)




; ------------------- These next productions find the race box and request a race box in the same row in the current snake direction

; Makes a visual location request for the current race title (Box)
(P Find-Current-Race-Title-Box

=goal>
	state		find-next-race

=visual-location>
	screen-top	=current-top
	
==>

+visual-location>
	ISA		visual-location
        kind            image
        <= screen-top    =current-top
        :nearest        current

	
=goal>
	state		attending-race-title-box

)


; Attend the current race box
(P Attend-Current-Race-Title-Box

=goal>
	state		attending-race-title-box
	
=visual-location>
	ISA	        visual-location	
	kind            image
        screen-right	=current-right
        screen-left     =current-left       

?visual>
	state		free

==>

+visual>
	ISA		move-attention
	screen-pos	=visual-location
	
=visual-location>
	
=goal>
	state		finding-racebox-same-row
	
)


; Makes a visual location request for the race title box
(P Find-RaceBox-Same-Row-Left

=goal>
	state		finding-racebox-same-row
	direction	leftwards
;	anchor		=anchor
	
=visual>

=visual-location>
        kind            image
        screen-right	=current-right
        screen-left     =current-left

==>

=visual>

+visual-location>
	ISA		        visual-location
        kind                    image
	< screen-right	        =current-left
;        >= screen-y             current
	screen-left		highest
        :nearest                current-y
	
=goal>
	state		attending-racebox-same-row

)

; when nagivating to the right,
; Find-RaceBox-Same-Row-Right and Find-RaceBox-Same-Row-Right-2 are used to check
; if there is a racebox on the next row which screen-y value is greater than the current

(P Find-RaceBox-Same-Row-Right

=goal>
	state		finding-racebox-same-row
        direction	rightwards

=visual>

=visual-location>
        kind            image
        screen-right	=current-right
        screen-left     =current-left
==>

=visual>

+visual-location>
        ISA		visual-location
        kind            image
        > screen-left	=current-right
        >= screen-y     current
        screen-left	lowest
        :nearest        current-y

=goal>
	state		finding-racebox-same-row2

)

(P Find-RaceBox-Same-Row-Right-2

=goal>
	state		finding-racebox-same-row2
        direction	rightwards

=visual>

=visual-location>
        kind            image
        screen-right	=current-right
        screen-left     =current-left
==>

=visual>

+visual-location>
        ISA		visual-location
        kind            image
	< screen-right	        =current-left
;        >= screen-top           =current-top
	screen-left		highest
        :nearest                current-y      

=goal>
	state		finding-racebox-same-row3

)


(P Find-RaceBox-Same-Row-Right-3

=goal>
	state		finding-racebox-same-row3
	direction	rightwards
	
=visual>

=visual-location>
        kind            image
	screen-right	=current-right
;	screen-top	=current-top

==>
	
+visual-location>
	ISA		        visual-location
        kind                    image
        > screen-left	        =current-right
        ;>= screen-top          =current-top
 ;       >= screen-y             current
        screen-left		lowest
        :nearest                current-y

=goal>
	state		attending-racebox-same-row

)



; These next productions either pass control to encoding if a race is found or move to the next row

; We have found the next race within this row and so we pass control to encoding process

(P Attend-RaceBox-Same-Row

=goal>
	state		attending-racebox-same-row
	
?visual>
	state		free

=visual-location>
	ISA		visual-location	
	kind		image

=imaginal>

==>

=imaginal>
	race-group		none
	candidate-group  	none
	party-group		none
;        button-group            none
 	first-race-col	        nil

+visual>
	ISA			move-attention
	screen-pos	        =visual-location
	
=visual-location>
	ISA		visual-location	
	kind		image
	
=goal>
	state		finding-race-title
        first-race-col	nil	
)




; This production starts the process of starting a new snake row
(P Find-RaceBox-Same-Row-No-Match

=goal>
	state		attending-racebox-same-row	
	
?visual-location>
	buffer		failure
	
==>

=goal>
	state		find-racebox-new-row

)


; this production strats the process of starting a new row
(P Check-RaceBox-Same-Row-Right

=goal>
	state		finding-racebox-same-row2
        direction	rightwards

?visual-location>
	buffer		failure

==>

=goal>
	state		find-racebox-new-row

)

(P Check-RaceBox-Same-Row-Left

=goal>
	state		finding-racebox-same-row
        direction	leftwards

?visual-location>
	buffer		failure

==>

=goal>
	state		find-racebox-new-row

)

;------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------
; --------------- Finds the next race down and switches directions from right to left
(P Find-Next-Race-Down-Right-Side

=goal>
	state		find-racebox-new-row
	direction	rightwards

==>

+visual-location>
	ISA		visual-location
	;screen-left	current
	kind            image
        > screen-y	current
        <= screen-x     current
        screen-x        highest
	screen-y	lowest
;        :nearest       current-x

=goal>
	state		attending-racebox-new-row
	direction	leftwards

)


; Finds the next race down and switches directions from left to right
(P Find-Next-Race-Down-Left-Side

=goal>
	state		find-racebox-new-row
	direction	leftwards

==>

+visual-location>
	ISA		visual-location
	screen-left	current
	kind            image
	> screen-y	current
	screen-y	lowest

=goal>
	state		attending-racebox-new-row
	direction	rightwards
)

; attend race box new row
(P Attend-RaceBox-New-Row

=goal>
	state		attending-racebox-new-row
	
=visual-location>
	ISA		visual-location	
	kind            image

?visual>
	state		free

=imaginal>

==>

=imaginal>
	race-group		none
	candidate-group  	none
	party-group		none
;        button-group            none
 	first-race-col	        true


+visual>
	ISA		move-attention
	screen-pos	=visual-location

=visual-location>
	
=goal>
	state		finding-race-title
        first-race-col	true
	
)



; ;****************************************
; ; ------------------- If there is nothing found when looking for a new row,
; ; we are at the bottom corner of the ballet and there are no more races, so we can end
(P Find-Race-Next-Row-No-Match

=goal>
	state			attending-racebox-new-row

?visual-location>
	buffer			failure
		
==>

=goal>
	state  			end
	
)
; ;****************************************


;------------------------------------------------------------------------------------
; ------------------- These next productions deal with cases in which we see a race we have already voted on


; Deals with if we've already seen this race and are at the edges
(P Deal-With-Already-Voted-Edges

=goal> 
	state			already-voted

=imaginal>
	first-race-col	        true

==>

=imaginal>

=goal> 
        state                   find-next-race1
)

(P Find-Current-Race-Title-Box-At-Edges

=goal>
	state		find-next-race1
		
==>

+visual-location>
	ISA		visual-location
        kind            image
        :nearest        current

      
=goal>
	state		attending-race-title-box1
)

; Attend the current race box
(P Attend-Current-Race-Title-Box-At-Edges

=goal>
	state		attending-race-title-box1
	
=visual-location>
	ISA	        visual-location	
	kind            image
        screen-right	=current-right
        screen-left     =current-left       

?visual>
	state		free

==>

+visual>
	ISA		move-attention
	screen-pos	=visual-location
	
=visual-location>
	
=goal>
	state		finding-racebox-next-row
	
)

(P Find-RaceBox-Next-Row

=goal>
	state 		finding-racebox-next-row
	
;=visual>

=visual-location>
	ISA		visual-location
        kind            image
	screen-left	=current-left
;        screen-top      =screen-top
==>

;=visual>

; Order is very important here
+visual-location>
	ISA		visual-location
	kind		image
	= screen-left	=current-left
	> screen-y	current
	screen-y	lowest
;        screen-left     lowest
;	screen-y	lowest
	
=goal>
       state	 attending-racebox-new-row

)


; Deals with if we've seen this race and are somewhere in the middle

(P Deal-With-Already-Voted-Col-Middle

=goal> 
	state			already-voted

=imaginal>
	first-race-col	        nil

==>

=imaginal>

=goal> 
	state			find-next-race2
)


(P Find-Current-Race-Title-Box-Middle

=goal>
	state		find-next-race2
		
==>

+visual-location>
	ISA		visual-location
        kind            image
        :nearest        current
      
=goal>
	state		attending-race-title-box

)

